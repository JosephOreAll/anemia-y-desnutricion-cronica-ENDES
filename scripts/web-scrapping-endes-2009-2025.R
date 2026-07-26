# ============================================================================
# CONSTRUCCIÓN DE INDICADORES ENDES 2009-2025 EN RSTUDIO
# Anemia infantil y desnutrición crónica infantil por departamento y área
# ============================================================================

setwd("C:/Users/Joseph/Documents/TRABAJO FINAL - ORELLANA ALLPOC JOSEPH")
getwd()

# EXPLICACIÓN GENERAL
# -------------------
# Este programa realiza una descarga automatizada de las bases oficiales de la
# Encuesta Demográfica y de Salud Familiar (ENDES) publicadas por el INEI.
# Habitualmente se le llama "web scraping", aunque técnicamente no copiamos una
# tabla visible de una página web. R ingresa a las direcciones oficiales,
# descarga los archivos ZIP, extrae las bases SPSS y procesa las variables.
#
# El procedimiento económico-estadístico es el siguiente:
#
# 1. Para cada año entre 2009 y 2025 se identifican los módulos oficiales.
# 2. Se descargan y guardan tres bases originales:
#       RECH0: datos generales del hogar, región, área y peso muestral.
#       RECH1: relación de integrantes del hogar y condición de facto.
#       RECH6: antropometría y hemoglobina de niñas y niños.
# 3. RECH6 se une con RECH1 usando dos llaves:
#       HHID = identificación del hogar.
#       HC0  = número de orden de la niña o niño en el hogar.
#       HVIDX = número de orden de la persona en RECH1.
# 4. Luego se incorpora RECH0 mediante HHID.
# 5. Se considera únicamente la población de facto para los indicadores,
#    es decir, niñas y niños que durmieron en el hogar la noche anterior:
#       HV103 == 1.
# 6. Para anemia se exige además cuestionario completo (HV015 == 1) y
#    medición válida de hemoglobina (HC55 == 0).
# 7. Se calculan porcentajes ponderados con el factor de medición infantil:
#       2015: HV005X.
#       2020: HV005A, disponible en RECH6.
#       Demás años: HV005.
#    Todos los factores se dividen entre 1 000 000.
# 8. Se guardan los resultados por año y una base consolidada 2009-2025.

# ============================================================================

# 0. CONFIGURACIÓN DEL DIRECTORIO -------------------------------------------
# Antes de ejecutar el script se puede elegir una carpeta con setwd().

CARPETA_PRINCIPAL <- file.path(
  getwd(),
  "ENDES_2009_2025"
)

dir.create(
  CARPETA_PRINCIPAL,
  recursive = TRUE,
  showWarnings = FALSE
)

FORZAR_REPROCESAMIENTO <- FALSE

VERSION_METODO <- "RECH1_HV103_HV015_HC55_HV005X2015_HV005A2020_2026_07"

# 1. PAQUETES ---------------------------------------------------------------
paquetes <- c(
  "curl",
  "haven",
  "dplyr",
  "tibble",
  "openxlsx"
)

faltan <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltan) > 0) {
  install.packages(faltan)
}

invisible(lapply(paquetes, library, character.only = TRUE))

# Algunas bases del INEI son pesadas. Se amplía el tiempo de espera.
options(timeout = 1800)

# 2. CATÁLOGO DE ENCUESTAS Y MÓDULOS ---------------------------------------
# El módulo de hogar contiene RECH0 y RECH1.
# El módulo de antropometría y anemia contiene RECH6.

catalogo <- tibble::tibble(
  anio = 2009:2025,
  codigo = c(
    238, 260, 290, 323, 407, 441, 504, 548, 605,
    638, 691, 739, 760, 786, 910, 968, 1036
  ),
  modulo_hogar = c(rep(64L, 11), rep(1629L, 6)),
  modulo_ninos = c(rep(74L, 11), rep(1638L, 6))
)

REGIONES <- c(
  `1` = "Amazonas",
  `2` = "Áncash",
  `3` = "Apurímac",
  `4` = "Arequipa",
  `5` = "Ayacucho",
  `6` = "Cajamarca",
  `7` = "Callao",
  `8` = "Cusco",
  `9` = "Huancavelica",
  `10` = "Huánuco",
  `11` = "Ica",
  `12` = "Junín",
  `13` = "La Libertad",
  `14` = "Lambayeque",
  `15` = "Lima",
  `16` = "Loreto",
  `17` = "Madre de Dios",
  `18` = "Moquegua",
  `19` = "Pasco",
  `20` = "Piura",
  `21` = "Puno",
  `22` = "San Martín",
  `23` = "Tacna",
  `24` = "Tumbes",
  `25` = "Ucayali"
)

AREAS <- c(
  Total = NA_integer_,
  Urbano = 1L,
  Rural = 2L
)

# Direcciones oficiales que se prueban en orden.
BASES_INEI <- c(
  "https://proyectos.inei.gob.pe/iinei/srienaho/descarga/SPSS/",
  "https://proyectos.inei.gob.pe/iinei/srienaho/Descarga/SPSS/",
  "http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/"
)

# En 2025 los enlaces directos pueden responder HTTP 404.
# Este paquete complementario oficial funciona como fuente alternativa.
URL_PUBLICACION_ENDES_2025 <- paste0(
  "https://www.inei.gob.pe/media/MenuRecursivo/",
  "publicaciones_digitales/Est/Lib2089/",
  "informacion_complementaria.zip"
)

# 3. FUNCIONES PARA DESCARGAR Y EXTRAER ARCHIVOS ---------------------------

archivo_valido <- function(ruta, minimo_bytes = 1000) {
  file.exists(ruta) &&
    !is.na(file.info(ruta)$size) &&
    file.info(ruta)$size > minimo_bytes
}

es_zip_valido <- function(ruta) {
  if (!archivo_valido(ruta)) {
    return(FALSE)
  }

  conexion <- file(ruta, open = "rb")
  on.exit(close(conexion), add = TRUE)

  firma <- readBin(
    conexion,
    what = "raw",
    n = 2
  )

  length(firma) == 2 && identical(firma, charToRaw("PK"))
}

# Descarga un módulo ZIP en la carpeta temporal del sistema.
# El ZIP no queda almacenado en la carpeta de trabajo.
descargar_zip_temporal <- function(codigo, modulo) {
  nombre_zip <- paste0(codigo, "-Modulo", modulo, ".zip")
  errores <- character()

  for (base in BASES_INEI) {
    url <- paste0(base, nombre_zip)
    destino <- tempfile(fileext = ".zip")

    handle <- curl::new_handle(
      followlocation = TRUE,
      connecttimeout = 60,
      timeout = 1800,
      useragent = "Mozilla/5.0 R-ENDES"
    )

    respuesta <- try(
      curl::curl_download(
        url = url,
        destfile = destino,
        quiet = TRUE,
        mode = "wb",
        handle = handle
      ),
      silent = TRUE
    )

    if (!inherits(respuesta, "try-error") && es_zip_valido(destino)) {
      return(list(ruta = destino, url = url))
    }

    detalle <- if (inherits(respuesta, "try-error")) {
      as.character(respuesta)
    } else {
      "La respuesta no es un ZIP válido"
    }

    errores <- c(
      errores,
      paste(url, detalle, sep = " -> ")
    )

    unlink(destino, force = TRUE)
  }

  stop(
    "No se pudo descargar ", nombre_zip, ".\n",
    paste(errores, collapse = "\n")
  )
}

# Descarga un ZIP desde una dirección específica.
descargar_url_temporal <- function(url, etiqueta = "archivo") {
  destino <- tempfile(fileext = ".zip")

  handle <- curl::new_handle(
    followlocation = TRUE,
    connecttimeout = 60,
    timeout = 2400,
    useragent = "Mozilla/5.0 R-ENDES"
  )

  respuesta <- try(
    curl::curl_download(
      url = url,
      destfile = destino,
      quiet = TRUE,
      mode = "wb",
      handle = handle
    ),
    silent = TRUE
  )

  if (inherits(respuesta, "try-error") || !es_zip_valido(destino)) {
    unlink(destino, force = TRUE)

    stop(
      "No se pudo descargar ", etiqueta, ".\n",
      url, " -> ", as.character(respuesta)
    )
  }

  list(ruta = destino, url = url)
}

# Localiza y extrae una base SAV concreta.
# useBytes = TRUE evita errores por nombres de PDF mal codificados, como el
# problema encontrado dentro del ZIP de 2013.
extraer_sav_desde_zip <- function(zip_path, nombre_sav, ruta_destino) {
  inventario <- utils::unzip(zip_path, list = TRUE)

  patron <- paste0(
    "(^|[/\\\\])",
    nombre_sav,
    "(_[0-9]{4})?\\.sav$"
  )

  candidatos <- grep(
    patron,
    inventario$Name,
    ignore.case = TRUE,
    useBytes = TRUE
  )

  if (length(candidatos) == 0) {
    stop(
      "No se encontró ",
      nombre_sav,
      ".SAV dentro del ZIP."
    )
  }

  elegido <- candidatos[
    which.min(
      nchar(
        inventario$Name[candidatos],
        type = "bytes"
      )
    )
  ]

  carpeta_temporal <- tempfile(pattern = "endes_extraer_")
  dir.create(carpeta_temporal, recursive = TRUE)

  on.exit(
    unlink(
      carpeta_temporal,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  utils::unzip(
    zipfile = zip_path,
    files = inventario$Name[elegido],
    exdir = carpeta_temporal,
    junkpaths = TRUE,
    overwrite = TRUE
  )

  archivo_extraido <- list.files(
    carpeta_temporal,
    pattern = paste0(
      "^",
      nombre_sav,
      "(_[0-9]{4})?\\.sav$"
    ),
    ignore.case = TRUE,
    full.names = TRUE
  )

  if (length(archivo_extraido) == 0) {
    stop(
      "El ZIP contiene ",
      nombre_sav,
      ", pero no se pudo extraer."
    )
  }

  dir.create(
    dirname(ruta_destino),
    recursive = TRUE,
    showWarnings = FALSE
  )

  copiado <- file.copy(
    from = archivo_extraido[1],
    to = ruta_destino,
    overwrite = TRUE,
    copy.date = TRUE
  )

  if (!copiado || !archivo_valido(ruta_destino)) {
    stop(
      "No se pudo guardar la base en: ",
      ruta_destino
    )
  }

  invisible(ruta_destino)
}

# Descarga una sola vez el módulo de hogar y extrae RECH0 y RECH1.
descargar_modulo_hogar <- function(codigo, modulo, ruta_rech0, ruta_rech1) {
  descarga <- descargar_zip_temporal(codigo, modulo)
  on.exit(unlink(descarga$ruta, force = TRUE), add = TRUE)

  if (!archivo_valido(ruta_rech0)) {
    extraer_sav_desde_zip(
      zip_path = descarga$ruta,
      nombre_sav = "RECH0",
      ruta_destino = ruta_rech0
    )
  }

  if (!archivo_valido(ruta_rech1)) {
    extraer_sav_desde_zip(
      zip_path = descarga$ruta,
      nombre_sav = "RECH1",
      ruta_destino = ruta_rech1
    )
  }

  descarga$url
}

# Descarga el módulo de niñas y niños y extrae RECH6.
descargar_modulo_ninos <- function(codigo, modulo, ruta_rech6) {
  descarga <- descargar_zip_temporal(codigo, modulo)
  on.exit(unlink(descarga$ruta, force = TRUE), add = TRUE)

  extraer_sav_desde_zip(
    zip_path = descarga$ruta,
    nombre_sav = "RECH6",
    ruta_destino = ruta_rech6
  )

  descarga$url
}

# Busca un SAV dentro de un ZIP y también dentro de ZIP anidados.
# Se usa principalmente con el paquete complementario oficial de 2025.
buscar_sav_recursivo <- function(
  zip_path,
  nombre_sav,
  carpeta_temporal,
  nivel = 0L,
  max_nivel = 6L
) {
  inventario <- try(
    utils::unzip(zip_path, list = TRUE),
    silent = TRUE
  )

  if (inherits(inventario, "try-error")) {
    return(NULL)
  }

  patron_sav <- paste0(
    "(^|[/\\\\])",
    nombre_sav,
    "(_[0-9]{4})?\\.sav$"
  )

  candidatos_sav <- grep(
    patron_sav,
    inventario$Name,
    ignore.case = TRUE,
    useBytes = TRUE
  )

  if (length(candidatos_sav) > 0) {
    elegido <- candidatos_sav[
      which.min(
        nchar(
          inventario$Name[candidatos_sav],
          type = "bytes"
        )
      )
    ]

    carpeta_sav <- file.path(
      carpeta_temporal,
      paste0("sav_nivel_", nivel, "_", nombre_sav)
    )

    dir.create(
      carpeta_sav,
      recursive = TRUE,
      showWarnings = FALSE
    )

    extraido <- try(
      utils::unzip(
        zipfile = zip_path,
        files = inventario$Name[elegido],
        exdir = carpeta_sav,
        junkpaths = TRUE,
        overwrite = TRUE
      ),
      silent = TRUE
    )

    if (!inherits(extraido, "try-error")) {
      archivo <- list.files(
        carpeta_sav,
        pattern = paste0(
          "^",
          nombre_sav,
          "(_[0-9]{4})?\\.sav$"
        ),
        ignore.case = TRUE,
        full.names = TRUE
      )

      if (length(archivo) > 0) {
        return(archivo[1])
      }
    }
  }

  if (nivel >= max_nivel) {
    return(NULL)
  }

  candidatos_zip <- grep(
    "\\.zip$",
    inventario$Name,
    ignore.case = TRUE,
    useBytes = TRUE
  )

  if (length(candidatos_zip) == 0) {
    return(NULL)
  }

  for (j in seq_along(candidatos_zip)) {
    indice <- candidatos_zip[j]

    carpeta_zip <- file.path(
      carpeta_temporal,
      paste0("zip_nivel_", nivel, "_", j)
    )

    dir.create(
      carpeta_zip,
      recursive = TRUE,
      showWarnings = FALSE
    )

    extraido <- try(
      utils::unzip(
        zipfile = zip_path,
        files = inventario$Name[indice],
        exdir = carpeta_zip,
        junkpaths = TRUE,
        overwrite = TRUE
      ),
      silent = TRUE
    )

    if (inherits(extraido, "try-error")) {
      next
    }

    zip_interno <- list.files(
      carpeta_zip,
      pattern = "\\.zip$",
      ignore.case = TRUE,
      full.names = TRUE
    )

    if (length(zip_interno) == 0) {
      next
    }

    for (ruta_zip_interno in zip_interno) {
      encontrado <- buscar_sav_recursivo(
        zip_path = ruta_zip_interno,
        nombre_sav = nombre_sav,
        carpeta_temporal = carpeta_temporal,
        nivel = nivel + 1L,
        max_nivel = max_nivel
      )

      if (!is.null(encontrado)) {
        return(encontrado)
      }
    }
  }

  NULL
}

# Extrae RECH0, RECH1 y RECH6 desde el paquete complementario de 2025.
extraer_savs_paquete_2025 <- function(
  zip_path,
  ruta_rech0,
  ruta_rech1,
  ruta_rech6
) {
  carpeta_temporal <- tempfile(pattern = "endes_2025_")
  dir.create(carpeta_temporal, recursive = TRUE)

  on.exit(
    unlink(
      carpeta_temporal,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  rutas_destino <- c(
    RECH0 = ruta_rech0,
    RECH1 = ruta_rech1,
    RECH6 = ruta_rech6
  )

  for (nombre_sav in names(rutas_destino)) {
    if (archivo_valido(rutas_destino[[nombre_sav]])) {
      next
    }

    encontrado <- buscar_sav_recursivo(
      zip_path = zip_path,
      nombre_sav = nombre_sav,
      carpeta_temporal = carpeta_temporal
    )

    if (is.null(encontrado)) {
      stop(
        "No se encontró ",
        nombre_sav,
        ".SAV en el paquete complementario ENDES 2025."
      )
    }

    copiado <- file.copy(
      from = encontrado,
      to = rutas_destino[[nombre_sav]],
      overwrite = TRUE,
      copy.date = TRUE
    )

    if (!copiado || !archivo_valido(rutas_destino[[nombre_sav]])) {
      stop(
        "No se pudo guardar ",
        nombre_sav,
        " de ENDES 2025."
      )
    }
  }

  invisible(rutas_destino)
}

# 4. OBTENER LAS TRES BASES DE CADA AÑO -----------------------------------

obtener_bases_anuales <- function(
  anio,
  codigo,
  modulo_hogar,
  modulo_ninos
) {
  carpeta_anio <- file.path(
    CARPETA_PRINCIPAL,
    as.character(anio)
  )

  dir.create(
    carpeta_anio,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ruta_rech0 <- file.path(
    carpeta_anio,
    paste0("RECH0_", anio, ".sav")
  )

  ruta_rech1 <- file.path(
    carpeta_anio,
    paste0("RECH1_", anio, ".sav")
  )

  ruta_rech6 <- file.path(
    carpeta_anio,
    paste0("RECH6_", anio, ".sav")
  )

  ruta_fuente <- file.path(
    carpeta_anio,
    "FUENTE_DESCARGA.txt"
  )

  rutas <- c(
    RECH0 = ruta_rech0,
    RECH1 = ruta_rech1,
    RECH6 = ruta_rech6
  )

  todas_validas <- all(
    vapply(
      rutas,
      archivo_valido,
      logical(1)
    )
  )

  if (todas_validas) {
    message("  Usando RECH0, RECH1 y RECH6 ya guardadas para ", anio, ".")

    return(list(
      rech0 = ruta_rech0,
      rech1 = ruta_rech1,
      rech6 = ruta_rech6
    ))
  }

  message("  Descargando las bases oficiales de ", anio, "...")

  descarga_directa <- try({
    fuentes <- character()

    if (!archivo_valido(ruta_rech0) || !archivo_valido(ruta_rech1)) {
      fuentes["hogar"] <- descargar_modulo_hogar(
        codigo = codigo,
        modulo = modulo_hogar,
        ruta_rech0 = ruta_rech0,
        ruta_rech1 = ruta_rech1
      )
    } else {
      fuentes["hogar"] <- "Bases de hogar reutilizadas desde el directorio"
    }

    if (!archivo_valido(ruta_rech6)) {
      fuentes["ninos"] <- descargar_modulo_ninos(
        codigo = codigo,
        modulo = modulo_ninos,
        ruta_rech6 = ruta_rech6
      )
    } else {
      fuentes["ninos"] <- "RECH6 reutilizada desde el directorio"
    }

    fuentes
  }, silent = TRUE)

  if (!inherits(descarga_directa, "try-error")) {
    writeLines(
      c(
        paste0("Año ENDES: ", anio),
        paste0("Versión del método: ", VERSION_METODO),
        "Método: descarga directa desde Microdatos del INEI",
        paste0("Módulo hogar: ", descarga_directa["hogar"]),
        paste0("Módulo niñas y niños: ", descarga_directa["ninos"]),
        paste0("Fecha y hora: ", Sys.time())
      ),
      con = ruta_fuente,
      useBytes = TRUE
    )
  } else if (anio == 2025) {
    message(
      "  Los enlaces directos de 2025 no están disponibles. ",
      "Se usará el paquete complementario oficial..."
    )

    paquete_2025 <- descargar_url_temporal(
      URL_PUBLICACION_ENDES_2025,
      etiqueta = "paquete complementario ENDES 2025"
    )

    on.exit(
      unlink(paquete_2025$ruta, force = TRUE),
      add = TRUE
    )

    extraer_savs_paquete_2025(
      zip_path = paquete_2025$ruta,
      ruta_rech0 = ruta_rech0,
      ruta_rech1 = ruta_rech1,
      ruta_rech6 = ruta_rech6
    )

    writeLines(
      c(
        paste0("Año ENDES: ", anio),
        paste0("Versión del método: ", VERSION_METODO),
        "Método: paquete complementario oficial ENDES 2025",
        paste0("Fuente: ", paquete_2025$url),
        paste0("Fecha y hora: ", Sys.time())
      ),
      con = ruta_fuente,
      useBytes = TRUE
    )
  } else {
    stop(as.character(descarga_directa))
  }

  todas_validas <- all(
    vapply(
      rutas,
      archivo_valido,
      logical(1)
    )
  )

  if (!todas_validas) {
    faltantes <- names(rutas)[
      !vapply(rutas, archivo_valido, logical(1))
    ]

    stop(
      "No se guardaron correctamente las siguientes bases de ",
      anio,
      ": ",
      paste(faltantes, collapse = ", ")
    )
  }

  list(
    rech0 = ruta_rech0,
    rech1 = ruta_rech1,
    rech6 = ruta_rech6
  )
}

# 5. FUNCIONES ESTADÍSTICAS ------------------------------------------------

# La media ponderada de un indicador 0/1 multiplicada por 100 produce el
# porcentaje estimado de la población representada por la muestra.
media_ponderada_pct <- function(indicador, peso) {
  validos <- !is.na(indicador) &
    !is.na(peso) &
    peso > 0

  if (!any(validos)) {
    return(NA_real_)
  }

  suma_pesos <- sum(peso[validos])

  if (!is.finite(suma_pesos) || suma_pesos <= 0) {
    return(NA_real_)
  }

  100 * stats::weighted.mean(
    indicador[validos],
    peso[validos]
  )
}

n_valido <- function(indicador, peso) {
  validos <- !is.na(indicador) &
    !is.na(peso) &
    peso > 0

  as.integer(sum(validos))
}

# Se utiliza el mismo factor oficial de medición infantil para los resultados
# nacionales y departamentales. El factor cambia únicamente en 2015 y 2020.
resumir_grupo <- function(
  datos,
  anio,
  ambito,
  region_codigo,
  region_nombre,
  area
) {
  peso_usado <- if (ambito == "Regional") {
    datos$peso_regional
  } else {
    datos$peso_nacional
  }

  tibble::tibble(
    anio = anio,
    ambito = ambito,
    region_codigo = region_codigo,
    region_nombre = region_nombre,
    area = area,
    n_obs_rech6 = nrow(datos),
    n_ninos_de_facto = as.integer(
      sum(datos$HV103 == 1, na.rm = TRUE)
    ),
    n_anemia_comparable = n_valido(
      datos$ana_comp_ind,
      peso_usado
    ),
    anemia_6_35_comparable_pct = media_ponderada_pct(
      datos$ana_comp_ind,
      peso_usado
    ),
    n_anemia_nueva = n_valido(
      datos$ana_nueva_ind,
      peso_usado
    ),
    anemia_6_35_nueva_pct = media_ponderada_pct(
      datos$ana_nueva_ind,
      peso_usado
    ),
    n_dci = n_valido(
      datos$dci_ind,
      peso_usado
    ),
    dci_menores5_pct = media_ponderada_pct(
      datos$dci_ind,
      peso_usado
    ),
    dci_severa_menores5_pct = media_ponderada_pct(
      datos$dci_severa_ind,
      peso_usado
    )
  )
}

# 6. FUNCIONES PARA CREAR EXCEL --------------------------------------------

estilo_cabecera <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#1F4E78",
  textDecoration = "bold",
  halign = "center",
  valign = "center"
)

estilo_decimal <- openxlsx::createStyle(
  numFmt = "0.0"
)

agregar_hoja_formateada <- function(wb, nombre_hoja, datos) {
  openxlsx::addWorksheet(wb, nombre_hoja)

  openxlsx::writeData(
    wb = wb,
    sheet = nombre_hoja,
    x = datos,
    withFilter = nrow(datos) > 0,
    keepNA = FALSE
  )

  if (ncol(datos) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = nombre_hoja,
      style = estilo_cabecera,
      rows = 1,
      cols = seq_len(ncol(datos)),
      gridExpand = TRUE
    )

    openxlsx::setColWidths(
      wb = wb,
      sheet = nombre_hoja,
      cols = seq_len(ncol(datos)),
      widths = "auto"
    )
  }

  openxlsx::freezePane(
    wb,
    nombre_hoja,
    firstRow = TRUE
  )

  columnas_pct <- grep("_pct$|diferencia_pp$", names(datos))

  if (length(columnas_pct) > 0 && nrow(datos) > 0) {
    openxlsx::addStyle(
      wb = wb,
      sheet = nombre_hoja,
      style = estilo_decimal,
      rows = 2:(nrow(datos) + 1),
      cols = columnas_pct,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}

# 7. PROCESAR UN AÑO -------------------------------------------------------

procesar_anio <- function(
  anio,
  codigo,
  modulo_hogar,
  modulo_ninos
) {
  message("Procesando ENDES ", anio, "...")

  carpeta_anio <- file.path(
    CARPETA_PRINCIPAL,
    as.character(anio)
  )

  dir.create(
    carpeta_anio,
    recursive = TRUE,
    showWarnings = FALSE
  )

  ruta_rds <- file.path(
    carpeta_anio,
    paste0(
      "INDICADORES_ENDES_CORREGIDOS_",
      anio,
      ".rds"
    )
  )

  ruta_excel <- file.path(
    carpeta_anio,
    paste0(
      "INDICADORES_ENDES_CORREGIDOS_",
      anio,
      ".xlsx"
    )
  )

  # El RDS corregido permite continuar luego de cerrar RStudio.
  # No se usan los RDS creados por el código anterior.
  if (
    !FORZAR_REPROCESAMIENTO &&
    archivo_valido(ruta_rds, minimo_bytes = 100)
  ) {
    objeto_guardado <- readRDS(ruta_rds)

    if (
      is.list(objeto_guardado) &&
      identical(objeto_guardado$version_metodo, VERSION_METODO)
    ) {
      message("  El año ", anio, " ya estaba procesado con el método corregido.")
      return(objeto_guardado)
    }
  }

  archivos <- obtener_bases_anuales(
    anio = anio,
    codigo = codigo,
    modulo_hogar = modulo_hogar,
    modulo_ninos = modulo_ninos
  )

  # Leemos los archivos SPSS. user_na = FALSE convierte los valores perdidos
  # definidos por SPSS en NA, lo cual facilita los cálculos en R.
  rech0 <- haven::read_sav(
    archivos$rech0,
    user_na = FALSE
  )

  rech1 <- haven::read_sav(
    archivos$rech1,
    user_na = FALSE
  )

  rech6 <- haven::read_sav(
    archivos$rech6,
    user_na = FALSE
  )

  names(rech0) <- toupper(names(rech0))
  names(rech1) <- toupper(names(rech1))
  names(rech6) <- toupper(names(rech6))

  requeridas_rech0 <- c(
    "HHID",
    "HV005",
    "HV015",
    "HV024",
    "HV025"
  )

  requeridas_rech1 <- c(
    "HHID",
    "HVIDX",
    "HV103"
  )

  requeridas_rech6 <- c(
    "HHID",
    "HC0",
    "HC1",
    "HC55",
    "HC57",
    "HC70"
  )

  faltan_rech0 <- setdiff(
    requeridas_rech0,
    names(rech0)
  )

  faltan_rech1 <- setdiff(
    requeridas_rech1,
    names(rech1)
  )

  faltan_rech6 <- setdiff(
    requeridas_rech6,
    names(rech6)
  )

  if (length(faltan_rech0) > 0) {
    stop(
      "Faltan variables en RECH0 de ",
      anio,
      ": ",
      paste(faltan_rech0, collapse = ", ")
    )
  }

  if (length(faltan_rech1) > 0) {
    stop(
      "Faltan variables en RECH1 de ",
      anio,
      ": ",
      paste(faltan_rech1, collapse = ", ")
    )
  }

  if (length(faltan_rech6) > 0) {
    stop(
      "Faltan variables en RECH6 de ",
      anio,
      ": ",
      paste(faltan_rech6, collapse = ", ")
    )
  }

  # HC57A corresponde a la metodología nueva de anemia y no está presente en
  # todos los años. Si no existe, se crea como una columna vacía.
  if (!"HC57A" %in% names(rech6)) {
    rech6$HC57A <- NA_real_
  }

  # HV005X es el factor especial indicado por el INEI para 2015 y se
  # encuentra en RECH0. HV005A es el factor de mediciones infantiles de 2020
  # y se encuentra en RECH6. Para los demás años se utiliza HV005 de RECH0.
  if (!"HV005X" %in% names(rech0)) {
    rech0$HV005X <- NA_real_
  }

  if (anio == 2020 && !"HV005A" %in% names(rech6)) {
    stop(
      "En RECH6 de 2020 no se encontró HV005A, ",
      "factor oficial de mediciones infantiles para ese año."
    )
  }

  if (!"HV005A" %in% names(rech6)) {
    rech6$HV005A <- NA_real_
  }

  hogar <- rech0 |>
    dplyr::select(
      HHID,
      HV005,
      HV005X,
      HV015,
      HV024,
      HV025
    ) |>
    dplyr::mutate(
      HHID = trimws(as.character(HHID)),
      dplyr::across(
        -HHID,
        ~ as.numeric(.x)
      )
    ) |>
    dplyr::distinct(
      HHID,
      .keep_all = TRUE
    )

  miembros <- rech1 |>
    dplyr::select(
      HHID,
      HVIDX,
      HV103
    ) |>
    dplyr::mutate(
      HHID = trimws(as.character(HHID)),
      HVIDX = as.numeric(HVIDX),
      HV103 = as.numeric(HV103)
    ) |>
    dplyr::distinct(
      HHID,
      HVIDX,
      .keep_all = TRUE
    )

  ninos <- rech6 |>
    dplyr::select(
      HHID,
      HC0,
      HC1,
      HC55,
      HC57,
      HC57A,
      HC70,
      HV005A
    ) |>
    dplyr::mutate(
      HHID = trimws(as.character(HHID)),
      dplyr::across(
        -HHID,
        ~ as.numeric(.x)
      )
    )

  # Unión correcta:
  #   RECH6.HHID = RECH1.HHID
  #   RECH6.HC0  = RECH1.HVIDX
  # Luego se añaden las variables del hogar mediante HHID.
  datos <- ninos |>
    dplyr::left_join(
      miembros,
      by = c(
        "HHID" = "HHID",
        "HC0" = "HVIDX"
      )
    ) |>
    dplyr::left_join(
      hogar,
      by = "HHID"
    ) |>
    dplyr::mutate(
      # Factor oficial utilizado en los indicadores de medición infantil.
      # 2015 usa HV005X; 2020 usa HV005A de RECH6; los demás años usan HV005.
      peso_oficial = dplyr::case_when(
        anio == 2015 & !is.na(HV005X) & HV005X > 0 ~ HV005X / 1000000,
        anio == 2020 & !is.na(HV005A) & HV005A > 0 ~ HV005A / 1000000,
        !is.na(HV005) & HV005 > 0 ~ HV005 / 1000000,
        TRUE ~ NA_real_
      ),
      peso_nacional = peso_oficial,
      peso_regional = peso_oficial,

      # Población de facto: durmió en el hogar la noche anterior.
      de_facto = HV103 == 1,

      # Anemia comparable en niñas y niños de 6 a 35 meses.
      # HC57: 1 grave, 2 moderada, 3 leve y 4 sin anemia.
      ana_comp_ind = dplyr::case_when(
        de_facto &
          HV015 == 1 &
          HC55 == 0 &
          dplyr::between(HC1, 6, 35) &
          HC57 %in% 1:4 ~
          dplyr::if_else(HC57 %in% 1:3, 1, 0),
        TRUE ~ NA_real_
      ),

      # Anemia según la nueva directriz. Se usa HC57A cuando está disponible.
      ana_nueva_ind = dplyr::case_when(
        de_facto &
          HV015 == 1 &
          HC55 == 0 &
          dplyr::between(HC1, 6, 35) &
          HC57A %in% 1:4 ~
          dplyr::if_else(HC57A %in% 1:3, 1, 0),
        TRUE ~ NA_real_
      ),

      # HC70 es el puntaje talla/edad multiplicado por 100.
      # El rango plausible de la OMS es mayor que -6 y menor que +6 DE.
      dci_valida = de_facto &
        dplyr::between(HC1, 0, 59) &
        !is.na(HC70) &
        HC70 > -600 &
        HC70 < 600,

      # Menos de -2 desviaciones estándar: desnutrición crónica.
      dci_ind = dplyr::case_when(
        dci_valida ~ dplyr::if_else(HC70 < -200, 1, 0),
        TRUE ~ NA_real_
      ),

      # Menos de -3 desviaciones estándar: desnutrición crónica severa.
      dci_severa_ind = dplyr::case_when(
        dci_valida ~ dplyr::if_else(HC70 < -300, 1, 0),
        TRUE ~ NA_real_
      )
    )

  # Auditoría de la unión. Idealmente, los casos sin RECH1 o sin hogar deben ser
  # cero o muy pocos. Esta tabla ayuda a detectar una base equivocada.
  auditoria <- tibble::tibble(
    anio = anio,
    version_metodo = VERSION_METODO,
    filas_rech0 = nrow(rech0),
    filas_rech1 = nrow(rech1),
    filas_rech6 = nrow(rech6),
    filas_despues_union = nrow(datos),
    casos_sin_hv103 = as.integer(sum(is.na(datos$HV103))),
    casos_sin_peso = as.integer(sum(is.na(datos$peso_oficial))),
    cuestionario_hogar_incompleto = as.integer(
      sum(datos$HV015 != 1, na.rm = TRUE)
    ),
    hemoglobina_no_medida = as.integer(
      sum(datos$HC55 != 0, na.rm = TRUE)
    ),
    casos_de_facto = as.integer(sum(datos$HV103 == 1, na.rm = TRUE)),
    casos_no_de_facto = as.integer(sum(datos$HV103 != 1, na.rm = TRUE)),
    casos_anemia_comparable = n_valido(
      datos$ana_comp_ind,
      datos$peso_nacional
    ),
    casos_anemia_nueva = n_valido(
      datos$ana_nueva_ind,
      datos$peso_nacional
    ),
    casos_dci = n_valido(
      datos$dci_ind,
      datos$peso_nacional
    ),
    usa_hv005x_2015 = anio == 2015 && any(
      !is.na(datos$HV005X) & datos$HV005X > 0
    ),
    usa_hv005a_2020 = anio == 2020 && any(
      !is.na(datos$HV005A) & datos$HV005A > 0
    )
  )

  if (auditoria$casos_sin_hv103 > 0) {
    warning(
      "En ",
      anio,
      " existen ",
      auditoria$casos_sin_hv103,
      " registros de RECH6 que no encontraron HV103 en RECH1."
    )
  }

  salida <- list()
  posicion <- 1L

  # Resultados nacionales: total, urbano y rural.
  for (area_nombre in names(AREAS)) {
    area_codigo <- unname(AREAS[[area_nombre]])

    subconjunto <- if (is.na(area_codigo)) {
      datos
    } else {
      dplyr::filter(datos, HV025 == area_codigo)
    }

    salida[[posicion]] <- resumir_grupo(
      datos = subconjunto,
      anio = anio,
      ambito = "Nacional",
      region_codigo = 0L,
      region_nombre = "Perú",
      area = area_nombre
    )

    posicion <- posicion + 1L
  }

  # Resultados para los 25 departamentos: total, urbano y rural.
  for (codigo_region_chr in names(REGIONES)) {
    codigo_region <- as.integer(codigo_region_chr)
    nombre_region <- unname(REGIONES[[codigo_region_chr]])

    datos_region <- dplyr::filter(
      datos,
      HV024 == codigo_region
    )

    for (area_nombre in names(AREAS)) {
      area_codigo <- unname(AREAS[[area_nombre]])

      subconjunto <- if (is.na(area_codigo)) {
        datos_region
      } else {
        dplyr::filter(
          datos_region,
          HV025 == area_codigo
        )
      }

      salida[[posicion]] <- resumir_grupo(
        datos = subconjunto,
        anio = anio,
        ambito = "Regional",
        region_codigo = codigo_region,
        region_nombre = nombre_region,
        area = area_nombre
      )

      posicion <- posicion + 1L
    }
  }

  indicadores <- dplyr::bind_rows(salida)

  objeto_anual <- list(
    version_metodo = VERSION_METODO,
    indicadores = indicadores,
    auditoria = auditoria
  )

  saveRDS(
    objeto_anual,
    ruta_rds
  )

  # Excel anual con indicadores y control de la unión.
  wb_anual <- openxlsx::createWorkbook()

  indicadores_excel <- indicadores |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_pct"),
        ~ round(.x, 1)
      )
    ) |>
    dplyr::rename(`año` = anio)

  agregar_hoja_formateada(
    wb_anual,
    "Indicadores",
    indicadores_excel
  )

  agregar_hoja_formateada(
    wb_anual,
    "Auditoria_union",
    auditoria
  )

  openxlsx::saveWorkbook(
    wb_anual,
    ruta_excel,
    overwrite = TRUE
  )

  rm(
    rech0,
    rech1,
    rech6,
    hogar,
    miembros,
    ninos,
    datos,
    salida
  )

  gc(verbose = FALSE)

  objeto_anual
}

# 8. EJECUTAR TODOS LOS AÑOS ----------------------------------------------

lista_anual <- vector(
  "list",
  nrow(catalogo)
)

for (i in seq_len(nrow(catalogo))) {
  lista_anual[[i]] <- procesar_anio(
    anio = catalogo$anio[i],
    codigo = catalogo$codigo[i],
    modulo_hogar = catalogo$modulo_hogar[i],
    modulo_ninos = catalogo$modulo_ninos[i]
  )
}

# Se unen los resultados de los 17 años.
base_series <- dplyr::bind_rows(
  lapply(
    lista_anual,
    function(x) x$indicadores
  )
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::ends_with("_pct"),
      ~ round(.x, 1)
    )
  ) |>
  dplyr::rename(`año` = anio)

control_uniones <- dplyr::bind_rows(
  lapply(
    lista_anual,
    function(x) x$auditoria
  )
)

# Comprobación estructural:
# 17 años x (1 nacional + 25 regiones) x 3 áreas = 1 326 filas.
filas_esperadas <- length(2009:2025) * (1 + 25) * 3

if (nrow(base_series) != filas_esperadas) {
  stop(
    "La base tiene ",
    nrow(base_series),
    " filas; se esperaban ",
    filas_esperadas,
    "."
  )
}

# 9. NOTAS METODOLÓGICAS PARA EL EXCEL ------------------------------------

notas_metodologia <- tibble::tribble(
  ~elemento, ~explicacion,
  "Fuente", "Bases oficiales SPSS de la Encuesta Demográfica y de Salud Familiar del INEI.",
  "Descarga", "Los ZIP se descargan automáticamente y se eliminan luego de extraer los SAV.",
  "Bases utilizadas", "RECH0, RECH1 y RECH6 para cada año entre 2009 y 2025.",
  "Llave RECH6-RECH1", "HHID y HC0 = HVIDX. Esto permite identificar a la misma niña o niño en el padrón del hogar.",
  "Llave con RECH0", "HHID, identificación única del hogar.",
  "Población de facto", "Los indicadores se calculan solo cuando HV103 == 1: la persona durmió en el hogar la noche anterior.",
  "Peso 2015", "Para 2015 se utiliza HV005X de RECH0 dividido entre 1 000 000.",
  "Peso 2020", "Para 2020 se utiliza HV005A de RECH6 dividido entre 1 000 000, porque es el factor de mediciones infantiles de ese año.",
  "Peso en otros años", "Para los demás años se utiliza HV005 de RECH0 dividido entre 1 000 000.",
  "Anemia: población válida", "Niñas y niños de 6 a 35 meses de facto, con cuestionario del hogar completo (HV015 = 1) y hemoglobina medida (HC55 = 0).",
  "Anemia comparable", "HC57 igual a 1, 2 o 3 se clasifica con anemia; HC57 igual a 4, sin anemia.",
  "Anemia nueva", "Se aplica la misma recodificación usando HC57A cuando la variable existe.",
  "Desnutrición crónica", "Menores de cinco años de facto con HC70 válido. Se clasifica DCI cuando HC70 < -200.",
  "Desnutrición severa", "Se clasifica como severa cuando HC70 < -300.",
  "Interpretación", "Los porcentajes son estimaciones ponderadas. El diseño muestral es necesario para errores estándar e intervalos, pero no cambia la media ponderada puntual."
)

# 10. RESÚMENES PARA EL EXCEL FINAL ---------------------------------------

columnas_resumen <- c(
  "año",
  "anemia_6_35_comparable_pct",
  "anemia_6_35_nueva_pct",
  "dci_menores5_pct",
  "dci_severa_menores5_pct",
  "n_anemia_comparable",
  "n_anemia_nueva",
  "n_dci"
)

resumen_nacional <- base_series |>
  dplyr::filter(
    ambito == "Nacional",
    area == "Total"
  ) |>
  dplyr::select(
    dplyr::all_of(columnas_resumen)
  )

resumen_region <- function(nombre_region) {
  base_series |>
    dplyr::filter(
      ambito == "Regional",
      region_nombre == nombre_region,
      area == "Total"
    ) |>
    dplyr::select(
      dplyr::all_of(columnas_resumen)
    )
}

hojas <- list(
  Base_series = base_series,
  Control_uniones = control_uniones,
  Notas_metodologia = notas_metodologia,
  Nacional_resumen = resumen_nacional,
  Junin_resumen = resumen_region("Junín"),
  Arequipa_resumen = resumen_region("Arequipa"),
  Cusco_resumen = resumen_region("Cusco"),
  Piura_resumen = resumen_region("Piura"),
  Lambayeque_resumen = resumen_region("Lambayeque"),
  La_Libertad_resumen = resumen_region("La Libertad")
)

# 11. GUARDAR RDS Y EXCEL CONSOLIDADOS ------------------------------------

archivo_rds <- file.path(
  CARPETA_PRINCIPAL,
  "ENDES_REGIONES_2009_2025.rds"
)

saveRDS(
  list(
    version_metodo = VERSION_METODO,
    base_series = base_series,
    control_uniones = control_uniones,
    notas_metodologia = notas_metodologia
  ),
  archivo_rds
)

archivo_excel <- file.path(
  CARPETA_PRINCIPAL,
  "ENDES_REGIONES_2009_2025.xlsx"
)

wb <- openxlsx::createWorkbook()

for (nombre_hoja in names(hojas)) {
  agregar_hoja_formateada(
    wb,
    nombre_hoja,
    hojas[[nombre_hoja]]
  )
}

openxlsx::saveWorkbook(
  wb,
  archivo_excel,
  overwrite = TRUE
)

# En RStudio se abre la base consolidada en el visor.
if (interactive()) {
  View(base_series)
}

# 12. MENSAJE FINAL --------------------------------------------------------

message("============================================================")
message("PROCESO TERMINADO CORRECTAMENTE")
message("Método aplicado: ", VERSION_METODO)
message(
  "Carpeta principal: ",
  normalizePath(
    CARPETA_PRINCIPAL,
    winslash = "/",
    mustWork = FALSE
  )
)
message(
  "Excel consolidado: ",
  normalizePath(
    archivo_excel,
    winslash = "/",
    mustWork = FALSE
  )
)
message("La base consolidada está en el objeto: base_series")
message("Revise la hoja Control_uniones para verificar las llaves.")
message("============================================================")
