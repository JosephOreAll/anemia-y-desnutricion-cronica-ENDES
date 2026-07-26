#===============================================================================
# PROYECTO FINAL - PARTE 1
# ANALISIS EXPLORATORIO DE DATOS (EDA)
# Tema: anemia y desnutricion cronica infantil en el Peru, 2009-2025
# Fuente: Encuesta Demografica y de Salud Familiar (ENDES) - INEI
#===============================================================================

#-------------------------------------------------------------------------------
# 0. LIBRERIAS
#-------------------------------------------------------------------------------

# Para desarrollar el EDA utilizo las librerias trabajadas durante el curso.
# dplyr y tidyr permiten ordenar la base, ggplot2 se usa para los graficos,
# mientras que geodata y sf permiten elaborar el mapa departamental del Peru.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(geodata)
library(sf)
library(scales)

# Evito que los numeros grandes se presenten con notacion cientifica.
options(scipen = 999)


#-------------------------------------------------------------------------------
# 1. CONTEXTO DEL CONJUNTO DE DATOS
#-------------------------------------------------------------------------------

# La base proviene de la Encuesta Demografica y de Salud Familiar (ENDES),
# elaborada por el Instituto Nacional de Estadistica e Informatica (INEI).
# Esta encuesta constituye una fuente oficial para estudiar las condiciones de
# salud de la poblacion peruana y permite realizar comparaciones en el tiempo.
#
# En este trabajo analizo la evolucion de la anemia infantil y la desnutricion
# cronica infantil entre 2009 y 2025, considerando resultados nacionales,
# diferencias entre el area urbana y rural, y patrones departamentales.
#
# Las variables principales son:
# - anio: año de la encuesta.
# - departamento: departamento o region del Peru.
# - area: Total, Urbano o Rural.
# - anemia_pct: porcentaje de niños de 6 a 35 meses con anemia.
# - anemia_nueva_pct: porcentaje de anemia con la nueva metodologia.
# - dci_pct: porcentaje de menores de 5 años con desnutricion cronica.
# - dci_severa_pct: porcentaje con desnutricion cronica severa.
# - variables n_: numero de casos sin ponderar usados en cada indicador.


#-------------------------------------------------------------------------------
# 2. DIRECTORIO DE TRABAJO E IMPORTACION
#-------------------------------------------------------------------------------

# Primero fijo como directorio la carpeta donde se encuentran el script y la
# base de datos. Como ambos archivos estan en la misma carpeta, solo indico el
# nombre del Excel al momento de importarlo.

setwd("C:\\Users\\Joseph\\Documents\\TRABAJO FINAL - ORELLANA ALLPOC JOSEPH")
getwd()


df <- read_excel(
  "ENDES_REGIONES_2009_2025.xlsx",
  sheet = "Base_series"
)

# Creo la carpeta donde se guardaran los graficos del trabajo.

dir.create("figures", showWarnings = FALSE)

# Realizo una primera revision de la base importada.

df %>% dim()
df %>% names()
df %>% glimpse()
df %>% head()

# View(df) # esta linea se puede activar para observar la base en RStudio


#-------------------------------------------------------------------------------
# 3. LIMPIEZA Y PREPARACION DE LOS DATOS
#-------------------------------------------------------------------------------

# Cambio algunos nombres para trabajar con variables mas cortas y faciles de
# reconocer durante el analisis.

df <- df %>%
  rename(
    anio                 = `año`,
    codigo_region        = region_codigo,
    departamento         = region_nombre,
    n_registros_rech6    = n_obs_rech6,
    n_de_facto           = n_ninos_de_facto,
    anemia_pct           = anemia_6_35_comparable_pct,
    anemia_nueva_pct     = anemia_6_35_nueva_pct,
    dci_pct              = dci_menores5_pct,
    dci_severa_pct       = dci_severa_menores5_pct
  ) %>%
  mutate(
    anio          = as.integer(anio),
    codigo_region = as.integer(codigo_region),
    ambito        = as.character(ambito),
    departamento  = as.character(departamento),
    area          = as.character(area)
  )

# Reviso nuevamente la estructura despues del cambio de nombres.
glimpse(df)

# Identifico la cantidad de valores faltantes en cada variable.
# Los valores faltantes de anemia_nueva_pct antes de 2024 son esperados,
# debido a que dicha clasificacion no esta disponible en todos los años.

valores_faltantes <- df %>%
  is.na() %>%
  colSums()

valores_faltantes

# Compruebo si existen observaciones repetidas para una misma combinacion de
# año, ambito, departamento y area.

duplicados <- df %>%
  count(anio, ambito, departamento, area) %>%
  filter(n > 1)

duplicados

# Los porcentajes deberian encontrarse entre 0 y 100. Por ello realizo este
# control para detectar posibles valores fuera de un rango razonable.

control_porcentajes <- df %>%
  summarise(
    anemia_fuera_rango = sum(anemia_pct < 0 | anemia_pct > 100, na.rm = TRUE),
    anemia_nueva_fuera_rango = sum(anemia_nueva_pct < 0 |
                                      anemia_nueva_pct > 100, na.rm = TRUE),
    dci_fuera_rango = sum(dci_pct < 0 | dci_pct > 100, na.rm = TRUE),
    dci_severa_fuera_rango = sum(dci_severa_pct < 0 |
                                    dci_severa_pct > 100, na.rm = TRUE)
  )

control_porcentajes

# Compruebo el periodo, el numero de registros y la cantidad de departamentos.

control_general <- df %>%
  summarise(
    primer_anio = min(anio, na.rm = TRUE),
    ultimo_anio = max(anio, na.rm = TRUE),
    numero_registros = n(),
    numero_departamentos = n_distinct(
      departamento[ambito == "Regional"]
    )
  )

control_general


#-------------------------------------------------------------------------------
# 4. ESTADISTICAS DESCRIPTIVAS
#-------------------------------------------------------------------------------

# Primero analizo la serie nacional total. Estas estadisticas permiten conocer
# el promedio, la dispersion y los valores minimo y maximo de los indicadores.

estadisticas_nacionales <- df %>%
  filter(ambito == "Nacional", area == "Total") %>%
  summarise(
    anemia_promedio = mean(anemia_pct, na.rm = TRUE),
    anemia_mediana  = median(anemia_pct, na.rm = TRUE),
    anemia_sd       = sd(anemia_pct, na.rm = TRUE),
    anemia_min      = min(anemia_pct, na.rm = TRUE),
    anemia_max      = max(anemia_pct, na.rm = TRUE),
    dci_promedio    = mean(dci_pct, na.rm = TRUE),
    dci_mediana     = median(dci_pct, na.rm = TRUE),
    dci_sd          = sd(dci_pct, na.rm = TRUE),
    dci_min         = min(dci_pct, na.rm = TRUE),
    dci_max         = max(dci_pct, na.rm = TRUE)
  )

estadisticas_nacionales

# Luego comparo los promedios nacionales de las areas total, urbana y rural.
# Esta tabla ayuda a observar si existen brechas territoriales persistentes.

resumen_por_area <- df %>%
  filter(ambito == "Nacional") %>%
  group_by(area) %>%
  summarise(
    anemia_promedio = mean(anemia_pct, na.rm = TRUE),
    dci_promedio = mean(dci_pct, na.rm = TRUE),
    dci_severa_promedio = mean(dci_severa_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(dci_promedio))

resumen_por_area

# Obtengo las brechas entre el area rural y urbana para cada año.
# Una brecha positiva significa que el porcentaje rural es mayor al urbano.

brechas_nacionales <- df %>%
  filter(
    ambito == "Nacional",
    area %in% c("Urbano", "Rural")
  ) %>%
  select(anio, area, anemia_pct, dci_pct) %>%
  pivot_wider(
    names_from = area,
    values_from = c(anemia_pct, dci_pct)
  ) %>%
  mutate(
    brecha_anemia = anemia_pct_Rural - anemia_pct_Urbano,
    brecha_dci = dci_pct_Rural - dci_pct_Urbano
  )

brechas_nacionales

# Para el analisis regional tomo el ultimo año disponible en la base.
anio_final <- max(df$anio, na.rm = TRUE)

datos_regionales_ultimo <- df %>%
  filter(
    ambito == "Regional",
    area == "Total",
    anio == anio_final
  )

# Departamentos con los mayores porcentajes de desnutricion cronica infantil.
ranking_dci <- datos_regionales_ultimo %>%
  select(departamento, dci_pct, anemia_pct, dci_severa_pct) %>%
  arrange(desc(dci_pct))

ranking_dci

# Departamentos con los mayores porcentajes de anemia infantil.
ranking_anemia <- datos_regionales_ultimo %>%
  select(departamento, anemia_pct, dci_pct) %>%
  arrange(desc(anemia_pct))

ranking_anemia

# Para complementar el EDA comparo el primer y el ultimo año de la base.
# Esta tabla permite identificar en que departamentos los indicadores se
# redujeron y en cuales aumentaron durante el periodo analizado.

anio_inicial <- min(df$anio, na.rm = TRUE)

datos_regionales_inicio <- df %>%
  filter(
    ambito == "Regional",
    area == "Total",
    anio == anio_inicial
  ) %>%
  select(
    departamento,
    anemia_inicio = anemia_pct,
    dci_inicio = dci_pct
  )

datos_regionales_fin <- datos_regionales_ultimo %>%
  select(
    departamento,
    anemia_fin = anemia_pct,
    dci_fin = dci_pct
  )

cambios_regionales <- datos_regionales_inicio %>%
  inner_join(datos_regionales_fin, by = "departamento") %>%
  mutate(
    cambio_anemia = anemia_fin - anemia_inicio,
    cambio_dci = dci_fin - dci_inicio
  ) %>%
  arrange(cambio_dci)

cambios_regionales


#-------------------------------------------------------------------------------
# 5. VISUALIZACION DE DATOS
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# GRAFICO 1: EVOLUCION NACIONAL DE ANEMIA Y DCI
#-------------------------------------------------------------------------------

# Cambio la base de formato ancho a largo para colocar los dos indicadores en
# un mismo grafico.

serie_nacional <- df %>%
  filter(ambito == "Nacional", area == "Total") %>%
  select(anio, anemia_pct, dci_pct) %>%
  pivot_longer(
    cols = c(anemia_pct, dci_pct),
    names_to = "indicador",
    values_to = "porcentaje"
  ) %>%
  mutate(
    indicador = case_when(
      indicador == "anemia_pct" ~ "Anemia en niños de 6 a 35 meses",
      indicador == "dci_pct" ~ "Desnutricion cronica en menores de 5 años"
    )
  )

p1 <- serie_nacional %>%
  ggplot(aes(x = anio, y = porcentaje, color = indicador))+
  geom_line(linewidth = 1.1)+
  geom_point(size = 2.4)+
  scale_x_continuous(breaks = seq(min(df$anio), max(df$anio), by = 2))+
  scale_y_continuous(labels = scales::label_number(suffix = "%"))+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Peru: evolucion de la anemia y la desnutricion cronica infantil",
    subtitle = paste0(min(df$anio), "-", max(df$anio), ", estimaciones ponderadas de la ENDES"),
    x = "Año",
    y = "Porcentaje",
    color = "Indicador",
    caption = "Fuente: INEI - Encuesta Demografica y de Salud Familiar (ENDES)."
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p1


#-------------------------------------------------------------------------------
# GRAFICO 2: COMPARACION ENTRE AREA URBANA Y RURAL
#-------------------------------------------------------------------------------

serie_area <- df %>%
  filter(
    ambito == "Nacional",
    area %in% c("Urbano", "Rural")
  ) %>%
  select(anio, area, anemia_pct, dci_pct) %>%
  pivot_longer(
    cols = c(anemia_pct, dci_pct),
    names_to = "indicador",
    values_to = "porcentaje"
  ) %>%
  mutate(
    indicador = case_when(
      indicador == "anemia_pct" ~ "Anemia infantil",
      indicador == "dci_pct" ~ "Desnutricion cronica infantil"
    )
  )

p2 <- serie_area %>%
  ggplot(aes(x = anio, y = porcentaje, color = area))+
  geom_line(linewidth = 1)+
  geom_point(size = 1.8)+
  facet_wrap(~indicador, scales = "free_y")+
  scale_x_continuous(breaks = seq(min(df$anio), max(df$anio), by = 4))+
  scale_y_continuous(labels = scales::label_number(suffix = "%"))+
  scale_color_brewer(palette = "Dark2")+
  labs(
    title = "Brechas entre el area urbana y rural",
    subtitle = "Comparacion de los principales indicadores infantiles",
    x = "Año",
    y = "Porcentaje",
    color = "Area",
    caption = "Fuente: INEI - ENDES."
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p2


#-------------------------------------------------------------------------------
# GRAFICO 3: PERFIL DE ANEMIA Y DCI POR DEPARTAMENTO
#-------------------------------------------------------------------------------

# En este grafico mantengo la idea de relacionar ambos indicadores, pero cambio
# la forma de presentarla para que sea mas facil de interpretar. No estoy
# afirmando que la desnutricion cause anemia. El objetivo es identificar cuatro
# perfiles departamentales en el ultimo año disponible:
#
# 1. alta anemia y alta desnutricion cronica;
# 2. alta anemia y baja desnutricion cronica;
# 3. baja anemia y alta desnutricion cronica;
# 4. baja anemia y baja desnutricion cronica.
#
# Como punto de referencia uso las medianas departamentales. Las lineas
# punteadas separan los valores que se encuentran por encima o por debajo de la
# mediana. Solo coloco nombres en algunos casos destacados para evitar que todas
# las etiquetas se superpongan y hagan dificil la lectura.

mediana_anemia <- median(datos_regionales_ultimo$anemia_pct, na.rm = TRUE)
mediana_dci <- median(datos_regionales_ultimo$dci_pct, na.rm = TRUE)

datos_relacion <- datos_regionales_ultimo %>%
  mutate(
    perfil = case_when(
      anemia_pct >= mediana_anemia & dci_pct >= mediana_dci ~
        "Alta anemia / alta DCI",
      anemia_pct >= mediana_anemia & dci_pct < mediana_dci ~
        "Alta anemia / baja DCI",
      anemia_pct < mediana_anemia & dci_pct >= mediana_dci ~
        "Baja anemia / alta DCI",
      TRUE ~ "Baja anemia / baja DCI"
    ),

    # Selecciono departamentos que ayudan a explicar los principales patrones.
    destacar = departamento %in% c(
      "Puno", "Madre de Dios", "Cusco", "Loreto", "Huancavelica",
      "Ucayali", "Cajamarca", "Tacna", "Lima"
    ),

    # Estos pequeños ajustes separan las etiquetas de sus respectivos puntos.
    ajuste_x = case_when(
      departamento == "Puno" ~ 0.5,
      departamento == "Madre de Dios" ~ -0.3,
      departamento == "Cusco" ~ 0.5,
      departamento == "Loreto" ~ -0.8,
      departamento == "Huancavelica" ~ -0.8,
      departamento == "Ucayali" ~ 0.6,
      departamento == "Cajamarca" ~ 0.5,
      departamento == "Tacna" ~ 0.5,
      departamento == "Lima" ~ 0.5,
      TRUE ~ 0
    ),
    ajuste_y = case_when(
      departamento == "Puno" ~ 1.8,
      departamento == "Madre de Dios" ~ 1.4,
      departamento == "Cusco" ~ 1.4,
      departamento == "Loreto" ~ 1.4,
      departamento == "Huancavelica" ~ -1.5,
      departamento == "Ucayali" ~ -1.5,
      departamento == "Cajamarca" ~ -1.4,
      departamento == "Tacna" ~ 1.3,
      departamento == "Lima" ~ 1.3,
      TRUE ~ 0
    )
  )

# Esta tabla muestra cuantos departamentos pertenecen a cada perfil.
resumen_perfiles <- datos_relacion %>%
  count(perfil, name = "numero_departamentos") %>%
  arrange(desc(numero_departamentos))

resumen_perfiles

p3 <- datos_relacion %>%
  ggplot(aes(x = dci_pct, y = anemia_pct))+
  geom_vline(
    xintercept = mediana_dci,
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.7
  )+
  geom_hline(
    yintercept = mediana_anemia,
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.7
  )+
  geom_point(
    aes(fill = perfil),
    shape = 21,
    size = 3.8,
    color = "grey20",
    alpha = 0.9
  )+
  geom_segment(
    data = datos_relacion %>% filter(destacar),
    aes(
      x = dci_pct,
      y = anemia_pct,
      xend = dci_pct + ajuste_x,
      yend = anemia_pct + ajuste_y
    ),
    inherit.aes = FALSE,
    color = "grey50",
    linewidth = 0.35
  )+
  geom_label(
    data = datos_relacion %>% filter(destacar),
    aes(
      x = dci_pct + ajuste_x,
      y = anemia_pct + ajuste_y,
      label = departamento
    ),
    inherit.aes = FALSE,
    size = 2.8,
    label.padding = grid::unit(0.12, "lines"),
    label.r = grid::unit(0.08, "lines"),
    linewidth = 0.15,
    fill = "white"
  )+
  scale_x_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0.04, 0.08))
  )+
  scale_y_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0.05, 0.10))
  )+
  scale_fill_brewer(palette = "Set2")+
  labs(
    title = "Perfiles departamentales de anemia y desnutricion cronica",
    subtitle = paste0(
      "Año ", anio_final,
      ". Las lineas punteadas representan las medianas departamentales"
    ),
    x = "Desnutricion cronica infantil",
    y = "Anemia infantil",
    fill = "Perfil",
    caption = paste0(
      "Fuente: INEI - ENDES. Elaboracion propia. ",
      "El grafico describe asociaciones, no relaciones de causalidad."
    )
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p3


#-------------------------------------------------------------------------------
# GRAFICO 4: MAPA DE DESNUTRICION CRONICA INFANTIL
#-------------------------------------------------------------------------------

# Creo una funcion para uniformizar los nombres de los departamentos.
# Esto es necesario porque el mapa y el Excel pueden escribir algunos nombres
# de manera diferente, por ejemplo usando tildes o mayusculas.

normalizar_departamento <- function(x){
  x <- toupper(trimws(x))
  x <- chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", x)
  x <- gsub("^PROVINCIA CONSTITUCIONAL DEL ", "", x)
  x <- gsub("^PROVINCIA CONSTITUCIONAL DE ", "", x)
  x <- ifelse(x == "EL CALLAO", "CALLAO", x)
  return(x)
}

# Descargo el mapa departamental del Peru. Se utiliza tempdir() para que los
# archivos espaciales se guarden temporalmente y no llenen la carpeta del
# proyecto. La primera ejecucion requiere conexion a internet.

mp <- geodata::gadm(
  country = "PER",
  level = 1,
  path = tempdir()
) %>%
  sf::st_as_sf()

# Creo una variable comun para unir el mapa con los datos regionales.

mp <- mp %>%
  mutate(
    departamento_join = normalizar_departamento(NAME_1)
  )

datos_mapa <- datos_regionales_ultimo %>%
  mutate(
    departamento_join = normalizar_departamento(departamento)
  ) %>%
  select(departamento_join, departamento, dci_pct, anemia_pct)

mapa_dci <- mp %>%
  left_join(datos_mapa, by = "departamento_join")

# Este control muestra si algun poligono del mapa no encontro datos.
# Lo esperado es que no aparezcan departamentos sin unir.

control_union_mapa <- mapa_dci %>%
  sf::st_drop_geometry() %>%
  filter(is.na(dci_pct)) %>%
  select(NAME_1, departamento_join)

control_union_mapa

p4 <- mapa_dci %>%
  ggplot()+
  geom_sf(aes(fill = dci_pct), color = "white", linewidth = 0.25)+
  geom_sf_text(
    aes(label = ifelse(
      is.na(dci_pct),
      "",
      paste0(round(dci_pct, 1), "%")
    )),
    size = 2.3,
    check_overlap = TRUE
  )+
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    na.value = "grey90",
    labels = scales::label_number(suffix = "%")
  )+
  labs(
    title = "Peru: desnutricion cronica infantil por departamento",
    subtitle = paste0("Resultados departamentales de la ENDES ", anio_final),
    fill = "Porcentaje",
    caption = "Fuente: INEI - ENDES. Mapa base: GADM, obtenido mediante geodata."
  )+
  theme_void()+
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    plot.caption = element_text(hjust = 0),
    legend.position = "right"
  )

p4


#-------------------------------------------------------------------------------
# GRAFICO 5: CAMBIO DE LA DCI ENTRE EL PRIMER Y EL ULTIMO AÑO
#-------------------------------------------------------------------------------

# En lugar del mapa de calor, utilizo un grafico de cambio. Considero que este
# formato es mas directo porque permite comparar el valor inicial y final de
# cada departamento. Cada linea une el resultado del primer año con el ultimo:
# si la linea se desplaza hacia la izquierda, la desnutricion cronica disminuyo;
# si se desplaza hacia la derecha, el indicador aumento.

cambio_dci_grafico <- cambios_regionales %>%
  filter(!is.na(dci_inicio), !is.na(dci_fin)) %>%
  mutate(
    cambio_pp = dci_fin - dci_inicio,
    etiqueta_cambio = paste0(
      ifelse(cambio_pp > 0, "+", ""),
      round(cambio_pp, 1),
      " pp"
    ),
    x_etiqueta = pmax(dci_inicio, dci_fin) + 1.2
  )

# Ordeno los departamentos segun el cambio observado. De esta manera, los que
# consiguieron una mayor reduccion aparecen en la parte superior del grafico.

orden_cambio <- cambio_dci_grafico %>%
  arrange(desc(cambio_pp)) %>%
  pull(departamento)

cambio_dci_grafico <- cambio_dci_grafico %>%
  mutate(
    departamento = factor(departamento, levels = orden_cambio)
  )

p5 <- cambio_dci_grafico %>%
  ggplot(aes(y = departamento))+
  geom_segment(
    aes(x = dci_inicio, xend = dci_fin, yend = departamento),
    color = "grey70",
    linewidth = 0.8
  )+
  geom_point(
    aes(x = dci_inicio, color = as.character(anio_inicial)),
    size = 2.8
  )+
  geom_point(
    aes(x = dci_fin, color = as.character(anio_final)),
    size = 2.8
  )+
  geom_text(
    aes(x = x_etiqueta, label = etiqueta_cambio),
    hjust = 0,
    size = 2.7,
    color = "grey25"
  )+
  scale_x_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0.02, 0.18))
  )+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "Cambio departamental de la desnutricion cronica infantil",
    subtitle = paste0(
      "Comparacion entre ", anio_inicial, " y ", anio_final,
      ". Los valores negativos indican una reduccion"
    ),
    x = "Porcentaje de menores de 5 años con DCI",
    y = "Departamento",
    color = "Año",
    caption = "Fuente: INEI - ENDES. Elaboracion propia. pp = puntos porcentuales."
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

p5


#-------------------------------------------------------------------------------
# 6. GUARDAR LOS GRAFICOS Y CREAR EL COLLAGE
#-------------------------------------------------------------------------------

# Guardo cada grafico individualmente para poder utilizarlos por separado.

ggsave(
  filename = "figures/01_evolucion_nacional.png",
  plot = p1,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/02_brecha_urbano_rural.png",
  plot = p2,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/03_perfiles_departamentales.png",
  plot = p3,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/04_mapa_dci.png",
  plot = p4,
  width = 9,
  height = 9,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = "figures/05_cambio_dci_2009_2025.png",
  plot = p5,
  width = 13,
  height = 9,
  dpi = 300,
  bg = "white"
)

# Finalmente junto los cinco graficos en un collage. El grafico de perfiles
# permite comparar los dos indicadores y el grafico de cambio resume la
# evolucion departamental entre el primer y el ultimo año.

collage_graficos <- (p1 | p2) / (p3 | p4) / p5 +
  patchwork::plot_layout(heights = c(1, 1.20, 1.35)) +
  patchwork::plot_annotation(
    title = "EDA: anemia y desnutricion cronica infantil en el Peru",
    subtitle = paste0("Analisis nacional y departamental, ",
                      min(df$anio), "-", max(df$anio)),
    caption = "Fuente: elaboracion propia con datos de la ENDES - INEI.",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 9, hjust = 0)
    )
  )

collage_graficos

ggsave(
  filename = "figures/collage_graficos.png",
  plot = collage_graficos,
  width = 20,
  height = 23,
  dpi = 300,
  bg = "white"
)

#-------------------------------------------------------------------------------
# 7. MENSAJE FINAL
#-------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("EDA PARTE 1 TERMINADO CORRECTAMENTE\n")
cat("Los graficos se guardaron en la carpeta: figures\n")
cat("Archivo principal: figures/collage_graficos.png\n")
cat("Grafico adicional: figures/05_cambio_dci_2009_2025.png\n")
cat("Año regional analizado en los graficos: ", anio_final, "\n", sep = "")
cat("============================================================\n")
