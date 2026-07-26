#===============================================================================
# PROYECTO FINAL - PARTE 2
# DESNUTRICION CRONICA INFANTIL EN EL PERU
# Avances y brechas territoriales, 2009-2025
# Fuente: Encuesta Demografica y de Salud Familiar (ENDES) - INEI
#===============================================================================

#-------------------------------------------------------------------------------
# 0. LIBRERIAS
#-------------------------------------------------------------------------------

# En esta segunda parte utilizo readxl para importar el Excel, dplyr y tidyr
# para ordenar la informacion, y ggplot2 para elaborar el grafico final.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Evito que R presente numeros grandes mediante notacion cientifica.

options(scipen = 999)


#-------------------------------------------------------------------------------
# 1. PREGUNTA DE ANALISIS
#-------------------------------------------------------------------------------

# La pregunta que orienta esta parte del trabajo es la siguiente:
#
# ¿La reduccion de la desnutricion cronica infantil en el Peru entre 2009 y
# 2025 fue homogenea entre departamentos y areas de residencia, o todavia
# persisten brechas territoriales importantes?
#
# El alcance es descriptivo. Comparo la evolucion nacional, la diferencia entre
# las areas rural y urbana, y los cambios registrados en cada departamento.


#-------------------------------------------------------------------------------
# 2. DIRECTORIO DE TRABAJO E IMPORTACION
#-------------------------------------------------------------------------------

setwd("C:\\Users\\Joseph\\Documents\\TRABAJO FINAL - ORELLANA ALLPOC JOSEPH")
getwd()

df <- read_excel(
  "ENDES_REGIONES_2009_2025.xlsx",
  sheet = "Base_series"
)

dir.create("figures", showWarnings = FALSE)


#-------------------------------------------------------------------------------
# 3. PREPARACION DE LA BASE
#-------------------------------------------------------------------------------

# Cambio los nombres de las variables que utilizare. Mantengo la misma
# denominacion del script EDA para que ambos archivos sean consistentes.

df <- df %>%
  rename(
    anio             = `año`,
    codigo_region    = region_codigo,
    departamento     = region_nombre,
    anemia_pct       = anemia_6_35_comparable_pct,
    anemia_nueva_pct = anemia_6_35_nueva_pct,
    dci_pct          = dci_menores5_pct,
    dci_severa_pct   = dci_severa_menores5_pct
  ) %>%
  mutate(
    anio          = as.integer(anio),
    codigo_region = as.integer(codigo_region),
    ambito        = as.character(ambito),
    departamento  = as.character(departamento),
    area          = as.character(area)
  )

# Defino el primer y el ultimo año disponibles en la base.

anio_inicial <- min(df$anio, na.rm = TRUE)
anio_final   <- max(df$anio, na.rm = TRUE)


#-------------------------------------------------------------------------------
# 4. ANALISIS DE LOS RESULTADOS
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# 4.1. CAMBIO NACIONAL ENTRE 2009 Y 2025
#-------------------------------------------------------------------------------

# Comparo los resultados nacionales del area total, urbana y rural.
# El cambio se expresa en puntos porcentuales. Un valor negativo representa
# una disminucion de la desnutricion cronica infantil.

resumen_nacional <- df %>%
  filter(
    ambito == "Nacional",
    area %in% c("Total", "Urbano", "Rural"),
    anio %in% c(anio_inicial, anio_final)
  ) %>%
  select(anio, area, dci_pct) %>%
  pivot_wider(
    names_from = anio,
    values_from = dci_pct,
    names_prefix = "dci_"
  ) %>%
  mutate(
    cambio_pp = dci_2025 - dci_2009,
    reduccion_relativa_pct = (dci_2025 / dci_2009 - 1) * 100
  ) %>%
  arrange(factor(area, levels = c("Total", "Urbano", "Rural")))

resumen_nacional


#-------------------------------------------------------------------------------
# 4.2. EVOLUCION DE LA BRECHA RURAL-URBANA
#-------------------------------------------------------------------------------

# Calculo la brecha restando el porcentaje urbano al porcentaje rural.
# Una brecha positiva representa una mayor DCI en el area rural.

brecha_nacional_anual <- df %>%
  filter(
    ambito == "Nacional",
    area %in% c("Urbano", "Rural")
  ) %>%
  select(anio, area, dci_pct) %>%
  pivot_wider(
    names_from = area,
    values_from = dci_pct
  ) %>%
  mutate(
    brecha_rural_urbana_pp = Rural - Urbano,
    razon_rural_urbana = Rural / Urbano
  ) %>%
  arrange(anio)

brecha_nacional_anual

brecha_inicio <- brecha_nacional_anual %>%
  filter(anio == anio_inicial) %>%
  pull(brecha_rural_urbana_pp)

brecha_final <- brecha_nacional_anual %>%
  filter(anio == anio_final) %>%
  pull(brecha_rural_urbana_pp)

razon_final <- brecha_nacional_anual %>%
  filter(anio == anio_final) %>%
  pull(razon_rural_urbana)


#-------------------------------------------------------------------------------
# 4.3. CAMBIO DEPARTAMENTAL ENTRE 2009 Y 2025
#-------------------------------------------------------------------------------

# Comparo el porcentaje total de cada departamento en el primer y ultimo año.
# La tabla permite identificar la magnitud de la reduccion en cada territorio.

dci_departamental_inicio <- df %>%
  filter(
    ambito == "Regional",
    area == "Total",
    anio == anio_inicial
  ) %>%
  select(
    departamento,
    dci_inicial = dci_pct
  )

dci_departamental_final <- df %>%
  filter(
    ambito == "Regional",
    area == "Total",
    anio == anio_final
  ) %>%
  select(
    departamento,
    dci_final = dci_pct
  )

dci_nacional_final <- df %>%
  filter(
    ambito == "Nacional",
    area == "Total",
    anio == anio_final
  ) %>%
  pull(dci_pct)

cambios_departamentales <- dci_departamental_inicio %>%
  inner_join(
    dci_departamental_final,
    by = "departamento"
  ) %>%
  mutate(
    cambio_pp = dci_final - dci_inicial,
    reduccion_pp = dci_inicial - dci_final,
    posicion_2025 = if_else(
      dci_final > dci_nacional_final,
      "Por encima del promedio nacional",
      "Igual o por debajo del promedio nacional"
    )
  ) %>%
  arrange(cambio_pp)

cambios_departamentales

# Cuento los departamentos que redujeron la DCI y los que quedaron por encima
# del resultado nacional en 2025.

numero_departamentos_redujeron <- cambios_departamentales %>%
  summarise(valor = sum(cambio_pp < 0, na.rm = TRUE)) %>%
  pull(valor)

numero_departamentos_sobre_promedio <- cambios_departamentales %>%
  summarise(valor = sum(dci_final > dci_nacional_final, na.rm = TRUE)) %>%
  pull(valor)

# Identifico las cinco mayores reducciones y los cinco porcentajes mas altos
# registrados en 2025.

mayores_reducciones <- cambios_departamentales %>%
  arrange(cambio_pp) %>%
  select(departamento, dci_inicial, dci_final, cambio_pp) %>%
  slice_head(n = 5)

mayores_reducciones

mayor_dci_2025 <- cambios_departamentales %>%
  arrange(desc(dci_final)) %>%
  select(departamento, dci_final, cambio_pp) %>%
  slice_head(n = 5)

mayor_dci_2025


#-------------------------------------------------------------------------------
# 4.4. DIFERENCIAS ENTRE DEPARTAMENTOS
#-------------------------------------------------------------------------------

# Comparo la desviacion estandar y el rango de los porcentajes departamentales
# en 2009 y 2025. Una disminucion representa una menor separacion entre los
# resultados de los departamentos.

dispersion_departamental <- df %>%
  filter(
    ambito == "Regional",
    area == "Total",
    anio %in% c(anio_inicial, anio_final)
  ) %>%
  group_by(anio) %>%
  summarise(
    promedio_departamental = mean(dci_pct, na.rm = TRUE),
    desviacion_estandar = sd(dci_pct, na.rm = TRUE),
    minimo = min(dci_pct, na.rm = TRUE),
    maximo = max(dci_pct, na.rm = TRUE),
    rango = maximo - minimo,
    .groups = "drop"
  )

dispersion_departamental

sd_inicial <- dispersion_departamental %>%
  filter(anio == anio_inicial) %>%
  pull(desviacion_estandar)

sd_final <- dispersion_departamental %>%
  filter(anio == anio_final) %>%
  pull(desviacion_estandar)

variacion_sd_pct <- (sd_final / sd_inicial - 1) * 100


#-------------------------------------------------------------------------------
# 4.5. BRECHAS URBANO-RURALES POR DEPARTAMENTO EN 2025
#-------------------------------------------------------------------------------

# Calculo la diferencia entre el area rural y urbana de cada departamento.
# Callao queda fuera porque la base carece de una estimacion rural comparable.

brechas_departamentales_2025 <- df %>%
  filter(
    ambito == "Regional",
    anio == anio_final,
    area %in% c("Urbano", "Rural")
  ) %>%
  select(departamento, area, dci_pct) %>%
  pivot_wider(
    names_from = area,
    values_from = dci_pct
  ) %>%
  filter(
    !is.na(Urbano),
    !is.na(Rural)
  ) %>%
  mutate(
    brecha_pp = Rural - Urbano,
    razon_rural_urbana = Rural / Urbano
  ) %>%
  arrange(desc(brecha_pp))

brechas_departamentales_2025

mayores_brechas_2025 <- brechas_departamentales_2025 %>%
  select(departamento, Urbano, Rural, brecha_pp) %>%
  slice_head(n = 5)

mayores_brechas_2025


#-------------------------------------------------------------------------------
# 5. GRAFICO FINAL
#-------------------------------------------------------------------------------

# El grafico compara la DCI urbana y rural en 2025. Cada linea une los dos
# porcentajes de un territorio. La etiqueta presenta la diferencia en puntos
# porcentuales.

brecha_peru_2025 <- df %>%
  filter(
    ambito == "Nacional",
    anio == anio_final,
    area %in% c("Urbano", "Rural")
  ) %>%
  select(area, dci_pct) %>%
  pivot_wider(
    names_from = area,
    values_from = dci_pct
  ) %>%
  mutate(
    territorio = "Perú",
    brecha_pp = Rural - Urbano,
    tipo = "Nacional"
  ) %>%
  select(territorio, Urbano, Rural, brecha_pp, tipo)

datos_grafico_final <- brechas_departamentales_2025 %>%
  transmute(
    territorio = departamento,
    Urbano,
    Rural,
    brecha_pp,
    tipo = "Departamental"
  ) %>%
  bind_rows(brecha_peru_2025) %>%
  mutate(
    etiqueta_brecha = paste0(round(brecha_pp, 1), " pp"),
    x_etiqueta = pmax(Urbano, Rural) + 1.2
  )

# Coloco al Peru como referencia y ordeno los departamentos según el tamaño de
# la brecha.

orden_territorios <- c(
  "Perú",
  datos_grafico_final %>%
    filter(tipo == "Departamental") %>%
    arrange(desc(brecha_pp)) %>%
    pull(territorio)
)

datos_grafico_final <- datos_grafico_final %>%
  mutate(
    territorio = factor(
      territorio,
      levels = rev(orden_territorios)
    )
  )

p_final <- datos_grafico_final %>%
  ggplot(aes(y = territorio))+
  geom_segment(
    aes(
      x = Urbano,
      xend = Rural,
      yend = territorio,
      linewidth = tipo
    ),
    color = "grey65"
  )+
  geom_point(
    aes(x = Urbano, color = "Urbano"),
    size = 3
  )+
  geom_point(
    aes(x = Rural, color = "Rural"),
    size = 3
  )+
  geom_text(
    aes(
      x = x_etiqueta,
      label = etiqueta_brecha
    ),
    hjust = 0,
    size = 3,
    color = "grey25"
  )+
  scale_color_manual(
    values = c(
      "Urbano" = "#2C7FB8",
      "Rural" = "#D95F02"
    )
  )+
  scale_linewidth_manual(
    values = c(
      "Nacional" = 1.2,
      "Departamental" = 0.7
    ),
    guide = "none"
  )+
  scale_x_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0.02, 0.18))
  )+
  labs(
    title = "Desnutricion cronica infantil: brecha urbano-rural por departamento",
    subtitle = paste0(
      "Peru, ", anio_final,
      ". Diferencia entre el porcentaje rural y urbano"
    ),
    x = "Porcentaje de menores de 5 años con desnutricion cronica",
    y = NULL,
    color = "Area de residencia",
    caption = paste0(
      "Fuente: INEI - ENDES ", anio_final,
      ". Elaboracion propia. Callao se excluye porque carece ",
      "de una estimacion rural comparable."
    )
  )+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 70, 10, 10)
  )+
  coord_cartesian(clip = "off")

p_final

ggsave(
  filename = "figures/06_brecha_territorial_dci_2025.png",
  plot = p_final,
  width = 11,
  height = 12,
  dpi = 300,
  bg = "white"
)


#-------------------------------------------------------------------------------
# 6. PRINCIPALES CONCLUSIONES
#-------------------------------------------------------------------------------

# Organizo las conclusiones en una tabla para revisarlas en RStudio y
# trasladarlas al README del repositorio.

dci_total_inicial <- resumen_nacional %>%
  filter(area == "Total") %>%
  pull(dci_2009)

dci_total_final <- resumen_nacional %>%
  filter(area == "Total") %>%
  pull(dci_2025)

dci_urbana_final <- resumen_nacional %>%
  filter(area == "Urbano") %>%
  pull(dci_2025)

dci_rural_final <- resumen_nacional %>%
  filter(area == "Rural") %>%
  pull(dci_2025)

conclusiones <- tibble(
  numero = 1:5,
  conclusion = c(
    paste0(
      "La DCI nacional paso de ",
      round(dci_total_inicial, 1), "% en ", anio_inicial,
      " a ", round(dci_total_final, 1), "% en ", anio_final,
      ". La reduccion fue de ",
      round(dci_total_inicial - dci_total_final, 1),
      " puntos porcentuales."
    ),
    paste0(
      "En ", anio_final, ", la DCI rural alcanzo ",
      round(dci_rural_final, 1), "% y la urbana ",
      round(dci_urbana_final, 1), "%. La brecha fue de ",
      round(brecha_final, 1), " puntos porcentuales."
    ),
    paste0(
      numero_departamentos_redujeron,
      " de 25 departamentos registraron una disminucion entre ",
      anio_inicial, " y ", anio_final,
      ". La magnitud del cambio fue distinta entre departamentos."
    ),
    paste0(
      "La desviacion estandar departamental paso de ",
      round(sd_inicial, 1), " a ", round(sd_final, 1),
      " puntos. La separacion entre los porcentajes departamentales ",
      "se redujo durante el periodo."
    ),
    paste0(
      "En ", anio_final, ", ",
      numero_departamentos_sobre_promedio,
      " de 25 departamentos superaron el promedio nacional de ",
      round(dci_nacional_final, 1), "%."
    )
  )
)

conclusiones


#-------------------------------------------------------------------------------
# 7. MENSAJE FINAL
#-------------------------------------------------------------------------------

cat("\n============================================================\n")
cat("PARTE 2 TERMINADA\n")
cat("Pregunta analizada:\n")
cat("¿La reduccion de la DCI fue homogenea o persisten brechas?\n\n")
cat("DCI nacional en ", anio_inicial, ": ",
    round(dci_total_inicial, 1), "%\n", sep = "")
cat("DCI nacional en ", anio_final, ": ",
    round(dci_total_final, 1), "%\n", sep = "")
cat("Brecha rural-urbana en ", anio_final, ": ",
    round(brecha_final, 1), " pp\n", sep = "")
cat("Departamentos con reduccion: ",
    numero_departamentos_redujeron, " de 25\n", sep = "")
cat("Grafico guardado en:\n")
cat("figures/06_brecha_territorial_dci_2025.png\n")
cat("============================================================\n")
