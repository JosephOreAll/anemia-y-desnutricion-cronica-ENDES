# Anemia y desnutrición crónica infantil en el Perú, 2009-2025

## Descripción

Este repositorio reúne un análisis de la anemia infantil y de la desnutrición crónica infantil (DCI) en el Perú con información de la Encuesta Demográfica y de Salud Familiar (ENDES), elaborada por el Instituto Nacional de Estadística e Informática (INEI).

La primera parte presenta la evolución nacional, las diferencias entre las áreas urbana y rural y la distribución departamental. La segunda parte examina la reducción de la DCI entre 2009 y 2025 y compara las brechas que permanecen al final del periodo.

## Pregunta de análisis

> **¿La reducción de la desnutrición crónica infantil en el Perú entre 2009 y 2025 fue homogénea entre departamentos y áreas de residencia, o todavía persisten brechas territoriales importantes?**

## Fuente

La base fue construida con microdatos oficiales de la ENDES para el periodo 2009-2025. Incluye estimaciones ponderadas para el ámbito nacional y departamental, con desagregación total, urbana y rural.

Archivo principal:

```text
ENDES_REGIONES_2009_2025_CORREGIDO.xlsx
```

Hoja utilizada:

```text
Base_series
```

## Variables principales

| Variable | Descripción |
|---|---|
| `anio` | Año de la encuesta |
| `departamento` | Departamento o ámbito nacional |
| `area` | Total, Urbano o Rural |
| `anemia_pct` | Anemia en niños de 6 a 35 meses, metodología comparable |
| `anemia_nueva_pct` | Anemia según la nueva metodología disponible |
| `dci_pct` | Desnutrición crónica en menores de cinco años |
| `dci_severa_pct` | Desnutrición crónica severa en menores de cinco años |
| `n_dci` | Número de observaciones sin ponderar usadas para estimar la DCI |

## Estructura del repositorio

```text
Proyecto_Final/
│
├── ENDES_REGIONES_2009_2025_CORREGIDO.xlsx
│
├── figures/
│   ├── 01_evolucion_nacional.png
│   ├── 02_brecha_urbano_rural.png
│   ├── 03_perfiles_departamentales.png
│   ├── 04_mapa_dci.png
│   ├── 05_cambio_dci_2009_2025.png
│   ├── 06_brecha_territorial_dci_2025.png
│   └── collage_graficos.png
│
├── scripts/
│   ├── EDA.R
│   └── 04_analisis_final.R
│
└── README.md
```

## Librerías

```r
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(geodata)
library(sf)
library(scales)
```

## Ejecución

1. Abrir el proyecto en RStudio.
2. Ajustar la ruta indicada en `setwd()`.
3. Ejecutar `scripts/EDA.R`.
4. Ejecutar `scripts/04_analisis_final.R`.

La segunda parte genera el siguiente archivo:

```text
figures/06_brecha_territorial_dci_2025.png
```

## Resultados principales

### La DCI nacional cayó 11,7 puntos porcentuales

La desnutrición crónica infantil pasó de **23,8% en 2009 a 12,1% en 2025**. La reducción acumulada fue de **11,7 puntos porcentuales**.

### La brecha rural permanece elevada

En 2025, la DCI alcanzó **21,7% en el área rural** y **7,9% en el área urbana**. La diferencia fue de **13,8 puntos porcentuales**. El porcentaje rural equivalió a cerca de 2,7 veces el urbano.

### Casi todos los departamentos redujeron la DCI

Entre 2009 y 2025, **24 de los 25 departamentos** registraron una disminución. Las mayores reducciones correspondieron a:

- Huancavelica: **33,3 puntos porcentuales**;
- Cusco: **26,8 puntos porcentuales**;
- Ayacucho: **26,5 puntos porcentuales**.

Callao aumentó **0,5 puntos porcentuales** y Tacna permaneció cerca de su nivel inicial.

### Las diferencias departamentales se acortaron

La desviación estándar de los porcentajes departamentales pasó de **13,5 puntos en 2009 a 5,5 puntos en 2025**. El rango entre el porcentaje más alto y el más bajo también disminuyó.

### Persisten departamentos con porcentajes altos

En 2025, **14 de 25 departamentos** superaron el promedio nacional de 12,1%. Los porcentajes más altos fueron:

- Loreto: **20,8%**;
- Huancavelica: **20,3%**;
- Ucayali: **18,1%**.

Las brechas urbano-rurales más amplias se registraron en La Libertad, Piura y Ucayali.

## Conclusiones

El Perú redujo la desnutrición crónica infantil durante el periodo analizado. La caída alcanzó a casi todos los departamentos y la separación entre sus porcentajes disminuyó.

La brecha por área de residencia continúa siendo amplia. En 2025, el porcentaje rural superó al urbano en 13,8 puntos porcentuales. Catorce departamentos también permanecieron por encima del promedio nacional.

La programación de intervenciones puede priorizar los departamentos con mayor prevalencia y las zonas rurales con brechas más amplias. El seguimiento anual permite verificar si esas diferencias se acortan.

## Alcance del análisis

- La base contiene estimaciones agregadas.
- El análisis compara niveles y cambios; la identificación de causas requiere información adicional y un diseño específico.
- Los cálculos presentados no incluyen intervalos de confianza ni pruebas de significancia estadística.
- La base consolidada no incorpora variables de pobreza, agua, saneamiento, acceso a salud o educación.

## Referencias

Instituto Nacional de Estadística e Informática. (2026a, 19 de junio). *Perú: Encuesta Demográfica y de Salud Familiar, ENDES 2025*. https://www.gob.pe/institucion/inei/informes-publicaciones/8284085-peru-encuesta-demografica-y-de-salud-familiar-endes-2025

Instituto Nacional de Estadística e Informática. (2026b, 19 de junio). *Perú: Series anuales de principales indicadores de la ENDES, 1986-2025*. https://www.gob.pe/institucion/inei/informes-publicaciones/8284167-peru-series-anuales-de-principales-indicadores-de-la-endes-1986-2025

Organización Mundial de la Salud. (2025, 12 de febrero). *Patrones de crecimiento infantil*. https://www.who.int/es/news-room/questions-and-answers/item/child-growth-standards

## Autor

**Nombre:** Joseph Orellana Allpoc  
