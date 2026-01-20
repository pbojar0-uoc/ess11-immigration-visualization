# ==============================================================================
# EUROPEAN SOCIAL SURVEY ROUND 11 (2023-2024)
# PREPARACIÓN DE DATOS PARA VISUALIZACIÓN INTERACTIVA
# ==============================================================================
# Autor: Patryk Bojar
# Asignatura: Visualización de Datos (UOC)
# Fecha: Enero 2026
# 
# OBJETIVO:
# Preparar datos del ESS Round 11 para análisis comparativo de actitudes 
# hacia la inmigración en Europa, con foco en España vs Polonia.
#
# PREGUNTAS DE INVESTIGACIÓN:
# 1. ¿Diferencias España vs Polonia? ¿Y Occidental vs Oriental?
# 2. ¿Derecha política más negativa? ¿Igual en todos los países?
# 3. ¿Jóvenes más abiertos que mayores?
# 4. ¿Educación relacionada con apertura hacia diversidad?
#
# FUENTE DE DATOS:
# European Social Survey European Research Infrastructure (ESS ERIC) (2025) 
# ESS11 - integrated file, edition 4.1 [Data set]. 
# doi:10.21338/ess11e04_1
# ==============================================================================

# Configurar directorio de trabajo
setwd("C:/Users/patry/Desktop/UOC/visualizacion_datos/PRA2")

# 1. GESTIÓN DE PAQUETES -------------------------------------------------------
# Usar pacman para instalar/cargar paquetes automáticamente
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Manipulación de datos (dplyr, ggplot2, tidyr)
  haven,        # Lectura de archivos SPSS (por si se usa .sav en futuro)
  countrycode   # Conversión de códigos ISO y nombres de países
)

cat("\n")
cat("Paquetes cargados correctamente\n")
cat("==============================================================================\n\n")

# 2. IMPORTACIÓN DE DATOS ------------------------------------------------------
# NOTA: El archivo CSV fue previamente descargado desde:
# https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef

ess11 <- read_csv("ESS11e04_1.csv", show_col_types = FALSE)

cat("✓ Datos cargados:", nrow(ess11), "filas,", ncol(ess11), "columnas\n")
cat("  Países incluidos:", n_distinct(ess11$cntry), "\n")
cat("  Ronda ESS: 11 (2023-2024)\n\n")

# 3. SELECCIÓN DE VARIABLES CLAVE ---------------------------------------------
# Seleccionamos solo las variables necesarias para responder a las preguntas
# de investigación, reduciendo el dataset de 691 a 9 columnas.

datos <- ess11 %>%
  select(
    # Identificación geográfica
    cntry,           # País (código ISO-2: ES, PL, DE...)
    
    # Variables demográficas
    agea,            # Edad del respondente (calculada desde año de nacimiento)
    gndr,            # Género (1=Hombre, 2=Mujer)
    eisced,          # Nivel educativo ES-ISCED (1=Básica ... 7=Doctorado)
    
    # Variable política
    lrscale,         # Escala izquierda-derecha (0=Izquierda ... 10=Derecha)
    
    # Actitudes hacia inmigración (3 variables core, escala 0-10)
    imwbcnt,         # ¿Inmigrantes hacen país mejor/peor lugar? (0=Peor, 10=Mejor)
    imueclt,         # ¿Vida cultural enriquecida/dañada? (0=Dañada, 10=Enriquecida)
    imbgeco,         # ¿Inmigración buena/mala para economía? (0=Mala, 10=Buena)
    
    # Peso muestral (CRUCIAL para análisis correcto)
    anweight         # Peso de análisis (corrige sobremuestreo regional)
  )

cat("✓ Variables seleccionadas:", ncol(datos), "columnas\n\n")

# 4. LIMPIEZA DE DATOS ---------------------------------------------------------
# DOCUMENTACIÓN DE VALORES ESPECIALES EN ESS:
# Según codebook ESS11 (https://ess.sikt.no/):
#   77/7777 = Refusal (persona rechazó responder)
#   88/8888 = Don't know (no sabe la respuesta)
#   99/9999 = No answer (sin respuesta por error técnico)
#   66/6666 = Not applicable (pregunta no aplicable al respondente)
#   55/5555 = Other (categoría 'otro')
#
# ESTRATEGIA DE LIMPIEZA:
# Convertir TODOS estos valores a NA porque:
#   1. No son respuestas válidas en escalas 0-10
#   2. Si se dejan, distorsionan promedios (ej: 77 = "extrema derecha" falsa)
#   3. El análisis debe basarse solo en respuestas válidas

datos_limpios <- datos %>%
  mutate(
    # Recodificar missing values a NA
    agea    = ifelse(agea >= 999, NA, agea),
    lrscale = ifelse(lrscale >= 77, NA, lrscale),
    imwbcnt = ifelse(imwbcnt >= 77, NA, imwbcnt),
    imueclt = ifelse(imueclt >= 77, NA, imueclt),
    imbgeco = ifelse(imbgeco >= 77, NA, imbgeco),
    eisced  = ifelse(eisced >= 55, NA, eisced),
    gndr    = ifelse(gndr >= 9, NA, gndr)
  ) %>%
  # Eliminar casos con NAs en variables críticas para el análisis
  # (Mantenemos casos con NA en educación/género para no perder muestra)
  filter(
    !is.na(agea),      # Edad necesaria para grupos generacionales
    !is.na(lrscale),   # Política necesaria para análisis de polarización
    !is.na(imwbcnt),   # Variable core de inmigración
    !is.na(imueclt),   # Variable core de inmigración
    !is.na(cntry)      # País necesario para comparación geográfica
  )

cat("Limpieza completada\n")
cat("  Casos válidos:", nrow(datos_limpios), 
    "(", round(nrow(datos_limpios)/nrow(ess11)*100, 1), "% del total)\n")
cat("  Casos eliminados:", nrow(ess11) - nrow(datos_limpios), 
    "(missing values en variables críticas)\n\n")

# 5. FEATURE ENGINEERING -------------------------------------------------------
# Creamos variables derivadas para facilitar el análisis y la visualización

datos_final <- datos_limpios %>%
  mutate(
    # A) GRUPOS GENERACIONALES (corte en año 2024)
    # Basado en definiciones sociológicas estándar:
    #   Gen Z: nacidos 1997-2009 (15-27 años en 2024)
    #   Millennials: nacidos 1981-1996 (28-43 años)
    #   Gen X: nacidos 1965-1980 (44-59 años)
    #   Boomers: nacidos antes de 1965 (60+ años)
    Age_Group = case_when(
      agea < 27  ~ "Gen Z (<27)",
      agea >= 27 & agea < 43 ~ "Millennials (27-42)",
      agea >= 43 & agea < 59 ~ "Gen X (43-58)",
      agea >= 59 ~ "Boomers+ (59+)"
    ) %>% 
      factor(levels = c("Gen Z (<27)", "Millennials (27-42)", 
                        "Gen X (43-58)", "Boomers+ (59+)")),
    
    # B) ESPECTRO POLÍTICO (simplificación a 3 categorías)
    # Agrupamos escala 0-10 en tercios para análisis más robusto
    Political_Spectrum = case_when(
      lrscale >= 0 & lrscale <= 3 ~ "Izquierda (0-3)",
      lrscale >= 4 & lrscale <= 6 ~ "Centro (4-6)",
      lrscale >= 7 & lrscale <= 10 ~ "Derecha (7-10)"
    ) %>% 
      factor(levels = c("Izquierda (0-3)", "Centro (4-6)", "Derecha (7-10)")),
    
    # C) NIVEL EDUCATIVO SIMPLIFICADO (desde ES-ISCED de 8 niveles a 3)
    # ES-ISCED 1-2: Primaria y secundaria básica = "Básica"
    # ES-ISCED 3-5: Bachillerato y FP = "Media"
    # ES-ISCED 6-7: Universidad y posgrado = "Superior"
    Education_Level = case_when(
      eisced %in% c(1, 2) ~ "Básica",
      eisced %in% c(3, 4, 5) ~ "Media",
      eisced %in% c(6, 7) ~ "Superior",
      TRUE ~ NA_character_
    ) %>% 
      factor(levels = c("Básica", "Media", "Superior")),
    
    # D) DESTACAR ESPAÑA Y POLONIA (para uso en colores/filtros)
    Country_Highlight = case_when(
      cntry == "ES" ~ "España",
      cntry == "PL" ~ "Polonia",
      TRUE ~ "Resto de Europa"
    ) %>% 
      factor(levels = c("España", "Polonia", "Resto de Europa")),
    
    # E) NOMBRES DE PAÍSES EN ESPAÑOL (para etiquetas en visualizaciones)
    # Usamos nomenclatura CLDR en español para mayor claridad
    Country_Name = countrycode(
      cntry, 
      origin = "iso2c", 
      destination = "cldr.name.es"
    ),
    
    # F) REGIÓN GEOGRÁFICA (para análisis Occidental vs Oriental)
    # Clasificación basada en criterios geopolíticos del ESS
    Region = case_when(
      cntry %in% c("AT", "BE", "CH", "DE", "FR", "GB", "IE", "NL") ~ "Europa Occidental",
      cntry %in% c("BG", "CZ", "EE", "HR", "HU", "LT", "LV", "PL", "RS", "SI", "SK", "UA") ~ "Europa Oriental",
      cntry %in% c("DK", "FI", "IS", "NO", "SE") ~ "Europa Nórdica",
      cntry %in% c("CY", "ES", "GR", "IT", "ME", "PT") ~ "Europa Meridional",
      TRUE ~ "Otro"
    ),
    
    # G) ÍNDICE COMPUESTO DE ACTITUDES HACIA INMIGRACIÓN (variable sintética)
    # Promediamos 3 dimensiones para tener un indicador global:
    #   - Impacto general en el país (imwbcnt)
    #   - Impacto cultural (imueclt)
    #   - Impacto económico (imbgeco)
    # Valores altos (>7) = muy abierto, valores bajos (<4) = muy cerrado
    Immigration_Index = (imwbcnt + imueclt + imbgeco) / 3,
    
    # H) GÉNERO EN ESPAÑOL (etiquetas legibles)
    Gender = case_when(
      gndr == 1 ~ "Hombre",
      gndr == 2 ~ "Mujer",
      TRUE ~ NA_character_
    )
  )

cat("Feature engineering completado\n")
cat("Nuevas variables creadas: Age_Group, Political_Spectrum, Education_Level,\n")
cat("                           Region, Immigration_Index\n\n")

# ==============================================================================
# 6. GENERACIÓN DE DATASETS PARA FLOURISH
# ==============================================================================
# Generamos 4 archivos CSV especializados para diferentes visualizaciones.
# Cada archivo está optimizado para un template específico de Flourish.

# -----------------------------------------------------------------------------
# DATASET 1: MAPA COROPLÉTICO (responde a P1a, P1b)
# -----------------------------------------------------------------------------
# Propósito: Mostrar distribución geográfica de actitudes hacia inmigración
# Template Flourish: "Projection map" o "Choropleth map"
# Variables clave: Códigos ISO + promedios ponderados por país

cat("Generando Dataset 1: Mapa de Europa...\n")

countries_summary <- datos_final %>%
  group_by(cntry, Country_Name, Country_Highlight, Region) %>%
  summarise(
    # PROMEDIOS PONDERADOS (usando anweight)
    # ¿Por qué ponderar? El ESS sobremuestrea ciertas regiones dentro de cada país
    # para permitir análisis subnacionales. Sin ponderación, regiones como 
    # Cataluña (ES) o Mazovia (PL) pesarían artificialmente más que otras.
    Avg_Immigration_Index = weighted.mean(Immigration_Index, anweight, na.rm = TRUE),
    Avg_Culture_Enrichment = weighted.mean(imueclt, anweight, na.rm = TRUE),
    Avg_Better_Place = weighted.mean(imwbcnt, anweight, na.rm = TRUE),
    Avg_Economy = weighted.mean(imbgeco, anweight, na.rm = TRUE),
    Avg_Political_Position = weighted.mean(lrscale, anweight, na.rm = TRUE),
    
    # Métricas adicionales para tooltip
    n_respondents = n(),                              # Tamaño muestral
    SD_Immigration = sd(Immigration_Index, na.rm = TRUE),  # Dispersión interna
    
    .groups = "drop"
  ) %>%
  mutate(
    # Añadir códigos ISO necesarios para Flourish
    ISO2 = cntry,  # Código de 2 letras (ES, PL, DE...)
    ISO3 = countrycode(cntry, origin = "iso2c", destination = "iso3c"),  # ESP, POL, DEU...
    
    # Ranking europeo (1=más abierto, 30=más cerrado)
    Ranking = row_number(desc(Avg_Immigration_Index)),
    
    # Clasificación cualitativa (para narrativa)
    Categoria = case_when(
      Avg_Immigration_Index > 6.5 ~ "Muy abierto",
      Avg_Immigration_Index > 5.5 ~ "Moderadamente abierto",
      Avg_Immigration_Index > 4.5 ~ "Neutral",
      Avg_Immigration_Index > 3.5 ~ "Moderadamente cerrado",
      TRUE ~ "Muy cerrado"
    )
  ) %>%
  # Reordenar columnas (ISO primero para reconocimiento automático en Flourish)
  select(ISO2, ISO3, Country_Name, Country_Highlight, Region, 
         Avg_Immigration_Index, Avg_Culture_Enrichment, Avg_Better_Place, Avg_Economy,
         Avg_Political_Position, Ranking, Categoria, n_respondents, SD_Immigration) %>%
  arrange(desc(Avg_Immigration_Index))

# Exportar
write_csv(countries_summary, "ESS11_Countries_Summary.csv")
cat("Exportado: ESS11_Countries_Summary.csv (", nrow(countries_summary), "países)\n\n")

# -----------------------------------------------------------------------------
# DATASET 2: DUMBBELL CHART POLARIZACIÓN (responde a P2a, P2b)
# -----------------------------------------------------------------------------
# Propósito: Comparar actitudes de izquierda vs derecha en cada país
# Template Flourish: "Dot plot" o custom HTML
# Formato: LONG (2 filas por país: 1 para izquierda, 1 para derecha)

cat("Generando Dataset 2: Polarización política...\n")

# Paso 1: Calcular promedios por país e ideología (formato WIDE)
polarizacion_wide <- datos_final %>%
  filter(
    !is.na(Political_Spectrum), 
    Political_Spectrum %in% c("Izquierda (0-3)", "Derecha (7-10)")
  ) %>%
  group_by(cntry, Country_Name, Country_Highlight, Region, Political_Spectrum) %>%
  summarise(
    Avg_Immigration = weighted.mean(Immigration_Index, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Political_Spectrum,
    values_from = c(Avg_Immigration, n),
    names_sep = "_"
  ) %>%
  mutate(
    # Calcular brecha ideológica (valores positivos = izquierda más abierta)
    Polarizacion = `Avg_Immigration_Izquierda (0-3)` - `Avg_Immigration_Derecha (7-10)`,
    
    # Labels redondeados para visualización
    Label_Izq = round(`Avg_Immigration_Izquierda (0-3)`, 1),
    Label_Der = round(`Avg_Immigration_Derecha (7-10)`, 1),
    
    # Clasificación de intensidad de polarización
    Tipo_Polarizacion = case_when(
      Polarizacion > 2.0 ~ "Muy polarizado",
      Polarizacion > 1.0 ~ "Polarizado",
      Polarizacion > 0.5 ~ "Moderadamente polarizado",
      TRUE ~ "Poco polarizado"
    ),
    
    # Tamaño muestral total (control de calidad)
    Total_n = `n_Izquierda (0-3)` + `n_Derecha (7-10)`
  ) %>%
  # CONTROL DE CALIDAD: Filtrar países con muestras pequeñas
  # Criterio: mínimo 50 personas en cada extremo ideológico
  filter(`n_Izquierda (0-3)` >= 50, `n_Derecha (7-10)` >= 50) %>%
  arrange(desc(Polarizacion))

# Paso 2: Transformar a formato LONG (requerido por Flourish dumbbell)
dumbbell_flourish <- polarizacion_wide %>%
  pivot_longer(
    cols = c(`Avg_Immigration_Izquierda (0-3)`, `Avg_Immigration_Derecha (7-10)`),
    names_to = "Ideologia_Temp",
    values_to = "Immigration_Score"
  ) %>%
  mutate(
    # Crear etiqueta limpia de ideología
    Ideologia = ifelse(
      str_detect(Ideologia_Temp, "Izquierda"), 
      "Izquierda (0-3)", 
      "Derecha (7-10)"
    ),
    
    # Asignar sample size correspondiente
    Sample_Size = ifelse(
      Ideologia == "Izquierda (0-3)",
      `n_Izquierda (0-3)`,
      `n_Derecha (7-10)`
    ),
    
    # Ordenar países por polarización (eje Y del gráfico)
    Country_Order = dense_rank(desc(Polarizacion))
  ) %>%
  select(
    Country_Name, Country_Highlight, Region,
    Ideologia, Immigration_Score, 
    Polarizacion, Sample_Size, Country_Order
  ) %>%
  arrange(Country_Order, Ideologia)

# Exportar
write_csv(dumbbell_flourish, "ESS11_Dumbbell_Flourish.csv")
cat("Exportado: ESS11_Dumbbell_Flourish.csv (", nrow(dumbbell_flourish), 
    "filas = ", nrow(dumbbell_flourish)/2, "países × 2 ideologías)\n\n")

# -----------------------------------------------------------------------------
# DATASET 3: HEATMAP EDAD × POLÍTICA (responde a P3, pregunta principal)
# -----------------------------------------------------------------------------
# Propósito: Mostrar interacción entre edad e ideología en actitudes
# Template Flourish: "Heatmap" o "Matrix chart"
# Formato: Una fila por cada combinación país-edad-política

cat("Generando Dataset 3: Heatmap Edad × Política...\n")

heatmap_data <- datos_final %>%
  filter(!is.na(Political_Spectrum), !is.na(Age_Group)) %>%
  group_by(cntry, Country_Name, Country_Highlight, Age_Group, Political_Spectrum) %>%
  summarise(
    Avg_Immigration = weighted.mean(Immigration_Index, anweight, na.rm = TRUE),
    n = n(),  # incluimos n para verificar robustez de cada celda
    .groups = "drop"
  ) %>%
  mutate(
    # Clasificación cualitativa de intensidad (para escala de color)
    Intensidad = case_when(
      Avg_Immigration >= 7 ~ "Muy positivo",
      Avg_Immigration >= 6 ~ "Positivo",
      Avg_Immigration >= 5 ~ "Neutral",
      Avg_Immigration >= 4 ~ "Negativo",
      TRUE ~ "Muy negativo"
    ) %>% 
      factor(levels = c("Muy negativo", "Negativo", "Neutral", "Positivo", "Muy positivo"))
  )

# Exportar
write_csv(heatmap_data, "ESS11_Heatmap_Data.csv")
cat("Exportado: ESS11_Heatmap_Data.csv (", nrow(heatmap_data), 
    "celdas = países × 4 edades × 3 ideologías)\n\n")

# -----------------------------------------------------------------------------
# DATASET 4: EDUCACIÓN × INMIGRACIÓN (responde a P4)
# -----------------------------------------------------------------------------
# Propósito: Responder a la pregunta 4 que faltaba en las visualizaciones
# Template Flourish: "Column chart" grouped o "Line chart"
# Formato: Una fila por país-educación

cat("Generando Dataset 4: Educación × Inmigración (PREGUNTA 4)...\n")

education_immigration <- datos_final %>%
  filter(!is.na(Education_Level)) %>%
  group_by(cntry, Country_Name, Country_Highlight, Region, Education_Level) %>%
  summarise(
    Avg_Immigration = weighted.mean(Immigration_Index, anweight, na.rm = TRUE),
    Avg_Culture = weighted.mean(imueclt, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  # Calcular gradiente educativo (diferencia Básica-Superior)
  group_by(cntry, Country_Name, Country_Highlight, Region) %>%
  mutate(
    Education_Gradient = max(Avg_Immigration) - min(Avg_Immigration)
  ) %>%
  ungroup() %>%
  # Filtrar solo países con datos en los 3 niveles educativos
  group_by(cntry) %>%
  filter(n_distinct(Education_Level) == 3) %>%
  ungroup()

# Exportar
write_csv(education_immigration, "ESS11_Education_Flourish.csv")
cat("Exportado: ESS11_Education_Flourish.csv (", nrow(education_immigration), 
    "filas)\n")

# ==============================================================================
# 7. REPORTES DE VERIFICACIÓN (para incluir en video y documento)
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("                    REPORTES DE HALLAZGOS CLAVE                           \n")
cat("==============================================================================\n\n")

# REPORTE 1: Comparación directa España vs Polonia -------------------------
cat("REPORTE 1: COMPARACIÓN ESPAÑA VS POLONIA\n")
cat("------------------------------------------------------------------------------\n")
comparacion_ES_PL <- datos_final %>%
  filter(cntry %in% c("ES", "PL")) %>%
  group_by(Country_Name) %>%
  summarise(
    Índice_General = round(mean(Immigration_Index, na.rm = TRUE), 2),
    Cultura = round(mean(imueclt, na.rm = TRUE), 2),
    Economía = round(mean(imbgeco, na.rm = TRUE), 2),
    Mejor_Lugar = round(mean(imwbcnt, na.rm = TRUE), 2),
    Ideología_Promedio = round(mean(lrscale, na.rm = TRUE), 2),
    N = n()
  )
print(comparacion_ES_PL)
cat("\n Interpretación: España", comparacion_ES_PL$Índice_General[1] - comparacion_ES_PL$Índice_General[2],
    "puntos más abierta que Polonia\n")
cat("   Mayor diferencia en: Cultura (", comparacion_ES_PL$Cultura[1] - comparacion_ES_PL$Cultura[2], 
    "puntos)\n\n")

# REPORTE 2: Polarización política España vs Polonia -----------------------
cat(" REPORTE 2: POLARIZACIÓN IDEOLÓGICA (España vs Polonia)\n")
cat("------------------------------------------------------------------------------\n")
polarizacion_ES_PL <- dumbbell_flourish %>%
  filter(Country_Name %in% c("España", "Polonia")) %>%
  select(Country_Name, Ideologia, Immigration_Score, Sample_Size) %>%
  pivot_wider(names_from = Ideologia, values_from = c(Immigration_Score, Sample_Size))
print(polarizacion_ES_PL)
cat("\n Interpretación:\n")
cat("   España: Brecha = ", 
    round(polarizacion_ES_PL$`Immigration_Score_Izquierda (0-3)`[1] - 
            polarizacion_ES_PL$`Immigration_Score_Derecha (7-10)`[1], 2), 
    "puntos (polarización MEDIA)\n")
cat("   Polonia: Brecha = ", 
    round(polarizacion_ES_PL$`Immigration_Score_Izquierda (0-3)`[2] - 
            polarizacion_ES_PL$`Immigration_Score_Derecha (7-10)`[2], 2), 
    "puntos (polarización BAJA)\n\n")

# REPORTE 3: Top 5 países más/menos polarizados ----------------------------
cat(" REPORTE 3: RANKING DE POLARIZACIÓN IDEOLÓGICA\n")
cat("------------------------------------------------------------------------------\n")
cat("TOP 5 MÁS POLARIZADOS:\n")
dumbbell_flourish %>%
  distinct(Country_Name, Polarizacion) %>%
  arrange(desc(Polarizacion)) %>%
  mutate(Polarizacion = round(Polarizacion, 2)) %>%
  head(5) %>%
  print()

cat("\nTOP 5 MENOS POLARIZADOS (o polarización invertida):\n")
dumbbell_flourish %>%
  distinct(Country_Name, Polarizacion) %>%
  arrange(Polarizacion) %>%
  mutate(Polarizacion = round(Polarizacion, 2)) %>%
  head(5) %>%
  print()
cat("\n")

# REPORTE 4: Ranking general de apertura -----------------------------------
cat(" REPORTE 4: RANKING DE APERTURA A LA INMIGRACIÓN\n")
cat("------------------------------------------------------------------------------\n")
cat("TOP 5 PAÍSES MÁS ABIERTOS:\n")
countries_summary %>%
  select(Ranking, Country_Name, Avg_Immigration_Index, Categoria) %>%
  head(5) %>%
  mutate(Avg_Immigration_Index = round(Avg_Immigration_Index, 2)) %>%
  print()

cat("\nTOP 5 PAÍSES MÁS CERRADOS:\n")
countries_summary %>%
  select(Ranking, Country_Name, Avg_Immigration_Index, Categoria) %>%
  tail(5) %>%
  mutate(Avg_Immigration_Index = round(Avg_Immigration_Index, 2)) %>%
  print()
cat("\n")

# REPORTE 5: Brecha generacional (responde a P3) ---------------------------
cat(" REPORTE 5: BRECHA GENERACIONAL (Gen Z vs Boomers)\n")
cat("------------------------------------------------------------------------------\n")
brecha_gen <- datos_final %>%
  filter(
    cntry %in% c("ES", "PL"), 
    Age_Group %in% c("Gen Z (<27)", "Boomers+ (59+)")
  ) %>%
  group_by(Country_Name, Age_Group) %>%
  summarise(
    Avg_Immigration = round(mean(Immigration_Index, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Age_Group,
    values_from = c(Avg_Immigration, n)
  ) %>%
  mutate(
    Brecha = `Avg_Immigration_Gen Z (<27)` - `Avg_Immigration_Boomers+ (59+)`,
    Interpretacion = case_when(
      Brecha > 0.5 ~ "Jóvenes MÁS abiertos",
      Brecha < -0.5 ~ "Mayores MÁS abiertos",
      TRUE ~ "Sin diferencia significativa"
    )
  )
print(brecha_gen)
cat("\n Hallazgo inesperado: NO hay brecha generacional fuerte en ningún país\n")
cat("   España: Gen Z solo 0.04 puntos más abierta que Boomers\n")
cat("   Polonia: Gen Z 0.16 puntos MENOS abierta que Boomers (invertido)\n\n")

# REPORTE 6: Gradiente educativo (responde a P4)
cat(" REPORTE 6: EFECTO DE LA EDUCACIÓN (PREGUNTA 4)\n")
cat("------------------------------------------------------------------------------\n")
cat("¿A mayor educación, mayor apertura?\n\n")

gradiente_educativo <- education_immigration %>%
  filter(Country_Name %in% c("España", "Polonia")) %>%
  select(Country_Name, Education_Level, Avg_Immigration, n) %>%
  arrange(Country_Name, Education_Level) %>%
  mutate(Avg_Immigration = round(Avg_Immigration, 2))
print(gradiente_educativo)

# Calcular diferencia Básica-Superior
diferencia_edu <- education_immigration %>%
  filter(Country_Name %in% c("España", "Polonia")) %>%
  select(Country_Name, Education_Level, Avg_Immigration) %>%
  pivot_wider(names_from = Education_Level, values_from = Avg_Immigration) %>%
  mutate(
    Gradiente = Superior - Básica
  )
cat("\n Gradiente educativo (Superior - Básica):\n")
print(diferencia_edu %>% select(Country_Name, Básica, Superior, Gradiente))

# En REPORTE 6, AÑADIR después de imprimir diferencia_edu:

cat("\n Interpretación:\n")
cat("   - España tiene gradiente MAYOR (1.22 puntos)\n")
cat("   - Polonia tiene gradiente menor (0.98 puntos)\n")
cat("   - Conclusión: La educación superior amplía MÁS las actitudes en España\n")
cat("   - Con educación superior, la brecha ES-PL se reduce a 0.84 puntos\n\n")

# ==============================================================================
# 8. RESUMEN FINAL
# ==============================================================================

cat("\n")
cat("==============================================================================\n")
cat("                    PROCESO COMPLETADO CON ÉXITO                          \n")
cat("==============================================================================\n\n")

cat(" ARCHIVOS GENERADOS (4 CSV):\n")
cat("  1  ESS11_Countries_Summary.csv       = Mapa coroplético (Viz 1)\n")
cat("  2  ESS11_Education_Flourish.csv      = Bar chart educación (Viz 2)\n")
cat("  3  ESS11_Dumbbell_Flourish.csv       = Dumbbell polarización (Viz 3)\n")
cat("  4  ESS11_Heatmap_Data.csv            = Heatmap edad×política (Viz 4)\n\n")

cat(" ESTADÍSTICAS DEL DATASET LIMPIO:\n")
cat("  • Países analizados:", n_distinct(datos_final$cntry), "\n")
cat("  • Observaciones válidas:", nrow(datos_final), "\n")
cat("  • Rango de edad:", min(datos_final$agea), "-", max(datos_final$agea), "años\n")
cat("  • Periodo de campo: 2023-2024\n\n")

# Extras para el bar de educación.

# 1. Calcular la media europea exacta por nivel educativo
media_europea <- datos_final %>%
  filter(!is.na(Education_Level)) %>%
  group_by(Education_Level) %>%
  summarise(
    Avg_Immigration = weighted.mean(Immigration_Index, anweight, na.rm = TRUE),
    Avg_Culture = weighted.mean(imueclt, anweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    cntry = "AVG",
    Country_Name = "Media Europea", # Nombre para que salga en la leyenda
    Country_Highlight = "Media Europea", # Para colorear de gris
    Region = "Total",
    Education_Gradient = 0 # Valor dummy
  )

# 2. Filtrar solo España y Polonia de tus datos originales
esp_pol <- education_immigration %>%
  filter(Country_Name %in% c("España", "Polonia"))

# 3. Unir todo en un dataset final para esta visualización
viz_educacion_final <- bind_rows(esp_pol, media_europea)

# 4. Exportar
write_csv(viz_educacion_final, "ESS11_Education_Final_Viz.csv")

viz_educacion_wide <- viz_educacion_final %>%
  # Nos quedamos solo con lo que importa
  select(Education_Level, Country_Name, Avg_Immigration) %>%
  # Giramos la tabla: Los nombres de países se convierten en columnas
  pivot_wider(
    names_from = Country_Name,
    values_from = Avg_Immigration
  ) %>%
  # Ordenamos las filas para que salga: Básica, Media, Superior
  mutate(
    Education_Level = factor(Education_Level, levels = c("Básica", "Media", "Superior"))
  ) %>%
  arrange(Education_Level)

write_csv(viz_educacion_wide, "ESS11_Education_Wide.csv")
print(viz_educacion_wide)