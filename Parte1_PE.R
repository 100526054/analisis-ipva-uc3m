# ==============================================================================
# PROYECTO ANÁLISIS ESTADÍSTICO AVANZADO (UC3M) - FASE I
# SCRIPT COMPLETO: CARGA, LIMPIEZA, ANÁLISIS UNIVARIANTE, BIVARIANTE Y VISUALIZACIÓN
# Dataset: Ponderaciones del Índice de Precios de Vivienda en Alquiler (INE)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. VERIFICACIÓN E INSTALACIÓN AUTOMÁTICA DE LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes_requeridos <- c("readxl", "dplyr", "stringr", "ggplot2", "e1071", "patchwork")
paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[, "Package"])]

if (length(paquetes_faltantes) > 0) {
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)     # Para coeficientes de asimetría y curtosis de Fisher
library(patchwork) # Para composición profesional de gráficos

# ------------------------------------------------------------------------------
# 1. CARGA DE DATOS Y LIMPIEZA INICIAL DE METADATOS
# ------------------------------------------------------------------------------
ruta_archivo <- file.choose()

datos_raw <- read_excel(
  path = ruta_archivo,
  skip = 6,
  n_max = 765
)

colnames(datos_raw)[1] <- "municipio_raw"

# Renombrar columnas de años con el prefijo 'peso_' para fijar la unidad (‰) y evitar comillas invertidas
datos_raw <- datos_raw %>%
  rename_with(~ paste0("peso_", .x), matches("^(19|20)\\d{2}$"))

# ------------------------------------------------------------------------------
# 2. MAPEO TERRITORIAL (48 PROVINCIAS / CIUDADES AUTÓNOMAS DE TERRITORIO COMÚN)
# ------------------------------------------------------------------------------
mapa_territorial <- data.frame(
  cod_prov = c(
    "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
    "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45",
    "46", "47", "49", "50", "51", "52"
  ),
  provincia = c(
    "Albacete", "Alicante/Alacant", "Almería", "Ávila", "Badajoz", "Balears, Illes",
    "Barcelona", "Burgos", "Cáceres", "Cádiz", "Castellón/Castelló", "Ciudad Real",
    "Córdoba", "A Coruña", "Cuenca", "Girona", "Granada", "Guadalajara", "Huelva",
    "Huesca", "Jaén", "León", "Lleida", "La Rioja", "Lugo", "Madrid", "Málaga",
    "Murcia", "Ourense", "Asturias", "Palencia", "Las Palmas", "Pontevedra",
    "Salamanca", "Santa Cruz de Tenerife", "Cantabria", "Segovia", "Sevilla",
    "Soria", "Tarragona", "Teruel", "Toledo", "Valencia/València", "Valladolid",
    "Zamora", "Zaragoza", "Ceuta", "Melilla"
  ),
  ccaa = c(
    "Castilla-La Mancha", "Comunitat Valenciana", "Andalucía", "Castilla y León",
    "Extremadura", "Illes Balears", "Cataluña", "Castilla y León", "Extremadura",
    "Andalucía", "Comunitat Valenciana", "Castilla-La Mancha", "Andalucía", "Galicia",
    "Castilla-La Mancha", "Cataluña", "Andalucía", "Castilla-La Mancha", "Andalucía",
    "Aragón", "Andalucía", "Castilla y León", "Cataluña", "La Rioja", "Galicia",
    "Comunidad de Madrid", "Andalucía", "Región de Murcia", "Galicia", "Principado de Asturias",
    "Castilla y León", "Canarias", "Galicia", "Castilla y León", "Canarias",
    "Cantabria", "Castilla y León", "Andalucía", "Castilla y León", "Cataluña",
    "Aragón", "Castilla-La Mancha", "Comunitat Valenciana", "Castilla y León",
    "Castilla y León", "Aragón", "Ceuta", "Melilla"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 3. TRANSFORMACIÓN E INGENIERÍA DE VARIABLES (DATA WRANGLING)
# ------------------------------------------------------------------------------
datos_limpios <- datos_raw %>%
  mutate(
    municipio_raw = str_trim(municipio_raw),
    tipo_entidad = case_when(
      municipio_raw == "Total Nacional" ~ "Total Nacional",
      str_detect(municipio_raw, "^Resto") ~ "Agregado Provincial",
      TRUE ~ "Municipio >10k"
    ),
    cod_ine = ifelse(tipo_entidad == "Municipio >10k", str_sub(municipio_raw, 1, 5), NA_character_),
    nombre_municipio = ifelse(tipo_entidad == "Municipio >10k", str_trim(str_sub(municipio_raw, 7)), municipio_raw),
    cod_prov = str_sub(cod_ine, 1, 2)
  ) %>%
  left_join(mapa_territorial, by = "cod_prov") %>%
  mutate(
    provincia = ifelse(tipo_entidad == "Agregado Provincial", str_replace(nombre_municipio, "Resto ", ""), provincia)
  ) %>%
  select(
    cod_ine, nombre_municipio, tipo_entidad, provincia, ccaa,
    starts_with("peso_")
  )

# Filtrar submuestra de trabajo: Municipios de >10.000 habitantes
df_municipios <- datos_limpios %>%
  filter(tipo_entidad == "Municipio >10k") %>%
  mutate(across(starts_with("peso_"), ~ as.numeric(as.character(.x))))

# ------------------------------------------------------------------------------
# 4. ANÁLISIS UNIVARIANTE: ESTADÍSTICOS DESCRIPTIVOS (2012 vs 2024)
# ------------------------------------------------------------------------------
calcular_descriptivos <- function(vector_datos, nombre_variable) {
  datos_val = vector_datos[!is.na(vector_datos)]
  
  q1 <- quantile(datos_val, 0.25)
  q3 <- quantile(datos_val, 0.75)
  iqr_val <- q3 - q1
  media_val <- mean(datos_val)
  sd_val <- sd(datos_val)
  
  data.frame(
    Variable = nombre_variable,
    N_validos = length(datos_val),
    Media = round(media_val, 4),
    Mediana = round(median(datos_val), 4),
    Desv_Tipica = round(sd_val, 4),
    CV_Porc = round((sd_val / media_val) * 100, 2),
    Minimo = round(min(datos_val), 4),
    Q1 = round(q1, 4),
    Q3 = round(q3, 4),
    Maximo = round(max(datos_val), 4),
    IQR = round(iqr_val, 4),
    Asimetria_Fisher = round(skewness(datos_val), 4),
    Curtosis_Fisher = round(kurtosis(datos_val), 4)
  )
}

tabla_univariante <- rbind(
  calcular_descriptivos(df_municipios$peso_2024, "Ponderación IPVA 2024 (‰)"),
  calcular_descriptivos(df_municipios$peso_2012, "Ponderación IPVA 2012 (‰)")
)

cat("====================================================\n")
cat("2. TABLA DE ESTADÍSTICOS DESCRIPTIVOS UNIVARIANTES\n")
cat("====================================================\n")
print(tabla_univariante)

# ------------------------------------------------------------------------------
# 5. DETECCIÓN DE VALORES ATÍPICOS (OUTLIERS) - REGLA 1.5 x IQR
# ------------------------------------------------------------------------------
q1_24 <- quantile(df_municipios$peso_2024, 0.25, na.rm = TRUE)
q3_24 <- quantile(df_municipios$peso_2024, 0.75, na.rm = TRUE)
iqr_24 <- q3_24 - q1_24
limite_superior_24 <- q3_24 + 1.5 * iqr_24

outliers_2024 <- df_municipios %>%
  filter(peso_2024 > limite_superior_24) %>%
  select(cod_ine, nombre_municipio, provincia, ccaa, peso_2024) %>%
  arrange(desc(peso_2024))

cat("\n====================================================\n")
cat("3. DIAGNÓSTICO DE VALORES ATÍPICOS (EJERCICIO 2024)\n")
cat("====================================================\n")
cat("Límite superior atípicos (Q3 + 1.5*IQR):", round(limite_superior_24, 4), "‰\n")
cat("Número total de municipios atípicos:", nrow(outliers_2024), "\n\n")
cat("Top 10 observaciones más extremas (grandes ciudades):\n")
print(head(outliers_2024, 10))

# ------------------------------------------------------------------------------
# 6. ANÁLISIS BIVARIANTE Y MODELO DE REGRESIÓN LINEAL SIMPLE (MCO)
# ------------------------------------------------------------------------------
df_bivariante <- df_municipios %>%
  filter(!is.na(peso_2012), !is.na(peso_2024))

correlacion_pearson <- cor(df_bivariante$peso_2012, df_bivariante$peso_2024, method = "pearson")
covarianza_val <- cov(df_bivariante$peso_2012, df_bivariante$peso_2024)

modelo_mco <- lm(peso_2024 ~ peso_2012, data = df_bivariante)
summary_mco <- summary(modelo_mco)

cat("\n====================================================\n")
cat("4. RESULTADOS BIVARIANTES Y REGRESIÓN LINEAL (2012 vs 2024)\n")
cat("====================================================\n")
cat("Número de municipios pareados (N):", nrow(df_bivariante), "\n")
cat("Covarianza Muestral (Sxy):", round(covarianza_val, 4), "\n")
cat("Coeficiente de Correlación de Pearson (r):", round(correlacion_pearson, 4), "\n")
cat("Coeficiente de Determinación (R^2):", round(summary_mco$r.squared, 4), "\n\n")
cat("Ecuación MCO ajustada: peso_2024 = beta0 + beta1 * peso_2012\n")
cat("  Intercepto (beta0):", round(coef(modelo_mco)[1], 4), "\n")
cat("  Pendiente  (beta1):", round(coef(modelo_mco)[2], 4), "\n")

# ------------------------------------------------------------------------------
# 7. GENERACIÓN, DIBUJO EN PANTALLA Y EXPORTACIÓN A DISCO DE FIGURAS
# ------------------------------------------------------------------------------
# Figura 1A: Histograma Logarítmico
g_hist <- ggplot(df_municipios %>% filter(!is.na(peso_2024)), aes(x = peso_2024)) +
  geom_histogram(aes(y = ..density..), bins = 35, fill = "#2b5c8f", color = "white", alpha = 0.8) +
  geom_density(color = "#e74c3c", linewidth = 1) +
  scale_x_log10() +
  theme_minimal(base_size = 11) +
  labs(
    title = "A. Distribución de Ponderaciones (Escala Log10)",
    subtitle = "Histograma y densidad estimada para 2024",
    x = "Ponderación IPVA 2024 en ‰ (Escala log10)",
    y = "Densidad"
  )

# Figura 1B: Boxplot con Media (Rombo rojo)
g_box <- ggplot(df_municipios %>% filter(!is.na(peso_2024)), aes(y = peso_2024, x = "")) +
  geom_boxplot(fill = "#d4e6f1", color = "#1b4f72", outlier.color = "#c0392b", outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5, color = "red") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_minimal(base_size = 11) +
  labs(
    title = "B. Boxplot IPVA 2024 (Detalle Rango 0-4‰)",
    subtitle = "El rombo rojo indica la Media (1.2931‰)",
    y = "Ponderación IPVA 2024 (‰)",
    x = ""
  )

# Combinar, GUARDAR en disco y MOSTRAR en la pestaña Plots de RStudio
figura1_univariante <- g_hist + g_box
ggsave("figura1_univariante.png", figura1_univariante, width = 10, height = 4.5, dpi = 300) # Descarga en disco
print(figura1_univariante) # Representación interactiva en RStudio

# Figura 2: Diagrama de Dispersión y Regresión Lineal MCO
df_sub_grafico <- df_bivariante %>% filter(peso_2024 < 20)

figura2_regresion <- ggplot(df_sub_grafico, aes(x = peso_2012, y = peso_2024)) +
  geom_point(color = "#34495e", alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", color = "#27ae60", se = TRUE, fill = "#27ae60", alpha = 0.2) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Relación Lineal entre Ponderación IPVA 2012 y 2024",
    subtitle = "Ajuste por Mínimos Cuadrados Ordinarios (MCO) con IC del 95% | R² = 0.9911",
    x = "Ponderación IPVA 2012 (‰)",
    y = "Ponderación IPVA 2024 (‰)"
  )

# GUARDAR en disco y MOSTRAR en la pestaña Plots de RStudio
ggsave("figura2_regresion.png", figura2_regresion, width = 7.5, height = 5, dpi = 300) # Descarga en disco
print(figura2_regresion) # Representación interactiva en RStudio

cat("\n====================================================\n")
cat("EJECUCIÓN COMPLETADA EXITOSAMENTE:\n")
cat("  1. Gráficos representados en el panel 'Plots' de RStudio.\n")
cat("  2. Archivos PNG descargados en alta resolución (300 DPI) para el informe PDF.\n")
cat("====================================================\n")