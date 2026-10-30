# ==============================================================================
# PROYECTO ANÁLISIS ESTADÍSTICO AVANZADO (UC3M) - FASE I (SCRIPT DEFINITIVO V4)
# Dataset: Ponderaciones IPVA (INE - Tabla 50083)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. VERIFICACIÓN E INSTALACIÓN DE LIBRERÍAS
# ------------------------------------------------------------------------------
paquetes <- c("readxl", "dplyr", "stringr", "ggplot2", "e1071", "patchwork", 
              "janitor", "naniar", "car", "gt", "tidyr")
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
if (length(paquetes_faltantes) > 0) install.packages(paquetes_faltantes)

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(e1071)
library(patchwork)
library(janitor)
library(naniar)
library(car)
library(gt)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. CARGA DE DATOS Y RUTA RELATIVA (REPRODUCIBILIDAD)
# ------------------------------------------------------------------------------
# Se utiliza ruta relativa dentro del proyecto RStudio
ruta_datos <- "datos/50083.xlsx"

if (!file.exists(ruta_datos)) {
  # Fallback si no está en la carpeta local
  message("Advertencia: No se encontró 'datos/50083.xlsx'. Seleccione el archivo manualmente:")
  ruta_datos <- file.choose()
}

datos_raw <- read_excel(path = ruta_datos, skip = 6, n_max = 765)
colnames(datos_raw)[1] <- "municipio_raw"
datos_raw <- datos_raw %>% rename_with(~ paste0("peso_", .x), matches("^(19|20)\\d{2}$"))

# ------------------------------------------------------------------------------
# 2. MAPEO TERRITORIAL Y WRANGLING
# ------------------------------------------------------------------------------
mapa_territorial <- data.frame(
  cod_prov = c("02","03","04","05","06","07","08","09","10","11","12","13","14","15",
               "16","17","18","19","21","22","23","24","25","26","27","28","29","30",
               "32","33","34","35","36","37","38","39","40","41","42","43","44","45",
               "46","47","49","50","51","52"),
  provincia = c("Albacete","Alicante/Alacant","Almería","Ávila","Badajoz","Balears, Illes",
                "Barcelona","Burgos","Cáceres","Cádiz","Castellón/Castelló","Ciudad Real",
                "Córdoba","A Coruña","Cuenca","Girona","Granada","Guadalajara","Huelva",
                "Huesca","Jaén","León","Lleida","La Rioja","Lugo","Madrid","Málaga",
                "Murcia","Ourense","Asturias","Palencia","Las Palmas","Pontevedra",
                "Salamanca","Santa Cruz de Tenerife","Cantabria","Segovia","Sevilla",
                "Soria","Tarragona","Teruel","Toledo","Valencia/València","Valladolid",
                "Zamora","Zaragoza","Ceuta","Melilla"),
  ccaa = c("Castilla-La Mancha","Comunitat Valenciana","Andalucía","Castilla y León",
           "Extremadura","Illes Balears","Cataluña","Castilla y León","Extremadura",
           "Andalucía","Comunitat Valenciana","Castilla-La Mancha","Andalucía","Galicia",
           "Castilla-La Mancha","Cataluña","Andalucía","Castilla-La Mancha","Andalucía",
           "Aragón","Andalucía","Castilla y León","Cataluña","La Rioja","Galicia",
           "Comunidad de Madrid","Andalucía","Región de Murcia","Galicia","Principado de Asturias",
           "Castilla y León","Canarias","Galicia","Castilla y León","Canarias",
           "Cantabria","Castilla y León","Andalucía","Castilla y León","Cataluña",
           "Aragón","Castilla-La Mancha","Comunitat Valenciana","Castilla y León",
           "Castilla y León","Aragón","Ceuta","Melilla"),
  stringsAsFactors = FALSE
)

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
  select(cod_ine, nombre_municipio, tipo_entidad, provincia, ccaa, starts_with("peso_"))

df_municipios <- datos_limpios %>%
  filter(tipo_entidad == "Municipio >10k") %>%
  mutate(across(starts_with("peso_"), ~ as.numeric(as.character(.x))))

# ------------------------------------------------------------------------------
# 3. VERIFICACIÓN DE CALIDAD DE DATOS (PUNTO CRÍTICO 1)
# ------------------------------------------------------------------------------
cat("--- AUDITORÍA DE DUPLICADOS ---\n")
duplicados <- df_municipios %>% janitor::get_dupes(cod_ine)
cat("Número de registros duplicados detectados por código INE:", nrow(duplicados), "\n\n")

cat("--- AUDITORÍA DE VALORES AUSENTES (NA) ---\n")
na_conteo <- colSums(is.na(df_municipios %>% select(starts_with("peso_"))))
print(na_conteo)

# Submuestra pareada para comparaciones homogéneas (N=700)
df_paired <- df_municipios %>% filter(!is.na(peso_2012) & !is.na(peso_2024))

# ------------------------------------------------------------------------------
# 4. TUKEY EN ESCALA NATURAL VS LOGARÍTMICA (PUNTO IMPORTANTE 11)
# ------------------------------------------------------------------------------
# Escala original
q1_raw <- quantile(df_municipios$peso_2024, 0.25, na.rm = TRUE)
q3_raw <- quantile(df_municipios$peso_2024, 0.75, na.rm = TRUE)
iqr_raw <- q3_raw - q1_raw
cutoff_raw <- q3_raw + 1.5 * iqr_raw
outliers_raw <- sum(df_municipios$peso_2024 > cutoff_raw, na.rm = TRUE)

# Escala Log10
log_peso24 <- log10(df_municipios$peso_2024)
q1_log <- quantile(log_peso24, 0.25, na.rm = TRUE)
q3_log <- quantile(log_peso24, 0.75, na.rm = TRUE)
iqr_log <- q3_log - q1_log
cutoff_log_sup <- q3_log + 1.5 * iqr_log
cutoff_log_inf <- q1_log - 1.5 * iqr_log
outliers_log <- sum(log_peso24 > cutoff_log_sup | log_peso24 < cutoff_log_inf, na.rm = TRUE)

cat("\n--- DIAGNÓSTICO DE ATÍPICOS (TUKEY) ---\n")
cat("Atípicos en escala original (cola de asimetría):", outliers_raw, "(", round(outliers_raw/7.16, 2), "%)\n")
cat("Atípicos en escala Log10 (tras corregir asimetría):", outliers_log, "(", round(outliers_log/7.16, 2), "%)\n")

# ------------------------------------------------------------------------------
# 5. ANÁLISIS BIVARIANTE Y MODELIZACIÓN LOG-LOG (PUNTOS CRÍTICOS 2 Y 3)
# ------------------------------------------------------------------------------
# Correlaciones
r_pearson <- cor(df_paired$peso_2012, df_paired$peso_2024, method = "pearson")
r_spearman <- cor(df_paired$peso_2012, df_paired$peso_2024, method = "spearman")

# Modelo lineal en escala original
mco_raw <- lm(peso_2024 ~ peso_2012, data = df_paired)

# Modelo Log-Log
mco_log <- lm(log(peso_2024) ~ log(peso_2012), data = df_paired)
summary_log <- summary(mco_log)

# Contraste de Hipótesis: H0: beta1 = 1 (Elasticidad unitaria)
test_beta1 <- car::linearHypothesis(mco_log, "log(peso_2012) = 1")

# Distancia de Cook (Influencia de Madrid y Barcelona)
df_paired$cookd <- cooks.distance(mco_raw)
top_cook <- df_paired %>% select(nombre_municipio, peso_2012, peso_2024, cookd) %>% arrange(desc(cookd)) %>% head(5)

cat("\n--- RESULTADOS BIVARIANTES ROBUSTOS ---\n")
cat("Correlación de Pearson (sensible a outliers):", round(r_pearson, 4), "\n")
cat("Correlación de Spearman (robusta a asimetría):", round(r_spearman, 4), "\n")
cat("R^2 Modelo Log-Log:", round(summary_log$r.squared, 4), "\n")
cat("Pendiente estimada Log-Log (Elasticidad):", round(coef(mco_log)[2], 4), "\n")
cat("p-value Contraste H0: Beta1 = 1:", test_beta1$`Pr(>F)`[2], "\n")

# ------------------------------------------------------------------------------
# 6. ANÁLISIS POR CCAA Y SERIE TEMPORAL (PUNTOS IMPORTANTES 8 Y 9)
# ------------------------------------------------------------------------------
# Frecuencias por CCAA
tabla_ccaa <- df_municipios %>% count(ccaa) %>% arrange(desc(n))

# Kruskal-Wallis (diferencias de distribución entre CCAA)
kw_test <- kruskal.test(peso_2024 ~ ccaa, data = df_municipios)

# Serie temporal Top 10 municipios (2012-2024)
top10_muni <- df_municipios %>% arrange(desc(peso_2024)) %>% head(10) %>% pull(cod_ine)

df_long <- df_municipios %>%
  filter(cod_ine %in% top10_muni) %>%
  pivot_longer(cols = starts_with("peso_"), names_to = "anio", values_to = "peso") %>%
  mutate(anio = as.numeric(str_replace(anio, "peso_", "")))

# ------------------------------------------------------------------------------
# 7. GENERACIÓN Y EXPORTACIÓN DE FIGURAS
# ------------------------------------------------------------------------------
# Figura 1: Histograma Log10 y Boxplot
g_hist <- ggplot(df_municipios %>% filter(!is.na(peso_2024)), aes(x = peso_2024)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#002060", color = "white", alpha = 0.8) +
  geom_density(color = "#e74c3c", linewidth = 1) +
  scale_x_log10() +
  theme_minimal(base_size = 11) +
  labs(title = "A. Distribución en Escala Log10 (2024)", x = "Ponderación IPVA (‰, log10)", y = "Densidad")

g_box <- ggplot(df_municipios %>% filter(!is.na(peso_2024)), aes(y = peso_2024, x = "")) +
  geom_boxplot(fill = "#d4e6f1", color = "#002060", outlier.color = "#c0392b", outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3.5, color = "red") +
  coord_cartesian(ylim = c(0, 4)) +
  theme_minimal(base_size = 11) +
  labs(title = "B. Detalle Rango 0-4‰ (Media en rojo)", y = "Ponderación IPVA (‰)", x = "")

figura1 <- g_hist + g_box
ggsave("figura1_univariante.png", figura1, width = 10, height = 4.5, dpi = 300)

# Figura 2: Regresión en Escala Log-Log (Solución a sesgos)
figura2 <- ggplot(df_paired, aes(x = log(peso_2012), y = log(peso_2024))) +
  geom_point(color = "#34495e", alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", color = "#27ae60", se = TRUE, fill = "#27ae60", alpha = 0.2) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Ajuste Log-Log entre Ponderaciones 2012 y 2024",
    subtitle = paste0("Modelo Log-Log | R² = ", round(summary_log$r.squared, 4), 
                      " | Spearman r = ", round(r_spearman, 4)),
    x = "log(Ponderación 2012)", y = "log(Ponderación 2024)"
  )
ggsave("figura2_regresion.png", figura2, width = 7.5, height = 5, dpi = 300)

# Figura 3 (NUEVA): Serie Temporal Top 10 Municipios (2012-2024)
figura3_serie <- ggplot(df_long, aes(x = anio, y = peso, color = nombre_municipio)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 11) +
  labs(title = "Evolución Temporal del Peso Muestral (Top 10 Municipios)",
       x = "Año", y = "Ponderación IPVA (‰)", color = "Municipio")
ggsave("figura3_serietemporal.png", figura3_serie, width = 8.5, height = 5, dpi = 300)

cat("\n====================================================\n")
cat("EJECUCIÓN COMPLETADA CON ÉXITO\n")
cat("====================================================\n")
