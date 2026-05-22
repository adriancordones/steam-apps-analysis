library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
library(httr)

figs_dir <- "figs"
results_dir <- "results"

if (!dir.exists(figs_dir)) dir.create(figs_dir, recursive = TRUE)
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

# =========================
# Parámetro principal
# =========================

max_games <- 500

# =========================
# Carga del dataset original
# =========================

df <- read.csv("datasets/steam_apps.csv", sep = ",", dec = ".")

df <- df %>%
  slice(1:min(max_games, n()))

# =========================
# Funciones auxiliares API
# =========================

get_var <- function(data, path) {
  if (is.null(data)) return(NA)
  result <- data
  for (key in path) {
    result <- result[[key]]
    if (is.null(result)) return(NA)
  }
  return(result)
}

get_response <- function(url, cooldown = 10) {
  response <- tryCatch(GET(url, timeout(cooldown)), error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) return(NULL)
  content(response, as = "parsed", type = "application/json")
}

get_steamspy <- function(app_id) {
  url <- paste0("https://steamspy.com/api.php?request=appdetails&appid=", app_id)
  data <- get_response(url)
  owners <- get_var(data, c("owners"))
  Sys.sleep(1)
  return(owners)
}

owners_to_numeric <- function(x) {
  nums <- str_extract_all(as.character(x), "\\d+")[[1]]
  if (length(nums) == 0) return(NA)
  mean(as.numeric(nums))
}

# =========================
# Enriquecimiento con SteamSpy
# =========================

cat("\nConsultando SteamSpy para", nrow(df), "juegos...\n")

df$owners <- sapply(df$id, get_steamspy)
df$owners_numeric <- sapply(df$owners, owners_to_numeric)

# =========================
# Variable Indie
# =========================

if ("genres" %in% names(df)) {
  df$is_indie <- str_detect(str_to_lower(df$genres), "indie")
} else {
  possible_indie_cols <- names(df)[str_detect(names(df), "indie")]
  
  if (length(possible_indie_cols) > 0) {
    df$is_indie <- as.logical(df[[possible_indie_cols[1]]])
  } else {
    stop("No se encontró información para identificar juegos indie.")
  }
}

df_analysis <- df %>%
  filter(!is.na(owners_numeric), !is.na(is_indie)) %>%
  mutate(indie_label = ifelse(is_indie, "Indie", "No indie"))

cat("\nJuegos válidos para análisis:\n")
print(table(df_analysis$indie_label))

# =========================
# Resumen descriptivo
# =========================

summary_indie <- df_analysis %>%
  group_by(indie_label) %>%
  summarise(
    n = n(),
    min_owners = min(owners_numeric, na.rm = TRUE),
    q1_owners = quantile(owners_numeric, 0.25, na.rm = TRUE),
    median_owners = median(owners_numeric, na.rm = TRUE),
    mean_owners = mean(owners_numeric, na.rm = TRUE),
    q3_owners = quantile(owners_numeric, 0.75, na.rm = TRUE),
    max_owners = max(owners_numeric, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_indie)

write.csv(summary_indie, file.path(results_dir, "summary_indie_owners.csv"), row.names = FALSE)

# =========================
# Contraste de hipótesis
# =========================

indie_games <- df_analysis %>% filter(is_indie == TRUE)
non_indie_games <- df_analysis %>% filter(is_indie == FALSE)

cat("\nShapiro Indie:\n")
if (nrow(indie_games) >= 3) print(shapiro.test(sample(indie_games$owners_numeric, min(5000, nrow(indie_games)))))

cat("\nShapiro No Indie:\n")
if (nrow(non_indie_games) >= 3) print(shapiro.test(sample(non_indie_games$owners_numeric, min(5000, nrow(non_indie_games)))))

wilcox_result <- wilcox.test(indie_games$owners_numeric, non_indie_games$owners_numeric)
print(wilcox_result)

wilcox_output <- data.frame(
  method = wilcox_result$method,
  statistic = as.numeric(wilcox_result$statistic),
  p_value = wilcox_result$p.value
)

write.csv(wilcox_output, file.path(results_dir, "wilcox_indie_owners.csv"), row.names = FALSE)

# =========================
# Boxplot
# =========================

p_boxplot <- ggplot(df_analysis, aes(x = indie_label, y = owners_numeric, fill = indie_label)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
  scale_y_log10(labels = label_number(big.mark = ".")) +
  labs(
    title = "Distribución de ventas estimadas según tipo de juego",
    subtitle = "Comparación entre juegos indie y no indie",
    x = "Tipo de juego",
    y = "Owners estimados (escala log10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_boxplot)

ggsave(
  file.path(figs_dir, "boxplot_indie_owners.png"),
  plot = p_boxplot,
  width = 9,
  height = 5.5,
  dpi = 300
)

# =========================
# PCA y K-Means
# =========================

cluster_df <- df %>%
  mutate(owners_numeric = owners_numeric) %>%
  select(where(is.numeric)) %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  select(where(~ sd(., na.rm = TRUE) > 0))

cat("\nDimensiones para PCA/K-Means:\n")
print(dim(cluster_df))

scaled_data <- scale(cluster_df)

pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_importance <- summary(pca_result)$importance

pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$owners_numeric <- df$owners_numeric
pca_data$is_indie <- df$is_indie
pca_data$indie_label <- ifelse(df$is_indie, "Indie", "No indie")

p_pca_zoom <- ggplot(pca_data, aes(x = PC1, y = PC2, color = indie_label)) +
  geom_point(alpha = 0.55, size = 1.8) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  labs(
    title = "Representación PCA de los juegos",
    subtitle = "Vista ampliada de la zona central",
    x = paste0("PC1 (", round(pca_importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(pca_importance[2, 2] * 100, 1), "%)"),
    color = "Categoría"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_pca_zoom)

ggsave(
  file.path(figs_dir, "pca_juegos_zoom.png"),
  plot = p_pca_zoom,
  width = 9,
  height = 5.5,
  dpi = 300
)

set.seed(123)

kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

pca_data$cluster <- as.factor(kmeans_result$cluster)

cluster_summary <- pca_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    median_owners = median(owners_numeric, na.rm = TRUE),
    mean_owners = mean(owners_numeric, na.rm = TRUE),
    indie_ratio = mean(is_indie, na.rm = TRUE),
    .groups = "drop"
  )

print(cluster_summary)

write.csv(cluster_summary, file.path(results_dir, "cluster_summary.csv"), row.names = FALSE)

p_cluster_zoom <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6, size = 1.9) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(-10, 10)) +
  labs(
    title = "Agrupación de juegos mediante K-Means",
    subtitle = "Vista ampliada de la zona central",
    x = paste0("PC1 (", round(pca_importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(pca_importance[2, 2] * 100, 1), "%)"),
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_cluster_zoom)

ggsave(
  file.path(figs_dir, "kmeans_clusters_zoom.png"),
  plot = p_cluster_zoom,
  width = 9,
  height = 5.5,
  dpi = 300
)

write.csv(df, "datasets/steam_apps_cleaned.csv", row.names = FALSE)

cat("\nAnálisis finalizado correctamente con", nrow(df_analysis), "juegos válidos.\n")