library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(ggplot2)
library(randomForest)
library(caret)
library(uwot)
library(cluster)
library(clue)
library(effsize)

# Paths de interés
dfs_path <- "datasets"
fig_path <- "docs/figs"
res_path <- "docs/results"
# Comprobación de que los paths existen (y si no, se crean)
check_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}
paths <- c(dfs_path, fig_path)
for (i in 1:length(paths)) {
  check_dir(paths[i])
}

# Carga del dataset de partida
df_path <- paste(dfs_path, "steam_apps_temp.csv", sep = "/")
df <- read.csv(df_path, sep = ",", dec = ".")

#### Selección de los datos ####

# Variables descriptivas (en su mayoría predictoras, y algunas de apoyo)
df <- df %>%
  select(title, date, original_price, achievements_number, langs_number, dlcs_number, has_ost, is_dlc, is_ost, genres,
         total_positive, total_reviews, required_age, pegi, owners, players_peak)

#### Limpieza de los datos ####

## Parte 1. Transformación de variables

# Transformación de columna 'owners' a variable objetivo 'estimate_sales' (categórica)
cat("\nValores únicos de 'owners':\n")
print(sort(unique(df$owners)))
df <- df %>%
  rename(estimate_sales = owners) %>%
  mutate(estimate_sales = case_when(
    estimate_sales %in% c("0 .. 20,000", "20,000 .. 50,000", "50,000 .. 100,000") ~ "low",
    estimate_sales %in% c("100,000 .. 200,000", "200,000 .. 500,000") ~ "medium_low",
    estimate_sales %in% c("500,000 .. 1,000,000", "1,000,000 .. 2,000,000") ~ "medium",
    estimate_sales %in% c("2,000,000 .. 5,000,000", "5,000,000 .. 10,000,000") ~ "medium_high",
    estimate_sales %in% c("10,000,000 .. 20,000,000", "20,000,000 .. 50,000,000", "50,000,000 .. 100,000,000") ~ "high",
    .default = NA) %>%
    factor(levels = c("low", "medium_low", "medium", "medium_high", "high"), ordered = TRUE))
cat("\nCOMPROBACIÓN: Valores únicos de 'estimate_sales':\n")
print(sort(unique(df$estimate_sales)))

# Transformación de columna 'date' a categórica
df <- df %>%
  mutate(date = case_when(
    str_detect(str_to_title(date), "Jan|Feb|Mar") ~ paste0("Q1-", str_extract(date, "\\d{4}")),
    str_detect(str_to_title(date), "Apr|May|Jun") ~ paste0("Q2-", str_extract(date, "\\d{4}")),
    str_detect(str_to_title(date), "Jul|Aug|Sep") ~ paste0("Q3-", str_extract(date, "\\d{4}")),
    str_detect(str_to_title(date), "Oct|Nov|Dec") ~ paste0("Q4-", str_extract(date, "\\d{4}")),
    .default = NA)) %>%
  mutate(date = factor(date, levels = unique(date[order(str_extract(date, "\\d{4}"), str_extract(date, "Q\\d"))]), ordered = TRUE))
cat("\nCOMPROBACIÓN: Valores únicos de 'date':\n")
print(sort(unique(df$date)))

# Corrección de columnas 'has_ost', 'is_dlc' y 'is_ost' a verdaderos bools
df <- df %>%
  mutate(across(c(has_ost, is_dlc, is_ost), as.logical))

# Creación de columnas booleanas para cada género
genre_names <- unique(unlist(strsplit(df$genres, "\\|")))
genre_cols <- genre_names %>%
  str_to_lower() %>%
  str_replace_all("&", "and") %>%
  str_replace_all("\\s+", "_")
for (i in seq_along(genre_cols)) {
  col_name <- paste0("is_", genre_cols[i])
  df[[col_name]] <- str_detect(df$genres, fixed(genre_names[i]))
}
# Corrección de columna 'is_strategy' y eliminación de 'is_estrategia'
df <- df %>%
  mutate(is_strategy = is_strategy | is_estrategia) %>%
  select(-is_estrategia)
# Eliminación de columna 'genres' original tras transformación
df <- df %>%
  select(-genres)

## Parte 2. Imputación de NAs

# Comprobación previa
cat("\nRecuento de NAs por columnas:\n")
print(colSums(is.na(df)))

cat("\nTítulos con NAs (excluyendo NAs en 'pegi'):\n")
na_cols <- names(df)[colSums(is.na(df)) > 0 & names(df) != "pegi"]
na_rows <- rowSums(is.na(df[, names(df) != "pegi"])) > 0
print(df[na_rows, c("title", na_cols)])

cat("\nTítulos con NAs (únicamente NAs en 'pegi'):\n")
na_rows <- is.na(df$pegi)
print(df[na_rows, c("title", "pegi")])

# Transformación de columna 'required_age' usando la misma y 'pegi' + imputación de NAs
cat("\nValores únicos de 'required_age':\n")
print(sort(unique(df$required_age)))
cat("\nValores únicos de 'pegi':\n")
print(sort(unique(df$pegi)))

df <- df %>%
  mutate(required_age = pmax(replace_na(required_age, 0), replace_na(pegi, 0), na.rm = TRUE)) %>%
  select(-pegi)

cat("\nCOMPROBACIÓN: Valores únicos de 'required_age':\n")
print(sort(unique(df$required_age)))

# Imputación para 'players_peak'
df <- df %>%
  mutate(langs_number = replace(langs_number, is.na(langs_number), 0)) %>%
  mutate(langs_number = as.integer(langs_number))

# Imputación para 'langs_number'
df <- df %>%
  mutate(players_peak = replace(players_peak, is.na(players_peak), 0)) %>%
  mutate(players_peak = as.integer(players_peak))

# Comprobación de variables
cat("\nCOMPROBACIÓN: Recuento de NAs por columnas:\n")
print(colSums(is.na(df)))

cat("\nCOMPROBACIÓN: Resumen del dataset\n\n")
str(df)

#### Análisis de los datos ####

# Fijar seed de aleatoriedad para reproducibilidad
set.seed(42)

total <- nrow(df)
ost_count <- sum(df$is_ost, na.rm = TRUE)
dlc_count <- sum(df$is_dlc, na.rm = TRUE)
cat("\nRecuento para análisis:\n")
cat(sprintf("Nº de OSTs: %4d (%4.1f%%)\n", ost_count, ost_count / total * 100))
cat(sprintf("Nº de DLCs: %4d (%4.1f%%)\n", dlc_count, dlc_count / total * 100))

# Para los análisis, dividimos el dataset en dos con juegos y con DLCs (descartando también OSTs)
df_games <- df %>%
  filter(!is_ost, !is_dlc) %>%
  select(-is_ost, -is_dlc)

df_dlcs <- df %>%
  filter(!is_ost, is_dlc) %>%
  select(-is_ost, -is_dlc)

## Parte 1. Análisis descriptivo

sales_labels <- c("Bajas (<100k)", "Medio-bajas (100k-500k)", "Medias (500k-2M)", "Medio-altas (2M-10M)", "Altas (>10M)")

p_games <- ggplot(df_games, aes(x = ifelse(is_indie, "Indie", "No indie (AA o AAA)"), fill = estimate_sales)) +
  geom_bar(position = "dodge") + scale_fill_discrete(labels = sales_labels) +
  labs(title = "Distribución de ventas estimadas según tipo de juego", subtitle = "Comparación entre juegos 'Indies' y 'No indies'",
       x = "Tipo de juego", y = "Número de juegos", fill = "Ventas estimadas") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

p_dlcs <- ggplot(df_dlcs, aes(x = ifelse(is_indie, "Indie", "No indie (AA o AAA)"), fill = estimate_sales)) +
  geom_bar(position = "dodge") + scale_fill_discrete(labels = sales_labels) +
  labs(title = "Distribución de ventas estimadas según tipo de DLC", subtitle = "Comparación entre DLCs 'Indies' y 'No indies'", 
       x = "Tipo de DLC", y = "Número de DLCs", fill = "Ventas estimadas") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

## Parte 2. Método supervisado (Random Forest)
cat("\n\n--- Método supervisado: Random Forest ---\n")

# Preparación de datos
rf_data <- df_games %>%
  select(-title)

# Separación en conjuntos de Train y Test (80/20)
train_idx <- createDataPartition(rf_data$estimate_sales, p = 0.8, list = FALSE)
train <- rf_data[train_idx, ]
test <- rf_data[-train_idx, ]

cat(sprintf("\nTrain: %d muestras\nTest:  %d muestras\n", nrow(train), nrow(test)))
cat("\nDistribución de clases en train:\n")
print(prop.table(table(train$estimate_sales)))
cat("\nDistribución de clases en test:\n")
print(prop.table(table(test$estimate_sales)))

# Entrenamiento del modelo
rf_model <- randomForest(estimate_sales ~ ., data = train, ntree = 500, importance = TRUE)
print(rf_model)

# Evaluación en test
pred_sales <- predict(rf_model, newdata = test)

# Matriz de confusión (test)
cm <- confusionMatrix(pred_sales, test$estimate_sales)
print(cm)

cm_df <- as.data.frame(cm$table) %>%
  rename(Predicted = Prediction, Actual = Reference)

# Gráfica: Matriz de confusión
p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +   geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(low = "lightskyblue1", high = "darkorchid1") +
  scale_y_discrete(limits = rev(levels(cm_df$Predicted))) +
  labs(title = "Matriz de confusión (Random Forest)",
       x = "Clase real", y = "Clase predicha", fill = "N") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

# F1 scores por clase y macro
f1_class <- cm$byClass[, "F1"]
f1_macro <- mean(f1_class, na.rm = TRUE)
cat(sprintf("\nF1 macro: %.4f\n", f1_macro))
cat("\nF1 por clase:\n\n")
print(round(f1_class, 4))

# Importancia de variables
df_importance <- as.data.frame(importance(rf_model)) %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(MeanDecreaseAccuracy))

cat("\nTop 15 variables más relevantes:\n")
print(head(df_importance, 15))

# Gráfica: Importancia de variables
p_importance <- ggplot(head(df_importance, 15), aes(x = reorder(variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  labs(title = "Importancia de variables (Random Forest)", subtitle = "Top 15 variables más relevantes", x = NULL, y = "MeanDecreaseAccuracy") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank()) +
  geom_col(fill = "steelblue") + coord_flip()

## Parte 3. Método no supervisado
cat("\n\n--- Método no supervisado: K-means ---\n")

# Descartamos título y variable objetivo (estimate_sales)
X_games <- df_games %>%
  select(-title, -estimate_sales) %>%
  mutate(date = as.integer(date), across(where(is.logical), as.integer))

X_dlc <- df_dlcs %>%
  select(-title, -estimate_sales) %>%
  mutate(date = as.integer(date), across(where(is.logical), as.integer))

# PCA
pca_result <- prcomp(X_games, center = TRUE, scale. = TRUE)

X_games_PCA <- as.data.frame(pca_result$x[, 1:2]) %>%
  mutate(estimate_sales = df_games$estimate_sales)

var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var <- cumsum(var_explained)

cat("\nUtilizando PCA se tiene:\n")
cat("\n- Varianza explicada por 10 primeras componentes:\n")
print(round(var_explained[1:10], 3))
cat("\n- Varianza acumulada para cada componente:\n")
print(round(cum_var[1:10], 3))

p_pca <- ggplot(X_games_PCA, aes(x = PC1, y = PC2, color = estimate_sales)) +
  geom_point(alpha = 0.5, size = 1.5) + scale_color_brewer(palette = "RdYlGn", labels = sales_labels) +
  labs(title = "Proyección PCA", x = "PC1", y = "PC2", color = "Ventas estimadas",
       subtitle = sprintf("Varianzas: PC1: %.1f%% / PC2: %.1f%% / Total: %.1f%%",
                          var_explained[1] * 100, var_explained[2] * 100, cum_var[2] * 100)) +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# UMAP
umap_result <- umap(X_games, n_components = 2, n_neighbors = 15, min_dist = 0.2, metric = "euclidean")

X_games_UMAP <- as.data.frame(umap_result[, 1:2]) %>%
  mutate(estimate_sales = df_games$estimate_sales)

p_umap <- ggplot(X_games_UMAP, aes(x = V1, y = V2, color = estimate_sales)) +
  geom_point(alpha = 0.5, size = 1.5) + scale_color_brewer(palette = "RdYlGn", labels = sales_labels) +
  labs(title = "Proyección UMAP", x = "Dimensión 1", y = "Dimensión 2", color = "Ventas estimadas") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Método del codo para K-Means
wcss <- sapply(1:10, function(k) {kmeans(umap_result, centers = k, nstart = 25, iter.max = 100)$tot.withinss})

df_elbow <- data.frame(k = 1:10, wcss = wcss)

p_elbow <- ggplot(df_elbow, aes(x = k, y = wcss)) + geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  labs(title = "Método del codo", x = "Número de clusters (k)", y = "Suma total de cuadrados intra-clusters (WCSS)") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Aplicación de K-Means (con clusters esperados: 5 categorías de ventas estimadas)
n_clusters <- 5
kmeans_result <- kmeans(umap_result, centers = n_clusters, nstart = 25, iter.max = 100)
X_games_UMAP$cluster <- factor(kmeans_result$cluster)

# Gráfica: K-Means sobre UMAP (5 clusters)
p_kmeans <- ggplot(X_games_UMAP, aes(x = V1, y = V2, color = cluster, shape = estimate_sales)) +
  geom_point(alpha = 0.5, size = 1.5) + scale_shape_discrete(labels = sales_labels) +
  labs(title = sprintf("K-Means sobre UMAP (k = %d)", n_clusters),
       x = "Dimensión 1", y = "Dimensión 2", color = "Cluster", shape = "Ventas estimadas") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Matriz de contingencia (sin ordenar)
df_games$cluster <- factor(kmeans_result$cluster)
cont_mat <- table(Cluster = df_games$cluster, Ventas = df_games$estimate_sales)

# Asignación óptima de clusters a clases
assignment <- solve_LSAP(cont_mat, maximum = TRUE)
# Mapear clusters a categorías de ventas según asignación óptima
cluster_to_sales <- colnames(cont_mat)[as.integer(assignment)]
names(cluster_to_sales) <- rownames(cont_mat)

assignment_order <- order(assignment)
df_games$cluster <- factor(df_games$cluster, levels = assignment_order)

# Matriz de contingencia (orden óptimo)
cont_mat <- table(Cluster = df_games$cluster, Ventas = df_games$estimate_sales)
cat("\nMatriz de contingencia (original):\n")
print(cont_mat)

# Gráfica: Matriz de contingencia
df_cont_mat <- as.data.frame(cont_mat) %>%
  group_by(Cluster) %>%
  mutate(prop = Freq / sum(Freq), Cluster = factor(Cluster, rev(levels(Cluster))))

new_labels <- c("Bajas", "Medio-bajas", "Medias", "Medio-altas", "Altas")

p_cont_mat <- ggplot(df_cont_mat, aes(x = Ventas, y = Cluster, fill = prop)) +
  geom_tile(color = "white") + geom_text(aes(label = Freq), size = 3.5) +
  scale_fill_gradient(low = "lightskyblue1", high = "darkorchid1") +
  scale_x_discrete(labels = new_labels) +
  labs(title = "Matriz de contingencia (original)", subtitle = "Clusters vs. ventas estimadas",
       x = "Ventas estimadas", y = "Cluster", fill = "Proporción") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

# Medir accuracy
y_pred <- cluster_to_sales[as.character(df_games$cluster)]
y_real <- df_games$estimate_sales
accuracy <- mean(y_pred == y_real, na.rm = TRUE)
cat(sprintf("\nAccuracy: %.1f%%\n", accuracy * 100))

# Re-aplicación de K-Means (con clusters de elbow rule y clases fusionadas)
df_games$estimate_sales_3c <- df_games$estimate_sales
levels(df_games$estimate_sales_3c) <- c("low", "medium", "high", "high", "high")

cat("\nDistribución de ventas (3 niveles):\n")
print(table(df_games$estimate_sales_3c))

n_clusters <- 3
kmeans_result_3c <- kmeans(umap_result, centers = n_clusters, nstart = 25, iter.max = 100)
X_games_UMAP$cluster_3c <- factor(kmeans_result_3c$cluster)
X_games_UMAP$estimate_sales_3c <- df_games$estimate_sales_3c

# Gráfica: K-Means sobre UMAP (3 clusters)
new_sales_labels <- c("Bajas (<100k)", "Medias (100k-500k)", "Altas (>500k)")

p_kmeans_3c <- ggplot(X_games_UMAP, aes(x = V1, y = V2, color = cluster_3c, shape = estimate_sales_3c)) +
  geom_point(alpha = 0.5, size = 1.5) + scale_shape_discrete(labels = new_sales_labels) +
  labs(title = sprintf("K-Means sobre UMAP (k = %d)", n_clusters),
       x = "Dimensión 1", y = "Dimensión 2", color = "Cluster", shape = "Ventas estimadas") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Matriz de contingencia (sin ordenar)
df_games$cluster_3c <- factor(kmeans_result_3c$cluster)
cont_mat_3c <- table(Cluster = df_games$cluster_3c, Ventas = df_games$estimate_sales_3c)

# Asignación óptima de clusters a clases
assignment_3c <- solve_LSAP(cont_mat_3c, maximum = TRUE)
# Mapear clusters a categorías de ventas según asignación óptima
cluster_to_sales_3c <- colnames(cont_mat_3c)[as.integer(assignment_3c)]
names(cluster_to_sales_3c) <- rownames(cont_mat_3c)

assignment_order_3c <- order(assignment_3c)
df_games$cluster_3c <- factor(df_games$cluster_3c, levels = assignment_order_3c)

# Matriz de contingencia (orden óptimo)
cont_mat_3c <- table(Cluster = df_games$cluster_3c, Ventas = df_games$estimate_sales_3c)
cat("\nMatriz de contingencia (3 clusters):\n")
print(cont_mat_3c)

# Gráfica: Matriz de contingencia
df_cont_mat_3 <- as.data.frame(cont_mat_3c) %>%
  group_by(Cluster) %>%
  mutate(prop = Freq / sum(Freq), Cluster = factor(Cluster, rev(levels(Cluster))))

new_labels_3c <- c("Bajas", "Medias", "Altas")

p_cont_mat_3c <- ggplot(df_cont_mat_3, aes(x = Ventas, y = Cluster, fill = prop)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = Freq), size = 3.5) +
  scale_fill_gradient(low = "lightskyblue1", high = "darkorchid1") +
  scale_x_discrete(labels = new_labels_3c) +
  labs(title = "Matriz de contingencia (3 clusters)", subtitle = "Clusters vs. ventas estimadas",
       x = "Ventas estimadas", y = "Cluster", fill = "Proporción") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

print(p_cont_mat_3c)

# Medir accuracy
y_pred_3c <- cluster_to_sales_3c[as.character(df_games$cluster_3c)]
y_real_3c <- df_games$estimate_sales_3c
accuracy_3c <- mean(y_pred_3c == y_real_3c, na.rm = TRUE)
cat(sprintf("\nAccuracy: %.1f%%\n", accuracy_3c * 100))

## Parte 4. Contraste de hipótesis
cat("\n\n--- Contraste de hipótesis: ---\n")
cat("\nPregunta: ¿Los juegos en early access son en promedio más baratos que el resto?\n\n")
cat("H0: Valen igual en promedio.\n")
cat("H1: Los juegos en early access son más baratos en promedio.\n")

early_games <- df_games[df_games$is_early_access == TRUE, ]
nonearly_games <- df_games[df_games$is_early_access == FALSE, ]

# Limpieza de outliers
cat("\nJuegos en early access:\n")
cat("\nNº de muestras:", nrow(early_games),"\n")
print(summary(early_games$original_price))
cat("\nJuegos fuera de early access:\n")
cat("\nNº de muestras:", nrow(nonearly_games),"\n")
print(summary(nonearly_games$original_price))
cat("\nCOMPROBACIÓN (outliers): Juegos caros (precio > 80€):", nrow(nonearly_games[nonearly_games$original_price >= 80, ]), "\n")

# Gráfica: boxplot
df_price <- bind_rows(early_games %>% select(original_price) %>% mutate(group = "Early Access"),
                      nonearly_games %>% select(original_price) %>% mutate(group = "Non-Early Access")) %>%
  mutate(group = factor(group))

p_boxplot <- ggplot(df_price, aes(x = group, y = original_price, fill = group)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.35) +
  labs(title = "Distribución de precios según tipo de juego", subtitle = "Comparación entre juegos en Early Access y el resto",
       x = NULL, y = "Precio (€)") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

# Corrección: eliminación de juegos superiores a 80€
nonearly_games <- nonearly_games[nonearly_games$original_price < 80, ]
# También en df para guardar
df <- df[df$original_price < 80, ]

# Test de homocedasticidad
var_test <- var.test(early_games$original_price, nonearly_games$original_price)
print(var_test)

# Test de Welch
welch_test <- t.test(early_games$original_price, nonearly_games$original_price, alternative = "less", var.equal = FALSE)
print(welch_test)

# Calcular las medias para interpretar
mean_early <- mean(early_games$original_price)
mean_nonearly <- mean(nonearly_games$original_price)
dif <- mean_nonearly - mean_early

cat("\nRESUMEN (Contraste de hipótesis):\n")
cat("- Precio medio (juegos en early access):\t", round(mean_early, 2), "€\n")
cat("- Precio medio (juegos fuera de early access):\t", round(mean_nonearly, 2), "€\n")
cat("\nDiferencia:", round(dif, 2), "€\n")

alpha <- 0.05
if (welch_test$p.value < alpha) {
  cat("\nConclusión: Se rechaza H0. Los juegos en early access son más baratos.\n")
} else {
  cat("\nConclusión: No se puede rechazar H0 a favor de H1.\n\n")
}
# Tamaño del efecto
cohen_d <- cohen.d(early_games$original_price, nonearly_games$original_price)
print(cohen_d)

# Guardado del dataset limpio
df_path <- paste(dfs_path, "steam_apps_cleaned.csv", sep = "/")
write.csv(df, df_path, row.names = FALSE)

# Guardado de gráficas
ggsave(file.path(fig_path, "estimate_sales_games.png"), plot = p_games, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "estimate_sales_dlcs.png"), plot = p_dlcs, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "rf_confusion_matrix.png"), plot = p_cm, width = 9, height = 8, dpi = 300)
ggsave(file.path(fig_path, "rf_importance.png"), plot = p_importance, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "pca.png"), plot = p_pca, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "umap.png"), plot = p_umap, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "elbow_method.png"), plot = p_elbow, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "kmeans_umap.png"), plot = p_kmeans, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "kmeans_contingency_mat.png"), plot = p_cont_mat, width = 9, height = 8, dpi = 300)
ggsave(file.path(fig_path, "kmeans_contingency_mat_3c.png"), plot = p_cont_mat_3c, width = 9, height = 8, dpi = 300)
ggsave(file.path(fig_path, "kmeans_umap_3c.png"), plot = p_kmeans_3c, width = 9, height = 6, dpi = 300)
ggsave(file.path(fig_path, "price_boxplot.png"), plot = p_boxplot, width = 9, height = 6, dpi = 300)