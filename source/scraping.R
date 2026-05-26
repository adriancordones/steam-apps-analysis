library(dplyr)
library(httr)
library(stringr)

# Variables para hacer pruebas
# En la ejecución final 'test_mode' debe estar en FALSE !!!
test_mode <- TRUE
test_n <- 1000

# Archivo privado con la API-key de ITAD
readRenviron(".Renviron")
itad_key <- Sys.getenv("ITAD_KEY")

# Paths de interés
dfs_path <- "datasets"
fig_path <- "docs/figs"
# Comprobación de que los paths existen (y si no, se crean)
check_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}
paths <- c(dfs_path, fig_path)
for (i in 1:length(paths)) {
  check_dir(paths[i])
}

# Carga del dataset de partida
df_path <- paste(dfs_path, "steam_apps.csv", sep = "/")
df <- read.csv(df_path, sep = ",", dec = ".")

# Request headers utilizados en Práctica 1, para scraping ético
request_headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:148.0) Gecko/20100101 Firefox/148.0",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Encoding" = "gzip, deflate, br, zstd",
  "DNT" = "1",
  "Sec-Fetch-Dest" = "document",
  "Sec-Fetch-Mode" = "navigate",
  "Sec-Fetch-Site" = "none",
  "Sec-Fetch-User" = "?1",
  "Sec-GPC" = "1",
  "Upgrade-Insecure-Requests" = "1"
)
# Función genérica para requests
get_response <- function(url, delay = 1, cooldown = 10) {
  Sys.sleep(delay)
  response <- tryCatch(GET(url, add_headers(.headers = request_headers), timeout(cooldown)),
                       error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) return(NULL)
  data <- content(response, as = "parsed", type = "application/json")
  
  return(data)
}

# Función de ayuda para obtener datos de JSON extraídos
get_var <- function(data, path) {
  if (is.null(data)) return(NA)
  result <- data
  for (key in path) {
    result <- result[[key]]
    if (is.null(result)) return(NA)
  }
  return(result)
}

# Función para consultas a API de Steam: restricciones de edad y reviews
get_steam <- function(app_id) {
  url <- paste0("https://store.steampowered.com/appreviews/", app_id, "?json=1&purchase_type=all&num_per_page=0")
  data <- get_response(url, delay=1.75)
  total_positive <- get_var(data, c("query_summary", "total_positive"))
  total_reviews <- get_var(data, c("query_summary", "total_reviews"))
  
  url <- paste0("https://store.steampowered.com/api/appdetails?appids=", app_id)
  data <- get_response(url, delay=1.75)
  app_id <- as.character(app_id)
  required_age <- get_var(data, c(app_id, "data", "required_age"))
  pegi <- get_var(data, c(app_id, "data", "ratings", "pegi", "rating"))
  
  result <- list(total_positive = total_positive, total_reviews = total_reviews,
                 required_age = required_age, pegi = pegi)
  
  return(result)
}

# Función para consultas a API de SteamSpy: ventas estimadas
get_steamspy <- function(app_id, cooldown = 1) {
  url <- paste0("https://steamspy.com/api.php?request=appdetails&appid=", app_id)
  data <- get_response(url, delay=1)
  owners <- get_var(data, c("owners"))
  
  return(owners)
}

# Función para consultas a API de ITAD: pico máximo de jugadores
get_itad <- function(app_id) {
  url <- paste0("https://api.isthereanydeal.com/games/lookup/v1?key=", itad_key, "&appid=", app_id)
  data <- get_response(url, delay=0.3)
  itad_id <- get_var(data, c("game", "id"))
  
  url <- paste0("https://api.isthereanydeal.com/games/info/v2?key=", itad_key, "&id=", itad_id)
  data <- get_response(url, delay=0.3)
  players_peak <- get_var(data, c("players", "peak"))
  
  return(players_peak)
}

# Función wrapper para scraping de todas las APIs
scrape_all <- function(df, api_name, scrape_function) {
  cat(sprintf("\n[%s] INICIANDO SCRAPING...\n", api_name))
  total_apps <- nrow(df)
  results <- list()
  for (i  in 1:total_apps) {
    results[[i]] <- scrape_function(df$id[i])
    # Aviso cada 50 elementos
    if (i %% 50 == 0) {
      cat(sprintf("[%s] Scraping: %4d de %4d elementos completados (%5.1f%%).\n",
                  api_name, i, total_apps, (i/total_apps)*100))
    }
  }
  # Si el total no es múltiplo de 50, aviso final
  if (total_apps %% 50 != 0) {
    cat(sprintf("[%s] Scraping: %4d de %4d elementos completados (100.0%%).\n", 
                api_name, total_apps, total_apps))
  }
  cat(sprintf("[%s] SCRAPING FINALIZADO...\n\n", api_name))
  return(results)
}

# Integración final
if (test_mode) {
  df <- df[1:test_n, ]
}

# Steam API
steam_results <- scrape_all(df, "Steam API", get_steam)
# Asignar resultados
df$total_positive <- sapply(steam_results, function(x) x$total_positive)
df$total_reviews <- sapply(steam_results, function(x) x$total_reviews)
df$required_age <- sapply(steam_results, function(x) x$required_age)
df$pegi <- sapply(steam_results, function(x) x$pegi)

# SteamSpy API
steamspy_results <- scrape_all(df, "SteamSpy API", get_steamspy)
# Asignar resultados
df$owners <- sapply(steamspy_results, function(x) x)

# ITAD API
itad_results <- scrape_all(df, "ITAD API", get_itad)
# Asignar resultados
df$players_peak <- sapply(itad_results, function(x) x)

# Guardado del dataset con variables integradas
df_path <- paste(dfs_path, "steam_apps_temp.csv", sep = "/")
write.csv(df, df_path, row.names = FALSE)