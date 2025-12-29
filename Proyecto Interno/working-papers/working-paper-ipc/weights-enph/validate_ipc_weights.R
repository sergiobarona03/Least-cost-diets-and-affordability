############################################################
## 07_validate_ipc_weights_and_forward_prices_70_30.R
## (FIXED) Robust city column detection in retail
## Time split validation (70/30 chronological)
############################################################


library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(writexl)

# -----------------------------
# 1) Paths (EDIT ONLY THIS BLOCK)
# -----------------------------
base_dir <- "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"

in_retail <- file.path(base_dir, "Precios DANE/OUTPUT_DANE/precios_unadj_DANE_1999_2018.xlsx")
in_ipc1   <- file.path(base_dir, "var-ipc/IPC.xls")
in_ipc2   <- file.path(base_dir, "var-ipc/IPC_2.xls")
in_map    <- file.path(base_dir, "var-ipc/correlativa_ipc_articulos.xlsx")

out_dir  <- file.path(base_dir, "working-papers/working-paper-ipc/output/ipc_weights")
val_dir  <- file.path(out_dir, "validation_70_30")
plot_dir <- file.path(val_dir, "plots")

dir.create(val_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Helpers
# -----------------------------
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

make_date <- function(a, m) as.Date(sprintf("%04d-%02d-01", as.integer(a), as.integer(m)))

norm_city <- function(x) {
  x <- toupper(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT"))
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x
}

extract_code8 <- function(x) {
  x <- as.character(x)
  x <- gsub("\\D", "", x)
  ifelse(
    nchar(x) >= 8, substr(x, 1, 8),
    ifelse(nchar(x) == 6, paste0(x, "00"), NA_character_)
  )
}

softmax <- function(theta) {
  z <- theta - max(theta)
  w <- exp(z)
  w / sum(w)
}

safe_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))
mape <- function(a, b) mean(abs((a - b) / a), na.rm = TRUE) * 100

# -----------------------------
# 3) Cities to keep
# -----------------------------
cities_keep <- c("BOGOTA D.C.", "CALI", "MEDELLIN")

# -----------------------------
# 4) Load IPC (bind IPC + IPC_2)
# -----------------------------
ipc <- bind_rows(
  read_excel(in_ipc1) %>% clean_names(),
  read_excel(in_ipc2) %>% clean_names()
) %>%
  mutate(
    ciudad = norm_city(ciudad),
    cod_subclase = extract_code8(subclase),
    mes_num = match(mes, meses_esp),
    ano = suppressWarnings(as.integer(ano)),
    ipc = suppressWarnings(as.numeric(numero_indice)),
    fecha = make_date(ano, mes_num)
  ) %>%
  filter(
    !is.na(ciudad), !is.na(cod_subclase), !is.na(fecha), !is.na(ipc),
    ciudad %in% cities_keep
  ) %>%
  select(ciudad, cod_subclase, fecha, ipc) %>%
  group_by(ciudad, cod_subclase, fecha) %>%
  summarise(ipc = last(ipc), .groups = "drop") %>%
  arrange(ciudad, cod_subclase, fecha)

message("IPC columns OK. Example cities in IPC: ",
        paste(head(sort(unique(ipc$ciudad)), 10), collapse=", "))

# -----------------------------
# 5) Load retail prices (DANE) - robust city column
# -----------------------------
retail_raw <- read_excel(in_retail) %>% clean_names()

message("Retail columns detected: ", paste(names(retail_raw), collapse=", "))

# Detect city column name
city_col <- dplyr::case_when(
  "nombre_ciudad" %in% names(retail_raw) ~ "nombre_ciudad",
  "ciudad" %in% names(retail_raw) ~ "ciudad",
  TRUE ~ NA_character_
)

if (is.na(city_col)) {
  stop("Could not find a city column in retail file. Expected 'nombre_ciudad' or 'ciudad'.")
}

# Detect price column name (your file usually has precio_500g, but be robust)
price_col <- dplyr::case_when(
  "precio_500g" %in% names(retail_raw) ~ "precio_500g",
  "price" %in% names(retail_raw) ~ "price",
  "precio" %in% names(retail_raw) ~ "precio",
  TRUE ~ NA_character_
)

if (is.na(price_col)) {
  stop("Could not find a price column in retail file. Expected 'precio_500g' (preferred), or 'precio', or 'price'.")
}

# Detect month column
mes_col <- dplyr::case_when(
  "mes_num" %in% names(retail_raw) ~ "mes_num",
  "mes" %in% names(retail_raw) ~ "mes",
  TRUE ~ NA_character_
)

if (is.na(mes_col)) {
  stop("Could not find month column in retail file. Expected 'mes_num' or 'mes'.")
}

# Build retail clean
retail <- retail_raw %>%
  mutate(
    ciudad = norm_city(.data[[city_col]]),
    ano = suppressWarnings(as.integer(ano)),
    mes_num = if (mes_col == "mes") suppressWarnings(as.integer(mes)) else suppressWarnings(as.integer(.data[[mes_col]])),
    fecha = make_date(ano, mes_num),
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    articulo = as.character(articulo),
    price = suppressWarnings(as.numeric(.data[[price_col]]))
  ) %>%
  filter(
    !is.na(ciudad), !is.na(fecha), !is.na(codigo_articulo),
    !is.na(price), price > 0,
    ciudad %in% cities_keep
  ) %>%
  select(ciudad, fecha, codigo_articulo, articulo, price) %>%
  group_by(ciudad, fecha, codigo_articulo, articulo) %>%
  summarise(price = mean(price), .groups = "drop") %>%
  arrange(ciudad, articulo, fecha)

message("Retail cities kept: ", paste(sort(unique(retail$ciudad)), collapse=", "))

# -----------------------------
# 6) Load mapping product -> IPC subclase
# -----------------------------
map <- read_excel(in_map) %>%
  clean_names() %>%
  rename(codigo_articulo = cod_dane, subclase = cod_ipc) %>%
  mutate(
    codigo_articulo = suppressWarnings(as.numeric(codigo_articulo)),
    cod_subclase = extract_code8(subclase)
  ) %>%
  filter(!is.na(codigo_articulo), !is.na(cod_subclase)) %>%
  select(codigo_articulo, cod_subclase) %>%
  distinct()

retail_m <- retail %>%
  left_join(map, by = "codigo_articulo") %>%
  filter(!is.na(cod_subclase))

# -----------------------------
# 7) Keys with overlap
# -----------------------------
keys <- retail_m %>%
  distinct(ciudad, cod_subclase) %>%
  inner_join(ipc %>% distinct(ciudad, cod_subclase), by = c("ciudad", "cod_subclase")) %>%
  arrange(ciudad, cod_subclase)

# -----------------------------
# 8) Main validation loop (70/30 time split)
# -----------------------------
min_months_total <- 24
min_products     <- 2

ipc_metrics_out   <- list()
price_metrics_out <- list()
weights_out       <- list()
fail_out          <- list()

for (k in seq_len(nrow(keys))) {

  
  city <- keys$ciudad[k]
  sc   <- keys$cod_subclase[k]
  
  df_p <- retail_m %>% filter(ciudad == city, cod_subclase == sc)
  df_i <- ipc      %>% filter(ciudad == city, cod_subclase == sc)
  
  dates <- sort(as.Date(intersect(as.character(df_p$fecha), as.character(df_i$fecha))))
  
  if (length(dates) < min_months_total) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc,
                                             motivo="Too few overlap months", n_overlap=length(dates))
    next
  }
  
  df_p2 <- df_p %>%
    filter(fecha %in% dates) %>%
    group_by(fecha, articulo) %>%
    summarise(price = mean(price), .groups = "drop")
  
  wide <- df_p2 %>%
    pivot_wider(names_from = articulo, values_from = price) %>%
    left_join(df_i %>% filter(fecha %in% dates) %>% select(fecha, ipc), by="fecha") %>%
    arrange(fecha)
  
  X <- as.matrix(wide %>% select(-fecha, -ipc))
  keep_cols <- which(colSums(!is.na(X)) > 0)
  X <- X[, keep_cols, drop=FALSE]
  
  if (ncol(X) < min_products) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc,
                                             motivo="Too few products after pivot", n_products=ncol(X))
    next
  }
  
  Tn <- nrow(wide)
  t_split <- floor(0.70 * Tn)
  split_date <- wide$fecha[t_split]
  train_rows <- which(wide$fecha <= split_date)
  test_rows  <- which(wide$fecha >  split_date)
  
  if (length(train_rows) < 12 || length(test_rows) < 6) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc,
                                             motivo="Too few rows for 70/30", n_train=length(train_rows), n_test=length(test_rows))
    next
  }
  
  Xlog <- suppressWarnings(log(X))
  ylog <- suppressWarnings(log(wide$ipc))
  
  # Fill NA in Xlog using TRAIN means only
  for (j in seq_len(ncol(Xlog))) {
    mu_train <- mean(Xlog[train_rows, j], na.rm = TRUE)
    Xlog[is.na(Xlog[, j]), j] <- mu_train
  }
  
  ok <- is.finite(ylog)
  Xlog2 <- Xlog[ok, , drop=FALSE]
  ylog2 <- ylog[ok]
  fecha2 <- wide$fecha[ok]
  
  train_rows2 <- which(fecha2 <= split_date)
  test_rows2  <- which(fecha2 >  split_date)
  
  if (length(train_rows2) < 12 || length(test_rows2) < 6) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc,
                                             motivo="Too few rows after y-clean", n_train=length(train_rows2), n_test=length(test_rows2))
    next
  }
  
  # ---- Fit weights on TRAIN
  obj <- function(theta) {
    w <- softmax(theta)
    xb <- as.vector(Xlog2[train_rows2, , drop=FALSE] %*% w)
    a  <- mean(ylog2[train_rows2] - xb)
    sum((ylog2[train_rows2] - (a + xb))^2)
  }
  
  fit <- tryCatch(optim(rep(0, ncol(Xlog2)), obj, method="BFGS"),
                  error = function(e) NULL)
  
  if (is.null(fit)) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc, motivo="optim failed")
    next
  }
  
  w_hat <- softmax(fit$par)
  
  xb_train <- as.vector(Xlog2[train_rows2, , drop=FALSE] %*% w_hat)
  alpha_hat <- mean(ylog2[train_rows2] - xb_train)
  
  xb_all <- as.vector(Xlog2 %*% w_hat)
  
  df_ipc_fit <- tibble(
    ciudad = city,
    cod_subclase = sc,
    fecha = fecha2,
    sample = if_else(fecha <= split_date, "train", "test"),
    ipc_obs = exp(ylog2),
    ipc_fit = exp(alpha_hat + xb_all)
  )
  
  df_test <- df_ipc_fit %>% filter(sample=="test")
  
  ipc_metrics_out[[length(ipc_metrics_out)+1]] <- tibble(
    ciudad = city,
    cod_subclase = sc,
    split_date = split_date,
    n_train = sum(df_ipc_fit$sample=="train"),
    n_test  = sum(df_ipc_fit$sample=="test"),
    rmse_test = rmse(df_test$ipc_obs, df_test$ipc_fit),
    mape_test = mape(df_test$ipc_obs, df_test$ipc_fit),
    cor_test  = suppressWarnings(cor(df_test$ipc_obs, df_test$ipc_fit, use="complete.obs"))
  )
  
  weights_out[[length(weights_out)+1]] <- tibble(
    ciudad = city, cod_subclase = sc,
    articulo = colnames(Xlog2),
    weight = w_hat
  ) %>% arrange(desc(weight))
  
  # IPC plot
  p_ipc <- ggplot(df_ipc_fit, aes(x=fecha)) +
    geom_line(aes(y=ipc_obs, color="IPC observado"), linewidth=0.6) +
    geom_line(aes(y=ipc_fit, color="IPC ajustado (train)"), linewidth=0.6, linetype="dashed") +
    geom_vline(xintercept = split_date, linetype=3) +
    labs(
      title = paste0("Validación 70/30 — IPC observado vs ajustado (", city, ")"),
      subtitle = paste0("Subclase: ", sc, " | corte: ", split_date,
                        " | MAPE_test=", round(ipc_metrics_out[[length(ipc_metrics_out)]]$mape_test, 2), "%"),
      x=NULL, y="Índice (nivel)", color=""
    ) +
    theme_bw(base_size=11) +
    theme(legend.position="bottom")
  
  ggsave(file.path(plot_dir, paste0("ipc_oos__", safe_name(city), "__", sc, ".png")),
         p_ipc, width=11, height=4.8, dpi=220)
  
  # ---- Forward-fill prices in TEST using IPC ratios (observed IPC)
  anchor_date <- split_date
  
  ipc_anchor <- df_ipc_fit %>% filter(fecha == anchor_date) %>% pull(ipc_obs)
  if (length(ipc_anchor) != 1 || is.na(ipc_anchor)) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc, motivo="Missing IPC at anchor")
    next
  }
  
  price_long <- wide %>%
    select(fecha, ipc, everything()) %>%
    pivot_longer(cols = -c(fecha, ipc), names_to="articulo", values_to="price_obs")
  
  anchor_prices <- price_long %>%
    filter(fecha <= anchor_date, !is.na(price_obs)) %>%
    group_by(articulo) %>%
    slice_max(order_by = fecha, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(articulo, anchor_price = price_obs)
  
  price_long2 <- price_long %>%
    left_join(anchor_prices, by="articulo") %>%
    mutate(
      sample = if_else(fecha <= anchor_date, "train", "test"),
      price_hat = anchor_price * (ipc / ipc_anchor)
    )
  
  eval_test <- price_long2 %>%
    filter(sample=="test", !is.na(price_obs), !is.na(price_hat), price_obs > 0)
  
  if (nrow(eval_test) < 50) {
    fail_out[[length(fail_out)+1]] <- tibble(ciudad=city, cod_subclase=sc,
                                             motivo="Too few price eval points in test", n_points=nrow(eval_test))
    next
  }
  
  prod_metrics <- eval_test %>%
    group_by(articulo) %>%
    summarise(
      n = n(),
      rmse = rmse(price_obs, price_hat),
      mape = mape(price_obs, price_hat),
      .groups="drop"
    ) %>%
    mutate(ciudad=city, cod_subclase=sc, anchor_date=anchor_date)
  
  price_metrics_out[[length(price_metrics_out)+1]] <- prod_metrics
  
  # Price plots
  p_mape <- ggplot(prod_metrics, aes(x=mape)) +
    geom_histogram(bins=25) +
    labs(
      title = "Forward-fill precios con ratio IPC — MAPE por producto (TEST)",
      subtitle = paste0(city, " | Subclase ", sc, " | corte ", anchor_date,
                        " | productos evaluados: ", nrow(prod_metrics)),
      x = "MAPE (%)", y = "Conteo"
    ) +
    theme_bw(base_size=11)
  
  ggsave(file.path(plot_dir, paste0("price_mape_hist__", safe_name(city), "__", sc, ".png")),
         p_mape, width=10, height=4.5, dpi=220)
}

# -----------------------------
# 9) Save tables
# -----------------------------
ipc_metrics <- if (length(ipc_metrics_out)==0) tibble() else bind_rows(ipc_metrics_out)
price_metrics <- if (length(price_metrics_out)==0) tibble() else bind_rows(price_metrics_out)
weights_df <- if (length(weights_out)==0) tibble() else bind_rows(weights_out)
fail_df <- if (length(fail_out)==0) tibble() else bind_rows(fail_out)

ipc_metrics_city <- ipc_metrics %>%
  group_by(ciudad) %>%
  summarise(
    n_subclases = n(),
    rmse_test_mean = mean(rmse_test, na.rm=TRUE),
    mape_test_mean = mean(mape_test, na.rm=TRUE),
    cor_test_mean  = mean(cor_test,  na.rm=TRUE),
    .groups="drop"
  )

price_metrics_city <- price_metrics %>%
  group_by(ciudad) %>%
  summarise(
    n_products = n(),
    mape_mean = mean(mape, na.rm=TRUE),
    mape_median = median(mape, na.rm=TRUE),
    rmse_mean = mean(rmse, na.rm=TRUE),
    .groups="drop"
  )

write_csv(ipc_metrics, file.path(val_dir, "ipc_oos_metrics_by_city_subclase.csv"))
write_csv(price_metrics, file.path(val_dir, "price_forwardfill_metrics_by_product.csv"))
write_csv(weights_df, file.path(val_dir, "weights_train_70pct.csv"))
write_csv(fail_df, file.path(val_dir, "failures_log.csv"))

write_xlsx(
  list(
    ipc_oos_metrics_by_city_subclase = ipc_metrics,
    ipc_oos_metrics_by_city = ipc_metrics_city,
    price_forwardfill_by_product = price_metrics,
    price_forwardfill_by_city = price_metrics_city,
    weights_train_70pct = weights_df,
    failures = fail_df
  ),
  file.path(val_dir, "validation_70_30_outputs.xlsx")
)

# Summary plots
if (nrow(ipc_metrics) > 0) {
  p_ipc_box <- ggplot(ipc_metrics, aes(x=ciudad, y=mape_test)) + geom_boxplot() +
    labs(title="IPC OOS (TEST) — MAPE por ciudad (subclases)", x=NULL, y="MAPE (%)") +
    theme_bw(base_size=11)
  ggsave(file.path(plot_dir, "IPC_OOS_MAPE_box_by_city.png"), p_ipc_box, width=9, height=4.8, dpi=220)
}

if (nrow(price_metrics) > 0) {
  p_price_box <- ggplot(price_metrics, aes(x=ciudad, y=mape)) + geom_boxplot() +
    labs(title="Forward-fill precios con IPC (TEST) — MAPE por ciudad (productos)", x=NULL, y="MAPE (%)") +
    theme_bw(base_size=11)
  ggsave(file.path(plot_dir, "PRICE_forwardfill_MAPE_box_by_city.png"), p_price_box, width=9, height=4.8, dpi=220)
}

message("DONE. Outputs in: ", val_dir)
message("Plots in: ", plot_dir)
message("Rows IPC metrics: ", nrow(ipc_metrics))
message("Rows price metrics: ", nrow(price_metrics))
message("Failures: ", nrow(fail_df))
