########################################################
## met_a_e_validation.R
## Comparación de metodologías A1, A2, B, C1, C2, D, E
## Ventana de validación (donde hay IPC observado real)
##
## FIGURAS:
##   1) Boxplots (4): MAPE, RMSE, MAE, ME
##      x = metodología | facet = ciudad | sin outliers
##
##   2) Heatmap A/B: MAPE por artículo IPC × ciudad
##      filas = articulo_ipc × ciudad | cols = A1, A2, B
##
##   3) Heatmap C/D/E: MAPE por alimento SIPSA × ciudad
##      filas = alimento_sipsa × ciudad | cols = C1, C2, D, E
##
## OUTPUT:
##   output/metricas_consolidadas.rds
##   output/tablas/metricas_agregadas.xlsx
##   output/figuras/boxplot_<metrica>.png  (4 figuras)
##   output/figuras/heatmap_a.png
##   output/figuras/heatmap_bcde.png
########################################################

library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

# -------------------------------------------------------
# PATHS
# -------------------------------------------------------
base_dir <- "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno/review-food-security-paper/0A_InformeFinal/"

path_a1 <- file.path(base_dir, "met_a1_ipc_forward/output/metA_precios_tres_ventanas.rds")
path_a2 <- file.path(base_dir, "met_a2_ipc_correction/output/metA2_precios_dos_ventanas.rds")
path_b  <- file.path(base_dir, "met_b_margen_mediano/output/metB_precios_dos_ventanas.rds")
path_c  <- file.path(base_dir, "met_c_passthrough/output/metC_precios_dos_ventanas.rds")
path_d  <- file.path(base_dir, "met_d_ecm/output/metD_precios_dos_ventanas.rds")
path_e  <- file.path(base_dir, "met_e_aecm/output/metE_precios_dos_ventanas.rds")

out_dir     <- file.path(base_dir, "met_a_e_validation/output")
out_figuras <- file.path(out_dir, "figuras")
out_tablas  <- file.path(out_dir, "tablas")
dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(out_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(out_tablas,  showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------
# PALETAS Y ETIQUETAS
# -------------------------------------------------------
MET_LEVELS <- c("A1", "A2", "B", "C1", "C2", "D", "E")
MET_COLORS <- c(
  "A1" = "#2C3E6B", "A2" = "#1A7A4A", "B"  = "#E67E22",
  "C1" = "#C0392B", "C2" = "#8E44AD", "D"  = "#16A085", "E" = "#2E86C1"
)
MET_LABELS <- c(
  "A1" = "A1\n(IPC forward)", "A2" = "A2\n(IPC corregido)",
  "B"  = "B\n(margen)",       "C1" = "C1\n(niveles)",
  "C2" = "C2\n(diferencias)", "D"  = "D\n(ECM)",  "E" = "E\n(A-ECM)"
)
CITY_LABELS <- c("CALI" = "Cali", "BOGOTA D.C." = "Bogotá", "MEDELLIN" = "Medellín")
CITY_ORDER  <- c("Bogotá", "Medellín", "Cali")

# -------------------------------------------------------
# HELPERS
# -------------------------------------------------------
add_errors <- function(df) {
  df %>% mutate(error = precio_hat - precio_obs,
                ape   = abs(error) / precio_obs * 100)
}

make_heatmap <- function(df_mape, x_var, y_var, met_subset,
                         titulo, subtitulo,
                         x_labels = MET_LABELS,
                         midpoint = NULL) {
  
  mets <- factor(met_subset, levels = met_subset)
  
  # Filas únicas ordenadas
  filas <- df_mape %>%
    filter(met %in% met_subset) %>%
    distinct({{ y_var }}) %>%
    pull({{ y_var }})
  
  # Grilla completa con NAs donde no hay cobertura
  grilla <- expand_grid(
    met     = mets,
    y_label = filas
  ) %>%
    left_join(
      df_mape %>%
        filter(met %in% met_subset) %>%
        rename(y_label = {{ y_var }}) %>%
        select(met, y_label, MAPE),
      by = c("met", "y_label")
    ) %>%
    mutate(
      met       = factor(met, levels = met_subset),
      y_label   = factor(y_label, levels = rev(filas)),
      mape_txt  = if_else(is.na(MAPE), "N/D", sprintf("%.1f%%", MAPE)),
      txt_color = if_else(is.na(MAPE) | MAPE > 50, "white", "black")
    )
  
  mp <- if (is.null(midpoint)) median(df_mape$MAPE[df_mape$met %in% met_subset], na.rm = TRUE) else midpoint
  
  ggplot(grilla, aes(x = met, y = y_label, fill = MAPE)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = mape_txt, color = txt_color), size = 3.1) +
    scale_fill_gradient2(
      low      = "#1A7A4A",
      mid      = "#F9E79F",
      high     = "#C0392B",
      midpoint = mp,
      na.value = "grey75",
      name     = "MAPE (%)",
      labels   = function(x) paste0(round(x, 1), "%")
    ) +
    scale_color_identity() +
    scale_x_discrete(labels = x_labels[met_subset], position = "top") +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x       = element_text(angle = 0, hjust = 0.5, size = 9),
      axis.text.y       = element_text(size = 8.5),
      legend.position   = "right",
      legend.key.height = unit(1.3, "cm"),
      panel.grid        = element_blank(),
      plot.title        = element_text(size = 12, face = "bold"),
      plot.subtitle     = element_text(size = 9, color = "grey40")
    )
}

# -------------------------------------------------------
# 1. CARGAR PANELES
# -------------------------------------------------------
message("Cargando paneles...")

# ---- A1: nivel articulo_ipc ----
a1 <- readRDS(path_a1) %>%
  filter(ventana == "ventana2_2015", !is.na(precio_obs), !is.na(precio_hat)) %>%
  transmute(met = "A1", ciudad, articulo_ipc = articulo, fecha,
            precio_obs, precio_hat) %>%
  add_errors()

# ---- A2: nivel articulo_ipc ----
a2 <- readRDS(path_a2) %>%
  filter(ventana == "ventana1_validacion", !is.na(price_obs), !is.na(price_S5)) %>%
  transmute(met = "A2", ciudad, articulo_ipc = articulo, fecha,
            precio_obs = price_obs, precio_hat = price_S5) %>%
  add_errors()

# ---- B: nivel alimento_sipsa (para boxplot y heatmap C/D/E)
#         nivel articulo_ipc (para heatmap A/B, promediando variedades SIPSA) ----
b_sipsa <- readRDS(path_b) %>%
  filter(ventana == "ventana1_validacion",
         !is.na(precio_ipc_obs), !is.na(precio_ipc_pred_q2)) %>%
  transmute(met = "B", ciudad, alimento_sipsa, fecha,
            precio_obs = precio_ipc_obs, precio_hat = precio_ipc_pred_q2) %>%
  add_errors()

b_ipc <- b_sipsa %>%
  left_join(mapping_sipsa_ipc, by = "alimento_sipsa") %>%
  filter(!is.na(articulo_ipc)) %>%
  group_by(met, ciudad, articulo_ipc, fecha) %>%
  summarise(precio_obs = mean(precio_obs, na.rm = TRUE),
            precio_hat = mean(precio_hat, na.rm = TRUE), .groups = "drop") %>%
  add_errors()

# ---- C: nivel alimento_sipsa ----
c_raw <- readRDS(path_c) %>%
  filter(ventana == "ventana1_validacion", !is.na(precio_ipc_obs)) %>%
  left_join(mapping_sipsa_ipc, by = "alimento_sipsa") %>%
  filter(!is.na(articulo_ipc))   # solo alimentos del mapping

c1 <- c_raw %>%
  filter(!is.na(pred_c1)) %>%
  transmute(met = "C1", ciudad, alimento_sipsa, articulo_ipc, fecha,
            precio_obs = precio_ipc_obs, precio_hat = pred_c1) %>%
  add_errors()

c2 <- c_raw %>%
  filter(!is.na(pred_c2)) %>%
  transmute(met = "C2", ciudad, alimento_sipsa, articulo_ipc, fecha,
            precio_obs = precio_ipc_obs, precio_hat = pred_c2) %>%
  add_errors()

# ---- D: nivel alimento_sipsa ----
d <- readRDS(path_d) %>%
  filter(ventana == "ventana1_validacion", !is.na(price_obs), !is.na(price_pred)) %>%
  transmute(met = "D", ciudad, alimento_sipsa, articulo_ipc, fecha,
            precio_obs = price_obs, precio_hat = price_pred) %>%
  add_errors()

# ---- E: nivel alimento_sipsa ----
e <- readRDS(path_e) %>%
  filter(ventana == "ventana1_validacion", !is.na(price_obs), !is.na(price_pred)) %>%
  transmute(met = "E", ciudad, alimento_sipsa, articulo_ipc, fecha,
            precio_obs = price_obs, precio_hat = price_pred) %>%
  add_errors()

# -------------------------------------------------------
# 2. PANEL PARA BOXPLOTS (todas las metodologías, nivel común)
#    A1/A2 no tienen alimento_sipsa -> usamos articulo_ipc para todos
#    B se agrega a articulo_ipc para el boxplot también
# -------------------------------------------------------
panel_box <- bind_rows(
  a1 %>% mutate(alimento_sipsa = articulo_ipc),
  a2 %>% mutate(alimento_sipsa = articulo_ipc),
  b_ipc %>% mutate(alimento_sipsa = articulo_ipc),
  c1 %>% select(-articulo_ipc),
  c2 %>% select(-articulo_ipc),
  d  %>% select(-articulo_ipc),
  e  %>% select(-articulo_ipc)
) %>%
  mutate(
    met          = factor(met, levels = MET_LEVELS),
    ciudad_label = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)
  ) %>%
  filter(!is.na(ciudad_label))

message(sprintf("  Panel boxplot: %d obs", nrow(panel_box)))

# -------------------------------------------------------
# 3. MÉTRICAS POR GRUPO
# -------------------------------------------------------
# Heatmap A: solo A1 y A2, nivel articulo_ipc × ciudad
mape_a <- bind_rows(a1, a2) %>%
  mutate(ciudad_label = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)) %>%
  filter(!is.na(ciudad_label)) %>%
  group_by(met, ciudad_label, articulo_ipc) %>%
  summarise(MAPE = mean(ape, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------------
# 2. MÉTRICAS POR GRUPO
# -------------------------------------------------------
# Heatmap A: articulo_ipc × ciudad (solo A1 y A2)
mape_a <- bind_rows(a1, a2) %>%
  mutate(ciudad_label = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)) %>%
  filter(!is.na(ciudad_label)) %>%
  group_by(met, ciudad_label, articulo_ipc) %>%
  summarise(MAPE = mean(ape, na.rm = TRUE), .groups = "drop")

# Heatmap B/C/D/E: alimento_sipsa × ciudad — sin mapping, libre
mape_bcde <- bind_rows(b_sipsa, c1, c2, d, e) %>%
  mutate(ciudad_label = factor(CITY_LABELS[ciudad], levels = CITY_ORDER)) %>%
  filter(!is.na(ciudad_label), !is.na(alimento_sipsa)) %>%
  group_by(met, ciudad_label, alimento_sipsa) %>%
  summarise(MAPE = mean(ape, na.rm = TRUE), .groups = "drop")

# Guardar tablas
write_xlsx(
  list(heatmap_A    = mape_a,
       heatmap_BCDE = mape_bcde,
       boxplot_raw = panel_box %>%
         group_by(met, ciudad_label) %>%
         summarise(MAPE = mean(ape, na.rm = TRUE),
                   RMSE = sqrt(mean(error^2, na.rm = TRUE)),
                   MAE  = mean(abs(error),   na.rm = TRUE),
                   ME   = mean(error,         na.rm = TRUE),
                   .groups = "drop")),
  file.path(out_tablas, "metricas_agregadas.xlsx")
)

saveRDS(list(panel_box = panel_box, mape_a = mape_a, mape_bcde = mape_bcde),
        file.path(out_dir, "metricas_consolidadas.rds"))
message("  Tablas guardadas.")

# -------------------------------------------------------
# 4. TEMA BASE BOXPLOT
# -------------------------------------------------------
tema_box <- theme_bw(base_size = 11) +
  theme(
    legend.position    = "none",
    strip.background   = element_rect(fill = "grey92"),
    strip.text         = element_text(size = 10, face = "bold"),
    axis.text.x        = element_text(angle = 35, hjust = 1, size = 8.5),
    panel.grid.major.x = element_blank()
  )

# -------------------------------------------------------
# 5. BOXPLOTS (sin outliers, eje recortado al p95)
# -------------------------------------------------------
message("Generando boxplots...")

met_box_info <- list(
  MAPE = list(col = "ape",       label = "APE (%)",       titulo = "Error porcentual absoluto (APE)"),
  RMSE = list(col = "error_abs", label = "|Error| (COP)", titulo = "Error absoluto — base para RMSE"),
  MAE  = list(col = "error_abs", label = "|Error| (COP)", titulo = "Error absoluto — base para MAE"),
  ME   = list(col = "error",     label = "Error (COP)",   titulo = "Sesgo — error con signo (ME)")
)

panel_box2 <- panel_box %>% mutate(error_abs = abs(error))

for (met_name in names(met_box_info)) {
  info   <- met_box_info[[met_name]]
  df_box <- panel_box2 %>% select(met, ciudad_label, y_val = all_of(info$col))
  y_max  <- quantile(df_box$y_val, 0.95, na.rm = TRUE)
  y_min  <- if (met_name == "ME") quantile(df_box$y_val, 0.05, na.rm = TRUE) else 0
  
  p <- ggplot(df_box, aes(x = met, y = y_val, fill = met)) +
    geom_boxplot(outlier.shape = NA, width = 0.65, alpha = 0.85) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    {if (met_name == "ME") geom_hline(yintercept = 0, linetype = "dashed",
                                      color = "black", linewidth = 0.5)} +
    facet_wrap(~ ciudad_label, ncol = 3) +
    scale_fill_manual(values = MET_COLORS) +
    scale_x_discrete(labels = MET_LABELS) +
    labs(title = info$titulo,
         subtitle = "Distribución sobre observaciones individuales (mes × alimento) | ventana 2016–2018 | eje recortado al p95",
         x = NULL, y = info$label) +
    tema_box
  
  ggsave(file.path(out_figuras, paste0("boxplot_", tolower(met_name), ".png")),
         p, width = 13, height = 5.5, dpi = 200)
  message(sprintf("  Boxplot %s guardado.", met_name))
}

# -------------------------------------------------------
# Función auxiliar: leyenda discreta para heatmaps
# Cortes: <5%, 5-10%, 10-20%, 20-35%, 35-50%, >50%
# -------------------------------------------------------
MAPE_BREAKS  <- c(0, 5, 10, 20, 35, 50, Inf)
MAPE_LABELS  <- c("<5%", "5–10%", "10–20%", "20–35%", "35–50%", ">50%")
MAPE_PALETTE <- c("#1A7A4A", "#52BE80", "#F9E79F", "#F39C12", "#C0392B", "#7B241C")

discretize_mape <- function(x) {
  cut(x, breaks = MAPE_BREAKS, labels = MAPE_LABELS,
      include.lowest = TRUE, right = FALSE)
}

tema_heat <- theme_bw(base_size = 10.5) +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y      = element_text(size = 8.5),
    legend.position  = "right",
    legend.title     = element_text(size = 9),
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey92"),
    strip.text       = element_text(size = 10, face = "bold"),
    plot.title       = element_text(size = 12, face = "bold"),
    plot.subtitle    = element_text(size = 9, color = "grey40")
  )

# -------------------------------------------------------
# 6. HEATMAP A (A1 y A2 | facet ciudad | articulo_ipc en filas)
# -------------------------------------------------------
message("Generando heatmap A...")

# Orden de articulo_ipc: alfabético desde los datos
articulos_a <- mape_a %>% distinct(articulo_ipc) %>% arrange(articulo_ipc) %>% pull(articulo_ipc)

# Con facet por ciudad, las filas son solo articulo_ipc
grilla_a <- expand_grid(
  met          = factor(c("A1","A2"), levels = c("A1","A2")),
  ciudad_label = factor(CITY_ORDER, levels = CITY_ORDER),
  articulo_ipc = factor(articulos_a, levels = rev(articulos_a))
) %>%
  left_join(
    mape_a %>% select(met, ciudad_label, articulo_ipc, MAPE),
    by = c("met", "ciudad_label", "articulo_ipc")
  ) %>%
  mutate(
    MAPE_disc = discretize_mape(MAPE),
    mape_txt  = if_else(is.na(MAPE), "N/D", sprintf("%.1f%%", MAPE)),
    txt_color = if_else(is.na(MAPE) | MAPE > 35, "white", "black")
  )

p_a <- ggplot(grilla_a, aes(x = met, y = articulo_ipc, fill = MAPE_disc)) +
  geom_tile(color = "white", linewidth = 0.7) +
  facet_wrap(~ ciudad_label, ncol = 3) +
  scale_fill_manual(
    values   = setNames(MAPE_PALETTE, MAPE_LABELS),
    na.value = "grey80",
    name     = "MAPE",
    drop     = FALSE
  ) +
  scale_color_identity() +
  scale_x_discrete(labels = MET_LABELS[c("A1","A2")], position = "top") +
  labs(
    title    = "MAPE — Metodologías A1 y A2",
    subtitle = "Nivel: artículo IPC | ventana 2016–2018",
    x = NULL, y = NULL
  ) +
  tema_heat

ggsave(file.path(out_figuras, "heatmap_a.png"),
       p_a, width = 9, height = 12, dpi = 200)
message("  Heatmap A guardado.")

# -------------------------------------------------------
# 7. HEATMAP B/C/D/E (alimentos en eje X | metodologías en eje Y | facet ciudad nrow=3)
# -------------------------------------------------------
message("Generando heatmap B/C/D/E...")

alimentos_bcde <- mape_bcde %>%
  distinct(alimento_sipsa) %>%
  arrange(alimento_sipsa) %>%
  pull(alimento_sipsa)

mets_bcde <- c("B","C1","C2","D","E")

grilla_bcde <- expand_grid(
  met            = factor(mets_bcde, levels = rev(mets_bcde)),
  ciudad_label   = factor(CITY_ORDER, levels = CITY_ORDER),
  alimento_sipsa = factor(alimentos_bcde, levels = alimentos_bcde)
) %>%
  left_join(
    mape_bcde %>% select(met, ciudad_label, alimento_sipsa, MAPE),
    by = c("met", "ciudad_label", "alimento_sipsa")
  ) %>%
  mutate(
    MAPE_disc = discretize_mape(MAPE),
    mape_txt  = if_else(is.na(MAPE), "N/D", sprintf("%.1f%%", MAPE)),
    txt_color = if_else(is.na(MAPE) | MAPE > 35, "white", "black")
  )

n_alimentos_bcde <- length(alimentos_bcde)

p_bcde <- ggplot(grilla_bcde, aes(x = alimento_sipsa, y = met, fill = MAPE_disc)) +
  geom_tile(color = "white", linewidth = 0.55) +
  facet_wrap(~ ciudad_label, nrow = 3) +
  scale_fill_manual(
    values   = setNames(MAPE_PALETTE, MAPE_LABELS),
    na.value = "grey80",
    name     = "MAPE",
    drop     = FALSE
  ) +
  scale_color_identity() +
  scale_y_discrete(labels = MET_LABELS[mets_bcde]) +
  scale_x_discrete(position = "bottom") +
  labs(
    title    = "MAPE — Metodologías B, C1, C2, D y E",
    subtitle = "Nivel: alimento SIPSA | ventana 2016–2018 | N/D = sin cobertura",
    x = NULL, y = NULL
  ) +
  tema_heat +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7.5),
        axis.text.y = element_text(size = 8.5),
        )



ggsave(file.path(out_figuras, "heatmap_bcde.png"),
       p_bcde, width = 12,
       height = 12, dpi = 200, limitsize = FALSE)
message("  Heatmap B/C/D/E guardado.")

# -------------------------------------------------------
# 8. RESUMEN EN CONSOLA
# -------------------------------------------------------
message("\n====== MAPE PROMEDIO GLOBAL POR METODOLOGÍA ======")
bind_rows(
  panel_box %>% group_by(met, ciudad_label) %>%
    summarise(MAPE = mean(ape, na.rm = TRUE), .groups = "drop")
) %>%
  pivot_wider(names_from = ciudad_label, values_from = MAPE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(met) %>% print()

message("\nListo. Outputs en: ", out_dir)