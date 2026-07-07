########################################################
## Supporting analysis: All food CPI subclasses vs CoCA foods
## Figure for Appendix: YoY variation of ALL food subclasses
## highlighting CoCA subclasses vs general food CPI
##
## Reads:  CACHE_DIR/dane.rds
##         CACHE_DIR/ipc.rds
##         REVIEW_DIR/input/prices/IPC.xls
##         02_models/coca/coca_results.rds
##
## Writes: FIG_DIR/final/figX_ipc_coca_subclass.png / .pdf
########################################################

source("00_config.R")
source(file.path(REVIEW_DIR, "06_figures", "00_fig_config.R"))
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(scales)

# -----------------------------------------------------------------------
# 1. Load CoCA model and identify selected foods and subclasses
# -----------------------------------------------------------------------
coca_model_path <- file.path(REVIEW_DIR, "02_models", "coca")

hcost <- readRDS(file.path(coca_model_path, "coca_results.rds")) %>%
  mutate(fecha = as.Date(fecha))

foods_coca <- levels(as.factor(hcost$Food))

dane_items <- readRDS(file.path(CACHE_DIR, "dane.rds")) %>%
  filter(articulo %in% foods_coca) %>%
  select(codigo_articulo, articulo, cod_subclase) %>%
  distinct()

coca_subclases <- unique(dane_items$cod_subclase)

cat("\n--- CoCA foods and their CPI subclasses ---\n")
print(dane_items)

# -----------------------------------------------------------------------
# 2. Load ALL food subclasses from CPI
# -----------------------------------------------------------------------
ipc_all_subclases <- readRDS(file.path(CACHE_DIR, "ipc.rds")) %>%
  filter(fecha >= "2018-12-01",
         fecha <  "2025-01-01") %>%
  mutate(
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ ciudad),
    ciudad_lbl = factor(ciudad_lbl,
                        levels = c("Bogotá", "Medellín", "Cali")),
    is_coca = cod_subclase %in% coca_subclases)

# -----------------------------------------------------------------------
# 3. Load general food CPI
# -----------------------------------------------------------------------
in_ipc    <- file.path(REVIEW_DIR, "input", "prices", "IPC.xls")
meses_esp <- c("Ene","Feb","Mar","Abr","May","Jun",
               "Jul","Ago","Sep","Oct","Nov","Dic")

ipc_clean <- read_excel(in_ipc) %>%
  clean_names() %>%
  mutate(
    mes_num    = match(mes, meses_esp),
    fecha      = as.Date(sprintf("%04d-%02d-01",
                                 as.integer(ano),
                                 as.integer(mes_num))),
    ipc        = as.numeric(numero_indice),
    ciudad_lbl = case_when(
      grepl("BOGOT", ciudad, ignore.case = TRUE) ~ "Bogotá",
      grepl("MEDEL", ciudad, ignore.case = TRUE) ~ "Medellín",
      grepl("CALI",  ciudad, ignore.case = TRUE) ~ "Cali",
      TRUE ~ toupper(ciudad))) %>%
  filter(ciudad_lbl %in% c("Bogotá", "Medellín", "Cali"),
         !is.na(fecha), !is.na(ipc)) %>%
  mutate(ciudad_lbl = factor(ciudad_lbl,
                             levels = c("Bogotá", "Medellín", "Cali"))) %>%
  select(ciudad_lbl, fecha, ipc)

# -----------------------------------------------------------------------
# 4. Compute YoY variation: all subclasses
# -----------------------------------------------------------------------
yoy_subclases <- ipc_all_subclases %>%
  group_by(ciudad_lbl, cod_subclase, is_coca) %>%
  arrange(fecha) %>%
  mutate(yoy = (ipc / lag(ipc, 12) - 1) * 100) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(yoy)) %>%
  ungroup()

# -----------------------------------------------------------------------
# 5. Compute YoY variation: general food CPI
# -----------------------------------------------------------------------
yoy_general <- ipc_clean %>%
  group_by(ciudad_lbl) %>%
  arrange(fecha) %>%
  mutate(yoy = (ipc / lag(ipc, 12) - 1) * 100) %>%
  filter(fecha >= PAPER_START, fecha <= PAPER_END,
         !is.na(yoy)) %>%
  ungroup()

# -----------------------------------------------------------------------
# 6. Figure
# -----------------------------------------------------------------------
fig_ipc <- ggplot() +
  # All non-CoCA subclasses — thin grey lines in background
  geom_line(
    data = yoy_subclases %>% filter(!is_coca),
    aes(x = fecha, y = yoy, group = cod_subclase),
    color     = "grey80",
    linewidth = 0.3,
    alpha     = 0.7) +
  # CoCA subclasses — colored and highlighted
  geom_line(
    data = yoy_subclases %>% filter(is_coca),
    aes(x = fecha, y = yoy,
        color    = cod_subclase,
        linetype = cod_subclase),
    linewidth = 0.9) +
  # General food CPI — thick black line
  geom_line(
    data = yoy_general,
    aes(x = fecha, y = yoy),
    color     = "black",
    linewidth = 1.1,
    linetype  = "dotted") +
  geom_hline(yintercept = 0, color = "grey40",
             linewidth = 0.3, linetype = "solid") +
  facet_wrap(~ ciudad_lbl, nrow = 1) +
  scale_color_manual(
    values = c(
      "01110100" = "#E67E22",
      "01110200" = "#C0392B"),
    labels = c(
      "01110100" = "Rice (01110100)",
      "01110200" = "Pre-cooked & wheat flour (01110200)"),
    name = "CoCA subclasses") +
  scale_linetype_manual(
    values = c(
      "01110100" = "solid",
      "01110200" = "dashed"),
    labels = c(
      "01110100" = "Rice (01110100)",
      "01110200" = "Pre-cooked & wheat flour (01110200)"),
    name = "CoCA subclasses") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = c(0.01, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = " ",
    subtitle = " ",
    caption  = " ",
    x = NULL,
    y = "Year-on-year variation (%)") +
  paper_theme() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.text       = element_text(family = "serif", size = 9),
    legend.key.width  = unit(1.4, "cm"),
    legend.background = element_rect(color = "black", fill = "white",
                                     linewidth = 0.5),
    legend.margin     = margin(3, 8, 3, 8),
    panel.spacing     = unit(0.4, "cm"))

ggsave(file.path(FIG_DIR, "final", "figX_ipc_coca_subclass.png"),
       fig_ipc, width = 13, height = 5, dpi = 300, bg = "white")
ggsave(file.path(FIG_DIR, "final", "fig_ipc_coca_subclass.pdf"),
       fig_ipc, width = 13, height = 5)

message("Figure IPC subclass comparison saved.")
