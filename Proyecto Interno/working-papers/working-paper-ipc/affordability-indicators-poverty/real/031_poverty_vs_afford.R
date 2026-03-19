#######################################################################
## Comparison: Monetary Poverty vs Affordability Poverty (CoCA/CoNA/CoRD)
#######################################################################

#----------------------------------------------------------------------
# Packages
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)
library(scales)
library(ggsci)
library(stringr)

#----------------------------------------------------------------------
# Base directory
#----------------------------------------------------------------------
base_dir <- "C:\\Users\\danie\\OneDrive\\Escritorio\\Least-cost-diets-and-affordability\\Proyecto Interno\\"
setwd(base_dir)

out_dir <- "working-papers/working-paper-ipc/output"

poverty_dir  <- file.path(out_dir, "poverty_rates")
afford_dir   <- file.path(out_dir, "affordability_metrics")

comparison_dir <- file.path(out_dir, "comparison_results")
dir.create(comparison_dir, recursive = TRUE, showWarnings = FALSE)

#======================================================================
# ----------------------------- MENSUAL -------------------------------
#======================================================================

#======================================================================
# Load datasets (monthly)
#======================================================================
poverty_df <- readRDS(file.path(poverty_dir, "poverty_rates_city_month.rds"))

afford_df  <- readRDS(file.path(afford_dir, "Afford_incidence_city_month.rds")) %>%
  select(-sum_rate)

#----------------------------------------------------------------------
# Harmonize structure (monthly)
#----------------------------------------------------------------------
poverty_df <- poverty_df %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = dominio
  ) %>%
  select(ciudad, fecha, pm, pme)

afford_df <- afford_df %>%
  mutate(
    fecha = as.Date(fecha)
  ) %>%
  pivot_wider(
    names_from  = model,
    values_from = incidence
  ) %>%
  dplyr::rename(
    afford_coca = CoCA,
    afford_cona = CoNA,
    afford_cord = CoRD    
  )

afford_df$ciudad[afford_df$ciudad == "MEDELLÍN"]     <- "MEDELLIN"
afford_df$ciudad[afford_df$ciudad == "BOGOTÁ D.C."]  <- "BOGOTA"

#======================================================================
# Merge datasets (monthly)
#======================================================================
comparison_df <- poverty_df %>%
  left_join(afford_df, by = c("ciudad", "fecha")) %>%
  arrange(ciudad, fecha)

#----------------------------------------------------------------------
# Differences (monthly)
#----------------------------------------------------------------------
comparison_df <- comparison_df %>%
  mutate(
    gap_coca_pm   = afford_coca - pm,
    gap_cona_pm   = afford_cona - pm,
    gap_cord_pm   = afford_cord - pm,   
    gap_coca_pme  = afford_coca - pme,
    gap_cona_pme  = afford_cona - pme,
    gap_cord_pme  = afford_cord - pme   
  )

# Save merged dataset (monthly)
saveRDS(comparison_df, file.path(comparison_dir, "poverty_vs_affordability_panel_month.rds"))
write.csv(comparison_df, file.path(comparison_dir, "poverty_vs_affordability_panel_month.csv"),
          row.names = FALSE)

#======================================================================
# Summary statistics (monthly)
#======================================================================
summary_table <- comparison_df %>%
  group_by(ciudad) %>%
  summarise(
    mean_pm   = mean(pm, na.rm = TRUE),
    mean_pme  = mean(pme, na.rm = TRUE),
    mean_coca = mean(afford_coca, na.rm = TRUE),
    mean_cona = mean(afford_cona, na.rm = TRUE),
    mean_cord = mean(afford_cord, na.rm = TRUE),                 
    mean_gap_coca_pm = mean(gap_coca_pm, na.rm = TRUE),
    mean_gap_cona_pm = mean(gap_cona_pm, na.rm = TRUE),
    mean_gap_cord_pm = mean(gap_cord_pm, na.rm = TRUE),          
    .groups = "drop"
  )

write.csv(summary_table, file.path(comparison_dir, "summary_means_by_city_month.csv"),
          row.names = FALSE)

#======================================================================
# Correlations (monthly)
#======================================================================
cor_table <- comparison_df %>%
  group_by(ciudad) %>%
  summarise(
    cor_pm_coca  = cor(pm,  afford_coca, use = "complete.obs"),
    cor_pm_cona  = cor(pm,  afford_cona, use = "complete.obs"),
    cor_pm_cord  = cor(pm,  afford_cord, use = "complete.obs"),  
    cor_pme_coca = cor(pme, afford_coca, use = "complete.obs"),
    cor_pme_cona = cor(pme, afford_cona, use = "complete.obs"),
    cor_pme_cord = cor(pme, afford_cord, use = "complete.obs"), 
    .groups = "drop"
  )

write.csv(cor_table, file.path(comparison_dir, "correlations_by_city_month.csv"),
          row.names = FALSE)

#======================================================================
# Time-series comparison plots (monthly)
#======================================================================

city_levels <- c("BOGOTA", "CALI", "MEDELLIN")

std_city <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("BOGOTÁ D.C.", "BOGOTA D.C.", "BOGOTA") ~ "BOGOTA",
    x %in% c("MEDELLÍN", "MEDELLIN")                 ~ "MEDELLIN",
    x %in% c("CALI")                                 ~ "CALI",
    TRUE ~ x
  )
}

theme_paper <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text  = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    plot.margin = margin(6, 6, 6, 6),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_blank()
  )

x_scale_6m <- scale_x_date(
  date_breaks = "6 months",
  date_labels = "%Y-%m",
  expand = expansion(mult = c(0.01, 0.01))
)

y_scale_pct <- scale_y_continuous(
  labels = label_percent(scale = 1, accuracy = 0.1),
  expand = expansion(mult = c(0.02, 0.02))
)

measure_colors <- scale_color_nejm(name = NULL)

# Monetary vs CoCA (monthly)
plot_coca <- comparison_df %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = factor(std_city(ciudad), levels = city_levels)
  ) %>%
  pivot_longer(cols = c(pme, afford_coca), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pme = "Extreme monetary poverty (PME)",
                     afford_coca = "Affordability poverty (CoCA)"),
    measure = factor(measure,
                     levels = c("Extreme monetary poverty (PME)",
                                "Affordability poverty (CoCA)"))
  ) %>%
  filter(!is.na(value), !is.na(fecha), !is.na(ciudad)) %>%
  arrange(ciudad, measure, fecha) %>%
  ggplot(aes(x = fecha, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_6m + y_scale_pct +
  labs(title = "Extreme monetary poverty vs affordability poverty (CoCA)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pme_vs_coca_month.png"),
       plot_coca, width = 12, height = 6, dpi = 300, bg = "white")

# Monetary vs CoNA (monthly)
plot_cona <- comparison_df %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = factor(std_city(ciudad), levels = city_levels)
  ) %>%
  pivot_longer(cols = c(pm, afford_cona), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pm = "Monetary poverty (PM)",
                     afford_cona = "Affordability poverty (CoNA)"),
    measure = factor(measure,
                     levels = c("Monetary poverty (PM)",
                                "Affordability poverty (CoNA)"))
  ) %>%
  filter(!is.na(value), !is.na(fecha), !is.na(ciudad)) %>%
  arrange(ciudad, measure, fecha) %>%
  ggplot(aes(x = fecha, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_6m + y_scale_pct +
  labs(title = "Monetary poverty vs affordability poverty (CoNA)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pm_vs_cona_month.png"),
       plot_cona, width = 12, height = 6, dpi = 300, bg = "white")

# Monetary vs CoRD (monthly)  
plot_cord <- comparison_df %>%
  mutate(
    fecha  = as.Date(fecha),
    ciudad = factor(std_city(ciudad), levels = city_levels)
  ) %>%
  pivot_longer(cols = c(pm, afford_cord), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pm = "Monetary poverty (PM)",
                     afford_cord = "Affordability poverty (CoRD)"),
    measure = factor(measure,
                     levels = c("Monetary poverty (PM)",
                                "Affordability poverty (CoRD)"))
  ) %>%
  filter(!is.na(value), !is.na(fecha), !is.na(ciudad)) %>%
  arrange(ciudad, measure, fecha) %>%
  ggplot(aes(x = fecha, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_6m + y_scale_pct +
  labs(title = "Monetary poverty vs affordability poverty (CoRD)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pm_vs_cord_month.png"),
       plot_cord, width = 12, height = 6, dpi = 300, bg = "white")

# Scatter (monthly): PM vs CoCA
scatter_coca <- comparison_df %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  filter(!is.na(pm), !is.na(afford_coca), !is.na(ciudad)) %>%
  ggplot(aes(x = pm, y = afford_coca)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = "Monetary poverty (PM) vs affordability poverty (CoCA)",
       x = "Monetary poverty (incidence)",
       y = "Affordability poverty (CoCA)") +
  theme_paper

ggsave(file.path(comparison_dir, "scatter_pm_vs_coca_month.png"),
       scatter_coca, width = 10, height = 6, dpi = 300, bg = "white")

# Scatter (monthly): PM vs CoNA
scatter_cona <- comparison_df %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  filter(!is.na(pm), !is.na(afford_cona), !is.na(ciudad)) %>%
  ggplot(aes(x = pm, y = afford_cona)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = "Monetary poverty (PM) vs affordability poverty (CoNA)",
       x = "Monetary poverty (incidence)",
       y = "Affordability poverty (CoNA)") +
  theme_paper

ggsave(file.path(comparison_dir, "scatter_pm_vs_cona_month.png"),
       scatter_cona, width = 10, height = 6, dpi = 300, bg = "white")

# Scatter (monthly): PM vs CoRD  
scatter_cord <- comparison_df %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  filter(!is.na(pm), !is.na(afford_cord), !is.na(ciudad)) %>%
  ggplot(aes(x = pm, y = afford_cord)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = "Monetary poverty (PM) vs affordability poverty (CoRD)",
       x = "Monetary poverty (incidence)",
       y = "Affordability poverty (CoRD)") +
  theme_paper

ggsave(file.path(comparison_dir, "scatter_pm_vs_cord_month.png"),
       scatter_cord, width = 10, height = 6, dpi = 300, bg = "white")

# Regression (monthly): CoCA on PM
reg_results_coca <- comparison_df %>%
  group_by(ciudad) %>%
  do(tidy(lm(afford_coca ~ pm, data = .)))

write.csv(reg_results_coca,
          file.path(comparison_dir, "regression_afford_coca_on_pm_month.csv"),
          row.names = FALSE)

# Regression (monthly): CoNA on PM
reg_results_cona <- comparison_df %>%
  group_by(ciudad) %>%
  do(tidy(lm(afford_cona ~ pm, data = .)))

write.csv(reg_results_cona,
          file.path(comparison_dir, "regression_afford_cona_on_pm_month.csv"),
          row.names = FALSE)

# Regression (monthly): CoRD on PM  
reg_results_cord <- comparison_df %>%
  group_by(ciudad) %>%
  do(tidy(lm(afford_cord ~ pm, data = .)))

write.csv(reg_results_cord,
          file.path(comparison_dir, "regression_afford_cord_on_pm_month.csv"),
          row.names = FALSE)

#======================================================================
# ---------------------------- TRIMESTRAL -----------------------------
#======================================================================

poverty_df_q <- readRDS(file.path(poverty_dir, "poverty_rates_city_cuartiles.rds"))
afford_df_q  <- readRDS(file.path(afford_dir, "Afford_incidence_city_cuartiles.rds"))

to_tri_date <- function(tri_str) {
  tri_str <- gsub("\\s+", "", as.character(tri_str))
  year <- as.integer(str_extract(tri_str, "^\\d{4}"))
  q    <- as.integer(str_extract(tri_str, "(?<=Q)\\d+"))
  as.Date(sprintf("%d-%02d-01", year, (q - 1) * 3 + 1))
}

# Harmonize poverty (quarter)
poverty_df_q <- poverty_df_q %>%
  mutate(
    tri_date = to_tri_date(trimestre),
    ciudad   = dominio
  ) %>%
  select(ciudad, tri_date, pm, pme)

# Harmonize afford (quarter)
afford_df_q <- afford_df_q %>%
  mutate(
    tri_date = to_tri_date(trimestre)
  ) %>%
  pivot_wider(
    names_from  = model,
    values_from = incidence
  ) %>%
  dplyr::rename(
    afford_coca = CoCA,
    afford_cona = CoNA,
    afford_cord = CoRD   
  )

afford_df_q$ciudad[afford_df_q$ciudad == "MEDELLÍN"]    <- "MEDELLIN"
afford_df_q$ciudad[afford_df_q$ciudad == "BOGOTÁ D.C."] <- "BOGOTA"

# Merge quarter
comparison_df_q <- poverty_df_q %>%
  left_join(afford_df_q, by = c("ciudad", "tri_date")) %>%
  arrange(ciudad, tri_date)

# Differences quarter
comparison_df_q <- comparison_df_q %>%
  mutate(
    gap_coca_pm   = afford_coca - pm,
    gap_cona_pm   = afford_cona - pm,
    gap_cord_pm   = afford_cord - pm,   
    gap_coca_pme  = afford_coca - pme,
    gap_cona_pme  = afford_cona - pme,
    gap_cord_pme  = afford_cord - pme   
  )

saveRDS(comparison_df_q, file.path(comparison_dir, "poverty_vs_affordability_panel_quarter.rds"))
write.csv(comparison_df_q, file.path(comparison_dir, "poverty_vs_affordability_panel_quarter.csv"),
          row.names = FALSE)

x_scale_q <- scale_x_date(
  date_breaks = "1 year",
  date_labels = "%Y",
  expand = expansion(mult = c(0.01, 0.01))
)

# Extreme Monetary Poverty vs CoCA (quarter)
plot_coca_q <- comparison_df_q %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  pivot_longer(cols = c(pme, afford_coca), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pme = "Extreme monetary poverty (PME)",
                     afford_coca = "Affordability poverty (CoCA)"),
    measure = factor(measure,
                     levels = c("Extreme monetary poverty (PME)",
                                "Affordability poverty (CoCA)"))
  ) %>%
  filter(!is.na(value), !is.na(tri_date), !is.na(ciudad)) %>%
  arrange(ciudad, measure, tri_date) %>%
  ggplot(aes(x = tri_date, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_q + y_scale_pct +
  labs(title = "Extreme monetary poverty vs affordability poverty (CoCA)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pme_vs_coca_quarter.png"),
       plot_coca_q, width = 12, height = 6, dpi = 300, bg = "white")

# Monetary Poverty vs CoNA (quarter)
plot_cona_q <- comparison_df_q %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  pivot_longer(cols = c(pm, afford_cona), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pm = "Monetary poverty (PM)",
                     afford_cona = "Affordability poverty (CoNA)"),
    measure = factor(measure,
                     levels = c("Monetary poverty (PM)",
                                "Affordability poverty (CoNA)"))
  ) %>%
  filter(!is.na(value), !is.na(tri_date), !is.na(ciudad)) %>%
  arrange(ciudad, measure, tri_date) %>%
  ggplot(aes(x = tri_date, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_q + y_scale_pct +
  labs(title = "Monetary poverty vs affordability poverty (CoNA)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pm_vs_cona_quarter.png"),
       plot_cona_q, width = 12, height = 6, dpi = 300, bg = "white")

# Monetary Poverty vs CoRD (quarter)  
plot_cord_q <- comparison_df_q %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  pivot_longer(cols = c(pm, afford_cord), names_to = "measure", values_to = "value") %>%
  mutate(
    measure = recode(measure,
                     pm = "Monetary poverty (PM)",
                     afford_cord = "Affordability poverty (CoRD)"),
    measure = factor(measure,
                     levels = c("Monetary poverty (PM)",
                                "Affordability poverty (CoRD)"))
  ) %>%
  filter(!is.na(value), !is.na(tri_date), !is.na(ciudad)) %>%
  arrange(ciudad, measure, tri_date) %>%
  ggplot(aes(x = tri_date, y = value, color = measure, group = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y", nrow = 1) +
  x_scale_q + y_scale_pct +
  labs(title = "Monetary poverty vs affordability poverty (CoRD)",
       x = NULL, y = "Incidence") +
  measure_colors + theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(comparison_dir, "comparison_pm_vs_cord_quarter.png"),
       plot_cord_q, width = 12, height = 6, dpi = 300, bg = "white")

# Scatter quarter: PM vs CoCA
scatter_coca_q <- comparison_df_q %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  filter(!is.na(pm), !is.na(afford_coca), !is.na(ciudad)) %>%
  ggplot(aes(x = pm, y = afford_coca)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = "Monetary poverty (PM) vs affordability poverty (CoCA)",
       x = "Monetary poverty (incidence)",
       y = "Affordability poverty (CoCA)") +
  theme_paper

ggsave(file.path(comparison_dir, "scatter_pm_vs_coca_quarter.png"),
       scatter_coca_q, width = 10, height = 6, dpi = 300, bg = "white")

# Scatter quarter: PM vs CoRD  
scatter_cord_q <- comparison_df_q %>%
  mutate(ciudad = factor(std_city(ciudad), levels = city_levels)) %>%
  filter(!is.na(pm), !is.na(afford_cord), !is.na(ciudad)) %>%
  ggplot(aes(x = pm, y = afford_cord)) +
  geom_point(alpha = 0.8, size = 1.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "black") +
  facet_wrap(~ciudad, nrow = 1) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(title = "Monetary poverty (PM) vs affordability poverty (CoRD)",
       x = "Monetary poverty (incidence)",
       y = "Affordability poverty (CoRD)") +
  theme_paper

ggsave(file.path(comparison_dir, "scatter_pm_vs_cord_quarter.png"),
       scatter_cord_q, width = 10, height = 6, dpi = 300, bg = "white")

# Regression quarter: CoCA on PM
reg_results_q_coca <- comparison_df_q %>%
  group_by(ciudad) %>%
  do(tidy(lm(afford_coca ~ pm, data = .)))

write.csv(reg_results_q_coca,
          file.path(comparison_dir, "regression_afford_coca_on_pm_quarter.csv"),
          row.names = FALSE)

# Regression quarter: CoRD on PM  
reg_results_q_cord <- comparison_df_q %>%
  group_by(ciudad) %>%
  do(tidy(lm(afford_cord ~ pm, data = .)))

write.csv(reg_results_q_cord,
          file.path(comparison_dir, "regression_afford_cord_on_pm_quarter.csv"),
          row.names = FALSE)

#######################################################################
## DONE
#######################################################################