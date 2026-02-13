#######################################################################
## Comparison: Monetary Poverty vs Affordability Poverty
#######################################################################

#----------------------------------------------------------------------
# Packages
#----------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)

#----------------------------------------------------------------------
# Base directory
#----------------------------------------------------------------------
base_dir <- "C:/Users/sergio.barona/Desktop/Least-cost-diets-and-affordability/Proyecto Interno"
setwd(base_dir)

out_dir <- "working-papers/working-paper-ipc/output"

poverty_dir  <- file.path(out_dir, "poverty_rates")
afford_dir   <- file.path(out_dir, "affordability_metrics")

comparison_dir <- file.path(out_dir, "comparison_results")
dir.create(comparison_dir, recursive = TRUE, showWarnings = FALSE)

#======================================================================
# 1️⃣ Load datasets
#======================================================================

poverty_df <- readRDS(file.path(poverty_dir,
                                "poverty_rates_city_month.rds"))

afford_df  <- readRDS(file.path(afford_dir,
                                "Afford_incidence_city_month.rds")) %>%
  select(-sum_rate)

#----------------------------------------------------------------------
# Harmonize structure
#----------------------------------------------------------------------

poverty_df <- poverty_df %>%
  mutate(
    fecha = as.Date(fecha),
    ciudad = dominio
  ) %>%
  select(ciudad, fecha, pm, pme)

afford_df <- afford_df %>%
  mutate(
    fecha = as.Date(fecha)
  ) %>%
  pivot_wider(
    names_from = model,
    values_from = incidence
  ) %>%
  dplyr::rename(
    afford_coca = CoCA,
    afford_cona = CoNA
  )

afford_df$ciudad[afford_df$ciudad == "MEDELLÍN"] = "MEDELLIN"
afford_df$ciudad[afford_df$ciudad == "BOGOTÁ D.C."] = "BOGOTA"

#======================================================================
# 2️⃣ Merge datasets
#======================================================================

comparison_df <- poverty_df %>%
  left_join(afford_df,
            by = c("ciudad", "fecha")) %>%
  arrange(ciudad, fecha) 


#----------------------------------------------------------------------
# Differences
#----------------------------------------------------------------------

comparison_df <- comparison_df %>%
  mutate(
    gap_coca_pm  = afford_coca - pm,
    gap_cona_pm  = afford_cona - pm,
    gap_coca_pme = afford_coca - pme,
    gap_cona_pme = afford_cona - pme
  )

# Save merged dataset
saveRDS(comparison_df,
        file.path(comparison_dir,
                  "poverty_vs_affordability_panel.rds"))

write.csv(comparison_df,
          file.path(comparison_dir,
                    "poverty_vs_affordability_panel.csv"),
          row.names = FALSE)

#======================================================================
# 3️⃣ Summary statistics
#======================================================================

summary_table <- comparison_df %>%
  group_by(ciudad) %>%
  summarise(
    mean_pm   = mean(pm, na.rm = TRUE),
    mean_pme  = mean(pme, na.rm = TRUE),
    mean_coca = mean(afford_coca, na.rm = TRUE),
    mean_cona = mean(afford_cona, na.rm = TRUE),
    mean_gap_coca_pm = mean(gap_coca_pm, na.rm = TRUE),
    mean_gap_cona_pm = mean(gap_cona_pm, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(summary_table,
          file.path(comparison_dir,
                    "summary_means_by_city.csv"),
          row.names = FALSE)

#======================================================================
# 4️⃣ Correlations
#======================================================================

cor_table <- comparison_df %>%
  group_by(ciudad) %>%
  summarise(
    cor_pm_coca  = cor(pm, afford_coca, use = "complete.obs"),
    cor_pm_cona  = cor(pm, afford_cona, use = "complete.obs"),
    cor_pme_coca = cor(pme, afford_coca, use = "complete.obs"),
    cor_pme_cona = cor(pme, afford_cona, use = "complete.obs"),
    .groups = "drop"
  )

write.csv(cor_table,
          file.path(comparison_dir,
                    "correlations_by_city.csv"),
          row.names = FALSE)

#======================================================================
# 5️⃣ Time-series comparison plots
#======================================================================

# Monetary vs CoCA
plot_coca <- comparison_df %>%
  pivot_longer(cols = c(pme, afford_coca),
               names_to = "measure",
               values_to = "value") %>%
  ggplot(aes(x = fecha, y = value, color = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y") +
  labs(title = "Monetary Poverty vs Affordability (CoCA)",
       x = "Date",
       y = "Incidence (%)",
       color = "") +
  theme_classic()

ggsave(file.path(comparison_dir,
                 "comparison_pm_vs_coca.png"),
       plot_coca, width = 12, height = 6)

# Monetary vs CoNA
plot_cona <- comparison_df %>%
  pivot_longer(cols = c(pm, afford_cona),
               names_to = "measure",
               values_to = "value") %>%
  ggplot(aes(x = fecha, y = value, color = measure)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ciudad, scales = "free_y") +
  labs(title = "Monetary Poverty vs Affordability (CoNA)",
       x = "Date",
       y = "Incidence (%)",
       color = "") +
  theme_classic()

ggsave(file.path(comparison_dir,
                 "comparison_pm_vs_cona.png"),
       plot_cona, width = 12, height = 6)

#======================================================================
# 6️⃣ Scatter plots
#======================================================================

scatter_coca <- ggplot(comparison_df,
                       aes(x = pm, y = afford_coca)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ciudad) +
  labs(title = "PM vs CoCA (Scatter)",
       x = "Monetary Poverty (%)",
       y = "Affordability CoCA (%)") +
  theme_classic()

ggsave(file.path(comparison_dir,
                 "scatter_pm_vs_coca.png"),
       scatter_coca, width = 10, height = 6)

#======================================================================
# 7️⃣ Simple regression (optional)
#======================================================================

reg_results <- comparison_df %>%
  group_by(ciudad) %>%
  do(
    tidy(lm(afford_coca ~ pm, data = .))
  )

write.csv(reg_results,
          file.path(comparison_dir,
                    "regression_afford_coca_on_pm.csv"),
          row.names = FALSE)

#######################################################################
## DONE
#######################################################################

