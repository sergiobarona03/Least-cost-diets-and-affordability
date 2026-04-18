########################################################
## Validation: Does the CoRD diet meet CoNA constraints?
## For each city × date × member:
##   1. Compute nutrient intake from CoRD composition
##   2. Compare against CoNA lower (EER_LL) and upper (UL) bounds
##   3. Report % of requirement met and pass/fail
########################################################

library(tidyverse)
library(readxl)
library(FoodpriceR)
library(scales)

##----------------------------------------------------------
## Directories
##----------------------------------------------------------

dirs <- c(
  "C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno",
  "C:/Users/danie/OneDrive/Escritorio/Least-cost-diets-and-affordability/Proyecto Interno"
)

base_dir <- dirs[dir.exists(dirs)][1]

if (is.na(base_dir)) {
  stop("Ninguno de los directorios existe")
}

out_cord   <- file.path(base_dir, "food-security-paper", "output", "cord")
out_eer    <- file.path(base_dir, "food-security-paper", "output", "eer")
out_val    <- file.path(base_dir, "food-security-paper", "output", "cord")
out_fig    <- file.path(base_dir, "food-security-paper", "output", "cord")
out_tabs   <- file.path(base_dir, "food-security-paper", "output", "cord")
input1_dir <- file.path(base_dir, "food-security-paper", "output", "tcac_food_table")

##----------------------------------------------------------
## Load CoRD composition
##----------------------------------------------------------

cord  <- readRDS(file.path(out_cord, "230326_cord_full.rds"))

df.comp <- cord$comp %>%
  mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Load nutrient data from food table
## (nutrients expressed per 100g)
##----------------------------------------------------------

nutrients_cols <- c("Energy", "Protein", "Lipids", "Carbohydrates",
                    "VitaminC", "Folate", "VitaminA", "Thiamine",
                    "Riboflavin", "Niacin", "VitaminB12",
                    "Magnesium", "Phosphorus", "Sodium",
                    "Calcium", "Iron", "Zinc")

data_nutrients <- readRDS(
  file.path(input1_dir, "panel_city_month_food_1999_2025.rds")
) %>%
  dplyr::select(ciudad, fecha, articulo,
         gramos_g_1_intercambio_1_intercambio,
         energia_kcal, proteina_g, lipidos_g, carbohidratos_totales_g,
         vitamina_c_mg, folatos_mcg, vitamina_a_er, tiamina_mg,
         riboflavina_mg, niacina_mg, vitamina_b12_mcg,
         magnesio_mg, fosforo_mg, sodio_mg, calcio_mg, hierro_mg, zinc_mg) %>%
  distinct() %>%
  filter(fecha >= "2019-01-01", fecha < "2025-01-01") %>%
  dplyr::rename(
    Food      = articulo,
    Serving_g = gramos_g_1_intercambio_1_intercambio,
    Energy        = energia_kcal,
    Protein       = proteina_g,
    Lipids        = lipidos_g,
    Carbohydrates = carbohidratos_totales_g,
    VitaminC      = vitamina_c_mg,
    Folate        = folatos_mcg,
    VitaminA      = vitamina_a_er,
    Thiamine      = tiamina_mg,
    Riboflavin    = riboflavina_mg,
    Niacin        = niacina_mg,
    VitaminB12    = vitamina_b12_mcg,
    Magnesium     = magnesio_mg,
    Phosphorus    = fosforo_mg,
    Sodium        = sodio_mg,
    Calcium       = calcio_mg,
    Iron          = hierro_mg,
    Zinc          = zinc_mg
  ) %>%
  mutate(fecha = as.Date(fecha))

##----------------------------------------------------------
## Load CoNA constraints
##----------------------------------------------------------

agg_eer <- read_excel(file.path(out_eer, "220326_agg_eer.xlsx"))

household_eer <- agg_eer %>%
  filter(
    (sex == "Masculino" & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[31,51)") |
      (sex == "Femenino"  & rango == "[10, 14)")
  ) %>%
  mutate(ciudad = case_when(
    cod_mun == "05001" ~ "MEDELLIN",
    cod_mun == "11001" ~ "BOGOTA",
    cod_mun == "76001" ~ "CALI",
    TRUE ~ cod_mun
  )) %>%
  filter(ciudad %in% c("BOGOTA", "MEDELLIN", "CALI")) %>%
  dplyr::rename(Age = rango, Sex = sex, Energy = eer) %>%
  mutate(Sex = if_else(Sex == "Masculino", 0L, 1L)) %>%
  as.data.frame()

eer_ll_base <- FoodpriceR::EER_LL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

ul_base <- FoodpriceR::UL %>%
  mutate(Age = recode(Age,
                      "31 a 50 años" = "[31,51)",
                      "9 a 13 años"  = "[10, 14)"))

# Lower bounds: merge EER_LL with household EER
cona_ll <- merge(household_eer,
                 eer_ll_base %>% select(-Energy),
                 by = c("Sex", "Age")) %>%
  pivot_longer(cols = all_of(nutrients_cols),
               names_to = "Nutrient", values_to = "LL")

# Upper bounds
cona_ul <- merge(household_eer,
                 ul_base %>% select(-Energy),
                 by = c("Sex", "Age")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 9999999))) %>%
  pivot_longer(cols = all_of(nutrients_cols),
               names_to = "Nutrient", values_to = "UL")

# Combined constraints
cona_constraints <- cona_ll %>%
  left_join(cona_ul %>% select(Sex, Age, ciudad, Nutrient, UL),
            by = c("Sex", "Age", "ciudad", "Nutrient"))

##----------------------------------------------------------
## Step 1: Compute nutrient intake from CoRD diet
## intake = Number_Serving × Serving_g / 100 × nutrient_per_100g
##----------------------------------------------------------

cord_intake <- df.comp %>%
  left_join(
    data_nutrients %>% select(ciudad, fecha, Food, Serving_g,
                              all_of(nutrients_cols)),
    by = c("ciudad", "fecha", "Food")
  ) %>%
  mutate(
    # Grams consumed = servings × grams per serving
    grams = Number_Serving * Serving_g
  ) %>%
  # Nutrients are per 100g → scale by grams / 100
  mutate(across(all_of(nutrients_cols), ~ .x * grams / 100,
                .names = "intake_{.col}")) %>%
  # Sum intake across all foods per member × city × date
  dplyr::group_by(ciudad, fecha, Demo_Group, Sex) %>%
  dplyr::summarize(across(starts_with("intake_"), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  # Clean column names
  rename_with(~ str_remove(.x, "^intake_"), starts_with("intake_"))

##----------------------------------------------------------
## Step 2: Compare intake against CoNA constraints
##----------------------------------------------------------

cord_long <- cord_intake %>%
  pivot_longer(cols = all_of(nutrients_cols),
               names_to = "Nutrient", values_to = "Intake") %>%
  dplyr::rename(Age = Demo_Group)


cord_long$Sex = as.factor(cord_long$Sex)
cona_constraints$Sex = as.factor(cona_constraints$Sex)

validation <- cord_long %>%
  left_join(cona_constraints %>%
              select(Sex, Age, ciudad, Nutrient, LL, UL),
            by = c("Sex", "Age", "ciudad", "Nutrient")) %>%
  mutate(
    # % of lower bound met
    pct_of_LL   = Intake / LL * 100,
    # Pass/fail
    meets_LL    = Intake >= LL,
    meets_UL    = Intake <= UL,
    meets_both  = meets_LL & meets_UL,
    # Gap from lower bound (negative = deficient)
    gap_LL      = Intake - LL,
    gap_LL_pct  = (Intake - LL) / LL * 100
  )

##----------------------------------------------------------
## Step 3: Summary — overall compliance rate
##----------------------------------------------------------

# By nutrient × member × city: % of months compliant
compliance_summary <- validation %>%
  dplyr::group_by(Nutrient, Age, Sex, ciudad) %>%
  dplyr::summarize(
    pct_meets_LL   = mean(meets_LL,   na.rm = TRUE) * 100,
    pct_meets_UL   = mean(meets_UL,   na.rm = TRUE) * 100,
    pct_meets_both = mean(meets_both, na.rm = TRUE) * 100,
    mean_pct_of_LL = mean(pct_of_LL,  na.rm = TRUE),
    mean_gap_pct   = mean(gap_LL_pct, na.rm = TRUE),
    .groups = "drop"
  )

# Overall compliance by nutrient (across all cities and members)
compliance_overall <- validation %>%
  dplyr::group_by(Nutrient) %>%
  dplyr::summarize(
    pct_meets_LL   = mean(meets_LL,   na.rm = TRUE) * 100,
    mean_pct_of_LL = mean(pct_of_LL,  na.rm = TRUE),
    mean_gap_pct   = mean(gap_LL_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(pct_meets_LL)

##----------------------------------------------------------
## Save validation outputs
##----------------------------------------------------------

saveRDS(validation,
        file.path(out_val, "230326_cord_cona_validation.rds"))

writexl::write_xlsx(
  list(validation  = validation,
       summary     = compliance_summary,
       overall     = compliance_overall),
  file.path(out_val, "230326_cord_cona_validation.xlsx")
)

##----------------------------------------------------------
## Aesthetics
##----------------------------------------------------------

city_labels <- c(
  "BOGOTA"   = "Bogotá",
  "MEDELLIN" = "Medellín",
  "CALI"     = "Cali"
)

member_labels <- c(
  "0_[31,51)"  = "Adult male",
  "1_[10, 14)" = "Female child",
  "1_[31,51)"  = "Adult female"
)

member_order <- c("Adult male", "Adult female", "Female child")

nutrient_order <- compliance_overall %>%
  arrange(pct_meets_LL) %>%
  pull(Nutrient)

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50", hjust = 0),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10)
  )

##----------------------------------------------------------
## Figure V1: % of lower bound met — heatmap
##            rows = nutrient, columns = member × city, fill = mean %
##----------------------------------------------------------

heat_data <- compliance_summary %>%
  mutate(
    member       = recode(paste0(Sex, "_", Age), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    col_label    = paste0(member, "\n(", ciudad_label, ")"),
    Nutrient     = factor(Nutrient, levels = nutrient_order)
  )

fig_v1 <- ggplot(heat_data,
                 aes(x = col_label, y = Nutrient, fill = mean_pct_of_LL)) +
  geom_tile(color = "white", linewidth = 0.4) +
  # Mark cells where compliance < 100% of months
  geom_point(data = filter(heat_data, pct_meets_LL < 100),
             aes(x = col_label, y = Nutrient),
             shape = 4, color = "white", size = 2,
             inherit.aes = FALSE) +
  scale_fill_gradientn(
    colours = c("#C0392B", "#E67E22", "#F1C40F", "#27AE60"),
    values  = rescale(c(0, 80, 100, 150)),
    limits  = c(0, NA),
    name    = "Mean intake\n(% of requirement)",
    labels  = percent_format(scale = 1, accuracy = 1)
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Figure V1. CoRD compliance with CoNA nutritional lower bounds",
    subtitle = "Mean daily intake as a percentage of the minimum nutritional requirement",
    caption  = paste0(
      "Note: Cell colour indicates mean daily nutrient intake as a percentage of the CoNA lower\n",
      "bound, averaged over 2019–2024. Green ≥ 100% (requirement met on average). Crosses (✕)\n",
      "indicate nutrient–member combinations where the lower bound is not met in all months.\n",
      "Nutrients ordered by ascending compliance rate across all city–month observations."
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    text              = element_text(family = "serif"),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 10, color = "grey40"),
    plot.caption      = element_text(size = 8, color = "grey50", hjust = 0),
    axis.text.x       = element_text(size = 8, angle = 30, hjust = 0),
    axis.text.y       = element_text(size = 9),
    legend.position   = "right",
    legend.title      = element_text(size = 9, face = "bold"),
    legend.key.height = unit(1.5, "cm"),
    panel.grid        = element_blank(),
    panel.spacing     = unit(0.3, "cm")
  )

ggsave(file.path(out_fig, "figV1_cord_cona_compliance_heatmap.png"),
       fig_v1, width = 12, height = 7, dpi = 300)

##----------------------------------------------------------
## Figure V2: % of months meeting LL — bar chart
##            ordered by ascending compliance, facet by member
##----------------------------------------------------------

bar_data <- compliance_summary %>%
  mutate(
    member       = recode(paste0(Sex, "_", Age), !!!member_labels),
    member       = factor(member, levels = member_order),
    ciudad_label = recode(ciudad, !!!city_labels),
    Nutrient     = factor(Nutrient, levels = nutrient_order)
  )

city_colors <- c(
  "Bogotá"   = "#2C3E6B",
  "Medellín" = "#C0392B",
  "Cali"     = "#27AE60"
)

fig_v2 <- ggplot(bar_data,
                 aes(x = Nutrient, y = pct_meets_LL / 100,
                     fill = ciudad_label)) +
  geom_col(position = position_dodge(width = 0.75),
           width = 0.68, alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey30", linewidth = 0.5) +
  facet_wrap(~ member, ncol = 1) +
  scale_fill_manual(values = city_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.05), breaks = seq(0, 1, 0.25)) +
  labs(
    title    = "Figure V2. Share of months in which CoRD meets CoNA lower bounds, 2019–2024",
    subtitle = "Proportion of city–month observations where daily intake ≥ minimum requirement",
    caption  = paste0(
      "Note: Dashed line at 100% indicates full compliance across all months. Nutrients are\n",
      "ordered by ascending overall compliance rate. Only lower bound (minimum) constraints shown."
    ),
    x = "Nutrient", y = "Share of compliant months"
  ) +
  paper_theme

ggsave(file.path(out_fig, "figV2_cord_cona_compliance_bars.png"),
       fig_v2, width = 10, height = 10, dpi = 300)

