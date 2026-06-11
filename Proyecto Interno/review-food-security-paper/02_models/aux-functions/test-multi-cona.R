########################################################
## Reproducible test: Multi_CoNA_paper()
## Uses the same synthetic data as cona_molp_pareto.R
########################################################

library(tidyverse)
library(lpSolve)

# Load the function
source("Multi_CoNA_paper.R")

##----------------------------------------------------------
## 1. Synthetic food table
##    All nutrients expressed per 100g
##----------------------------------------------------------

data.aux <- tribble(
  ~Food,            ~Group,       ~Price_100g, ~Energy, ~Protein, ~Iron, ~Calcium, ~VitaminC,
  "Rice",           "Cereals",    120,          365,     7.1,      0.8,   28,       0,
  "Bread",          "Cereals",    180,          265,     9.0,      2.5,   260,      0,
  "Oats",           "Cereals",    150,          389,     17.0,     4.7,   54,       0,
  "Banana",         "Fruits",      80,           89,     1.1,      0.3,   5,        8.7,
  "Orange",         "Fruits",      90,           47,     0.9,      0.1,   40,       53.2,
  "Guava",          "Fruits",     100,           68,     2.6,      0.3,   18,       228.3,
  "Carrot",         "Vegetables",  70,           41,     0.9,      0.3,   33,       5.9,
  "Tomato",         "Vegetables",  85,           18,     0.9,      0.3,   10,       13.7,
  "Spinach",        "Vegetables", 120,           23,     2.9,      2.7,   99,       28.1,
  "Milk",           "Dairy",      110,           61,     3.2,      0.0,   113,      0,
  "Yogurt",         "Dairy",      130,           59,     3.5,      0.1,   110,      0.5,
  "Chicken",        "Meat",       320,          165,    31.0,      1.0,   15,       0,
  "Dry beans",      "Meat",       140,          347,    21.6,      6.7,   123,      0,
  "Eggs",           "Meat",       220,          155,    13.0,      1.8,   50,       0,
  "Vegetable oil",  "Fats",       400,          884,     0.0,      0.0,   1,        0,
  "Sugar",          "Sugars",     100,          387,     0.0,      0.0,   1,        0
) %>% as.data.frame()

##----------------------------------------------------------
## 2. Nutritional lower bounds — EER_LL
##    Two demographic groups: adult female and female child
##----------------------------------------------------------

eer_ll.aux <- data.frame(
  Age      = c("[31,51)", "[10, 14)"),
  Sex      = c(1L, 1L),
  Energy   = c(2100, 2000),
  Protein  = c(46,   34),
  Iron     = c(18,    8),
  Calcium  = c(1000, 1300),
  VitaminC = c(75,   65)
)

##----------------------------------------------------------
## 3. Upper bounds — UL
##----------------------------------------------------------

ul.aux <- data.frame(
  Age      = c("[31,51)", "[10, 14)"),
  Sex      = c(1L, 1L),
  Protein  = c(200,  200),
  Iron     = c(45,   40),
  Calcium  = c(2500, 3000),
  VitaminC = c(2000, 1800)
)

##----------------------------------------------------------
## 4. IPC expenditure shares
##----------------------------------------------------------

ipc_shares <- tribble(
  ~Group,        ~share,
  "Cereals",     0.30,
  "Fruits",      0.10,
  "Vegetables",  0.10,
  "Dairy",       0.15,
  "Meat",        0.25,
  "Fats",        0.05,
  "Sugars",      0.05
) %>% as.data.frame()

##----------------------------------------------------------
## 5. Run Multi_CoNA_paper for a single lambda first
##    (sanity check before running the full frontier)
##----------------------------------------------------------

cat("========================================\n")
cat("TEST 1: Single lambda = 0 (standard CoNA)\n")
cat("========================================\n")

test_lam0 <- Multi_CoNA_paper(
  data       = data.aux,
  EER_LL     = eer_ll.aux,
  UL         = ul.aux,
  IPC_shares = ipc_shares,
  lambda     = 0
)

cat("\n$cost:\n");   print(test_lam0$cost)
cat("\n$comp:\n");   print(test_lam0$comp)
cat("\n$limit:\n");  print(test_lam0$limit)

cat("\n========================================\n")
cat("TEST 2: Single lambda = 0.5\n")
cat("========================================\n")

test_lam5 <- Multi_CoNA_paper(
  data       = data.aux,
  EER_LL     = eer_ll.aux,
  UL         = ul.aux,
  IPC_shares = ipc_shares,
  lambda     = 0.5
)

cat("\n$cost:\n");  print(test_lam5$cost)
cat("\n$comp:\n");  print(test_lam5$comp)

cat("\n========================================\n")
cat("TEST 3: Single lambda = 1 (pure IPC pattern)\n")
cat("========================================\n")

test_lam1 <- Multi_CoNA_paper(
  data       = data.aux,
  EER_LL     = eer_ll.aux,
  UL         = ul.aux,
  IPC_shares = ipc_shares,
  lambda     = 1
)

cat("\n$cost:\n");  print(test_lam1$cost)
cat("\n$comp:\n");  print(test_lam1$comp)

##----------------------------------------------------------
## 6. Run full Pareto frontier
##----------------------------------------------------------

cat("\n========================================\n")
cat("TEST 4: Full Pareto frontier\n")
cat("========================================\n")

lambdas <- seq(0, 1, by = 0.1)

pareto <- Multi_CoNA_paper(
  data       = data.aux,
  EER_LL     = eer_ll.aux,
  UL         = ul.aux,
  IPC_shares = ipc_shares,
  lambda     = lambdas
)

##----------------------------------------------------------
## 7. Extract and summarise results
##----------------------------------------------------------

# Cost across lambdas
costos <- map_dfr(pareto, function(res) {
  res$cost %>% mutate(lambda = res$lambda)
})

cat("\n--- Cost summary across lambdas ---\n")
print(costos %>% select(lambda, Demo_Group, Sex, cost_day, Cost_1000kcal))

# Diet composition across lambdas
composicion <- map_dfr(pareto, function(res) {
  res$comp %>% mutate(lambda = res$lambda)
})

# Number of foods and groups selected per lambda × demographic group
diversity <- composicion %>%
  group_by(lambda, Demo_Group, Sex) %>%
  summarize(
    n_foods  = n_distinct(Food),
    n_groups = n_distinct(Group),
    .groups  = "drop"
  )

cat("\n--- Diet diversity across lambdas ---\n")
print(diversity)

# IPC adherence: actual expenditure share vs IPC target
adherence <- composicion %>%
  left_join(data.aux %>% select(Food, Price_100g, Group),
            by = c("Food", "Group")) %>%
  group_by(lambda, Demo_Group, Sex) %>%
  mutate(
    cost_food  = (quantity / 100) * Price_100g,
    total_cost = sum(cost_food)
  ) %>%
  group_by(lambda, Demo_Group, Sex, Group) %>%
  summarize(
    actual_share = sum(cost_food) / first(total_cost),
    .groups = "drop"
  ) %>%
  left_join(ipc_shares, by = "Group") %>%
  mutate(deviation = actual_share - share)

cat("\n--- IPC adherence at lambda = 0 ---\n")
print(adherence %>% filter(lambda == 0) %>%
        select(Demo_Group, Group, actual_share, share, deviation))

cat("\n--- IPC adherence at lambda = 0.5 ---\n")
print(adherence %>% filter(lambda == 0.5) %>%
        select(Demo_Group, Group, actual_share, share, deviation))

cat("\n--- IPC adherence at lambda = 1 ---\n")
print(adherence %>% filter(lambda == 1) %>%
        select(Demo_Group, Group, actual_share, share, deviation))

##----------------------------------------------------------
## 8. Pareto frontier plot
##----------------------------------------------------------

# Mean absolute deviation from IPC shares per lambda × demo group
mad_ipc <- adherence %>%
  group_by(lambda, Demo_Group, Sex) %>%
  summarize(mad = mean(abs(deviation)), .groups = "drop")

# Join with cost
frontier_data <- costos %>%
  left_join(mad_ipc, by = c("lambda", "Demo_Group", "Sex")) %>%
  mutate(
    member = case_when(
      Sex == 0 & Demo_Group == "[31,51)"  ~ "Adult male",
      Sex == 1 & Demo_Group == "[31,51)"  ~ "Adult female",
      Sex == 1 & Demo_Group == "[10, 14)" ~ "Female child",
      TRUE ~ paste0(Sex, "_", Demo_Group)
    )
  )

# Cost premium relative to lambda = 0
cost_base <- frontier_data %>%
  filter(lambda == 0) %>%
  select(Demo_Group, Sex, cost_base = cost_day)

frontier_data <- frontier_data %>%
  left_join(cost_base, by = c("Demo_Group", "Sex")) %>%
  mutate(cost_premium = (cost_day / cost_base - 1) * 100)

library(scales)

p1 <- ggplot(frontier_data,
             aes(x = mad, y = cost_premium, color = member)) +
  geom_path(linewidth = 0.9) +
  geom_point(aes(shape = factor(round(lambda, 2))), size = 2.5) +
  scale_color_manual(values = c(
    "Adult male"   = "#2C3E6B",
    "Adult female" = "#C0392B",
    "Female child" = "#27AE60"
  )) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 1), "%")) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "Pareto frontier: diet cost vs. IPC dietary pattern adherence",
    subtitle = "Each point is one value of λ; arrows point toward λ = 1",
    x        = "Mean absolute deviation from IPC expenditure shares",
    y        = "Cost premium over standard CoNA (%)",
    color    = "Household member",
    shape    = expression(lambda),
    caption  = paste0(
      "Note: λ = 0 recovers the standard CoNA. λ = 1 maximises IPC adherence\n",
      "at fixed total expenditure E* = CoNA optimal cost."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8, color = "grey50", hjust = 0),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

p2 <- ggplot(frontier_data,
             aes(x = lambda, y = cost_premium, color = member)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Adult male"   = "#2C3E6B",
    "Adult female" = "#C0392B",
    "Female child" = "#27AE60"
  )) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 1), "%")) +
  labs(
    title  = "Cost premium by λ",
    x      = expression(lambda),
    y      = "Cost premium (%)",
    color  = "Household member"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11),
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )

p3 <- ggplot(diversity,
             aes(x = lambda, y = n_foods,
                 color = paste0(Sex, "_", Demo_Group))) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c("0_[31,51)"  = "#2C3E6B",
               "1_[31,51)"  = "#C0392B",
               "1_[10, 14)" = "#27AE60"),
    labels = c("Adult male", "Adult female", "Female child")
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Number of foods selected by λ",
    x     = expression(lambda),
    y     = "Foods in diet",
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    text             = element_text(family = "serif"),
    plot.title       = element_text(face = "bold", size = 11),
    legend.position  = "none",
    panel.grid.minor = element_blank()
  )

library(patchwork)
fig <- p1 / (p2 | p3) +
  plot_annotation(
    title = "Multi_CoNA_paper: reproducible test results",
    theme = theme(
      plot.title = element_text(family = "serif", face = "bold", size = 13)
    )
  )

print(fig)

cat("\nAll tests completed successfully.\n")
