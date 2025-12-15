# Load packages
library(tidyverse)
library(readxl)
library(seasonal)
library(Metrics)
library(ggplot2)

# Set working directory
setwd("C:/Users/Portatil/Desktop/Least-cost-diets-and-affordability/Proyecto Interno")

# Load dataset
infile <- "working-papers/working-paper-1225/mapeo ipc-sipsa/output/121225_dataset_ipc_sipsa.xlsx"
dataset <- read_excel(infile)

# Select food index
i <- 4
food.vector <- unique(dataset$alimento_sipsa)
food.x <- food.vector[i]

# Filter and preprocess data
data.food <- dataset %>%
  filter(alimento_sipsa == food.x) %>%
  drop_na(precio_ipc, precio_sipsa) %>%
  mutate(
    log_ipc   = log(precio_ipc),
    log_sipsa = log(precio_sipsa),
    fecha     = as.Date(paste(Year, Month, 1, sep = "-"))
  )

# Create time series
ts_ipc   <- ts(data.food$log_ipc, start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)
ts_sipsa <- ts(data.food$log_sipsa, start = c(data.food$Year[1], data.food$Month[1]), frequency = 12)

# Seasonal adjustment
sa_ipc   <- as.numeric(predict(seas(ts_ipc)))
sa_sipsa <- as.numeric(predict(seas(ts_sipsa)))

data.food <- data.food %>%
  mutate(
    sa_ipc   = sa_ipc,
    sa_sipsa = sa_sipsa
  )

# Train/test split (70/30)
n <- nrow(data.food)
cut <- floor(0.7 * n)
train <- data.food[1:cut, ]
test  <- data.food[(cut + 1):n, ]

# Fit linear regression model
model <- lm(sa_ipc ~ sa_sipsa, data = train)
summary(model)

# Predict
test$y_hat <- predict(model, newdata = test)

# Metrics
rmse_val <- rmse(test$sa_ipc, test$y_hat)
mape_val <- mape(exp(test$sa_ipc), exp(test$y_hat))  # Compare in levels

cat("\nRMSE (log):", round(rmse_val, 4))
cat("\nMAPE (levels):", round(1 * mape_val, 2), "%\n")

# Predict with confidence intervals
preds <- predict(model, newdata = test, interval = "confidence", level = 0.95)
test <- test %>%
  mutate(
    fit = preds[, "fit"],
    lwr = preds[, "lwr"],
    upr = preds[, "upr"]
  )

# Convert from log to level
plot_df <- test %>%
  mutate(
    obs_level = exp(sa_ipc),
    fit_level = exp(fit),
    lwr_level = exp(lwr),
    upr_level = exp(upr)
  )

# Combine with train for full timeline
train_df <- train %>%
  mutate(
    obs_level = exp(sa_ipc),
    fit_level = NA,
    lwr_level = NA,
    upr_level = NA
  )

plot_all <- bind_rows(train_df, plot_df)

# Plot with confidence ribbon
ggplot(plot_all, aes(x = fecha)) +
  geom_line(aes(y = obs_level, color = "Observado"), linewidth = 0.7) +
  geom_line(aes(y = fit_level, color = "Predicho"), linetype = "dashed", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lwr_level, ymax = upr_level), fill = "blue", alpha = 0.2) +
  scale_color_manual(values = c("Observado" = "black", "Predicho" = "blue")) +
  labs(
    title = paste("Modelo M6 con IC: log(Pmin) ~ log(Pmay)"),
    subtitle = paste0(food.x, " | IC 95% | MAPE (niveles): ", round(100 * mape_val, 2), "%"),
    x = "Fecha",
    y = "Precio (nivel)",
    color = ""
  ) +
  theme_minimal()
