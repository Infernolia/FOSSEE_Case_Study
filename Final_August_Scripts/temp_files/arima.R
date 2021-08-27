# install.packages("xgboost")
# install.packages("tidymodels")
# install.packages("modeltime")
# install.packages("lubridate")
# install.packages("timetk")
# install.packages("rsample")
# install.packages("rminer")
# install.packages("parsnip")
# install.packages("magrittr")
# install.packages("workflows")
# install.packages("recipes")
# install.packages("earth")

library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(rsample)
library(rminer)
library(parsnip)
library(magrittr)
library(workflows)
library(recipes)
library(earth)

rm(list=ls())

df <- read.csv("all_merged_done.csv")  # Reading the number of cases
df <- subset(df,select = -c(1))
df$x <- 0
for(i in 1:nrow(df)) {  df$x[i] <- i}
df1 <-subset(df,select = c(74,1))
colnames(df1) <- c("Cases","Date")


df1 %<>%
  mutate(Date= as.Date(Date, format= "%Y-%m-%d"))
View(df1)

df1 %>%
  plot_time_series(Date, Cases)

splits <- initial_time_split(df1, prop = 0.9)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Cases ~ Date, data = training(splits))

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(Cases ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
      data = training(splits))


# Model 3: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Cases ~ Date, data = training(splits))

# Model 4: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Cases ~ Date, data = training(splits))


# Model 5: lm ----
model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(Cases ~ as.numeric(Date) + factor(month(Date, label = TRUE), ordered = FALSE),
      data = training(splits))


# Model 6: earth ----
model_spec_mars <- mars(mode = "regression") %>%
  set_engine("earth") 

recipe_spec <- recipe(Cases ~ Date, data = training(splits)) %>%
  step_date(Date, features = "month", ordinal = FALSE) %>%
  step_mutate(date_num = as.numeric(Date)) %>%
  step_normalize(date_num) %>%
  step_rm(Date)

wflw_fit_mars <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(model_spec_mars) %>%
  fit(training(splits))

models_tbl <- modeltime_table(
  model_fit_arima_no_boost,
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet,
  model_fit_lm,
  wflw_fit_mars
)

models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = df1
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens

  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(

  )



hist(df$total_cases)