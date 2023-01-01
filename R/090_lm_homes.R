library(dplyr)
library(tidymodels)
#import
raw_data <- readRDS("./sarasota_prices.rds")
#clean
homes <-
	raw_data |>
	filter(sale_date >= max(sale_date)- 365) |>
	select(account,
	       address1,
	       stcd:lat
	) |>
	mutate(nghb = as.character(nghb)) |>
	mutate(sale_amt = log10(sale_amt)) |>
	mutate(bedr = as.numeric(as.character(bedr))) |>
	mutate(bath = as.numeric(as.character(bath))) |>
	na.omit()

# split data
set.seed(1)
homes_split <- initial_split(homes, prop = 0.80, strata = sale_amt)
homes_train <- training(homes_split)
homes_test  <-  testing(homes_split)

#recipe
formula <- recipe(sale_amt ~ ., data = homes_train)

#preprocess
recipe <-
	formula |>
	update_role(account, address1, new_role = "id") |>
	step_rm(qual_code) |>
	step_log(all_numeric_predictors()) |>
	step_dummy(all_nominal_predictors()) |>
	step_string2factor(all_nominal_predictors()) |>
	step_nzv(all_predictors())

summary(recipe) |> print(n = Inf)

# create model
lm_model <-
	linear_reg() |>
	set_engine("lm")
# workflow
lm_wflow <-
	workflow() %>%
	add_model(lm_model) %>%
	add_recipe(recipe)
# fit
lm_fit <- fit(lm_wflow, homes_train)
# test
homes_test_res <-
	predict(lm_fit, new_data = homes_test %>% select(-sale_amt))
homes_test_res <- bind_cols(
	homes_test_res,
	homes_test |> select(sale_amt))
homes_test_res |>
	ggplot() +
	aes(sale_amt, .pred) +
	geom_point(alpha = .5) +
	geom_abline(lty = 2) +
	labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
	coord_obs_pred()
homes_metrics <- metric_set(rmse, rsq, mae)
homes_metrics(homes_test_res, truth = sale_amt, estimate = .pred)
