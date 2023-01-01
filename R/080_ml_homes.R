library(dplyr)
library(tidymodels)
library(dotwhisker)
library(ranger)
library(plotly)
library(xgboost)
library(rsample)
sarasota_prices <- readRDS("./sarasota_prices.rds")

homes <-
	sarasota_prices |>
	filter(sale_date >= max(sale_date)- 365) |>
	select(account,
	       address1,
	       stcd:lat
	       ) |>
	select(-qual_code) |>
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

# recipe
homes_rec <-
	recipe(sale_amt ~
	       	age_now +
	       	living +
	       	lsqft +
	       	lat +
	       	lng +
	       	water +
	       	bath +
	       	land_use +
	       	municipality +
	       	account +
	       	address1,
	       	data = homes_train) |>
	update_role(account, address1, new_role = "id") |>
	step_dummy(all_nominal_predictors()) |>
	step_normalize(all_numeric_predictors()) |>
	step_other(all_nominal(), threshold = 0.01) |>
	recipes::step_nzv(all_nominal()) |>
	prep()


# linear model
lm_model <- linear_reg() |> set_engine("lm") |> translate()

lm_wflow <-
	workflow() |>
	add_model(lm_model) |>
	add_recipe(homes_rec)

lm_fit <-
	lm_wflow |>
	fit(data = homes_train)

#rf model
cores <- parallel::detectCores()
rf_model <-
	rand_forest(mtry = 10, min_n = 10, trees = 1000) |>
	set_engine("ranger", num.threads = cores) |>
	set_mode("regression")
rf_wflow <-
	workflow() %>%
	add_formula(
		sale_amt ~
		age_now +
		living +
		lsqft +
		lat +
		lng +
		water +
		bath +
		land_use +
		municipality +
		account +
		address1) %>%
	add_model(rf_model)

rf_fit <-
	rf_wflow |> fit(data = homes_train)
# create workflow set
library(tidymodels)
tidymodels_prefer()
#
preproc <-
	list(basic = homes_rec
	)
lm_models <- workflow_set(preproc, list(lm = lm_model), cross = FALSE)



two_models <-
	as_workflow_set(random_forest = rf_res) %>%
	bind_rows(lm_models)

#cross validation
set.seed(1001)
homes_folds <- vfold_cv(homes_train, v = 10)

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(1003)
rf_res <- rf_wflow %>% fit_resamples(resamples = homes_folds, control = keep_pred)



#function
estimate_perf <- function(model, dat) {
	# Capture the names of the `model` and `dat` objects
	cl <- match.call()
	obj_name <- as.character(cl$model)
	data_name <- as.character(cl$dat)
	data_name <- gsub("homes_", "", data_name)

	# Estimate these metrics:
	reg_metrics <- metric_set(rmse, rsq)

	model %>%
		predict(dat) %>%
		bind_cols(dat %>% select(sale_amt)) %>%
		reg_metrics(sale_amt, .pred) %>%
		select(-.estimator) %>%
		mutate(object = obj_name, data = data_name)
}
estimate_perf(rf_fit, homes_train)
estimate_perf(lm_fit, homes_train)
#inspect
results <-
	homes_fit %>%
	extract_fit_parsnip() %>%
	tidy() |>
	arrange(desc(estimate))
#plot
results |>
	dwplot(dot_args = list(size = 2, color = "black"),
	       whisker_args = list(color = "black"),
	       vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))
#predict
homes_test_res <- predict(homes_fit, new_data = homes_test %>% select(-sale_amt))
homes_test_res <- bind_cols(homes_test_res, homes_test %>% select(sale_amt))

#plot
ggplot(homes_test_res, aes(x = sale_amt, y = .pred)) +
	# Create a diagonal line:
	geom_abline(lty = 2) +
	geom_point(alpha = 0.5) +
	labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
	# Scale and size the x- and y-axis uniformly:
	coord_obs_pred()

#Rmse
rmse(homes_test_res, truth = sale_amt, estimate = .pred)
(homes_metrics <- metric_set(rmse, rsq, mae))
homes_metrics(homes_test_res, truth = sale_amt, estimate = .pred)
