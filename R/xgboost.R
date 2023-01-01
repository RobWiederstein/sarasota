library(tidymodels)
library(xgboost)
tidymodels_prefer()
sarco_raw <- readRDS("./sarasota_prices.rds")
#process
sarco_proc <-
	sarco_raw |>
	filter(sale_date >= max(sale_date)- 180) |>
	select(account,
	       address1,
	       sale_amt,
	       sale_date,
	       water,
	       lsqft,
	       assd,
	       living,
	       yrbl,
	       lng,
	       lat,
	       bath
	) |>
	mutate(month = lubridate::month(sale_date)) |>
	na.omit()
# split data
set.seed(1)
homes_split <- initial_split(sarco_proc, prop = 0.80, strata = sale_amt)
homes_train <- training(homes_split)
homes_test  <-  testing(homes_split)

#recipe
(
	sarco_rec <-
	recipe(sale_amt ~ ., data = homes_train) |>
	update_role(c(address1, account), new_role = "id") |>
	step_dummy(all_nominal_predictors()) |>
	step_rm(sale_date)

)


# cross-validation
set.seed(1001)
homes_folds <- vfold_cv(homes_train, v = 10, repeats = 5)
# set model
xgb_spec <-
	boost_tree(
		trees = 1000,
		tree_depth = tune(),
		min_n = tune(),
		mtry = tune(),
		sample_size = tune(),
		learn_rate = tune()
	) |>
	set_engine("xgboost") |>
	set_mode("regression")
# workflow
xgb_homes_wf <- workflow(sarco_rec, xgb_spec)
# create grid
set.seed(123)
(
	xgb_grid <-
		grid_max_entropy(
			tree_depth(c(5L, 10L)),
			min_n(c(10L, 40L)),
			mtry(c(5L, 10L)),
			sample_prop(c(0.5, 1.0)),
			learn_rate(c(-2, -1)),
			size = 20
		)
)
# tune
library(finetune)
doParallel::registerDoParallel()
set.seed(234)
(
	xgb_homes_rs <-
	tune_race_anova(
		xgb_homes_wf,
		homes_folds,
		grid = xgb_grid,
		metrics = metric_set(rmse, rsq),
		control = control_race(verbose_elim = TRUE)
	)
)
# evaluate results
plot_race(xgb_homes_rs)
show_best(xgb_homes_rs, metric = "rsq")
# last fit
xgb_last <-
	xgb_homes_wf |>
	finalize_workflow(select_best(xgb_homes_rs, "rmse")) |>
	last_fit(homes_split)
xgb_last
# predict
collect_predictions(xgb_last) |>
	rsq(truth = sale_amt, estimate = .pred)
collect_predictions(xgb_last) |>
	rmse(truth = sale_amt, estimate = .pred)

home_res <- collect_predictions(xgb_last)
library(ggplot2)
library(plotly)
p <-
	homes_res |>
	ggplot() +
	aes(sale_amt, .pred) +
	geom_point(alpha = 0.5) +
	geom_abline(lty = 2)
p
ggplotly(p)

homes_metrics <- metric_set(rmse, rsq, mae)
homes_metrics(homes_res, truth = sale_amt, estimate = .pred)
