library(tidymodels)
library(mlbench)
data(Ionosphere)
Ionosphere <- Ionosphere %>% select(-V1, -V2)
# model
svm_mod <-
    svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
    set_mode("classification") %>%
    set_engine("kernlab")
# recipe
iono_rec <-
    recipe(Class ~ ., data = Ionosphere) %>%
    # remove any zero variance predictors
    step_zv(all_predictors()) %>%
    # remove any linear combinations
    step_lincomb(all_numeric())
# resampling
set.seed(4943)
iono_rs <- bootstraps(Ionosphere, times = 30)
# optional
roc_vals <- metric_set(roc_auc)
ctrl <- control_grid(verbose = FALSE, save_pred = TRUE)
# execute with formula
set.seed(35)
(
    formula_res <-
        svm_mod %>%
        tune_grid(
            Class ~ .,
            resamples = iono_rs,
            metrics = roc_vals,
            control = ctrl
        )
)
# look at metrics
formula_res %>%
	select(.metrics) %>%
	slice(1) %>%
	pull(1)
# estimates
(estimates <- collect_metrics(formula_res))
## top 5
show_best(formula_res, metric = "roc_auc")
## best
select_best(formula_res)
