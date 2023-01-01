# 1.0 Load Prerequisites ----
## 1.1 libraries ----
library(forcats)
library(dplyr)
library(tidymodels)
library(randomForest)
## 1.2  import data ----
raw_data <- readRDS("./sarasota_prices.rds")
## 1.3  clean data ----
homes <-
    raw_data |>
    filter(sale_date >= max(sale_date) - 365) |>
    select(
        account,
        address1,
        stcd:lat
    ) |>
    mutate(nghb = as.character(nghb)) |>
    mutate(sale_amt = log10(sale_amt)) |>
    mutate(bedr = as.numeric(as.character(bedr))) |>
    mutate(bath = as.numeric(as.character(bath))) |>
    na.omit()
## 1.4 lump rare factor ----
homes <-
    homes |>
    mutate(nghb = factor(nghb) |> fct_lump_min(10)) |>
    mutate(txcd = factor(txcd) |> fct_lump_min(10)) |>
    mutate(subd = factor(subd) |> fct_lump_min(10))
## 1.5 omit vars ----
homes <-
    homes |>
    select(-usd_per_sq_ft) |>
    select(-sale_amt_infl_adj) |>
    select(-living) |>
    select(-yrbl) |>
    select(-age_now) |>
    select(-age_class) |>
    select(-qual_code) |>
    select(-home_owner) |>
    select(-year) |>
    select(-land_use) |>
    select(-halfbath) |>
    select(-water) |>
    select(-gulfbay) |>
    select(-stcd) |>
    select(-homestead) |>
    select(-subd) |>
    select(-nghb) |>
    select(-bath) |>
    select(-municipality) |>
    select(-bedr) |>
    select(-zoning_1) |>
    select(-txcd)

# 2.0 Split Data ----
set.seed(1)
homes_split <- initial_split(homes, prop = 0.80, strata = sale_amt)
homes_train <- training(homes_split)
homes_test <- testing(homes_split)

# 3.0 Create Recipe ----

## 3.1 formula ----
(
    formula <- recipe(sale_amt ~ ., data = homes_train)
)

## 3.2  preprocess ----
(
    recipe <-
        formula |>
        update_role(
            account,
            address1,
            new_role = "ID"
        )
)
summary(recipe)
# 4.0 Declare Model Untuned ----
(
    rf_model_untuned <-
        rand_forest() |>
        set_engine("ranger") |>
        set_mode("regression")
)

# 5.0 Untuned Model ----
## 5.1 Workflow ----
(
    rf_wflow <-
        workflow() |>
        add_model(rf_model_untuned) |>
        add_recipe(recipe)
)
## 5.2 Fit training data ----
set.seed(1)
rf_fit <- fit(rf_wflow, homes_train)

## 5.3 Predict test data ----
untuned_results <-
    homes_test |>
    select(sale_amt) |>
    bind_cols(predict(rf_fit, new_data = homes_test))
## 5.4 Plot untuned results ----
untuned_results |>
    ggplot() +
    aes(sale_amt, .pred) +
    geom_point(alpha = .25) +
    geom_abline(lty = 2) +
    coord_obs_pred() +
    labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
    labs(title = "NOT Tuned Sarasota - Random Forest Results")
filename <- "./img/not_tuned_sarasota_rf_results.png"
ggsave(
    filename = filename,
    height = 5,
    width = 5,
    units = "in"
)
# 6.0 Tuned Model ----
## 6.1 Declare Model ----
(
    rf_model <-
        rand_forest(
            mtry = tune(),
            min_n = tune()
        ) |>
        set_engine("ranger") |>
        set_mode("regression")
)
## 6.2 Set unknown parameters ----
(rf_param <- extract_parameter_set_dials(rf_model))
(rf_param <- rf_param |> update(mtry = mtry(c(1, 16))))
# finalize(mtry(), homes)
## 6.3 Set Grid ----
(
    rf_grid <-
        grid_regular(
            rf_param,
            levels = 5
        )
)
## 6.4 Define folds ----
set.seed(1001)
homes_folds <- vfold_cv(homes_train, v = 10)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

## 6.5 Define workflow ----
rf_res <-
    workflow() |>
    add_model(rf_model) |>
    add_recipe(recipe) |>
    tune_grid(
        resamples = homes_folds,
        grid = rf_grid
    )

## 6.6 Plot tuning metrics ----
show_best(rf_res, metric = "rmse")
(estimates <- collect_metrics(rf_res))
estimates |>
    mutate(mtry = factor(mtry)) |>
    ggplot() +
    aes(min_n, mean, group = mtry, color = mtry) +
    geom_line(size = 1.5, alpha = .6) +
    geom_point(size = 2) +
    facet_wrap(~.metric, scale = "free", nrow = 2) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
    theme_bw() +
    labs(title = "RF Tuning Optimization")
filename <- "./img/rf_tuning_optimization.png"
ggsave(
    filename = filename,
    height = 5,
    width = 5,
    units = "in"
)
## 6.7 Fit tuned model----
### 6.71 update workflow ----
(best_tree <- rf_res |> select_best("rmse"))
(
    rf_wflow_final <-
        workflow() |>
        add_model(rf_model) |>
        add_recipe(recipe) |>
        finalize_workflow(best_tree)
)
### 6.72 final fit ----
final_fit <-
    rf_wflow_final |>
    last_fit(homes_split)
### 6.73 goodness of fit ----
final_fit |>
    collect_metrics()
### 6.74 final tree ----
(final_tree <- extract_workflow(final_fit))
## 6.8 Predict tuned model ----
homes_test_res <-
    predict(final_tree, new_data = homes_test |> select(-sale_amt))
## 6.9 Retrieve results ---
homes_metrics <- metric_set(rmse, rsq)
homes_test_res <- bind_cols(
    homes_test_res,
    homes_test |> select(sale_amt)
)
homes_metrics(homes_test_res, truth = sale_amt, estimate = .pred)
## 6.91 Plot results ----
(
    p <-
        homes_test_res |>
        ggplot() +
        aes(sale_amt, .pred) +
        geom_point(alpha = .25) +
        geom_abline(lty = 2) +
        labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
        coord_obs_pred() +
        labs(title = "Tuned Sarasota - Random Forest Results") +
        annotate(
            "text",
            x = 5.75,
            y = 5,
            label = paste0("R Squared = .835\nRMSE = .0684")
        )
)
filename <- "./img/tuned_sarasota_rf_results.png"
ggsave(
    filename = filename,
    height = 5,
    width = 5,
    units = "in"
)
# 7.0 Variable Importance ----
library(DALEXtra)
explainer_rf <-
    explain_tidymodels(
        rf_fit,
        data = homes_train,
        y = homes_train$sale_amt,
        label = "random forest",
        verbose = FALSE
    )
set.seed(123)
vip_rf <- model_parts(explainer_rf, loss_function = loss_root_mean_square)
ggplot_imp <- function(...) {
    obj <- list(...)
    metric_name <- attr(obj[[1]], "loss_name")
    metric_lab <- paste(
        metric_name,
        "after permutations\n(higher indicates more important)"
    )

    full_vip <- bind_rows(obj) %>%
        filter(variable != "_baseline_")

    perm_vals <- full_vip %>%
        filter(variable == "_full_model_") %>%
        group_by(label) %>%
        summarise(dropout_loss = mean(dropout_loss))

    p <- full_vip %>%
        filter(variable != "_full_model_") %>%
        mutate(variable = fct_reorder(variable, dropout_loss)) %>%
        ggplot(aes(dropout_loss, variable))
    if (length(obj) > 1) {
        p <- p +
            facet_wrap(vars(label)) +
            geom_vline(
                data = perm_vals,
                aes(xintercept = dropout_loss, color = label),
                size = 1.4,
                lty = 2,
                alpha = 0.7
            ) +
            geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
    } else {
        p <- p +
            geom_vline(
                data = perm_vals,
                aes(xintercept = dropout_loss),
                size = 1.4,
                lty = 2,
                alpha = 0.7
            ) +
            geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    }
    p +
        theme(legend.position = "none") +
        labs(
            x = metric_lab,
            y = NULL,
            fill = NULL,
            color = NULL
        )
}
ggplot_imp(vip_rf)
filename <- "./img/variable_importance_plot.png"
ggsave(
    filename = filename,
    width = 5,
    height = 5,
    units = "in",
    dpi = 300
)
# 8.0 Example ----
saveRDS(final_tree, file = "./final_rand_forest_model.rds")
final_model <- readRDS("./final_rand_forest_model.rds")
## 8.1 Input ----
sale_amt = 280000
sale_date <- as.Date("2022-02-14")
grnd_area <- 1279
lsqft <- 6500
assd <- 27939
age_at_sale <- 69
lng <- 82.3
lat <- 27.1
new_data <- data.frame(
	sale_amt = sale_amt,
    sale_date = sale_date,
    grnd_area = grnd_area,
    lsqft = lsqft,
    assd = assd,
    age_at_sale = age_at_sale,
    lng = lng,
    lat = lat
)
## 8.2 Predict ----
new_data_res <-
    predict(
        final_model,
        new_data = new_data |> select(-sale_amt)
    )
10^new_data_res$.pred
