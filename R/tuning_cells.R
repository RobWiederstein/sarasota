library(tidymodels)
library(rpart.plot) # for visualizing a decision tree
library(vip) # for variable importance plots
data(cells, package = "modeldata")
cells

set.seed(123)
cell_split <- initial_split(
    cells %>% select(-case),
    strata = class
)
cell_train <- training(cell_split)
cell_test <- testing(cell_split)
(
    tune_spec <-
        decision_tree(
            cost_complexity = tune(),
            tree_depth = tune()
        ) %>%
        set_engine("rpart") %>%
        set_mode("classification")
)
# set up grid
(
    tree_grid <- grid_regular(
        cost_complexity(),
        tree_depth(),
        levels = 5
    )
)
tree_grid %>%
    count(tree_depth)
#resamples
set.seed(345)
(
    cell_folds <- vfold_cv(cell_train)
)
# workflow 1
(
    tree_wf <- workflow() %>%
        add_model(tune_spec) %>%
        add_formula(class ~ .)
)
#add tune grid to workflow "tree resampled"
(
    tree_res <-
        tree_wf %>%
        tune_grid(
            resamples = cell_folds,
            grid = tree_grid
        )
)

tree_res %>%
    collect_metrics() |>
    print(n = Inf)

tree_res %>%
    collect_metrics() %>%
    mutate(tree_depth = factor(tree_depth)) %>%
    ggplot(aes(cost_complexity, mean, color = tree_depth)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10(labels = scales::label_number()) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0)

# Top 5 best combos of cost_complexity & tree_depth
tree_res %>%
    show_best("accuracy")
# Best combo for accuracy
(
    best_tree <- tree_res %>%
        select_best("accuracy")
)
# final workflow
(
    final_wf <-
        tree_wf %>%
        finalize_workflow(best_tree)
)
# Last fit
final_fit <-
	final_wf %>%
	last_fit(cell_split)

final_fit %>%
	collect_metrics()
# plot
final_fit %>%
	collect_predictions() %>%
	roc_curve(class, .pred_PS) %>%
	autoplot()
#The final_fit object contains a finalized, fitted workflow
#that you can use for predicting on new data or further
#understanding the results. You may want to extract this
#object, using one of the extract_ helper functions.
final_tree <- extract_workflow(final_fit)
final_tree
# plot decision tree
final_tree %>%
	extract_fit_engine() %>%
	rpart.plot(roundint = FALSE)
# We can use the vip package to estimate variable
# importance based on the model’s structure.
final_tree %>%
	extract_fit_parsnip() %>%
	vip()
