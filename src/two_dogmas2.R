library(tidyverse)
library(tidymodels)
library(finetune)
library(corrr)
requireNamespace("lme4")
requireNamespace("xgboost")

# DATA SIM ####
n_obs <- 1000

set.seed(1234)
d.1 <- 
  tibble(
    A = rnorm(n_obs, 0, 1),
    B = rnorm(n_obs, 0, 1),
    # C = rnorm(n_obs),
    # D = rnorm(n_obs),
    epsilon = rnorm(n_obs),
    y = 1*A + 3*B + 6*epsilon
    # y = 1*A + 3*B^2 + 3*epsilon
  )
glimpse(d.1)
summary(d.1$y)
d.1 %>% 
  mutate(AB = A*B) %>% 
  correlate() %>% 
  rplot(print_cor = TRUE)

d.1 %>% 
  pivot_longer(!y) %>% 
  ggplot(aes(x = value, y = y)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ name)


# DATA SPLIT ####
set.seed(1234)
splits <- initial_split(d.1, prop = .8, strata = y)
train <- training(splits)
test <- testing(splits)
folds <- vfold_cv(train, v = 10)


# RECIPES ####
basic_rec <- 
  train %>% 
  recipe(y ~ .) %>% 
  step_rm(epsilon)


# PARSNIP MODELS ####
boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

linear_reg_lm_spec <-
  linear_reg() %>%
  set_engine('lm')

rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('ranger') %>%
  set_mode('regression')


# WORKFLOWS ####
wfs <- 
  workflow_set(
    preproc = list(basic = basic_rec),
    models = list(lm = linear_reg_lm_spec, rf = rand_forest_ranger_spec, xgb = boost_tree_xgboost_spec)
  )
wfs

# Bind all workflow sets together
# wfs_all <- 
#    bind_rows(
#      wfs_null,
#      wfs_bsv,
#      wfs_basic,
#      wfs_normalized,
#      wfs_features
#    )
# wfs_all



# TUNE ####
library(doMC)
parallel::detectCores(logical = FALSE); parallel::detectCores(logical = TRUE)
registerDoMC(cores = 12)
# registerDoSEQ() #reset to serial processing

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

full_results_time <- 
  system.time(
    race_results.0 <-
      wfs %>%
      workflow_map(
        "tune_race_anova",
        seed = 1503,
        resamples = folds,
        grid = 20,
        control = race_ctrl,
        verbose = TRUE
      )
  )
full_results_time

# keep_pred <- 
#   control_grid(
#     save_pred = TRUE,
#     parallel_over = NULL
#   )
# 
# tune_results <- 
#   wfs %>% 
#   workflow_map(
#     "tune_grid", 
#     # Options to `workflow_map()`: 
#     grid = 5, seed = 1101, verbose = TRUE,
#     # Options to `tune_grid()`: 
#     resamples = folds, metrics = metric_set(rmse, rsq), control = keep_pred,
#   )
# 
# tune_results
# autoplot(tune_results)
# 
# tune_results$result %>% 
#   map_df(show_best, metric = "rsq", n = 1) %>% 
#   View()
#   #map(collect_metrics, metric = "rsq")
# 
# tune_results %>% 
#   workflow_map(select_best)
#   map(select_best, metric = "rsq", n = 1) 
#   autoplot()
#   #map(collect_metrics, metric = "rsq")




# SCREENING ####
# Which of these models are worth carrying forward for refinement?
race_results.0

# Drop any workflows that errored
race_results.1 <- 
  race_results.0 %>% 
  filter(map_int(result, length) > 0) %>% 
  mutate(cl = as.character(map(result, class))) %>% 
  filter(cl != "try-error")

# RMSE & R2 for best config of each workflow
race_results.1 %>% 
  rank_results(select_best = TRUE) %>% 
  select(rank, wflow_id, .config, rank, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean)

# Autoplot fits for best configs
race_results.1 %>% 
  autoplot(
    rank_metric = "rmse",  
    metric = c("rsq", "rmse"),       
    select_best = TRUE    
  ) +
  ggrepel::geom_label_repel(aes(label = wflow_id), size = 3, fill = "white") +
  #geom_label(aes(label = wflow_id), size = 2) +
  scale_color_viridis_d(end = .6, option = "C") +
  ggtitle("RMSE & RSq", subtitle = "95% CIs; Best Fit of each Model") +
  theme_bw() +
  theme(legend.position = "none")



# Obs/pred plots for best configs
best_fits <- 
  race_results.1 %>% 
  rank_results(rank_metric = "rmse", select_best = TRUE) %>% 
  unite(model_id, wflow_id, .config) %>% 
  pull(model_id) %>% 
  unique()

# Collect preds for best fits & mutate on obs/pred cols with raw outcome units
best_preds <- 
  collect_predictions(race_results.1, summarize = TRUE) %>% 
  unite(model_id, wflow_id, .config, remove = FALSE) %>% 
  filter(model_id %in% best_fits) %>% 
  #mutate(across(c(sales.newsales.MA7, .pred), ~ InvBoxCox(.x, bc_lambda), .names = "{.col}_raw")) %>% 
  #mutate(across(c(sales.newsales.MA7, .pred), ~ 10^.x, .names = "{.col}_raw")) %>%   
  mutate(
    diff = .pred - y
    # diff_raw = .pred_raw - y
  )

# Print out metrics for best fits (log & raw units)
best_preds %>%   
  group_by(model_id) %>% 
  summarize(
    rmse = sqrt(mean(diff^2)),
    mae = mean(abs(diff)),
    mean_diff = mean(diff),
    
    # rmse_raw = sqrt(mean(diff_raw^2)),
    # mae_raw = mean(abs(diff_raw)),
    # mean_diff_raw = mean(diff_raw)
    
  ) %>% 
  arrange(rmse)

# Obs/pred coord plots -- choose transformed or raw units
best_preds %>% 
  #filter(wflow_id == "basic_Cubist") %>% 
  #ggplot(aes(x = sales.newsales.MA7_raw, y = .pred_raw)) +
  mutate(wflow_id = factor(wflow_id, levels = gsub("_Preprocessor.*$", "", best_fits), ordered = TRUE)) %>% 
  # filter(across(c(sales.newsales.MA7, .pred), ~ .x > 0)) %>% 
  ggplot(aes(x = y, y = .pred)) +  
  geom_abline(lty = 2, alpha = 1, color = "black") +   
  geom_point(alpha = .25, color = "blue") +
  coord_obs_pred() +
  scale_color_viridis_d() +
  facet_wrap(~ wflow_id) +
  ggtitle("Obs vs. Predicted", subtitle = "Best fit of each model on training data") +
  theme_bw()

# # Resid histograms by model -- choose transformed or raw units
# best_preds %>% 
#   # filter(across(c(sales.newsales.MA7, .pred), ~ .x > 0)) %>% 
#   ggplot(aes(x = diff)) +
#   geom_histogram() +
#   facet_wrap(~ wflow_id) +
#   ggdark::dark_theme_bw()
# best_preds %>% 
#   # filter(across(c(sales.newsales.MA7, .pred), ~ .x > 0)) %>% 
#   ggplot(aes(x = diff, color = wflow_id)) +
#   geom_density() +
#   scale_color_viridis_d() +  
#   ggtitle("Residual Distributions by Model") +
#   ggdark::dark_theme_bw()

best_preds %>% 
  # filter(across(c(sales.newsales.MA7, .pred), ~ .x > 0)) %>% 
  mutate(wflow_id = factor(wflow_id, levels = rev(gsub("_Preprocessor.*$", "", best_fits)), ordered = TRUE)) %>%   
  ggplot(aes(x = diff, y = wflow_id, fill = wflow_id)) +
  ggridges::geom_density_ridges(alpha = .75, color = "darkgray") +
  xlab("Residual") +
  ylab("") +
  scale_fill_viridis_d(end = .8, option = "D") +  
  ggtitle("Residual Distributions by Model") + 
  theme_bw()




# # Obs/pred timelines
# best_preds %>% 
#   mutate(wflow_id = factor(wflow_id, levels = gsub("_Preprocessor.*$", "", best_fits), ordered = TRUE)) %>%     
#   select(wflow_id, .row, y, .pred) %>%   
#   #select(wflow_id, .row, sales.newsales.MA7_raw, .pred_raw) %>%     
#   pivot_longer(!c(wflow_id, .row)) %>% 
#   ggplot(aes(x = .row, y = value, color = name, size = name, alpha = name)) +
#   geom_line() +
#   #scale_color_viridis_d(begin = 0.5, option = "C") +  
#   scale_color_manual(values = c("green", "white")) +
#   scale_size_manual(values = c(.5, 1.5)) +
#   scale_alpha_manual(values = c(.8, .3)) +
#   facet_wrap(~ wflow_id) +  
#   ggtitle("Observed vs. Predicted Timeseries", subtitle = "Full training data") +
#   ggdark::dark_theme_bw()




# OPTIMIZE ####
# Select subset of most promising models for tweaking & finalization

# Look at obs/pred plot outliers: anything special about those days that could indicate additional features?
wf_nm <- "basic_Cubist"
best_results <- 
  race_results.1 %>% 
  extract_workflow_set_result(wf_nm) %>% 
  select_best(metric = "rmse")
best_results

best_results_fit <- 
  race_results.1 %>% 
  extract_workflow(wf_nm) %>% 
  finalize_workflow(best_results) %>% 
  #last_fit(split = aoa_split)  
  fit(aoa_train)

resids <- 
  predict(best_results_fit, new_data = aoa_train) %>% 
  mutate(
    dim_date = aoa_train$dim_date,
    .obs = aoa_train$sales.newsales.MA7,
    across(c(.pred, .obs), ~ InvBoxCox(.x, bc_lambda), .names = "{.col}_raw"),
    delta = .pred - .obs,
    delta_raw = .pred_raw - .obs_raw,
    reldiff_raw = scales::percent(delta_raw / .obs_raw, accuracy = 1)
  ) %>% 
  select(dim_date, everything()) %>% 
  arrange(desc(abs(delta_raw)))

# What predicts the (abs) residuals? Fit a lm and look at coeffs?
all.resid <- 
  aoa_train %>% 
  dplyr::left_join(
    resids %>% 
      select(dim_date, delta)
  ) %>% 
  mutate(delta = abs(delta))

resid_rec <- 
  recipe(delta ~ ., data = all.resid) %>% 
  # step_nzv(all_numeric_predictors()) %>%   
  step_rm(sales.newsales.MA7) %>% 
  step_holiday(dim_date) %>%
  step_date(dim_date, features = c("dow", "month")) %>%   
  # remove date from the list of predictors  
  update_role(dim_date, new_role = "id") %>%   
  step_dummy(all_nominal_predictors()) %>% 
  step_naomit(all_predictors())
resid_fit <- 
  workflow() %>%
  add_recipe(resid_rec) %>%
  add_model(lm_spec) %>%
  fit(all.resid)
tidy(resid_fit) %>% 
  ggplot(aes(x = reorder(term, p.value), y = p.value)) +
  geom_point() +
  geom_hline(yintercept = 0.05) +
  coord_flip() +
  ggdark::dark_theme_bw()

# Scatterplot each of these sig predictors against the outcome?
resid_preds <- 
  tidy(resid_fit) %>% 
  arrange(p.value) %>% 
  filter(p.value < .05) %>% 
  pull(p.value, term)
resid_preds

prep(resid_rec, all.resid) %>% 
  bake(new_data = all.resid) %>% 
  select(dim_date, !!! names(resid_preds)) %>% 
  dplyr::left_join(
    all.resid %>% select(dim_date, sales.newsales.MA7)
  ) %>% 
  pivot_longer(!c(dim_date, sales.newsales.MA7)) %>% 
  ggplot(aes(x = value, y = sales.newsales.MA7)) +
  geom_point(color = "green", alpha = .3) +
  geom_smooth(method = "loess", span = .5, se = FALSE, color = "white") +
  facet_wrap(~ name, scales = "free") +
  ggdark::dark_theme_bw()

# maybe... combine NYDay and ChristmasDay into the same feature?
#          or broaden out "holiday" dates to a couple days pre/post?
# for nonlinear relationships with residuals, maybe a spline transform would
# help improve the feature?



# best_results_fit %>% 
#   #collect_metrics()
#   collect_predictions()



# Look at fit for the best config of a specific workflow
wf_nm <- "basic_lm"
race_results.1 %>% 
  pull_workflow_set_result(wf_nm) %>% 
  autoplot() +
  geom_line() +
  ggdark::dark_theme_bw()

race_results.1 %>% 
  pull_workflow_set_result(wf_nm) %>% 
  show_best("rmse")

race_results.1 %>% 
  pull_workflow_set_result(wf_nm) %>% 
  show_best("rmse")

res <-
  workflow() %>%
  add_recipe(basic_rec) %>%
  add_model(lm_spec) %>%
  fit(aoa_train)
tidy(res) %>% 
  ggplot(aes(x = reorder(term, p.value), y = p.value)) +
  geom_point() +
  geom_hline(yintercept = 0.05) +
  coord_flip() +
  ggdark::dark_theme_bw()



# BAKE-OFF ####
# Compare performance of top models from race tuning on holdout data
# https://www.tmwr.org/workflow-sets.html#finalizing-a-model

# > The first step is to pick a workflow to finalize. Since the boosted tree model
# > worked well, we’ll extract that from the set, update the parameters with the
# > numerically best settings, and fit to the training set:

best_algos <- c("basic_boosting", "basic_Cubist", "normalized_neural_network", "basic_MARS", "basic_lm")
best_algos_train_results <- 
  best_algos %>% 
  map(
    ~ race_results.1 %>% 
      extract_workflow_set_result(.x) %>% 
      select_best(metric = "rmse")
  ) %>% 
  set_names(best_algos)
best_algos_train_results

# Assess fits for fully-trained models on holdout data
best_algos_test_results <- 
  best_algos_train_results %>% 
  imap(
    ~ race_results.1 %>% 
      extract_workflow(.y) %>% 
      # Use best hyperparameters from tuning & refit on entire training set
      finalize_workflow(.x) %>% 
      # Fit to holdout data
      last_fit(split = aoa_split)
  )
best_algos_test_results

# These are the performance metrics for holdout data alone
best_algos_test_results %>% map(collect_metrics)
best_algos_test_results %>% 
  map_dfr(collect_metrics, .id = "algo") %>% 
  ggplot(aes(x = algo, y = .estimate)) +
  #geom_bar(stat = "identity") +
  geom_point(alpha = 1, color = "lightgreen") +
  ggrepel::geom_label_repel(aes(label = round(.estimate, 2)), size = 3, alpha = .8, fill = "black", color = "white") +
  coord_flip() +
  facet_grid(~ .metric, scales = "free_x") +
  ggtitle("HOLDOUT DATA: Performance Metrics", subtitle = "Best Fit of each Model") +  
  ggdark::dark_theme_bw()

best_algos_test_results %>% 
  map_dfr(collect_predictions, .id = "algo") %>% 
  ggplot(aes(x = sales.newsales.MA7, y = .pred)) +  
  geom_abline(lty = 2, alpha = .5) +   
  geom_point(alpha = .25, color = "lightgreen") +
  coord_obs_pred() +
  scale_color_viridis_d() +
  facet_wrap(~ algo) +
  ggtitle("HOLDOUT DATA: Obs vs. Predicted", subtitle = "Best Fit of each Model") +
  ggdark::dark_theme_bw()

best_algos_test_results %>% 
  map_dfr(collect_predictions, .id = "algo") %>% 
  select(algo, .row, sales.newsales.MA7, .pred) %>%   
  pivot_longer(!c(algo, .row)) %>% 
  ggplot(aes(x = .row, y = value, color = name, size = name, alpha = name)) +
  geom_line() +
  #scale_color_viridis_d(begin = 0.5, option = "C") +  
  scale_color_manual(values = c("green", "white")) +
  scale_size_manual(values = c(.5, 1.5)) +
  scale_alpha_manual(values = c(.8, .3)) +
  facet_wrap(~ algo) +  
  ggtitle("HOLDOUT DATA: Observed vs. Predicted Timeseries", subtitle = "Best Fit of each Model") +
  ggdark::dark_theme_bw()



# Just for fun: check out demand score cross-correlations with predictors
# tmp <- 
#   all.4 %>% 
#   dplyr::left_join(
#     tibble(
#       dim_date = aoa_split$data$dim_date,
#       demand_score = 
#         predict(selected_model, aoa_split$data) %>% 
#         InvBoxCox(lambda = bc_lambda) %>% 
#         pull(.pred)
#     ),
#     by = "dim_date"
#   ) %>% 
#   filter(!is.na(demand_score))  
# 
# tmp %>% 
#   select(demand_score, starts_with("phd.")) %>% 
#   rename_with(~ gsub("phd.", "", .x, fixed = TRUE)) %>% 
#   #select(-input$index) %>% 
#   map_dfr(~ broom::tidy(ccf(.x, tmp$demand_score, lag.max = 14, plot = FALSE)), .id = ".id") %>% 
#   filter(lag <= 0) %>% 
#   ggplot(aes(x = lag, y = acf, fill = factor(acf < 0))) +
#   geom_bar(stat = "identity", alpha = .7) +
#   geom_text(aes(label = ifelse(abs(acf) >= .3, round(acf, 2), "")), size = 3, color = "white", vjust = -1) +
#   scale_x_continuous(breaks = -100:100) +
#   scale_y_continuous("ccf", limits = c(NA, 1.5)) +
#   scale_fill_manual(values = c("green", "red")) +
#   facet_grid(.id ~ .) +
#   ggdark::dark_theme_bw() +
#   theme(legend.position = "none")  







# SERIALIZE MODEL ARTIFACT ####
selected_model <- best_algos_test_results$basic_Cubist$.workflow[[1]]
selected_model
predict(selected_model, aoa_test) %>% 
  # Need to InvBoxCox the prediction to get back to raw outcome units  
  InvBoxCox(lambda = bc_lambda)

freeze(selected_model)
freeze(bc_lambda)

# For production, need to save these artifacts in a versioned location
# so they are accessible via GL -- ultimately a proper model registry would be the
# right solution here, rather than just saving RDS files to GL
saveRDS(selected_model, GLOBALS$RDS_MODEL)
saveRDS(bc_lambda, GLOBALS$RDS_BCLAMBDA)





# TEST DATA ####
