library(tidyverse)
library(glue)
library(tidymodels)
library(finetune)
library(corrr)
library(patchwork)
requireNamespace("lme4")
requireNamespace("glmnet")
requireNamespace("xgboost")
requireNamespace("nnet")
requireNamespace("kernlab")

# DATA SIM ####
n_obs <- 1000


# > Linear-mechanism Data ####
set.seed(1234)
d.1 <- 
  tibble(
    A = rnorm(n_obs, 0, 1),
    B = rnorm(n_obs, 0, 1),
    # C = rnorm(n_obs),
    # D = rnorm(n_obs),
    epsilon = rnorm(n_obs),
    y = 2*A + 3*B + 2*epsilon
    # y = 1*A + 3*B + 6*epsilon
    # y = 1*A + 3*B + 2*A*B + 6*epsilon 
  )
data_type <- "linear mechanism"


# > Ensemble-mechanism Data ####
# rf.unsup <- randomForest::randomForest(x = d.1[, c("A", "B")])
# rf.unsup
# summary(rf.unsup)
# str(rf.unsup$proximity)
# randomForest::MDSplot(rf.unsup, )
set.seed(1234)
simple_tree <- function(A, B) {
  # ifelse(A > B, A, B)
  ifelse(
    A * runif(1) > B,
    ifelse(
      B > 0,
      B,
      A
    ),
    sqrt(A^2 + B^2)
  )
}

set.seed(1234)
n_ensembles <- 250
res <- vector(mode = "numeric", length = nrow(d.1))
for (i in 1:nrow(d.1)) {
  row <- vector(mode = "numeric", length = n_ensembles)  
  for (j in 1:n_ensembles) {
    row[j] <- simple_tree(d.1$A[i], d.1$B[i])
  }
  res[i] <- mean(row)
}
summary(res)
hist(res)

d.1 <- 
  d.1 %>% 
  mutate(
    y = res + epsilon
  )
data_type <- "ensemble mechanism"




# > Inspect data ####
glimpse(d.1)
summary(d.1$y)
d.1 %>% 
  mutate(AB = A*B) %>% 
  correlate() %>% 
  rplot(print_cor = TRUE)

d.1 %>% 
  pivot_longer(!y) %>% 
  ggplot(aes(x = value, y = y)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "lm") +
  facet_wrap(~ name)


# How does y~A relationship change by quartiles of B?
a_by_b <- function(data, a, b, y, probs = seq(0, 1, 0.25)) {
  data %>% 
    mutate("{{b}}" := cut({{b}}, breaks = quantile({{b}}, probs), labels = FALSE)) %>% 
    filter(!is.na({{b}})) %>% 
    ggplot(aes(x = {{a}}, y = {{y}})) +
    geom_point(alpha = .2) +
    geom_smooth(method = "lm") +
    facet_wrap(vars({{b}})) + 
    ggtitle(glue::glue("{substitute(y)} ~ {substitute(a)} by quantiles of {substitute(b)}"))
}
a_by_b(d.1, A, B, y) + a_by_b(d.1, B, A, y)


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

interact_rec <- 
  train %>% 
  recipe(y ~ .) %>% 
  step_rm(epsilon) %>% 
  step_interact(~ (. -y):(. -y))

# prep(basic_rec) %>%
#   bake(train)
# prep(interact_rec) %>%
#   bake(train)



# PARSNIP MODELS ####
null_spec <- 
  null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression")

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune(), stop_iter = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

linear_reg_lm_spec <-
  linear_reg() %>%
  set_engine("lm")

linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

mlp_nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>%
  set_engine("nnet") %>%
  set_mode("regression")

svm_rbf_kernlab_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")




# WORKFLOWS ####
wfs <- 
  workflow_set(
    preproc = 
      list(
        basic = basic_rec
        #interx = interact_rec
      ),
    models = 
      list(
        # null = null_spec,
        lm = linear_reg_lm_spec, 
        # glmnet = linear_reg_glmnet_spec,
        mlp = mlp_nnet_spec,
        svm = svm_rbf_kernlab_spec,
        rf = rand_forest_ranger_spec, 
        xgb = boost_tree_xgboost_spec
      )
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
registerDoMC(cores = parallel::detectCores(logical = TRUE) - 2)
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
        grid = 10,
        control = race_ctrl,
        verbose = TRUE
      )
  )
full_results_time



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
  ggrepel::geom_label_repel(aes(label = wflow_id), size = 3, fill = "white", alpha = .5) +
  #geom_label(aes(label = wflow_id), size = 2) +
  scale_color_viridis_d(end = .6, option = "C") +  
  ggtitle("RMSE & RSq", subtitle = "95% CIs; Best fit of each model on training data") +
  theme_gray() +
  scale_x_continuous(breaks = 1:10) +
  theme(legend.position = "none")
ggsave(glue("./src/png/rmse_rsq_autoplot_{data_type}.png"), width = 6, height = 3)


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
  geom_point(alpha = .1, color = "blue") +
  geom_smooth() +
  coord_obs_pred() +
  scale_color_viridis_d() +
  facet_wrap(~ wflow_id) +
  ggtitle("Obs vs. Predicted", subtitle = "Best fit of each model on training data") +
  theme_gray()

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



# HOLDOUT DATA ####
best_algos <- wfs$wflow_id
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
      last_fit(split = splits)
  )
best_algos_test_results


# These are the performance metrics for holdout data alone
best_algos_test_results %>% map(collect_metrics)
# best_algos_test_results %>% 
#   map_dfr(collect_metrics, .id = "algo") %>% 
#   ggplot(aes(x = algo, y = .estimate)) +
#   #geom_bar(stat = "identity") +
#   geom_point(alpha = 1, color = "lightgreen") +
#   ggrepel::geom_label_repel(aes(label = round(.estimate, 2)), size = 3, alpha = .8, fill = "black", color = "white") +
#   coord_flip() +
#   facet_grid(~ .metric, scales = "free_x") +
#   ggtitle("HOLDOUT DATA: Performance Metrics", subtitle = "Best Fit of each Model") +
#   theme_gray()

mets_test <- 
  best_algos_test_results %>% 
  map_dfr(collect_metrics, .id = "wflow_id") %>% 
  select(wflow_id, .metric, .estimate) %>% 
  pivot_wider(wflow_id, names_from = .metric, values_from = .estimate) %>% 
  mutate(split = "test")

mets_train <- 
  race_results.1 %>% 
  rank_results(select_best = TRUE) %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  mutate(split = "train")

# bind_rows(mets_train, mets_test) %>% 
#   pivot_longer(!c(wflow_id, split), names_to = "metric") %>% 
#   pivot_wider(c(wflow_id, metric), names_from = split, values_from = value) %>% 
#   ggplot(aes(x = wflow_id, ymin = test, ymax = train, color = wflow_id)) +
#   geom_errorbar() +
#   facet_wrap(~ metric, scales = "free_y") +
#   theme_gray() 
  
# bind_rows(mets_train, mets_test) %>% 
#   pivot_longer(!c(wflow_id, split), names_to = "metric") %>%
#   ggplot(aes(x = wflow_id, y = value, color = split, shape = wflow_id)) +
#   geom_point(size = 3) +
#   scale_color_viridis_d(end = .8, option = "C", direction = -1) +  
#   facet_wrap(~ metric, scales = "free_y") +
#   ggtitle("Train vs. Test RMSE & RSq", subtitle = glue("Best fit of each model on {data_type} data")) +
#   theme_gray()
#   # theme(legend.position = "none")
# ggsave(glue("./src/png/rmse_rsq_test-vs-train_{data_type}.png"), width = 6, height = 3)

mets_traintest <- 
  bind_rows(mets_train, mets_test) %>% 
  mutate(
    wflow_id = factor(wflow_id, levels = stringr::str_extract(best_fits, "basic_[^_]+"), ordered = TRUE)
  )
traintest_rmse <- 
  bind_rows(mets_train, mets_test) %>% 
  ggplot(aes(x = reorder(wflow_id, -1* rmse, max), y = rmse, color = split, shape = split)) +
  geom_point(size = 3) +
  scale_color_viridis_d(end = .8, option = "C", direction = -1) +  
  coord_flip() +
  xlab("") +
  ggtitle("Train vs. Test: RMSE & RSq", subtitle = glue("Best fit of each model on {data_type} data")) +
  theme_gray()
traintest_rsq <- 
  bind_rows(mets_train, mets_test) %>% 
  ggplot(aes(x = reorder(wflow_id, rsq, max), y = rsq, color = split, shape = split)) +
  geom_point(size = 3) +
  scale_color_viridis_d(end = .8, option = "C", direction = -1) +  
  coord_flip() +
  xlab("") +
  theme_gray() +
  theme(legend.position = "none")
(traintest_rmse / traintest_rsq)
ggsave(glue("./src/png/rmse_rsq_test-vs-train_{data_type}.png"), width = 6, height = 4)



best_algos_test_results %>% 
  map_dfr(collect_predictions, .id = "algo") %>% 
  ggplot(aes(x = y, y = .pred)) +  
  geom_abline(lty = 2, alpha = .5) +   
  geom_point(alpha = .25, color = "blue") +
  geom_smooth() +
  coord_obs_pred() +
  scale_color_viridis_d() +
  facet_wrap(~ algo) +
  ggtitle("HOLDOUT DATA: Obs vs. Predicted", subtitle = "Best Fit of each Model")

# best_algos_test_results %>% 
#   map_dfr(collect_predictions, .id = "algo") %>% 
#   select(algo, .row, y, .pred) %>%   
#   pivot_longer(!c(algo, .row)) %>% 
#   ggplot(aes(x = .row, y = value, color = name, size = name, alpha = name)) +
#   geom_line() +
#   #scale_color_viridis_d(begin = 0.5, option = "C") +  
#   scale_color_manual(values = c("green", "white")) +
#   scale_size_manual(values = c(.5, 1.5)) +
#   scale_alpha_manual(values = c(.8, .3)) +
#   facet_wrap(~ algo) +  
#   ggtitle("HOLDOUT DATA: Observed vs. Predicted Timeseries", subtitle = "Best Fit of each Model")





# COEFFICIENTS ####
# How well does each model recover the actual coefficients of the equation?


