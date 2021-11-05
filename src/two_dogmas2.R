library(tidyverse)
library(tidymodels)
library(corrr)

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
    preproc = list(basic_rec = basic_rec),
    models = list(lm = linear_reg_lm_spec, rf = rand_forest_ranger_spec, xgb = boost_tree_xgboost_spec)
  )
wfs


# TUNE ####
keep_pred <- 
  control_grid(
    save_pred = TRUE,
    parallel_over = NULL
  )

tune_results <- 
  wfs %>% 
  workflow_map(
    "tune_grid", 
    # Options to `workflow_map()`: 
    grid = 5, seed = 1101, verbose = TRUE,
    # Options to `tune_grid()`: 
    resamples = folds, metrics = metric_set(rmse, rsq), control = keep_pred,
  )

tune_results
autoplot(tune_results)

tune_results$result %>% 
  map_df(show_best, metric = "rsq", n = 1) %>% 
  View()
  #map(collect_metrics, metric = "rsq")

tune_results %>% 
  workflow_map(select_best)
  map(select_best, metric = "rsq", n = 1) 
  autoplot()
  #map(collect_metrics, metric = "rsq")




# TEST DATA ####
