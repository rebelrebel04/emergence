library(tidyverse)
# library(tidymodels)
library(ranger)


library(AppliedPredictiveModeling)
data("abalone")
glimpse(abalone)
fit.ranger <-
  abalone %>% 
  ranger(Type ~ ., data = ., num.trees = 1000, seed = 1234, importance = "impurity")

library(mlbench)
data(Sonar)
glimpse(Sonar)
fit.ranger <-
  Sonar %>% 
  ranger(Class ~ ., data = ., num.trees = 1000, seed = 1234, importance = "impurity")

library(mlbench)
data(Glass)
glimpse(Glass)
fit.ranger <-
  Glass %>%
  ranger(Type ~ ., data = ., num.trees = 1000, seed = 1234, importance = "impurity")

glimpse(diamonds)
fit.ranger <-
  diamonds %>% 
  ranger(price ~ ., data = ., num.trees = 250, seed = 1234, importance = "impurity")

# glimpse(iris)
# fit.ranger <-
#   iris %>% 
#   ranger(Species ~ ., data = ., seed = 1234, importance = "impurity")

fit.ranger
prop.table(fit.ranger$confusion.matrix)
sort(fit.ranger$variable.importance)

get_pairs <- function(ti, side) {
  ti %>%
    filter(!terminal) %>%       
    select(parentNode = nodeID, childNode = {{ side }}, parent = splitvarName) %>% 
    left_join(select(ti, childNode = nodeID, child = splitvarName), by = "childNode") %>% 
    filter(!is.na(child))
}

get_tree_pairs <- function(ti) {
  c("leftChild", "rightChild") %>% 
    map_dfr(get_pairs, ti = ti)
}

all_pairs <- 
  seq(1, fit.ranger$num.trees) %>% 
  #c(1,2) %>% 
  map(treeInfo, object = fit.ranger) %>% 
  map_dfr(get_tree_pairs, .id = "treenum")

all_pairs %>%
  count(parent, child, sort = TRUE) %>% 
  mutate(
    parent = factor(parent, levels = rev(unique(parent)), ordered = TRUE),
    child = factor(child, levels = rev(unique(parent)), ordered = TRUE)
  ) %>% 
  ggplot(aes(x = parent, y = child, fill = n)) +
  # ggplot(aes(x = reorder(parent, n), y = reorder(child, n), fill = n)) +  
  geom_raster() +
  xlab("parent") +
  ylab("child") +
  scale_fill_viridis_c("n pairs", option = "B") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





# DIAMONDS ####
# Possible to visualize regression forest in 2D and then heatmap propensity scores
# onto each tree? e.g., where axes are first 2 components of trees? or just random (sqrt(ntree) * sqrt(ntree)) to start?

#TODO: test/train split & only visualize test predictions


diamonds <- 
  diamonds %>% 
  arrange(price) %>% 
  mutate(price = log(price))
glimpse(diamonds)

splits <- rsample::initial_split(diamonds, prop = .8, strata = price)
train <- rsample::training(splits)
test <- rsample::testing(splits)

numtrees_sqrt <- 50

fit.ranger <-
  train %>% 
  ranger(price ~ ., data = ., num.trees = numtrees_sqrt^2, seed = 1234, importance = "impurity")

# pred.case <- predict(fit.ranger, test[1, ], predict.all = TRUE, seed = 1234)
# pred.case <- predict(fit.ranger, test[2, ], predict.all = TRUE, seed = 1234)
# pred.case <- predict(fit.ranger, test[nrow(test)/2, ], predict.all = TRUE, seed = 1234)
# pred.case <- predict(fit.ranger, test[nrow(test), ], predict.all = TRUE, seed = 1234)


plot_forests <- function(data, fit, n_cases = 5, seed = 1234) {
  
  # TODO: rework case sampling to index actual case numbers & label them in plot for correct reference
  # overlay predicted, observed, reldiff on each tile
  
  set.seed(seed)
  pred.cases <- 
    data %>% 
    slice_sample(n = n_cases) %>% 
    predict(fit, data = ., predict.all = TRUE)
  
  pred.cases$predictions %>% 
    t() %>% 
    as_tibble() %>% 
    bind_cols(
      expand_grid(
        x = 1:ceiling(sqrt(fit$num.trees)),
        y = 1:ceiling(sqrt(fit$num.trees))
      )
    ) %>% 
    pivot_longer(!c(x, y), names_to = "case", values_to = "prediction") %>% 
    mutate(case = gsub("V", "case_", case, fixed = TRUE)) %>% 
    ggplot(aes(x = x, y = y, fill = prediction)) +
    #geom_hex(stat = "identity") +
    geom_raster() +
    scale_fill_viridis_c("Prediction", option = "B") +
    facet_wrap(~ case, scales = "fixed")
  
}

plot_forests(test, fit.ranger, 25, seed = 1234)


# set.seed(1234)
# pred.cases <- 
#   test %>% 
#   slice_sample(n = 5) %>% 
#   predict(fit.ranger, data = ., predict.all = TRUE)
# 
# pred.cases$predictions %>% 
#   t() %>% 
#   as_tibble() %>% 
#   bind_cols(
#     expand_grid(
#       x = 1:ceiling(sqrt(fit$num.trees)),
#       y = 1:ceiling(sqrt(fit$num.trees))
#     )
#   )



# So to reduce trees to 2 dimensions, need to figure out how to
# convert an individual tree into a single case with wide features:
# for example, could summarize  usage of each predcitor? (min depth, avg depth, n appearances)
# or first n splits?
# could I use ESOM here to generate the maps?

all_info <- 
  seq(1, fit.ranger$num.trees) %>% 
  #c(1:1000) %>% 
  map_dfr(treeInfo, object = fit.ranger, .id = "tree") %>% 
  mutate(tree = as.integer(tree))

all_info_wide <- 
  all_info %>% 
  filter(!is.na(splitvarName)) %>%
  group_by(tree, splitvarName) %>% 
  summarize(
    n_nodes = n(),
    min_node = min(nodeID, na.rm = TRUE),
    mean_node = mean(nodeID, na.rm = TRUE),
    max_node = max(nodeID, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  pivot_longer(!c(tree, splitvarName)) %>% 
  pivot_wider(tree, names_from = c(splitvarName, name))

pca <- princomp(all_info_wide[, -1])  
screeplot(pca)
biplot(pca)


# Biplot: Comp 1 & 2 loadings
pca$loadings[, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = .25) +
  ggrepel::geom_text_repel(aes(label = feature), color = "green") +
  ggdark::dark_theme_bw()


# TODO: vectorize below to facet_wrap plots given a vector of cases (test df indexes)

# Predict forest's predictions for a single case from holdout sample along biplot
preds <- predict(fit.ranger, test[1, ], predict.all = TRUE); head(preds$predictions[1, ])
preds <- predict(fit.ranger, test[ceiling(nrow(test)/2), ], predict.all = TRUE)
preds <- predict(fit.ranger, test[nrow(test), ], predict.all = TRUE)

pca$scores[, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("tree") %>% 
  mutate(
    pred = preds$predictions[1, ]
    # pred = preds$predictions[1, 1:1000]
  ) %>% 
  arrange(pred) %>% 
  ggplot(aes(x = Comp.1, y = Comp.2, color = pred)) +
  geom_point(alpha = .4, size = 2, shape = 16) +
  #scale_fill_viridis_c("Prediction", option = "B", begin = .3) +
  scale_color_viridis_c("Prediction", option = "B", begin = .2) +
  #theme_bw()
  ggdark::dark_theme_bw()
  #ggrepel::geom_text_repel(aes(label = feature))
  

  
# Colorize forest biplot by one of the features used to reduce it, e.g. clarity_mean_node
pca$scores[, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("tree") %>% 
  mutate(
    feature = all_info_wide$cut_mean_node
    #feature = all_info_wide$clarity_mean_node    
  ) %>%
  arrange(feature) %>%
  ggplot(aes(x = Comp.1, y = Comp.2, color = feature)) +
  geom_point(alpha = .5, size = 4, shape = 16) +
  scale_color_viridis_c("Feature", option = "B", begin = .2) +
  ggdark::dark_theme_bw()


features <- 
  pca$loadings[, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  pivot_longer(!feature) %>% 
  group_by(name) %>% 
  arrange(desc(abs(value))) %>% 
  mutate(rank = row_number(value)) %>% 
  ungroup() %>% 
  filter(rank <= 6) %>% 
  pull(feature) %>% 
  unique()
features
# features <- c("clarity_mean_node", "cut_mean_node")
# features <- c("cut_mean_node")
features <- names(all_info_wide)[-1]
features <- features[grepl("mean", features, fixed = TRUE)]

pca$scores[, 1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column("tree") %>% 
  mutate(tree = as.integer(tree)) %>% 
  left_join(all_info_wide) %>% 
  select(tree, Comp.1, Comp.2, features) %>% 
  mutate(across(features, ~ scale(log(.x)))) %>% 
  #head()
  pivot_longer(features) %>% 
  #arrange(tree, name, value) %>%  
  ggplot(aes(x = Comp.1, y = Comp.2, color = value)) +
  geom_point(alpha = .4, size = 3, shape = 16) +
  scale_color_viridis_c("Feature", option = "C", begin = 0) +
  facet_wrap(~ name, scales = "fixed") +
  ggdark::dark_theme_bw()
