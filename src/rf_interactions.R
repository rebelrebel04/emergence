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



forest <-
  tibble(varID = fit.ranger$forest$split.varIDs, tree = 1:fit.ranger$forest$num.trees) %>%
  bind_cols(tibble(values = fit.ranger$forest$split.values)) %>%
  unnest(c(varID, values))

treeInfo(fit.ranger, 1) %>% 
  View()

