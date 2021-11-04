library(tidyverse)
library(corrr)

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

fit.lm <- lm(y ~ . -epsilon, data = d.1)
summary(fit.lm)
tibble(
  y = d.1$y,
  yhat = fit.lm$fitted.values,
  delta = y - yhat
) %>% 
  pull(delta) %>% 
  summary()

fit.rf <- ranger::ranger(y ~ . -epsilon, data = d.1)
tibble(
  y = d.1$y,
  yhat = fit.rf$predictions,
  delta = y - yhat
) %>% 
  pull(delta) %>% 
  summary()


bind_rows(
  tibble(
    model = "lm",
    y = d.1$y,
    yhat = fit.lm$fitted.values,
    delta = yhat - y
  ),
  tibble(
    model = "rf",
    y = d.1$y,
    yhat = fit.rf$predictions,
    delta = yhat - y
  )
) %>% 
  ggplot(aes(x = y, y = yhat, color = model)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0) +
  facet_wrap(~ model)

# Compare residual distributions
bind_rows(
  tibble(
    model = "lm",
    y = d.1$y,
    yhat = fit.lm$fitted.values,
    delta = yhat - y
  ),
  tibble(
    model = "rf",
    y = d.1$y,
    yhat = fit.rf$predictions,
    delta = yhat - y
  )
) %>% 
  ggplot(aes(x = delta, color = model)) +
  geom_density()

tibble(
  y = d.1$y,
  yhat = fit.lm$fitted.values
) %>% 
  cor() %>% 
  map(~ .x^2)

tibble(
    y = d.1$y,
    yhat = fit.rf$predictions
  ) %>% 
  cor() %>% 
  map(~ .x^2)  


#////////////////////////////////
# Do a train/test split & compare fits to holdout data
# probably best to set up a tidymodels pipeline to make this all easier

# Possible to generate a dataset from a bunch of random trees?




d.1 <- 
  bind_rows(
    tibble(
      A = rnorm(n_obs/2, 0, 1),
      B = rnorm(n_obs/2, 0, 1),
      C = rnorm(n_obs/2, 0, 1),
      D = rnorm(n_obs/2, 0, 1),
      y = 0
    ),
    tibble(
      A = rnorm(n_obs/2, .5, 1),
      B = rnorm(n_obs/2, 1, 1),
      C = rnorm(n_obs/2, 1.5, 1),
      D = rnorm(n_obs/2, 0, 1),
      y = 1
    )
  ) %>% 
  mutate(y = factor(y))
glimpse(d.1)
table(d.1$y)

d.1 %>% 
  pivot_longer(!y) %>% 
  ggplot(aes(x = value, color = y)) +
  geom_density() +
  facet_wrap(~ name)

d.1 %>% 
  select(-y) %>% 
  correlate()
  #rplot(print_cor = TRUE)

fit.lm <- glm(y ~ ., family = "binomial", data = d.1)
summary(fit.lm)
tibble(
  y = d.1$y,
  yhat = round(fit.lm$fitted.values)
) %>% 
  count(y, yhat) %>% 
  mutate(pct = n / sum(n))

fit.rf <- ranger::ranger(y ~ ., data = d.1)
tibble(
  y = d.1$y,
  yhat = fit.rf$predictions
) %>% 
  count(y, yhat) %>% 
  mutate(pct = n / sum(n))






fit.lm$fitted.values
table(round(fit.lm$fitted.values))
set.seed(1234)
d.2 <- 
  d.1 %>% 
  mutate(
    epsilon = rnorm(n_obs),
    y = fit.lm$fitted.values + epsilon,
    #y = y / max(abs(y)),
    y = scales::rescale(y),
    y = factor(as.integer(round(y)))
  )
  #select(-epsilon)
glimpse(d.2)
summary(d.2$y)
summary(d.2$epsilon)

fit.lm.2 <- glm(y ~ . -epsilon, family = "binomial", data = d.2)
summary(fit.lm.2)
tibble(
  y = d.2$y,
  yhat = round(fit.lm.2$fitted.values)
) %>% 
  count(y, yhat) %>% 
  mutate(pct = n / sum(n))

fit.rf.2 <- ranger::ranger(y ~ . -epsilon, data = d.2)
tibble(
  y = d.2$y,
  yhat = fit.rf.2$predictions
) %>% 
  count(y, yhat) %>% 
  mutate(pct = n / sum(n))





# How to simulate a process that maps x to y as a weighted linear aggregate?

# And conversely, that maps x to y as a function of an ensemble of trees?