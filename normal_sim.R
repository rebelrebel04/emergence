library(tidyverse)

d <- 
  tibble(
    x = round(runif(100)),
    y = round(runif(100)),
    z = round(runif(100)),
    sum = x + y + z
  )

d

hist(d$sum)

pad_zeroes_left <- function(x, digits = 2) {
  x <- as.integer(x)
  n_digits <- nchar(paste(x))
  max_digits <- max(digits, max(n_digits, na.rm = TRUE), na.rm = TRUE)
  zeroes <- 
    n_digits %>% 
    purrr::map(~ paste0(rep.int("0", max_digits - .x), collapse = ""))
  glue::glue("{zeroes}{x}")
}

samples <- 1000
n_features <- 20
set.seed(1234)
d <- 
  1:n_features %>% 
  map(~ rnorm(samples)) %>% 
  # map(~ rpois(samples, 10)) %>%   
  # map(~ runif(samples)) %>%   
  setNames(glue::glue("x{pad_zeroes_left(1:n_features)}")) %>% 
  bind_cols() %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across()), mean = mean(c_across()))
glimpse(d)  

d %>% 
  select(starts_with("x")) %>%  
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ name)

hist(d$mean)
qqnorm(d$mean)
qqline(d$mean)

x <- runif(1000)
x <- rnorm(1000)
x <- rpois(1000, 10)
hist(x)
qqnorm(x)
qqline(x)
