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


set.seed(1234)
d <- 
  1:20 %>% 
  map(~ runif(10000)) %>% 
  #map(~ round(runif(1000))) %>%   
  setNames(glue::glue("x{1:length(.)}")) %>% 
  bind_cols() %>% 
  rowwise() %>% 
  mutate(sum = sum(c_across()), mean = mean(c_across()))
glimpse(d)  

hist(d$mean)
qqnorm(d$mean)
qqline(d$mean)

x <- runif(1000)
hist(x)
qqnorm(x)
qqline(x)
