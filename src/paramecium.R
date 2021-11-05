para <- assist::paramecium
para %>% 
  #mutate(exp = exp(day)) %>% 
  pivot_longer(!day) %>% 
  ggplot(aes(x = day, y = value, color = name)) +
  geom_line()

tibble(
  x = 1:20,
  y = exp(x)
) %>% 
  plot()
