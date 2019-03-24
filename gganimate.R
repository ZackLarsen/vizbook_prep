library(pacman)
p_load(tidyverse,tweenr,gganimate,gifski,ggthemes,MatchIt)



ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot(fill = "pink") +
  theme_economist()


ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot(fill = "pink") +
  coord_flip() +
  theme_economist() +
  # Here comes the gganimate code
  transition_states(
    gear,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Now showing {closest_state} gears')









data(lalonde)

lalonde %>% 
  mutate(index = rownames(.)) %>% 
  select(index, treat, re74, re75, re78) %>% 
  gather(key = "Year", value = "Earnings", -index, -treat) %>% 
  rename(Subject = index) %>% 
  tail()

earnings <- lalonde %>% 
  mutate(index = rownames(.)) %>% 
  select(index, treat, re74, re75, re78) %>% 
  gather(key = "Year", value = "Earnings", -index, -treat) %>% 
  rename(Subject = index)
earnings





ggplot(earnings, aes(factor(treat), Earnings)) + 
  geom_boxplot(fill = "pink") +
  coord_flip() +
  theme_economist() +
  # Here comes the gganimate code
  transition_states(
    Year,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  ggtitle('Now showing earnings for {closest_state}')

