# Serious violence conference

library(tidyverse)
library(janitor)
library(viridis)
library(ggthemes)
library(wesanderson)
library(gganimate)

homicide_rate <- read_csv('homicide_rate.csv') %>%
  rename(homicide_rate = homcide_rate) %>%
  mutate(homicide_k = homicide_rate*10) %>%
  select(year, homicide_k)

homicide <- read_csv('homicides.csv')
google <- read_csv('google.csv')
hansard <- read_csv('hansard.csv') %>%
  rename(hansard = knife_crime)

sv_trends <- homicide %>%
  left_join(google, by='year') %>%
  left_join(hansard, by='year') %>%
  left_join(homicide_rate) %>%
  drop_na()

sv_long <- sv_trends %>%
  pivot_longer(!year, names_to = "variable", values_to = "count")

# sv_trends %>%
#   ggplot(aes(x=year)) +
#   geom_line(aes(y=homicide_rate), colour='yellow') +
#   geom_line(aes(y=google), colour='blue') +
#   geom_line(aes(y=knife_crime), colour='red')

sv_long %>% 
  filter(!variable %in% c('serious_violence', 'homicide_k')) %>%
  ggplot(aes(x=year, y=count, colour = variable, group = variable)) +
  geom_line(linewidth=2) +
  theme(legend.title=element_blank()) +
  ggtitle("Knife crime mentions vs homicides, 2004-2022") +
  theme(plot.title = element_text(hjust = 0.9, vjust = -8, size=10, face = 'bold', lineheight = 10)) +
  labs(title='Knife crime mentions vs homicides, 2004-2022', 
     x='Year') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave('trends_hansard plot.png', height = 15, width=30, units = 'cm')

p_animated <- plot +
  transition_states(
    hansard,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_fade()

# Preview the animation
animate(p_animated)
