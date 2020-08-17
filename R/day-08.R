#Name: Stone SHI
#Date: 08/16/2020
#To visualize the COVID-19 cases data
library(zoo)
library(ggthemes)
library(tidyverse)
covid = read_csv("data/covid.csv")
state.of.interest = "Virginia"
covid %>%
  filter(state == state.of.interest) %>%
  group_by(date) %>%
  summarise(cases = sum(cases,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(newcases = cases - lag(cases),
         roll7 = rollmean(newcases, 7, fill = NA, align="right")) %>%
  ggplot(aes(x = as.Date(date))) +
  geom_col(aes(y = newcases), col = NA, fill = "#AD1453") +
  geom_line(aes(y = roll7), col = "blue", size = 1) +
  theme_update()+
  ggthemes::theme_gdocs() +
  labs(title = paste("New Reported cases by day in", state.of.interest)) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 13, face = 'bold')) +
  theme(aspect.ratio = .5)->
  newplot
ggsave(newplot, file = "img/newcasesstatelevel.png",
       width = 8,
       unit = "in")

