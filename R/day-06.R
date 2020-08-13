#Name: Stone SHI
#Date: 08/12/2020
#To visualize the COVID-19 cases data

#plot1
maxstate = covid %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  summarize(cases = sum(cases, na.rm = TRUE)) %>%
  slice_max(cases, n = 6) %>%
  pull(state) ->topsix

covid %>%
  filter(state %in% topsix) %>%
  group_by(date,state) %>%
  summarise(cases = sum(cases)) %>%
  ungroup() %>%
  ggplot(aes(x =  as.Date(date), y = cases, color = state, group =1)) +
  geom_line(size = 2) +
  facet_wrap(~state) +
  ggthemes::theme_gdocs() +
  labs(title = "Cummulative Case Counts",
       x = "Date",
       y = "Cases",
       caption = "Daily Exercise 06")+
  ggsave(file = "img/cummulative.png",
         width = 8,
         units = c("in"))
#plot2
covid %>%
  group_by(date) %>%
  summarize(cases = sum(cases)) %>%
  ggplot(aes(x = as.Date(date), y = cases,group=1)) +
  geom_line(color = "red", size = 1) +
  geom_col(fill = "red", color = "red", alpha = 0.2) +
  labs(title = "National Cummulative Case Counts",
       x = "Date",
       y = "Cases",
       caption = "Daily Exercise 06")+
  ggthemes::theme_gdocs()+
  ggsave(file = "img/national.png",
       width = 8,
       units = c("in"))


