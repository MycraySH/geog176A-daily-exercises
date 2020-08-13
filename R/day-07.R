#Name: Stone SHI
#Date: 08/12/2020
#To visualize the COVID-19 cases data

#the function to build a data frame of regions with cases and death cases
library(tidyverse) %>%
covid = read_csv("D:/LifeInUCSB/Study/GEOG176A/week2/assignment5/geog176A-daily-exercises/data/covid.csv") %>%
state = read_csv("D:/LifeInUCSB/Study/GEOG176A/week2/assignment5/geog176A-daily-exercises/data/covid.csv")
covid %>%
  select(state,cases) %>%
  region = data.frame(state = state.name, region = state.region)%>%
    right_join(covid,region, by = "state")%>%
    group_by(region,date)%>%
    summarize(cases  = sum(cases), deaths = sum(deaths))%>%
    ungroup()%>%
    pivot_longer(cols = c('cases', 'deaths')) ->
    regions_cases_deaths#pivot the frame to long

ggplot(data=regions_cases_deaths, aes(x =as.Date(date), y = value, color = state)) +
           geom_line(size = 1) +
           facet_wrap(~region) +
           ggthemes::theme_gdocs() +
           facet_grid(name~region, scale = "free_y")+
           labs(title = "Cummulative Cases and Deaths: Region",
                subtitle = "COVID-19 Data: NY-Times",
                x = "Date",
                y = "Daily Cummulative Count",
                caption = "Daily Exercise 07")+
           ggsave(file = "img/cummulative_regions.png",
                  units = c("in"))

