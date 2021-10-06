
#-------- Tidy Tuesday data for Oct 5 2021

library(tidyverse)     ## data wrangling + ggplot2
library(colorspace)    ## adjust colors
library(rcartocolor)   ## Carto palettes
library(ggforce)       ## sina plots
library(ggdist)        ## halfeye plots
library(ggridges)      ## ridgeline plots
library(ggbeeswarm)    ## beeswarm plots
library(gghalves)      ## off-set jitter
library(systemfonts)   ## custom fonts



# diamondplot(diamonds, bg= 'gray40', col = rainbow)

nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')



theme_set(theme_bw(base_family = "Raleway"))



library(scales)
library(janitor)


nurses = nurses %>% 
  clean_names()



nurses %>%
  count(year, wt = total_employed_rn) %>%
  ggplot(aes(x= year, y= n, )) +
  geom_col(fill='#6570f0') +
  labs(
    title = "US Nurses data, TidyTuesday",
    y = "# of registered nurses nationally")+
  ggdark::dark_mode()+
  scale_y_continuous(labels = comma_format())+
  coord_flip() + 
  theme(plot.subtitle = element_text(size = 11, colour = "gray70"), 
    plot.caption = element_text(size = 12, colour = "gray70"), 
    axis.title = element_text(size = 13), 
    axis.text = element_text(size = 11), 
    legend.title = element_text(size = 14))



nurses %>%
  filter(state %in% c("Washington", "Oregon","Illinois","Michigan","Virginia",
                      "California","Hawaii","New York",
                      "Maine","Montana", "North Dakota","Colorado")) %>%
  ggplot(aes(year, total_employed_rn, color = state)) +
  geom_line(size= 1.5) +
  expand_limits(y = 0) +
  labs(
    title = "US Nurses Data | #TidyTuesday Oct 5 2021",
    y = "# of employed registered nurses",
    caption = "@StarTrek_Lt | Credit: @drob") +
  scale_y_continuous(labels = comma_format())+
  ggdark::dark_mode()+
  theme(plot.subtitle = element_text(size = 11, colour = "gray70"), 
        plot.caption = element_text(size = 12, colour = "gray70"), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11), 
        legend.title = element_text(size = 14))





nurses %>%
  filter(state %in% c("New York", "California","Oregon","Maine",
                      "Texas", "Pennsylvania",
                      "Michigan", "Florida",
                      "Ohio", "Washington")) %>%
  ggplot(aes(year, hourly_wage_median, color = state)) +
  geom_point(fill='black', size= 3)+
  geom_line(size=1.5, alpha= 0.4) +
  
  expand_limits(y = 0) +
  labs(
    title = "US Nurse Data: RN Hourly Wage ($)",
    subtitle = "#TidyTuesday | Oct 5 2021",
    caption = "@StarTrek_Lt | Credit: @drob",
    y = "Hourly wage") +
  scale_y_continuous(labels = dollar_format())+
  ggdark::dark_mode()+
  theme(plot.subtitle = element_text(size = 11, colour = "gray70"), 
        plot.caption = element_text(size = 12, colour = "gray70"), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11), 
        legend.title = element_text(size = 14))


nurses %>%
  filter(year == 2020) %>%
  mutate(state = str_to_lower(state)) %>%
  inner_join(map_data("state"), by = c(state = "region")) %>%
  ggplot(aes(long, lat, group = group, fill = hourly_wage_median)) +
  geom_polygon() +
  coord_map() +
  scale_fill_viridis_c(labels = dollar_format()) +
  ggthemes::theme_map() +
  labs(title = "Hourly wage of registered nurses by state, 2020",
       fill = "Median wage")+
  ggdark::dark_mode()






nurses = nurses %>% 
  mutate("National Total Employed" = "Total Employed (National)_Aggregate",
         "National Healthcare Total Employed" = "Total Employed (Healthcare, National)_Aggregate",
         "State Total Healthcare Employed" = "Total Employed (Healthcare, State)_Aggregate",
         "Yearly State Total Employed" = "Yearly Total Employed (State)_Aggregate"
         )


nurses %>% 
  select(state, year, annual_salary_avg ) %>% 
  # filter(year %in% c(2019,2020)) %>%
  ggplot( aes( y= year, x= annual_salary_avg))+
  ggdist::geom_dots( size=5, color='#6ce091')+
  ggdark::dark_mode()+
  labs(title = "US Nurses Data: Average Annual Salary",
       subtitle = "TidyTuesday | Oct 5 2021",
       x="Annual Salary Avg",
       caption = "@StarTrek_Lt")+
  scale_x_continuous(labels = dollar_format())+
  theme(plot.subtitle = element_text(size = 11, colour = "gray70"), 
        plot.caption = element_text(size = 12, colour = "gray70"), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 11), 
        legend.title = element_text(size = 14))
















