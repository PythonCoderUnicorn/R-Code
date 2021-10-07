
#### Credit: Laura Navarro Soler for code tutorial for this to be made






library(tidyverse)
library(janitor)
library(ggtext)
library(geofacet)
library(extrafont)
library(statebins)
#install.packages("scico") -> https://github.com/thomasp85/scico
library(scico)
library(ggdark)


# data
nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv') %>% clean_names()

nurses$st <- state.abb[match(nurses$state, state.name)]

df1 <- nurses %>% 
  filter(year == 2020) %>% 
  arrange(hourly_wage_median) %>% 
  filter(state != "Guam")

#state bins

p1 <- ggplot(df1, aes(state=state, fill = hourly_wage_median)) +
  geom_statebins() +
  coord_equal() +
  scale_fill_scico(palette = "buda",
                   direction = -1,
                   name = "Hourly Wage Median", 
                   limits=c(10,60), 
                   breaks = c(0,20,40,60),
                   labels = c("$0","$20", "$40", "$60"),
                   guide = "colorbar") +
  labs(title = "RN's Hourly Median Wage, by state (2020)",
       subtitle = "Registered Nurses in US in the year of the Covid-19 pandemic\n #TidyTuesday | Oct 5 2021\n",
       caption = "@StarTrek_Lt | Credit: Laura Navarro Soler",
       x="",
       y="") +
  theme_void() +
  ggdark::dark_mode()+
  theme(legend.position = "top") +  
  guides(fill = guide_legend(direction = "horizontal")) +
  theme(text = element_text(family = "Lato"),
        legend.position = c(0.5, 0.92),
        legend.key.width = unit(30, 'pt'),
        legend.key.height = unit(5, 'pt'),
        legend.direction = "horizontal",
        legend.title = element_text(size=11),
        legend.text = element_text(size=11),
        panel.spacing = unit(1, 'pt'),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = -1.5,
                                  family = "Lato Bold"),
        plot.subtitle = element_text(hjust = 0.5, vjust = -2.5, size = 11),
        plot.caption = element_text(hjust = 0.5, size = 10),
        panel.grid.major = element_line(colour = "gray0", linetype = "blank"),
        panel.grid.minor = element_line(colour = "gray0", linetype = "blank"),
        axis.line = element_line(colour = "#000000",linetype = "blank"), 
        axis.ticks = element_line(colour = "#000000", linetype = "blank"), 
        axis.text = element_text(colour = "#000000")) +
  
  annotate(geom="richtext", x= 0.5, y= -4.5, 
           label="<span style='color: #ffffff;'>$ 56.90 </span>", 
           size = 4, 
           fill = NA, 
           label.color = NA) +
  geom_curve(
    aes(x = 0.5, 
        y = -4.8, 
        xend = 1.5, 
        yend = -5), 
    arrow = arrow(length = unit(0.015, "npc")))

        
p1








