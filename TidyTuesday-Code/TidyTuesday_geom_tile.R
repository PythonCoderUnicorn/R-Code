
#============  SCALE BREWER
# Palettes: 
#   
# Diverging
# BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral
# 
# Qualitative
# Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3
# 
# Sequential
# Blues, BuGn, BuPu, GnBu, Greens, Greys, 
# Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, 
# RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd


library(systemfonts)
theme_set(theme_bw(base_family = "Lato"))


library(tidytuesdayR)
library(tidyverse)
library(geofacet)
library(ggrepel)
library(reshape2)

library(RColorBrewer)
tuesdata <- tidytuesdayR::tt_load(2021, week = 41)

tuesdata$nurses->nurses

nurses = nurses %>% clean_names()


nurses %>% select(state, year, annual_salary_median)%>%
  distinct(state, year, .keep_all = TRUE) -> salary # assignment at the end insteads of the beginning


salary = salary %>% 
  group_by(state, year) %>% 
  arrange( state, year) 

salary = salary %>% 
  mutate(salary = annual_salary_median / 1000) # not sure why but for the scale bar to fit


salary

# another way of transmuting column names
# colnames(salary) = c("state","year","salary")


ggplot(data = salary,
       aes(x= year, y= state, fill= salary))+
  geom_tile( aes(width= 0.9, height= 0.9), size= 2)+
  scale_fill_distiller(palette="greens", 
                       direction = 1,
                       limits=c(20, 120), 
                       breaks=seq(20,120, by=20),
                       expand = c(0, 0))+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(breaks=c(1998,1999,2000,2001,
                              2002,2003,2004,2005,
                              2006,2007,2008,2009,
                              2010,2011,2012,2013,
                              2014,2015,2016,2017,
                              2018,2019,2020))+
  labs(title = "US Nurse median salary wages by State",
       subtitle = "#TidyTuesday : data 1998 to 2020 (in 1000 dollars)",
       caption = "\n@StarTrek_Lt | Credit: @annapurani93")+
  theme_minimal()+
  ggdark::dark_mode()+
  theme(
    plot.title = element_markdown(face = 'bold', color = '#38c25d', size = 15, hjust = 0.5),
    plot.subtitle = element_text(color = 'grey90', size = 12, hjust = 0.5),
    plot.caption = element_markdown(color ='#77e093' , face = 'italic',size = 11, hjust = 0.2),
    legend.title  = element_markdown(face = 'bold', size = 11, vjust = 5),
    legend.text = element_markdown(face = 'italic', size = 10 ),
    legend.key.height = unit(1.5, 'cm'),
    axis.text.x = element_markdown(angle = 45, hjust = 1.2, size = 11),
    axis.title.x = element_markdown(size = 12, vjust = 1.1),
    axis.text.y = element_markdown(size = 11, colour = 'white')
  )


