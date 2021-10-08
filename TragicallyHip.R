library(tidyverse)
library(ggplot2)
library(ggdark)
library(lubridate)

hip = read_csv("../TragicallyHip.csv")

head(hip)  


hip = hip %>% 
  mutate(Album = as_factor(Album),
         Released = ymd(Released))

max(hip$Song_Length)
min(hip$Song_Length)  
mean(hip$Song_Length)


ggplot(hip, aes(x= Song_Length, y=Song, fill= Album))+
  geom_bar(stat="identity")+
  ggdark::dark_mode()


ggplot(hip, aes(x= Song_Length, y=Album))+
  geom_point(stat="identity", size=3, alpha=0.5, color="#ff3333")+
  ggdark::dark_mode()


ggplot(hip, aes(x= Song_Length, y=Album))+
  geom_boxplot(color="#ff3333")+
  ggdark::dark_mode()


ggplot(hip, aes(x= Song_Length, y=Album))+
  geom_violin(color="#ff3333")+
  ggdark::dark_mode()


library(ggridges)

ggplot(hip, aes(x= Song_Length, y=Album), fill=Song_Length)+
  geom_density_ridges(color="#ff3333")+
  ggdark::dark_mode()+
  labs(x="Song Length",
       caption = "@StarTrek_Lt") + theme(plot.caption = element_text(size = 10,
                                                                     colour = "coral1", hjust = 0), axis.title = element_text(size = 13),
                                         axis.text = element_text(size = 12))

hip$Album

