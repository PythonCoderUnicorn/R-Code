



#======= BIKE RENTAL SALES DATA FOR 2011


library(tidyverse)
library(ggplot2)
library(palmerpenguins)
library(janitor)
library(systemfonts)
library(lubridate)

theme_set(theme_bw(base_family = "Raleway"))



#  Credit:  Darshana Daga
# https://github.com/ddaga-uci/BikeRentalAnalysis/blob/main/hw2_darshana_daga.R


# original source: https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset

# - dteday : date
# - season : season (1:winter, 2:spring, 3:summer, 4:fall)
# - yr : year (0: 2011, 1:2012)
# - mnth : month ( 1 to 12)
# - hr : hour (0 to 23)
# - holiday : weather day is holiday or not (extracted from [Web Link])
# - weekday : day of the week
# - workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
# + weathersit :
#   - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#   - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#   - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#   - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# - temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
# - atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
# - hum: Normalized humidity. The values are divided to 100 (max)
# - windspeed: Normalized wind speed. The values are divided to 67 (max)
# - casual: count of casual users
# - registered: count of registered users
# - cnt: count of total rental bikes including both casual and registered











bikes = read.csv('https://raw.githubusercontent.com/ddaga-uci/BikeRentalAnalysis/main/hw2_hour.csv')

bikes = bikes %>% clean_names()

options(stringsAsFactors = TRUE)

bikes = bikes %>% 
  mutate(date = as_date(dteday, format="%m/%d/%Y"),
         month = as_factor(mnth),
         season = as_factor(season),
         hr = as_factor(hr),
         wkday = as_factor(wkday),
         weathersit = as_factor(weathersit)
         )

glimpse(bikes)


# bikes[, c(15, 1,4)]

bikes = bikes %>% 
  transmute(obs, season, hr, wkday, weathersit, temp, atemp, hum, windspeed, cnt, date, month)

library(RColorBrewer)
library(ggtext)

bikes %>% 
  select(month, season, cnt, wkday) %>% 
  group_by(season) %>% 
  ggplot( aes(x= cnt, y= month, fill= wkday))+
  geom_col()+
  # geom_point(size= 4, alpha= .4)+
  ggdark::dark_mode()+
  scale_fill_brewer(palette = 5)+
  labs(title = "Bike Rental Sales Data (2011)",
       subtitle = "bike rental counts by month and weekday",
       caption = "\n @StarTrek_Lt | Source: Darshana Daga (GitHub)",
       x="count of rentals")+
  theme(
    plot.caption = element_markdown(face = 'bold', 
                                    colour = '#84b397',
                                    hjust= .3),
    plot.title = element_markdown(face = 'bold', 
                                  color = '#4dbd7c', 
                                  size= 16, 
                                  hjust = .5),
    plot.subtitle = element_markdown(size = 13,
                                     hjust = 0.5),
    axis.text.x = element_markdown(size = 12),
    axis.text.y = element_markdown(size = 12),
    axis.title.x = element_markdown(size = 12)
  )
