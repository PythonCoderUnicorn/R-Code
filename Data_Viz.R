
# Data Visualization

library(tidyverse)

#  question: Do cars with big engines use more fuel 
#  than cars with small engines? 

cars = mtcars

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# ===========================
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
# ===========================

# aesthetic is a visual property of the objects in your plot. 
# Aesthetics include things like the size, the shape, 
# or the color of your points.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, 
                           y = hwy, 
                           color = class))


# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))



# For x and y aesthetics, ggplot2 does not create a legend, 
# but it creates an axis line with tick marks and a label. 
# The axis line acts as a legend; it explains the mapping 
# between locations and values.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "red" )


# ----- facet_wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)



# geometric objects

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()


# Statistical transformations




demo = tribble(
  ~education, ~value,
  "PhD", 430,
  "Masters", 367,
  "Bachelors", 105,
  "Other",78
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = education, 
                         y = value, 
                         fill= education), 
           stat = "identity")
# "identity" maps the height of the bars to the raw values of a y variable.



ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, 
                         fill = color, 
                         y = after_stat(prop)))


# options: "identity", "dodge" or "fill"
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")


# use jitter to avoid overcrowding points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy,  col=displ), position = "jitter", alpha= 0.5,)



# coord_flip() switches the x and y axes. This is useful (for example), 
# if you want horizontal boxplots. It’s also useful for long labels: 
# it’s hard to get them to fit without overlapping on the x-axis.
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()


# coord_polar() uses polar coordinates. 

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()



# ====================
library(tidyverse)
library(dplyr)
library(nycflights13)

flights = nycflights13::flights

colnames(flights)

# get flights for Jan 1
(jan1 = filter(flights, month==1, day==1))

# get flights nov or dec using %in%
nov_dec = filter(flights, month %in% c(11,12))
nov_dec

# get flights not delayed arrival or departure by >2 hrs
filter(flights, !(arr_delay > 120 | dep_delay > 120))

# Had an arrival delay of two or more hours
filter(flights, arr_delay > 120)

# Flew to Houston (IAH or HOU)
unique(flights$dest)

houston = filter(flights, dest %in% c("IAH","HOU"))
houston

# Were operated by United, American, or Delta
unique(flights$carrier)

carriers3 = filter(flights, carrier %in% c("UA","AA","DL"))
carriers3

# Departed in summer (July, August, and September)
filter(flights, dep_time %in% c(7,8,9))

# Arrived more than two hours late, but didn’t leave late
late_early = filter(flights, arr_time > 120 & !(dep_delay >0))
late_early

# ==========================




# --- arrange
arrange(flights, month, day, year)

arrange(flights, desc(air_time))



# ---- select
select(flights, starts_with("A"))

select(flights, contains("TIME"))


# 
# =====================
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist_avg = mean(distance, na.rm = TRUE),
    delay_avg = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles 
# and then decrease. Maybe as flights get longer there's more 
# ability to make up delays in the air?
ggplot(data = delays, mapping = aes(x = dist_avg, y = delay_avg)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)






# ------- remove NA
flights %>% 
  group_by( origin, flight, carrier, tailnum) %>% 
  summarise(dep_delay_avg = mean(dep_delay, na.rm = TRUE))


# missing values represent canceled flights, 
# we could also tackle the problem by first removing the canceled flights.
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day, origin, tailnum, carrier) %>% 
  summarise(dep_delay_avg = mean(dep_delay))



# look at the planes (identified by their tail number) 
# that have the highest average delays
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# ---
fdelays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n() )

ggplot(data = fdelays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

fdelays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)


not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

















# Usage in R base plots:
barplot(1:5, col=rainbow(5))
# Use heat.colors
barplot(1:5, col=heat.colors(5))
# Use terrain.colors
barplot(1:5, col=terrain.colors(5))
# Use topo.colors
barplot(1:5, col=topo.colors(5))
# Use cm.colors
barplot(1:5, col=cm.colors(5))