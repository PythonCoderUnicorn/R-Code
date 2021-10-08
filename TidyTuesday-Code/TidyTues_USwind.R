

#  Tidy Tuesday @drob
# 2018/2018-11-06/ US Wind Turbine Data


pacman::p_load(
  tidyverse,
  scales,
  ggplot2,
  plotly,
  lubridate
)

library(systemfonts)
theme_set(theme_bw(base_family = "Raleway"))

wind = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-06/us_wind.csv")

view(wind)




# --- state counts
wind %>% 
  count(t_state, sort = T)


# --- project names
wind %>% 
  count(p_name, sort = T)



# install.packages('mapproj')
library(mapproj)
# -- plot the lat and long
wind %>% 
  ggplot( aes(x= xlong, y= ylat)) +
  geom_point()+
  ggdark::dark_mode()

# -- filter out the outlier 
wind %>% 
  filter(xlong < 100) %>% 
  ggplot( aes(x= xlong, y= ylat)) +
  geom_point() +
  coord_map()+
  ggdark::dark_mode()

# -- add state lines
wind %>% 
  filter(xlong < 100) %>% 
  ggplot( aes(x= xlong, y= ylat)) +
  borders("state") + # <<
  geom_point() +
  coord_map() +
  theme_void()+ # drop grid bg
  ggdark::dark_mode()


# -- filter out the non-continent US
wind %>% 
  filter(!t_state %in% c("AK","HI","GU","PR")) %>% 
  ggplot( aes(x= xlong, y= ylat, color=)) +
  borders("state") +
  geom_point() +
  coord_map() +
  theme_void() +
  ggdark::dark_mode()



# --- lump projects
wind %>% 
  filter(!t_state %in% c("AK","HI","GU","PR")) %>% 
  mutate(project_name = fct_lump(p_name, 6)) %>% 
  ggplot( aes(x= xlong, y= ylat, color= project_name, alpha= 0.7)) +
  borders("state") +
  geom_point() +
  coord_map() +
  theme_void()+
  ggdark::dark_mode()

# -- need to arrange the other to the bottom of list
wind %>% 
  filter(!t_state %in% c("AK","HI","GU","PR")) %>% 
  mutate(p_name = fct_lump(p_name, 10)) %>% 
  arrange(p_name != "Other") %>%  # arrange other 
  ggplot( aes(x= xlong, y= ylat, color= p_name)) +
  borders("state") +
  geom_point() +
  coord_map() +
  theme_void()+
  ggdark::dark_mode()

# -- map is still covered by 1 color





# -----------
# ------- deal with missing data values that are numeric
wind_processed = wind_processed %>% 
  filter(!t_state %in% c("AK","HI","GU","PR")) %>% 
  mutate(t_cap = ifelse(t_cap < 0, NA, t_cap)) %>% 
  mutate_if(is.numeric, ~ ifelse(. == -9999, NA, .))
# mutate if data is numeric and has -9999 then replace with NA

head(wind_processed)
# ------------



# --- just the continent US
wind_processed = wind %>% 
  filter(!t_state %in% c("AK","HI","GU","PR")) %>% 
  mutate(p_year = ifelse(p_year < -8000, NA, p_year),
         t_cap = ifelse(t_cap < 0, NA, t_cap))

# -- count just the turbines by project
wind_processed %>% 
  group_by(t_state, p_name) %>% 
  summarise(turbines = n()) %>% 
  filter(turbines > 2)

# --- count projects
wind_processed %>% 
  group_by( p_name, t_state) %>% 
  summarise(turbines = n()) %>% 
  count(sort= T)








# -- take the centroid of each project so it's just 1
# the standard deviation to see how spread out the turbines are
wind_projects = wind_processed %>% 
  group_by( p_name, t_state) %>% 
  summarise(year = min(p_year, na.rm=T),
            turbines = n(),
            long = mean(xlong),
            total_capacity = sum(t_cap, na.rm=T),
            lat = mean(ylat),
            long_sd = sd(xlong),
            lat_sd = sd(ylat)) %>% 
  ungroup()

# -- plot turbines by projects
wind_projects %>% 
  ggplot( aes(x= long, y= lat, size= turbines)) +
  borders("state") +
  geom_point() +
  coord_map() +
  theme_void()+
  ggdark::dark_mode()




# -- add color

library(viridis)

wind_projects %>% 
  ggplot( aes(x= long, y= lat, size= turbines, color= year)) +
  borders("state") +
  geom_point(alpha= 0.3, size= 3) +
  coord_map() +
  theme_void()  + 
  scale_color_viridis(option = "inferno" )+
  labs(
    title = "US Wind Turbines\n", 
    subtitle = "#TidyTuesday (2018) USA Wind Turbines (revised)",
    caption = "@StarTrek_Lt |  Credit: @drob\n") +
  theme(
    plot.subtitle = element_text(family = "Raleway",size = 11, colour = "#ffea5e", hjust = 0.5, ), 
    plot.caption = element_text(family = "Raleway",size = 12, colour = "#ffea5e", hjust = 0.2),
    panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(colour = NA, hjust = 0.25, vjust = 2.5),
    axis.text = element_text(family = "Raleway",size = 1, colour = NA, hjust = 1), 
    plot.title = element_text(family = "Raleway", size = 15, face = "bold", colour = "#ffea5e", vjust = 0.5, hjust = 0.5), 
    legend.text = element_text(family = "Raleway",colour = "#ffea5e", size = 9),
    legend.title = element_text(family = "Raleway",colour = "#ffea5e", size = 12),
    panel.background = element_rect(fill = "black", size = 2.1),
    plot.background = element_rect(fill = "black", colour = NA, size = 3.1), 
    legend.key = element_rect(fill = "black", colour = NA),
    legend.background = element_rect(fill = NA, size = 0.3),
    legend.position = "bottom", 
    legend.direction = "horizontal"
    ) 










# -- check out years for projects
wind_processed %>% 
  distinct(p_name, p_year) %>% 
  count(p_name, sort = T)

wind_processed %>% 
  filter(p_name =="San Gorgonio Farms Wind Farm") %>% 
  arrange(p_year) 


# -- count project by year sums
wind_processed %>% 
  filter(p_name =="San Gorgonio Farms Wind Farm") %>% 
  arrange(p_year) %>% 
  count(p_year, sort = T)


# -- histogram of turbines
wind_processed %>% 
  ggplot( aes(x= p_year)) +
  geom_histogram(bins = 60)









# --- project name and capacity
wind_processed %>% 
  distinct(p_name, p_cap) %>% 
  count(p_name, sort = T) %>% 
  top_n(10)

# -- project capacity count for 1 project
wind_processed %>% 
  filter(p_name =="McNeilus") %>% 
  count(p_cap, sort = T)

# -- projects by year
wind_projects %>% 
  filter(year < 2018) %>% 
  ggplot( aes(x= year, y= turbines)) +
  geom_point()+
  ggdark::dark_mode()



# wind turbine capacity over time
wind_projects %>% 
  ggplot( aes(x= year, y= total_capacity)) +
  geom_point() +
  geom_smooth(method = "lm")+
  ggdark::dark_mode()


# group by median capacity
wind_projects %>% 
  group_by(year) %>% 
  summarise(median_capacity = median(total_capacity)) %>% 
  ggplot( aes(x= year, y= median_capacity)) +
  geom_line() +
  ggdark::dark_mode()


wind_projects %>% 
  group_by(year) %>% 
  summarise(median_capacity = median(total_capacity),
            projects = n()) %>% 
  arrange(year)


# -- year and turbines
wind_projects %>% 
  filter( year < 2018) %>% 
  ggplot( aes(x=year, y= turbines)) +
  geom_point()+
  ggdark::dark_mode()
  
# -- turbine capacity and turbines by year
wind_projects %>% 
  filter( year < 2018) %>% 
  ggplot( aes(x=year, y= total_capacity /turbines)) +
  geom_point( color="green" ) +
  geom_smooth(method = "lm") +
  labs(title = "US Wind Turbines by Decade",
       x = NULL, 
       y="turbine capacity (kW)",
       subtitle = "#TidyTuesday (2018) US Wind Turbines data. ",
       caption = "@StarTrek_Lt")+
  theme(
    plot.subtitle = element_text(size = 11, colour = "gray80"), 
    plot.caption = element_text(colour = "gray90", vjust = -5), 
    axis.line = element_line(colour = "gray",linetype = "solid"), 
    axis.ticks = element_line(colour = "white"),
    panel.grid.major = element_line(colour = "gray46"),
    panel.grid.minor = element_line(colour = "gray46"),
    axis.title = element_text(colour = "white"),
    axis.text = element_text(size = 12, colour = "white"),
    axis.text.x = element_text(size = 11, colour = "white"), 
    axis.text.y = element_text(size = 11,colour = "white"), 
    plot.title = element_text(face = "bold", colour = "white"), 
    panel.background = element_rect(fill = "gray15"),
    plot.background = element_rect(fill = "gray15")) +
  ggdark::dark_mode()

  
# -- plot wind capacity
wind_projects %>% 
  ggplot( aes(x=long, y= lat, size= turbines, color= total_capacity)  ) +
  borders("state") +
  geom_point( alpha= 0.9) +
  coord_map() +
  theme_void() + 
  scale_colour_gradientn(colours= c("#ff66cc","#99ff99","#8000ff") ) +
  labs(title = "US Wind Turbines by total capacty (kW)",
    subtitle = "#TidyTuesday USA Wind Turbine data",
    caption = "By: Zane Dax")  + theme(plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) + theme(axis.line = element_line(colour = NA),
    axis.ticks = element_line(colour = NA),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    plot.title = element_text(face = "bold")) + 
  theme(panel.background = element_rect(fill = "gray30"),
    plot.background = element_rect(fill = "gray30"))















# --- turbine models
wind_processed %>% 
  filter(t_model != "missing" ) %>% 
  count(t_model, sort = T) %>% 
  top_n(20) %>% 
  ggplot( aes(x= t_model, y= n)) +
  geom_col() +
  coord_flip()
  
# -- model GE 1.5-77 is most popular model
wind_processed %>% 
  filter(t_model != "missing") %>%   #  t_model !="GE1.5-77"
  count(t_model, sort = T) %>% 
  mutate(t_model = fct_reorder(t_model, n)) %>% 
  top_n(20) %>%
  ggplot( aes(x= t_model, y= n, fill= t_model)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(title = "Wind Turbine models",
       x="Model Name", y="Turbine Count")


library(plotly)
wturbs = wind_processed %>% 
  filter(t_model != "missing",
         p_year > 1998 ) %>%
  group_by(t_model, p_year) %>% 
  count(t_model, sort = T) %>% 
  ggplot( aes(x= factor(p_year), y= n,  alpha= 0.5)) + #size= t_model,
  geom_point( ) +
  coord_flip() +
  theme(legend.position = "none")+ 
  scale_colour_gradientn(colours= c("#339933","#ff33cc") )+
  ggdark::dark_mode()

ggplotly(wturbs)





GE_turbine = wind %>% 
  filter(t_model =="GE1.5-77",
         !t_state %in% c("AK","HI","GU","PR")) %>% 
  transmute(year = p_year, long = xlong, lat = ylat, turbines = p_tnum, turbine_capacity = t_cap)


GE_turbine %>% 
  ggplot( aes(x= long, y= lat, size= turbines,  color= year, alpha= 0.3)) +
  borders("state") +
  geom_point() +
  coord_map() +
  theme_void() + 
  scale_colour_gradientn(colours= c("#33ff99","#004d26") ) +
  ggdark::dark_mode()







# ----- summarise_at(vars( (<columns>), <function> , na.rm= T)
wind_processed %>% 
  group_by(t_model) %>% 
  summarise_at(vars(t_hh, t_rd, t_rsa, t_ttlh), median)
# -------


turbine_models = wind_processed %>% 
  group_by(t_model) %>% 
  summarise(t_cap = median(t_cap), # turbine capacity (kW)
            t_hh = median(t_hh), # turbine hub height (meters)
            t_rd = median(t_rd), # turbine rotor diameter (meters)
            t_rsa= median(t_rsa), # turbine rotor swept area (meters^2)
            t_ttlh = median(t_ttlh), # turbine total height - calculated (meters)
            turbines = n(),
            projects = n_distinct(p_name)) %>% 
  arrange( desc(projects))

# ---  turbine height vs capacity
turbine_models %>% 
  mutate(turbine_capacity = t_cap,
         turbine_ht_m = t_ttlh,
         rotor_diameter = t_rd ) %>% 
  ggplot( aes(x= turbine_ht_m, y= turbine_capacity, color= rotor_diameter)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "US Turbines height vs capacity", 
       x= "Turbine total height (meters)",
       y= "Turbine capacity (kW)") +
  scale_color_gradientn(colors = c("#33cccc","#8c1aff" ))+
  ggdark::dark_mode()
  

























