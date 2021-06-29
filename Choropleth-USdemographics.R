
#----- Choropleth of US Demographics
# By; Zane Dax
# June 29, 2021
# Note: Tutorial by Julia Silge

# ---------------------- load libraries
library(choroplethr)
library(ggplot2)
library(RColorBrewer)

# ---------------------- load dataframe
data(df_county_demographics)

# - column names
colnames(df_county_demographics)


# -------------------- choropleth of state and population
library(choroplethrMaps)

df_county_demographics$value <- df_county_demographics$total_population
choro = CountyChoropleth$new(df_county_demographics)
choro$set_zoom("washington")
choro$title = "Where Do People in Washinton Live?"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Population", 
                                          colours = brewer.pal(8, "GnBu"))
choro$render()+ theme(plot.subtitle = element_text(size = 11,
                                                   colour = "gray"), 
                      plot.caption = element_text(size = 10,
                                                  colour = "gray", hjust = 0), 
                      panel.grid.major = element_line(linetype = "blank"),
                      panel.grid.minor = element_line(linetype = "blank"),
                      axis.title = element_text(colour = "white"),
                      axis.text = element_text(colour = "gray"),
                      plot.title = element_text(colour = "white",
                                                vjust = 1), legend.text = element_text(colour = "white"),
                      legend.title = element_text(colour = "white"),
                      panel.background = element_rect(fill = "gray20"),
                      plot.background = element_rect(fill = "gray20",
                                                     size = 1), 
                      legend.background = element_rect(fill = "gray20")) +labs(title = "Where did people in Washington Live in 2010?",
                                                                                                                           subtitle = "Choropleth of US Demographics using Choroplethr R package",
                                                                                                                           caption = "Made by Zane Dax, Credit: Julia Silge ")+ theme(legend.background = element_rect(size = .75))+ theme(legend.position = "left")



# --------------

df = df_county_demographics

# what was the median rent in Washington in 2010?
df$value <- df$median_rent
choro = CountyChoropleth$new(df)
choro$set_zoom("washington")
choro$title = "What was the median rent price in Washington in 2010?"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Median Rent ($)", 
                                          colours = brewer.pal(8, "GnBu"))
choro$render()  + theme(plot.caption = element_text(size = 12,
    colour = "gray", hjust = 0), panel.grid.major = element_line(linetype = "blank"),
    panel.grid.minor = element_line(linetype = "blank"),
    axis.title = element_text(colour = "white",
        vjust = 1.5), axis.text = element_text(colour = "gray",
        vjust = 0, angle = -10), plot.title = element_text(colour = "white"),
    legend.text = element_text(colour = "white"),
    legend.title = element_text(colour = "white"),
    panel.background = element_rect(fill = "gray20"),
    plot.background = element_rect(fill = "gray20",
        colour = NA, size = 3.5), legend.key = element_rect(colour = "white"),
    legend.background = element_rect(fill = NA,
        size = 0.3)) +labs(caption = "Made by: Zane Dax, Credit: Julia Silge")



# -------------- I checked for White and Asian then Black demographics
# what is the ethnicity in Washington in 2010?
df$value <- df$percent_black
choro = CountyChoropleth$new(df)
choro$set_zoom("washington")
choro$title = "what was the % of Black people in Washington in 2010?"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Percent (%)", 
                                          colours = brewer.pal(8, "GnBu"))
choro$render()  + theme(plot.caption = element_text(size = 12,
                                                    colour = "gray", hjust = 0), panel.grid.major = element_line(linetype = "blank"),
                        panel.grid.minor = element_line(linetype = "blank"),
                        axis.title = element_text(colour = "white",
                                                  vjust = 1.5), axis.text = element_text(colour = "gray",
                                                                                         vjust = 0, angle = -10), plot.title = element_text(colour = "white"),
                        legend.text = element_text(colour = "white"),
                        legend.title = element_text(colour = "white"),
                        panel.background = element_rect(fill = "gray20"),
                        plot.background = element_rect(fill = "gray20",
                                                       colour = NA, size = 3.5), legend.key = element_rect(colour = "white"),
                        legend.background = element_rect(fill = NA,
                                                         size = 0.3)) +labs(caption = "Made by: Zane Dax, Credit: Julia Silge")







