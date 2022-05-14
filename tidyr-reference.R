
# BOOKDOWN REFERENCE

library(tidyverse)
library(showtext)
library(ragg)

font_add(family="regular", "Lato-Regular.ttf")
showtext_auto()


font_add(family="regular", "Lato-Regular.ttf")
showtext_auto()

#--------themes
my_theme <- theme(
  #titles
  plot.title=element_text(family="regular", hjust=0.5, size=25, color="white"),
  plot.title.position = "plot",
  plot.caption=element_text(family="regular", size=15, color="#555a72", hjust=0.5),
  plot.caption.position = "plot",
  plot.subtitle = element_text(family = 'regular', size = 15, colour = 'white', hjust = 1),
  plot.subtitle.position='plot'
  #background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#0C1335"),
  plot.background = element_rect(fill = "#0C1335"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  # axis.title = element_blank(),
  axis.text.y = element_text(size=15, family="regular", color="white"),
  axis.text.x = element_text(size=15, family="regular", color="white"),
  # axis.text.x = element_blank(),
  axis.title = element_text(size = 13, colour = "white", family = 'regular'),
  #no legend
  legend.position = "right",
  legend.title = element_blank(),
  legend.background = element_rect(fill = "#0C1335", color = "#0C1335"),
  legend.key = element_rect(fill="#0C1335", color="#0C1335"),
  legend.text = element_text(size=15, family="regular", color="white")
  


# diamonds = diamonds

# large dataset => over-plotting, reduce density, use summarize

# head(diamonds)
# 
# diamonds %>% 
#   ggplot( aes(x= cut,y= price, fill=cut))+
#   geom_point()



diamonds %>% 
  ggplot( aes(x= cut,y= price))+
  geom_jitter(height = 0, color='purple', alpha=0.25, size= 0.5)+
  coord_flip() + theme(panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(colour = "white"),
    axis.text = element_text(size = 12, colour = "white", family = 'regular'),
    panel.background = element_rect(fill = "gray0"),
    plot.background = element_rect(fill = "black"))+
  labs(title = "Diamonds Data",
       subtitle = "Diamonds price based on quality",
       y="Quality Type",
       x="Price")   + 
  theme(plot.subtitle = element_text(size = 13,
    colour = "white", family = 'regular'),
    axis.title = element_text(size = 13, colour = "white"),
    plot.title = element_text(size = 15, colour = "white") )+
  my_theme + theme(plot.subtitle = element_text(family = "sans",
    hjust = 0.5), plot.caption = element_text(family = "sans",
    hjust = 1.5), axis.line = element_line(colour = "white"),
    axis.text = element_text(family = "sans"),
    plot.title = element_text(family = "sans"),
    legend.text = element_text(family = "sans"))+ theme(axis.line = element_line(colour = NA))




library(scales)

diamonds %>% 
  ggplot(aes(x=cut, y=price)) + 
  geom_col()+
  scale_y_continuous(labels = dollar_format())



diamonds %>% 
  ggplot(aes(x = cut, y= price, fill=clarity)) + 
  geom_col(position='dodge')

diamonds %>% 
  ggplot(aes(x = price)) +
  geom_histogram(bins=200, fill = 'purple') +
  xlim(0,2500)


# density plot
diamonds %>% 
  ggplot(aes(x = price)) + 
  geom_density(aes(fill =cut, alpha=0.5))




diamonds %>% 
  ggplot(aes(x=cut, y=price)) + 
  geom_jitter(height=0, alpha=0.25, size=0.5, width=0.3, color='#009999') +
  geom_boxplot(alpha=0)



starwars = starwars

starwars = starwars %>% 
  mutate(name = as_factor(name),
         eye_color = as_factor(eye_color),
         gender = as_factor(gender),
         species = as_factor(species),
         sex = as_factor(sex))

starwars %>% 
  filter(eye_color =="yellow") %>% dim()

starwars %>% 
  select(eye_color, species, homeworld) %>% 
  filter(eye_color =="yellow") %>% 
  group_by(eye_color, species) %>% 
  count(eye_color, sort = T)


starwars %>% 
  filter(eye_color =="yellow" & height>100 )

starwars %>% 
  filter(eye_color =="yellow" | species=='human' )



starwars %>% 
  filter(str_detect(name, "Skywalker"))

starwars %>% 
  filter(eye_color %in% c('yellow','green')) %>% 
  group_by(eye_color)


starwars %>% 
  filter(is.na(birth_year)) %>% 
  count()

starwars %>% 
  filter(!is.na(birth_year))




#  select with - , the except
starwars %>% select(-height,-mass)

# select by column position
starwars %>% select(1,4,6:9)

# contains() helper function, selects columns
starwars %>% select( contains('y'))

starwars %>% select( starts_with('s'))

# everything() function
starwars %>% select(name, eye_color, homeworld, everything())

#  arrange()
starwars %>% arrange(height)

starwars %>% arrange(height, birth_year)

starwars %>% arrange( desc(height))


starwars %>% filter(eye_color=='yellow') %>% 
  select(name, height, species) %>% 
  arrange(height)

# save to a file, tsv
# write_tsv(mpg_slim, '~/Desktop/StarWars_df.tsv')


# mutate
diamonds %>% 
  select(-x,-y,-z) %>% 
  mutate(AUD = price * 1.25)

diamonds %>% 
  mutate(weight = carat * 0.2,
         AUD = price * 1.25,
         AUD_gram = AUD/weight)


# IF ELSE
# ifelse() is a function that tests each value in a column of data for a particular condition
# 
# ifelse( condition,  output_True,  output_False)

diamonds %>% 
  select(-x,-y,-z) %>%         
  mutate(price_label = ifelse(price > 5000,'expensive','cheap')) %>% 
  ggplot( aes(x=price, fill=price_label))+
  geom_histogram(binwidth = 20)


# CASE_WHEN
# tests multiple conditions, order of rows matters
# need to be specific to general
# the catch-all is condition after the ~

# case_when(condition ~ output_True)    

# clarity : Levels: I1 < SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF

diamonds %>% 
  select(clarity) %>% 
  mutate(clarity_group = case_when(clarity =='IF' ~ 'flawless',
                                   str_detect(clarity,'VVS') ~'VV_slight',
                                   str_detect(clarity,'VS') ~ 'V_slight',
                                   str_detect(clarity,'SI') ~ 'slight',
                                   clarity =='I1' ~ 'impurity',
                                   TRUE ~ 'other'
                                   )) %>% 
  count(clarity_group, sort = T)



#  summarize
diamonds %>% 
  summarise(avg_price = mean(price))

diamonds %>% 
  summarise(avg_price = mean(price),
            price_std = sd(price),
            min_price = min(price),
            max_price = max(price))


#  n = n() just counts rows


# group_by() and summarize()
diamonds %>% 
  select(-x,-y,-z) %>% 
  group_by(cut) %>% 
  mutate(cut_avg_price = mean(price))


diamonds %>% 
  select(-x,-y,-z) %>% 
  group_by(cut) %>% 
  mutate(cut_avg_price = mean(price),
         price_diff = price - cut_avg_price) 


# ungroup()  to avoid slow code, weird results


diamonds %>% count(cut)


# ------- sample_n()  random sample of rows
diamonds %>% sample_n(10)

# sample subgroups, 2 of each cut group type
diamonds %>% group_by(cut) %>% sample_n(2)


diamonds %>% 
  select(-x,-y,-z) %>% 
  mutate(weight = carat * 0.2,
         Price_diff = max(price) - price) %>% 
  group_by(clarity, weight) %>% 
  arrange( desc(price,weight))

diamonds%>% 
  group_by(clarity) %>% 
  summarize(maxPrice = max(price))





# principals for tidy data:
#   
# each column represents a single measurement type
# each row represents a single observation
# each cell contains a single value

# library(readxl)
# read_excel()

# read_delim()  .txt files

# read_tsv()    tab separated 

# pivot_wider() makes more columns
# pivot_wider( names_from=, values_from=)

# pivot_longer()  makes more rows
# pivot_longer( cols= , names_to= , values_to=)

# separate() takes column and splits it into new columns
# separate( df, into = c('',''), sep = '-')

# separate_rows()  makes new row for each value in column

# rename( new_col_name= existing_name ) 
diamonds = diamonds %>% 
  rename(gem_cut = cut)

# unite( new_col_name, c(<columns>), sep=':') 


# left_join()  column shared in both dataframes 
df_A = data_frame(
  x1 = LETTERS[1:3],
  x2 = 1:3
)
df_B = data_frame(
  x1 = c('A','B','D'),
  x2 = c('T','F',T)
)

left_joined = left_join(df_A, df_B, by='x1')
left_joined


# ====== FOR LOOPS
i = 3
r = i*80
print(r)

loops = 1:9

for(i in loops){
  r = i * 7
  print(r)
}


# = paste()
paste("the weather is","sunny")

weather = c('sunny','raining','snowing')
for (i in weather){
  r = paste('the weather is ', i)
  print(r)
}

# -- save for loop results in a vector 
loop_vector = vector()
for (i in weather) {
  step_r_vector = paste('weather is',i)
  print(step_r_vector)
  loop_vector = c(loop_vector, step_r_vector)
}

loop_vector[2]

looped_df = data_frame()

for (i in weather) {
  step_r_vector = paste('the weather is', i)
  step_df_r = data_frame('weather' = step_r_vector)
  print(step_df_r)
  looped_df = bind_rows(looped_df, step_df_r)
}

weather







#C opy and paste the code to make a toy data frame
df <- data.frame(
  x = rep(c(2, 5, 7, 9, 12), 2),
  y = rep(c(1, 2), each = 5),
  z = factor(rep(1:5, each = 2)),
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
)

ggplot(df, aes(x, y, width = w)) +
  geom_tile(aes(fill = z), colour = "grey50")




# str_remove() for columns with rows that have char
#               new column          old-name, what to remobe
# df %>% mutate(RowNumber = str_remove(ROW,'R'))





starwars


mpg %>% mutate(cyl_factor=factor(cyl)) %>% 
  ggplot(aes(x=cyl_factor, y=cty)) + 
  geom_point(aes(col=cyl_factor))

# === forcats relevel
mpg_cyl_relevel <- mpg %>% 
  mutate(cyl_factor=factor(cyl)) %>% 
  mutate(cyl_factor = fct_relevel( cyl_factor, '8','6','5','4')) 

mpg_cyl_relevel %>%   ggplot(aes(x=cyl_factor, y=cty)) + geom_point(aes(col=cyl_factor))







mpg_class_factor <- mpg %>% 
  mutate(class_factor = factor(class)) %>% 
  mutate(class_factor = fct_relevel(class_factor, levels = 'compact', 'subcompact')) 

# -- facet_wrap
mpg_class_factor %>% 
  ggplot(aes(x=cty,y=hwy)) + 
  geom_point(aes(col=class_factor)) +
  facet_wrap(~class_factor, scales='free') 






mpg %>% 
  ggplot(aes(y=class)) + geom_bar()


# 
# This is a two-step problem. The simplest way to go about it is to 
# 1) bring compact and subcompact cars to the front of the factor 
#   levels using fct_relevel() as above; then, 
# 2) reverse the levels of the newly created factor levels using fct_rev()

mpg_class_relevel <- mpg %>% 
  mutate(class_factor= factor(class)) %>% 
  mutate(class_factor_relevel  = fct_relevel(class_factor,'2seater',
                                             'compact','subcompact',
                                             'midsize','minivan','pickup','suv'))

mpg_class_relevel %>%  
  ggplot(aes(y=class_factor_relevel)) + 
  geom_bar( )


mpg_class_relevel_rev <- mpg_class_relevel %>% 
  mutate(class_factor_relevel_rev  = fct_rev(class_factor_relevel))

mpg_class_relevel_rev %>% ggplot(aes(y=class_factor_relevel_rev)) + geom_bar()
mpg_class_relevel_rev %>% 
  ggplot(aes(y=class_factor_relevel_rev)) + 
  geom_bar(aes(fill=class_factor_relevel_rev))

mpg_class_relevel_rev %>% 
  ggplot(aes(y=class_factor_relevel_rev)) + 
  geom_bar(aes(fill=class_factor_relevel_rev)) +
  guides(fill = guide_legend( reverse=T ))











