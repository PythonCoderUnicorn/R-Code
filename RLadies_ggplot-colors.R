
# RLadies ggplot colors

library(tidyverse)
library(systemfonts)
# library(showtext)
library(ggplot2)

# theme_set(theme_bw(base_family = "Lato"))

penguins = palmerpenguins::penguins

penguins = penguins %>% drop_na()

head(penguins)

penguins_flipper = ggplot(penguins,
                           aes(x= flipper_length_mm,
                               y= body_mass_g) ) +
  geom_point(show.legend = F) +
  labs(title = "Palmer Penguins Flipper Length & Body Mass")+
  ggdark::dark_mode()


penguins_flipper = penguins_flipper + aes(col= body_mass_g)


penguins_bill = ggplot(
  penguins,
  aes(x= bill_length_mm, 
      y= bill_depth_mm)
)+
  geom_point()+
  # geom_smooth(method = 'lm')+
  labs(title = "Palmer Penguins Bill Length")+
  ggdark::dark_mode()

penguins_bill = penguins_bill+ 
  aes(col= species, fill= species)



penguins_island = penguins %>% 
  filter(island != "Torgersen") %>% 
  ggplot( aes(x= island) )+
  geom_bar(position = 'dodge') +
  labs(title = "Penguin Islands")+
  ggdark::dark_mode()




# ================= color time
penguins_island +
  aes(fill= island )

penguins_island +
  aes(col = island)

(penguins_island + aes(fill= species))


penguins_flipper +
  aes(col= body_mass_g)


penguins_bill +
  aes(col= species, fill= species)




# custom colors

colors()



# --- manual
penguins_island +
  aes(fill= species ) +
  scale_fill_manual(values =  c('orange2',
                                'lawngreen',
                                'purple2'))


penguins_island +
  aes(fill= species ) +
  scale_fill_manual(values =  c('turquoise1',
                                'purple3',
                                'lawngreen'))


penguins_island +
  aes(fill= species ) +
  scale_fill_manual(values =  c('paleturquoise3',
                                'mediumpurple3',
                                'steelblue2')) +
  scale_color_manual(values =  c('paleturquoise4',
                                'mediumpurple4',
                                'steelblue3'))



# ---- gradient

penguins_flipper +
  aes(color= flipper_length_mm)+
  scale_color_gradient(low = 'sienna', # great combo ! 
                       high = 'blue')


penguins_flipper +
  aes(color= body_mass_g)+
  scale_color_gradient2(
    low = 'yellow', high = 'pink',
    mid = 'purple', midpoint = 5000
  )


penguins_flipper +
  aes(color= body_mass_g)+
  scale_color_gradientn(
    colors = c('blue','tomato1','orange','yellow')
  )



penguins_flipper +
  aes(color= body_mass_g)+
  scale_color_gradient(
    low = "#a5de85",
    high = "#ff6083"
  )



# ============ PALETTES

library(RColorBrewer)

display.brewer.all() # shows color palettes
# 1st group = numerical
# 2nd group = categorical
# 3rd group = gradients, averages


penguins_bill +
  aes(col = species)+
  scale_color_brewer(palette = 'YlGnBu')+
  scale_fill_brewer(palette = "YlGnBu")

penguins_bill +
  aes(col = species)+
  scale_color_brewer(palette = 'Dark2')+
  scale_fill_brewer(palette = "Dark2")

penguins_bill +
  aes(col = species)+
  scale_color_brewer(palette = 'Spectral')+
  scale_fill_brewer(palette = "Spectral")


penguins_bill +
  aes(col = species)+
  scale_color_brewer(palette = 'Spectral',
                     direction = -1)+
  scale_fill_brewer(palette = "Spectral",
                    direction = -1)




# ---- continuous data palette
penguins_flipper +
  scale_color_distiller(palette = "Greens")

penguins_flipper +
  scale_color_distiller(palette = "Accent")

penguins_flipper +
  scale_color_distiller(palette = "RdYlBu", 
                        direction = 1)


# -------- viridis
library(viridis)

# scale_color_viridis() areas, points, lines
# scale_fill_viridis() large areas, bar, boxplots

penguins_flipper +
  # scale_color_viridis(option = "inferno") #
  # scale_color_viridis(option = "plasma")  #
  # scale_color_viridis(option = "magma")   #
  # scale_color_viridis(option = "mako")#-
  # scale_color_viridis(option = "viridis")#-
  # scale_color_viridis(option = "rocket")
  scale_color_viridis(option = "turbo") # ++



# --- discrete/ groups/ categories variables 
penguins_bill +
  scale_color_viridis(option ='inferno' , 
                      discrete = T,
                      direction = -1)+
  scale_fill_viridis(option ='inferno' , 
                     direction = -1,
                     discrete = T)






# -----------------------
library(paletteer)
library(scico)

# discrete: scale_color_paleteer_d()
# continuous: scale_color_paleteer_c()

# get names of palettes for continuous
palettes_c_names

# get names of palettes for discrete
palettes_d_names


palettes_c_names %>% 
  filter(package =="scico" & type=="diverging")


# ---- continuous data
penguins_flipper +
  # scale_color_paletteer_c("ggthemes::Orange")
  # scale_color_paletteer_c("scico::bamako")
  scale_color_paletteer_c("scico::vik")


# --- discrete data
penguins_bill +
  # scale_color_paletteer_d("futurevisions::earth")+
  # scale_fill_paletteer_d("futurevisions::earth")
  scale_color_paletteer_d("rockthemes::alice")+
  scale_fill_paletteer_d("rockthemes::alice")












