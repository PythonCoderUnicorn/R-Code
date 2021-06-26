
# Danielle Navarro _Flametree artwork _GitHub

library(flametree)
flametree_grow(seed = 2, trees = 3) %>% 
  flametree_plot(style = "voronoi")


flametree_grow(trees = 6) %>% flametree_plot()
flametree_grow(time = 10) %>% flametree_plot()
flametree_grow(trees = 3, time = 10) %>% flametree_plot()


flametree_grow(scale = c(.5, .8, 1.1)) %>% flametree_plot()
flametree_grow(scale = c(.8, .9)) %>% flametree_plot()

flametree_grow(angle = c(-20, 5, 10)) %>% flametree_plot()
flametree_grow(angle = -10:10) %>% flametree_plot()

# =============================
flametree_grow(time = 10, trees = 10) %>% 
  flametree_plot(
    palette = c(
      "#A06AB4", # lavender
      "#FFD743", # gold
      "#07BB9C", # blue green
      "#D773A2"  # lilac
    )
  )


shades <- c("#A06AB4", "#FFD743", "#07BB9C", "#D773A2")

flametree_grow(time = 14) %>% 
  flametree_plot(
    palette = shades, 
    style = "plain"
  )


# =================
flametree_grow(
  time = 10, 
  trees = 12, 
  shift_x = spark_nothing()
) %>% 
  flametree_plot(
    palette = shades, 
    style = "nativeflora"
  )




