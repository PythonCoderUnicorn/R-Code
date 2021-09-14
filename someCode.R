

# cominations

library(combinat)

# all the permutations for 3
p = permn(3)
p

length(permn(3))

#  combinations
combi = combinat::combn(3, 2)
combi

# readr::parse_number

n_colors = 10
palette1 = rainbow(n_colors)
pie( rep(1, n_colors),
     col = palette1,
     main = "grDevices Pkg rainbow()" )

#  list all colors
all_colors = grDevices::colors()
all_colors

noGreys = all_colors[grep("gr(a|e)y", grDevices::colors(), invert = T)]
noGreys

palette2 = sample(noGreys, n_colors)
palette2
pie(rep(1, n_colors),
    col= palette2,
    radius = 1,
    main = "grDevice pkg colors()")


library(randomcoloR)
palette4 = distinctColorPalette(n_colors)
palette4

pie(rep(1, n_colors), col= palette4, edges= 100, radius = 5, main = "randomcoloR pkg")

pie(rep(1, n_colors), col = palette4, main = "randomcoloR pkg", edges = 200)


library(crayon)
cat(blue("Hello", "world!\n"))




library(rtrek)

rtrek::st_datasets()

library(trekcolors)
library(trekfont)






library(ggplot2)
library(ggdark)

p <- ggplot(diamonds) + 
  geom_point(aes(carat, price, color = cut)) + 
  scale_y_continuous(label = scales::dollar) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(title = "Prices of 50,000 round cut diamonds by carat and cut",
       x = "Weight (carats)",
       y = "Price in US dollars",
       color = "Quality of the cut")

# p + theme_gray()  # ggplot default
# p + dark_theme_gray()
p + dark_theme_minimal()





invert_geom_defaults()  # change geom defaults back to black

library(gapminder)
library(fivethirtyeight)

p <- ggplot(subset(gapminder, continent != "Oceania")) +
  geom_line(aes(year, lifeExp, group = country, color = country), lwd = 1, show.legend = FALSE) + 
  facet_wrap(~ continent) +
  scale_color_manual(values = country_colors) +
  labs(title = "Life expectancy has increased worldwide")
p + dark_mode(theme_fivethirtyeight())
#> Inverted geom defaults of fill and color/colour.
#> To change them back, use invert_geom_defaults().









