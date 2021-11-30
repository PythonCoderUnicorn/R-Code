
library(tidyverse)

map(iris, mean)

map_df(iris, mean)

map_df(iris[1:4], mean)

mtcars

print_mpg = function(x, y){
  paste(x,"gets",y,"miles per gallon")
}

map2_chr( # char output
  rownames( mtcars ), # carnames are in the rows
  mtcars$mpg, #  column
  print_mpg  # function 
)


map2_chr(rownames(mtcars),
         mtcars$mpg,
         function(x,y)
         paste(x,"gets",y,"miles per gallon")
         )


peng = palmerpenguins::penguins

print_body_mass = function(x, y){
  paste(x, "has", y, "body mass")
}

map2_chr(
  rownames(peng), # length of rows in df
  peng$body_mass_g, # column 
  print_body_mass
)



ggplot(
  mtcars,
  aes(x= mpg,
      y= cyl,
      fill= as.character(carb),
      # shape= as.character(gear),
      size= as.character(gear),
      color= as.character(gear)
      )
)+
  geom_point(
    # size=4,
    stroke= 1.2)+
  scale_shape_manual(breaks = c(3, 4, 5), 
                     values = c(21,24,22)
                     # guide= guide_legend(override.aes = list(fill='green', 
                     #                                         color='green'))
                     )+
  scale_color_manual(breaks= c(3,4,5),
                     # values = c('purple','orange','green')
                     values = c('green','purple', 
                                'red')
                     )+
  scale_size_manual(breaks = c(3,4,5),
                    values = c(3,4,5))+
  scale_fill_manual(breaks = c(1,2,3,4,6,8),
                    values = rainbow(6),
                    guide= guide_legend(override.aes =  list(color= rainbow(6),
                                                             shape= 18,
                                                             size=5
                                                             )
                                        )
                    )+
  ggdark::dark_mode()




