
# aRtistry --- contouR pkg

library(tidyverse)
library(ggplot2)

devtools::install_github("Ijeamakaanyene/contouR")




library(contouR)

#set up your data
setup = contour_grid(grid_size = 900, 
                     point_dist = .5, 
                     z_method = "runif", 
                     z = -10, 
                     z_span = 3,
                     ) %>%
  contour_shape(radius = 300.2, 
                x_center = 1, 
                y_center = 70,
                ) 



# plot your data
contour_plot(setup$grid_shape,
             background_col = 'black',
             line_col = '#d7fc03' ) +
  ggplot2::xlim(1, 30) +
  ggplot2::ylim(1, 30) 
