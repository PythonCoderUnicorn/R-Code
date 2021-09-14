
# generative art

# devtools::install_github("cutterkom/generativeart")
library(generativeart)
library(ggplot2)



# set the paths
IMG_DIR <- "art/"
# IMG_SUBDIR <- "art"
# IMG_SUBDIR2 <- "made/"
IMG_PATH <- paste0(IMG_DIR)

LOGFILE_DIR <- "art/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)






# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(2, 5, 90) * x_i^2 - sin(y_i^15)),
  y = quote(runif(2, 5, 90) * y_i^2 - cos(x_i^15))
)


# call the main function 
generativeart::generate_img(formula = my_formula, 
                            nr_of_img = 1, 
                            polar = TRUE, 
                            filetype = "png", 
                            color = "#ace619", 
                            background_color = "black")


# 
# my_formula <- list(
#   x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
#   y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
# )
# 
# generativeart::generate_img(formula = my_formula,
#              nr_of_img = 1, 
#              polar = TRUE, 
#              color = "pink", 
#              background_color = "black")
# 
# 
# 
# 

















