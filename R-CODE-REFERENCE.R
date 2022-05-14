
# Statistics Globe - R reference


#============== LIBRARIES
pacman::p_load(
  tidyverse,
  ggplot2,
  lubridate
)
# Sys.Date()

# Terms ========================================================================== 
# “Rescaling” --  a vector means to add or subtract 
#     a constant and then multiply or divide by a constant, 
#     as you would do to change the units of measurement of the data, 
#     for example, to convert a temperature from Celsius to Fahrenheit.

# “Normalizing” -- a vector most often means dividing by a norm of the vector. 
#     It also often refers to rescaling by the minimum and range of the vector, 
#     to make all the elements lie between 0 and 1 thus bringing all the values of 
#     numeric columns in the dataset to a common scale.
# 
# Normalization is useful when your data has varying scales and the algorithm you 
#     are using does not make assumptions about the distribution of your data, such as KNNs etc.

# “Standardizing” -- a vector most often means subtracting a measure of location 
#     and dividing by a measure of scale. For example, if the vector contains random values 
#     with a Gaussian distribution (bell curve), you might subtract the mean and divide by the standard 
#     deviation, thereby obtaining a “standard normal” random variable with mean 0 and standard deviation 1.

# Standardizing the features around the center and 0 with a standard deviation of 1 
#     is important when we compare measurements that have different units. 
#     Variables that are measured at different scales do not contribute equally to the analysis 
# ========================================================================== 





#============ convert integer into categorical data
num1 = c(1:9)

Factor1 = as.factor(num1)

set.seed(2021)
num2 = rnorm(100) # random distributed
head(num2)

# empty data object
Factor2 = numeric()
# factor assignments
Factor2[num2 < -1] = 1
Factor2[num2 >= -1 & num2 < 0] = 2
Factor2[num2 >= 0 & num2 < 1] = 3
Factor2[num2 >= 1] = 4

# convert numeric to factor
Factor2 = as.factor(Factor2)
head(Factor2)
# ========================


# ====================== convert df column to numeric
df = data.frame(
  x1 = c(1:5),
  x2 = c(7:11),
  x3 = c(13:17))

df$x1 = as.factor(df$x1) # converts int to factors
df$x1



# ================ scale a dataframe
# create dataframe
df = data.frame(column.1 = runif(100),
                column.2 = runif(100))
head(df)

# scale df - 1
scaled_df = scale(df)
head(scaled_df)

# scale df - 2 
scaled_df.2 = df %>% 
  mutate_at( c('column.1', 'column.2'), ~(scale(.) %>% as.vector ))
head(scaled_df.2)

# ===========================





#=========================== heatmap with clusters
# rename column/ row names
set.seed(354896)

# dataframe
df = matrix( round(rnorm(150), 2), nrow = 15)
colnames(df) = LETTERS[1:10]
rownames(df) = letters[1:15]

view(df)

# use pheatmap pkg
library(pheatmap)
# basic default heatmap
pheatmap(df)


# Heatmap with k-Means clusters
pheatmap(df, kmeans_k = 4)

# Heatmap with row clusters
pheatmap(df, cutree_rows = 4)

# Heatmap with row & column clusters
pheatmap(df, cutree_rows = 4, cutree_cols = 3)

#=========================== 





#=========================== Standard Error, t-value, p-value, Linear Regression
# dataframe
set.seed(2021)
x1 <- round(rnorm(1500), 2)
x2 <- round(rnorm(1500) - 0.1 * x1, 2)
x3 <- round(rnorm(1500) + 0.1 * x1 - 0.5 * x2, 2)
x4 <- round(rnorm(1500) - 0.4 * x2 - 0.1 * x3, 2)
x5 <- round(rnorm(1500) + 0.1 * x1 - 0.2 * x3, 2)
x6 <- round(rnorm(1500) - 0.3 * x4 - 0.1 * x5, 2)
y <- round(rnorm(1500) + 0.5 * x1 + 0.5 * x2 + 0.15 * x3 - 0.4 * x4 - 0.25 * x5 - 0.1 * x6, 2)
data <- data.frame(y, x1, x2, x3, x4, x5, x6)
head(data) 


# ------------------------------- change color, shape and size of 1 data point
df = data.frame(
  x1 = 1:6,
  x2 = 6:1
)

plot(df$x1, df$x2) # basic

plot(df$x1, df$x2,
     col = ifelse(1:nrow(df) == 3, "red","black"),
     pch = ifelse(1:nrow(df) ==3,15,16),
     cex = ifelse(1:nrow(df) == 3,5,1))

library(ggplot2)
ggplot(df, aes(x1, x2)) +
  geom_point(
    col = ifelse(1:nrow(df) == 3, "red","black"),
    pch = ifelse(1:nrow(df) ==3,15,16),
    cex = ifelse(1:nrow(df) == 3,5,1))





# ------------------------ VENN DIAGRAM
library(venn) # 7 sets max

venn(6)
venn(5) # 5 sets

venn(7, ilabels = T)

venn(7, zcolor = "style")

# ggplot + venn

library(ggplot2)
library(ggpolypath)

venn(7, ggplot = T, linetype='dashed') + theme_dark() 



listVenn = list(
  A = sort(sample(1:100, 20)),
  B = sort(sample(1:100, 20)),
  C = sort(sample(1:100, 20)),
  D = sort(sample(1:100, 20))
)
listVenn

library(ggvenn)
library(tidyverse)

# Pairwise 
ggvenn(listVenn, c("A","C")) 

ggvenn(listVenn, c('A','B','C'))

ggvenn(listVenn)


data_venn <- data.frame(value = 1:100,    # Create example data frame
                        A = FALSE,
                        B = FALSE,
                        C = FALSE,
                        D = FALSE)
data_venn$A <- data_venn$value %in% listVenn$A
data_venn$B <- data_venn$value %in% listVenn$B
data_venn$C <- data_venn$value %in% listVenn$B
data_venn$D <- data_venn$value %in% listVenn$B
head(data_venn)                           # Head of example data frame


ggplot(data_venn,                         # Apply geom_venn function
       aes(A = A, B = B, C = C, D = D)) +
  geom_venn()

# ======= summary() 

library(showtext)
font_add(family="regular.ttf","Lato.ttf")
showtext::showtext_auto()

# Spartan font
ggsave("file.png",plot = prop_bar,)



# count(word) %>% 
# arrange(-n)

# coord_flip(expand=FALSE)




















