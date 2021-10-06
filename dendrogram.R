

# DENDROGRAM

# REQUIREMENTS
# dataframe - individuals in row, features in column
# dist()  - computes distance between sample
# hclust() - hierarchical clustering
# plot()   - 

library(tidyverse)


#  basic dendrogram

vorta = matrix( sample(seq(1,2000), 200), ncol= 10)
rownames(vorta) = paste0("clone_", seq(1,20))
colnames(vorta) = paste0("iteration_", seq(1,10))

vorta %>% view()

dist = dist(vorta[ , c(4:8)], diag = T)
dist

h_clust = hclust(dist)

plot(h_clust, main = "Dominion Vorta Cloning Hierarchical Clustering")













dend = mtcars %>% 
  select(mpg, cyl, disp) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() 

par(mar=c(7,3,1,1))  # Increase bottom margin to have the complete label
plot(dend)


