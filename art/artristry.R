
# artistry - @ivelasq3
# https://ivelasq.rbind.io/blog/rtistry-intro/

library(tidyverse)
library(viridis)
library(ggdark)

# parametric equations needed
# cosine and sine

circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- 
  circleFun(c(3, -3), 2.3, npoints = 1000)

# ----------------------- blank
ggplot(dat,aes(x, y)) +
  geom_path()
# -----------------------


genFun <- function(center = c(0, 0), 
                   npoints = 5000, 
                   c1 = 2.5, 
                   c2 = -5, 
                   c3 = 4.28, 
                   c4 = 2.3)
  {
  t <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + c1*(sin(c2*t)*sin(c2*t))*(2^cos(cos(c3*c4*t)))
  yy <- center[2] + c1*sin(sin(c2*t))*(cos(c3*c4*t)*cos(c3*c4*t))
  a <- data.frame(x = xx, y = yy)
  
  return(a)
}

# ========================== this section changes
dat <- 
  genFun(c(1,-1), npoints = 1000)

ggplot(dat, aes(x, y)) +
  geom_path()
# ========================== 


# ========================== 
dat <- 
  genFun(c(1,-1), npoints = 500, c1 = 5, c2 = -3, c3 = 5, c4 = 2)

ggplot(dat, aes(x, y)) +
  geom_path()


# ================== geoms
dat <- 
  genFun(c(1,-1), npoints = 5000)

ggplot(dat, aes(x, y)) +
  geom_line()


# -=============
set.seed(1234)

dat <- 
  genFun(c(-5,2,-4,4), npoints = 5000)

dat %>% 
  ggplot(aes(x, y)) +
  geom_point()


# -------- geom point size
set.seed(1111)

dat <- 
  genFun(c(1,-1), npoints = 5000) %>% 
  mutate(rand_w = sample(n())/3000)

dat %>% 
  ggplot(aes(x, y)) +
  geom_point(size = dat$rand_w) +
  theme_void()


# -------- geom shape
dat %>% 
  ggplot(aes(x, y)) +
  geom_point(size = dat$rand_w,
             shape = 8) +
  theme_void()


# ---------- opacity
set.seed(1234)

dat <- 
  dat %>% 
  mutate(rand_o = sample(n())/5000)

dat %>% 
  ggplot(aes(x, y)) +
  geom_point(size = dat$rand_w,
             alpha = dat$rand_o) +
  theme_void()


# ------------- COLOUR
set.seed(1234)

dat <- 
  dat %>% 
  mutate(rand_c = sample(n()))

dat %>% 
  ggplot(aes(x, y,  color = rand_c)) +
  geom_point(size = dat$rand_w,
             alpha = dat$rand_o) +
  scale_color_viridis(option = "plasma") +
  dark_theme_void() +
  theme(legend.position = "none") # remove legend







# =========== repetition
genFun <- function(center = c(4, 4), 
                   npoints = 10000, 
                   c1 = 11.5, 
                   c2 = -15, 
                   c3 = 18.28, 
                   c4 = -14.3, 
                   size_denom = 1, 
                   opacity_denom = 1, 
                   color_denom = 1){
  t <- seq(0, 2*pi, length.out = npoints)
  xx <- center[1] + c1*(sin(c2*t)*sin(c2*t))*(2^cos(cos(c3*c4*t)))
  yy <- center[2] + c1*sin(sin(c2*t))*(cos(c3*c4*t)*cos(c3*c4*t))
  rand_w <- sample(0:20, npoints, replace = TRUE)/size_denom
  rand_o <- sample(1:100, npoints, replace = TRUE)/opacity_denom
  rand_c <- sample(1:20, npoints, replace = TRUE)/color_denom
  a <- data.frame(x = xx, y = yy, rand_w = rand_w, rand_o = rand_o, rand_c = rand_c)
  
  return(a)
}
set.seed(1111)

dat <- 
  genFun(c(8, 8), 
         npoints = 5000, 
         c1 =  .5, 
         c2 =  -.35, 
         c3 =  .35, 
         c4 =  -15, 
         size_denom = 1.5, 
         opacity_denom = 150)

dat %>% 
  ggplot(aes(x, y,  color = rand_c)) +
  geom_point(size = dat$rand_w,
             alpha = dat$rand_o) +
  scale_color_viridis(option = "plasma") +
  dark_theme_void() +
  theme(legend.position = "none") # remove legend



















# --------
dat %>% 
  ggplot() +
  geom_point(aes(x, y,  color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-x, -y,  color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-y, x,  color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  geom_point(aes(-y, -x,  color = rand_c),
             size = dat$rand_w,
             alpha = dat$rand_o) +
  scale_color_gradientn(colors = rainbow(20))+
  # scale_color_viridis(option = 'rainbow' ) +
  dark_theme_void() +
  theme(legend.position = "none") # remove legend


