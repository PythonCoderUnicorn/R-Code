# code is from Georgiou on Twitter


library(ggplot2)
library(ggforce)

s=500
ggplot() + 
  geom_ellipse(
    aes(x0=sin(seq(0, 2*pi, length.out=s)), 
        y0=cos(seq(0, 3*pi, length.out=s)), 
        a=cos(seq(0, 4*pi, length.out=s)), 
        b=cos(seq(0, 5*pi, length.out=s)), 
        angle=0
        ), n=50, size=0.1
    ) + 
  coord_fixed() + 
  ggdark::dark_mode()+
  scale_fill_viridis_d(option = 'B')+
  theme(
    panel.background = element_blank(),
    plot.background = element_rect(color = 'black'),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )
