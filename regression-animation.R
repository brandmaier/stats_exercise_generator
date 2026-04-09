library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(cowplot)
library(animation)

num_frames <- 20

saveGIF({
  for (i in 1:(num_frames)) {
    
    
N <- 50
x <- rnorm(N)+2
e <- rnorm(N)
y <- 0.2*x + e

gp<-data.frame(x,y) %>% ggplot(aes(x=x,y=y))+geom_point()+geom_smooth(method = "lm",se = FALSE,lwd=2)+
  xlim(0,+4)+ylim(-2, 2)

plot(gp)

  }
  
}, interval=.1, ani.width=800, ani.height=400,
movie.name = "ani-regression.gif"
)