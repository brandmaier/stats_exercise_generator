library(plotly)
library(patchwork)
library(cowplot)
library(animation)

source("R/helper.R")
source("R/anova-2f.R")

#
# function to simulate ANOVA objects
#
generate <- function() {
  
  # simulate a random dependent variable
  # without effect of any factors
  av = rnorm(8*2*2,0,1)
  
  # OR: simulate an effect of factor B
  av <- rep(c(0,1),each=8,2)+rnorm(8*2*2,0,1) 
  
  # OR: simulate an effect of factor A
  av <- rep(c(0,1),each=16)+rnorm(8*2*2,0,1)  
  
  # create ANOVA object
  aov <- generate_anova_2f(
    av = av,
    factor.a.levels = c("a1","a2"),
    factor.b.levels=c("b1", "b2"), 
    nz=8)

  # return result
  aov
}


# number of frames for animation
num_frames <- 65

# generate all simulated ANOVA objects
stuff <- replicate(n = num_frames,expr = generate(),simplify = FALSE)

# extract F values
Fvals <- sapply(stuff, function(x){x$Fval_A})


# create animation in loop over all num_frames
saveGIF({
  for (i in 1:(num_frames)) {
    
    df1 <- stuff[[1]]$df_A
    df2 <- stuff[[1]]$df_inn
    
    # generate left plot
    gp1 <- ggplot( data=data.frame(F=Fvals[1:(i)]), aes(x=F))+

      geom_dotplot()+
      xlim(0,8)+ylim(0,1.25)+
    stat_function(fun=stats::df, args=list(df1=df1, df2=df2),
                  geom="area",fill="blue",alpha=.2)
    # generate right plot
    gp2 <- pie_plot(stuff[[i]])
    
    # combine plots
    gp<-gp1+gp2
    plot(gp)
    
    cat(i, "/", num_frames,"\n")
  }
}, interval=.5, ani.width=800, ani.height=400,
movie.name = "ani-noeffect.gif"
#movie.name = "ani-effect.gif"
)
