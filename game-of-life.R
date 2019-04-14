#
# A implementation of the game of life world
#

DIM_X = 120 # 100
DIM_Y = 30 # 100
NUM_GENERATIONS = 2000

randInitWorld <- function(dimX, dimY) {
  matrix(sample(c(T,F), dimX * dimY, replace = T), dimY, dimX)
}

countAliveNeighbors <- function(X) {
  d <- dim(X)
  C <- apply(merge(1:dim(X)[1], 1:dim(X)[2]), 1, 
        function(i) sum(X[max(1,i[1]-1):min(d[1], i[1]+1), max(1,i[2]-1):min(d[2], i[2]+1)]) - X[i[1],i[2]]
  )
  matrix(C, d[1], d[2])
}

evolveNextGeneration <- function(XWorld) {
  XWorldCount <- countAliveNeighbors(XWorld)
  (XWorldCount == 3) | XWorld & (XWorldCount == 2)
}

countAliveCells <- function(XWorld) {
  sum(XWorld)
}

plotWorld <- function(XWorld) {
  p <- ggplot(melt(XWorld), aes(X2,X1, fill=value)) + 
    geom_raster() +
    guides(fill=FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) +
    scale_fill_manual(values=c("white","black")) +
    scale_y_reverse( lim=c(dim(XWorld)[1],0)) # (0,0) should start top-left
  p
}

singleRun <- function(doPlot = F, svPlot = F) {
  XWorld <- randInitWorld(DIM_X, DIM_Y)
  if(doPlot) plot(plotWorld(XWorld))
  if(svPlot) ggsave(sprintf("png/gol%04d.png", 1), width=DIM_X/300.0*5,height=DIM_Y/300.0*5,plotWorld(XWorld))
  numAlive <- rep(0, NUM_GENERATIONS)
  for(i in 1:NUM_GENERATIONS) {
    numAlive[i] <- countAliveCells(XWorld)
    cat("Iteration:", i, "; Still alive:", numAlive[i], "\n") #track over time
    XWorld <- evolveNextGeneration(XWorld)
    if(doPlot) plot(plotWorld(XWorld))
    if(svPlot) ggsave(sprintf("png/gol%04d.png", i), width=DIM_X/30.0*5,height=DIM_Y/30.0*5,plotWorld(XWorld))
  }
  return (list(numAlive = numAlive, XWorld = XWorld))
}

# ffmpeg -i gol%03d.png output.gif
# convert -delay 10 -loop 0 *.png animation.gif
# xkcd plot
# how many generations? 400-500
singleRun(svPlot = T)

# Run many times
#allRuns <- mclapply(1:100, function(i) singleRun(), mc.cores = 20)
#saveRDS(allRuns, file="allRuns.rds") # 100x100 dim.

library(ggplot2)
allRuns <- readRDS("allRuns.rds")
tmp <- lapply(allRuns, function(i) data.frame(i$numAlive))
xx <- do.call(cbind.data.frame, tmp)
df <- data.frame(time=1:nrow(xx), mean=apply(xx, 1, mean), sd = apply(xx, 1, sd))
df <- cbind(df, se = df$sd / sqrt(length(allRuns)))
df <- cbind(df, q = qnorm(p = 0.01, mean = 0, sd = df$se, lower.tail =T)) 
df <- cbind(df, lwr = df$mean - 2*df$sd, upr=df$mean+2*df$sd) # 2x sd
#df <- melt(xx, id.vars = "time")
#df$variable <- NULL

p <- ggplot(df)+
  #geom_point()+
  geom_line(aes(time, mean))+
  geom_ribbon(aes(x=time, ymin=lwr,ymax=upr),alpha=0.3) +
  #scale_y_log10()+
  #scale_x_log10()+
  theme_bw()

