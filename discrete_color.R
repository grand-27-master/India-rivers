setwd("E:/Ranking paper/r")
par(mar = c(10,10,10,10))
#par(mfrow=c(1,4))
library(ggplot2)
library(grid)
library(gridExtra)
library(extrafont)
library(ggmap)
library(maptools)
library(plyr)
library(maps)

p <- read.csv("Ranking.csv")
chart<-list()
w <- c(1,2,3,4,Inf)

p$A1 <- cut(p$WPM,breaks = w,right = FALSE)
h <- ggplot(data = p, aes(x = x, y = y)) +
  geom_tile(aes(fill = A1)) +
  scale_fill_manual(breaks=w, values = c("yellow","blue","green","red"))+theme_classic()

NWH <- readShapeSpatial("4-17-2018-899072")
ch <- geom_polygon(data=NWH, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="WPM")

w <- c(1,2,3,4,Inf)
p$A2 <- cut(p$WPM_1,breaks = w,right = FALSE)
h <- ggplot(data = p, aes(x = x, y = y)) +
  geom_tile(aes(fill = A2)) +
  scale_fill_manual(breaks=w, values = c("yellow","blue","green","red"))+theme_classic()


NWH <- readShapeSpatial("4-17-2018-899072")
ch <- geom_polygon(data=NWH, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[2]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="WPM-1")

w <- c(1,2,3,4,Inf)
p$A3 <- cut(p$WPM_2,breaks = w,right = FALSE)
h <- ggplot(data = p, aes(x = x, y = y)) +
  geom_tile(aes(fill = A3)) +
  scale_fill_manual(breaks=w, values = c("yellow","blue","green","red"))+theme_classic()


NWH <- readShapeSpatial("4-17-2018-899072")
ch <- geom_polygon(data=NWH, aes(x=long, y=lat, group=group), colour="black", fill="white", alpha=0)
cha <- h+ch+xlab("Longitude")+ylab("Latitude") #+ labs(title= z[i])
cha <- cha+theme(text = element_text(family = "Times New Roman", size=16, face = "bold"))
chart[[3]] <- h+ch+xlab("Longitude")+ylab("Latitude") + labs(title="WPM-2")


##### Multiple Plot Function #####

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols)    # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}
tiff(filename = "NWH.tiff", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "cairo")
multiplot(chart[[1]], chart[[2]], chart[[3]], cols = 3)
dev.off()
jpeg(filename = "NWH.jpeg", pointsize =8, res = 600, units = "in", width = 8, height = 2, restoreConsole = TRUE, type = "windows")
multiplot(chart[[1]], chart[[2]], chart[[3]], cols = 3)
dev.off()