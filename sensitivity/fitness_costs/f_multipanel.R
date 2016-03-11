## Function
# Multiple plot function with alphabetical panel labelling
# based on mutiplo
multipanel <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # function for adding panel labels
  # adapted form http://www.r-bloggers.com/r-good-practice-%E2%80%93-adding-footnotes-to-graphics/, accessed 14/05/2015
  panel.label <- function(text="",size= 1, color="black", vp=viewport()){
    require(grid)
    pushViewport(vp)
    grid.text(label= text ,
              x = unit(0.02,"npc"),
              y= unit(0.95,"npc"),
              just=c("left", "bottom"),
              gp=gpar(cex= size, col=color))
    popViewport()
  }
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow=T)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
      # Print alphabetical panel label
      panel.label(paste("(", letters[i], ")", sep=""), vp=viewport(layout.pos.row = matchidx$row,
                                                                   layout.pos.col = matchidx$col))
    }
  }
}