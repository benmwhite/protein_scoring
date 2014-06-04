setwd("~/Dropbox/School/Research/protein_scoring")
require("ggplot2")
require("dplyr")

####preliminary functions####
rename_col <- function(data, oldname, newname) {
  names(data)[names(data) == oldname] <- newname
  return(data)
}

apply_col <- function(data, colname, f, ...) {
  data[[colname]] <- f(data[[colname]], ...)
  return(data)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
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
    }
  }
}

####Computing minkowski distances from 0####
#takes data frame as input, returns data frame with dists as new column
#also computes d_i^k - d_i, where d_i^k is the minkowski distance
#from 0 if x_ik is zer0. takes data frame input and returns data frame
#with results as a new columns.

compute_dist <- function(data, prey_col, count_col, p) {
  #p is minkowski distance parameter, prey_col is 
  #name of prey column in data set, count_col is name of
  #count column in data set
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(count_col, "Count") %.%
    group_by(Prey) %.% 
    mutate(D_i = sum(Count^p)^(1/p), 
          D_ik = (sum(Count^p) - Count^p)^(1/p)) %.%
    ungroup()
  out$D_ik[is.infinite(out$D_ik)] <- NA
  out$D_ik[out$D_ik == 0] <- NA
  return(out)
}