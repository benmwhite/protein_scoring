#Poisson scoring
source("init_fns.R")
source("mink_dist.R")
library(dplyr)

#function does not compute distances, use compute_dist first
#output is data frame with scores added as new column
pois_scores <- function(data, bait_col, prey_col, count_col, dist_col) {
  n_exp <- length(unique(data[[bait_col]]))
  out <- data %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(count_col, "Count") %.%
    rename_col(dist_col, "Dist") %.%
    group_by(Prey) %.%
    mutate(Specificity = length(Prey) / n_exp) %.%
    ungroup() %.%
    mutate(Score = ppois(Count, Dist * Specificity / n_exp))
  return(out)
}