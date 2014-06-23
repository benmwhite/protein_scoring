#COMPASS Z and S scores
require("dplyr")
source("init_fns.R")

#COMPASS Z score
#takes data input and returns data with scores as new column
compass_z <- function(data, prey_col, bait_col) {
  n_exp <- length(unique(data[[bait_col]]))
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.% #load from mink_dist.r
    rename_col(bait_col, "Bait") %.%
    group_by(Prey) %.%
    mutate(z_score=(Count-sparse_mean(Count,n_exp))/sparse_sd(Count,n_exp)) %.%
    ungroup()
  return(out)
}

#COMPASS S score
#takes data input and returns data with scores as new column
compass_s <- function(data, prey_col, bait_col) {
  n_exp <- length(unique(data[[bait_col]]))
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.% #load from mink_dist.r
    rename_col(bait_col, "Bait") %.%
    group_by(Prey) %.%
    mutate(s_score = sqrt(n_exp * Count / length(Count))) %.%
    ungroup()
  return(out)
}