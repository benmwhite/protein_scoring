#COMPASS Z and S scores
require("dplyr")

#mean function when rest of data are assumed to be zero
sparse_mean <- function(vec, n = length(vec)) {
  return(sum(vec) / n)
}

#sd function when rest of data are assumed to be zero
sparse_sd <- function(vec, n = length(vec)) {
  mu <- sum(vec) / n
  return(sum((vec - mu)^2) / (n - 1))
}

#COMPASS Z score
#takes data input and returns data with scores as new column
compass_z <- function(data, prey_col, bait_col) {
  n_exp <- length(unique(data[[bait_col]]))
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.% #load from mink_dist.r
    rename_col(bait_col, "ExpNo") %.%
    group_by(Prey) %.%
    mutate(z_score=(Count-sparse_mean(Count,n_exp))/sparse_sd(Count,n_exp)) %.%
    ungoup()
  return(out)
}

#COMPASS S score
#takes data input and returns data with scores as new column
compass_s <- function(data, prey_col, bait_col) {
  n_exp <- length(unique(data[[bait_col]]))
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.% #load from mink_dist.r
    rename_col(bait_col, "ExpNo") %.%
    group_by(Prey) %.%
    mutate(s_score = sqrt(n_exp * Count / length(Count))) %.%
    ungoup()
  return(out)
}