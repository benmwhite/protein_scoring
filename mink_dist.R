#setwd("~/Dropbox/School/Research/protein_scoring")
require("ggplot2")
require("dplyr")
source("init_fns.R")

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
          D_ij = (sum(Count^p) - Count^p)^(1/p)) %.%
    ungroup()
  out$D_ij[is.infinite(out$D_ij)] <- NA
  out$D_ij[out$D_ij == 0] <- NA
  return(out)
}

#function for returning weighted mean differences
#input data set should be output data set from compute_dist()

M_diff <- function(dists) {
  out <- dists %.%
    group_by(Prey) %.%
    summarise(M = mean((D_i - D_ij) / sd(D_ij)))
  out$M[is.infinite(out$M)] <- NA
  return(out)
}

#OLD
#function for getting mean modified coefficient of variation of the differences
#MCV <- function(dists) {
#  out <- dists %.%
#    group_by(Prey) %.%
#    summarise(M = mean(sd(D_ik) / mean(D_ik)))
#  out$M[is.infinite(out$M)] <- NA
#  return(out)
#}

#function for producing plot of M_i's over minkowski parameter p, returning mean
#output from M_diff
pplot_Mdiff <- function(data, pseq, prey_col, count_col, ...) {
  extract_diffs <- function(p) {
    tempdat <- compute_dist(data, prey_col, count_col, p)
    out <- c(MeanDiff = mean(M_diff(tempdat)$M, na.rm = TRUE), p = p)
    return(out)
  }
  plot_data <- sapply(pseq, extract_diffs) %.% 
    t() %.% 
    data.frame() %.% 
    tbl_df()
  qplot(p, MeanDiff, data = plot_data, geom = c("point", "line"), ...)
}

####Distance-based scoring methods####
DAll_scores <- function(data, prey_col, dij_col) {
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(dij_col, "D_ij") %.%
    group_by(Prey) %.%
    mutate(D_all = D_ij - mean(D_ij)) %.%
    ungroup()
  return(out)
}

Dratio_scores <- function(data, prey_col, dij_col) {
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(dij_col, "D_ij") %.%
    group_by(Prey) %.%
    mutate(D_ratio = (D_ij - mean(D_ij)) / mean(D_ij)) %.%
    ungroup()
  return(out)
}

lambda_scores <- function(data, bait_col, prey_col, dist_col, count_col,
                          length_col, tsc_col) {
  out <- tbl_df(data) %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(bait_col, "Bait") %.%
    rename_col(dist_col, "Dist") %.%
    rename_col(length_col, "Length") %.%
    rename_col(count_col, "Count") %.%
    rename_col(tsc_col, "TSC") %.%
    mutate(lambda_score = log1p(Count / (Length * TSC * Dist)))
  return(out)
}