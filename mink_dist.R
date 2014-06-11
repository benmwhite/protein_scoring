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
          D_ik = (sum(Count^p) - Count^p)^(1/p)) %.%
    ungroup()
  out$D_ik[is.infinite(out$D_ik)] <- NA
  out$D_ik[out$D_ik == 0] <- NA
  return(out)
}

#function for returning weighted mean differences
#input data set should be output data set from compute_dist()

M_diff <- function(dists) {
  out <- dists %.%
    group_by(Prey) %.%
    summarise(M = mean((D_i - D_ik) / sd(D_ik)))
  out$M[is.infinite(out$M)] <- NA
  return(out)
}

#function for getting mean modified coefficient of variation of the differences
MCV <- function(dists) {
  out <- dists %.%
    group_by(Prey) %.%
    summarise(M = mean(sd(D_ik) / mean(D_ik)))
  out$M[is.infinite(out$M)] <- NA
  return(out)
}

#function for iterating over minkowski parameter p, returning mean
#output from function (such as M_diff)
diffs_pseq <- function(data, pseq, prey_col, count_col, f) {
  extract_diffs <- function(p) {
    tempdat <- compute_dist(data, prey_col, count_col, p)
    out <- c(MeanDiff = mean(f(tempdat)$M, na.rm = TRUE),
             MedDiff = median(f(tempdat)$M, na.rm = TRUE),
             p = p)
    return(out)
  }
  out <- t(sapply(pseq, extract_diffs))
  return(tbl_df(data.frame(out)))
}