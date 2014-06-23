#function to test database overlap with biogrid
source("init_fns.R")
library(dplyr)
biogrid <- read.csv("biogrid.csv", header = TRUE, stringsAsFactors = FALSE)
names(biogrid) <- c("A", "B")

get_interactors <- function(protein, ints) { 
  #ints needs to be a two-column df of known interactions
  names(ints) <- c("A", "B")
  ints <- filter(ints, A == protein | B == protein)
  out <- as.character(unique(unlist(ints)))
  out <- out[out != protein]
  if (length(out) < 1) out <- NA
  return(out)
}

n_interactors <- function(bait, preys, ints) {
  db_interactors <- get_interactors(bait, ints)
  return(sum(preys %in% db_interactors))
}

bg_overlap <- function(data, prey_col, bait_col, score_col, n, ints) {
  nscores <- nrow(data)
  overlaps <- tbl_df(data) %.%
    rename_col(score_col, "Score_col") %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(bait_col, "Bait") %.%
    arrange(desc(Score_col)) %.%
    filter(row_number() <= n) %.%
    group_by(Bait) %.%
    summarise(nprey = length(Prey), 
           overlap = n_interactors(unique(Bait), Prey, ints))
  out <- sum(overlaps$overlap) / sum(overlaps$nprey)
  return(out)
}
