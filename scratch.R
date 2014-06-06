#scratch work for scoring
#setwd("~/Dropbox/School/Research/protein_scoring")
source("init_fns.R")
source("mink_dist.R")

#loading data, removing 1 and 2 counts, normalizing by length
dat <- tbl_df(read.csv("~/Dropbox/School/Research/Minkowski/Chen-all.csv",
              header = TRUE, stringsAsFactors = FALSE)) %.%
  filter(Count > 2) %.%
  apply_col("Length", function(x) {x[is.na(x)] <- 300; return(x)}) %.%
  mutate(normCount = Count/Length)

dists <- compute_dist(dat, "Prey", "normCount", 0.25)
qplot(log(D_i), data = dists)

#looking at prey specificity
spec <- group_by(dat, Prey) %.%
  summarise(occurrences = length(Prey), 
            Length = max(Length),
            medTSC = median(Count),
            meanTSC = mean(Count)) %.%
  ungroup()
qplot(Length, occurrences, data = spec, geom = "bin2d")
qplot(log(occurrences), data = spec)
qplot(log(medTSC), log(meanTSC), data = spec)

#looking at total spectral counts accross baits
bait_sc <- group_by(dat, ExpNo) %.%
  summarise(TSC = sum(Count), meanTSC = mean(Count)) %.%
  ungroup()
qplot(TSC, meanTSC, data = bait_sc, main = "Bait TSC")
