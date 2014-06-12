#scratch work for scoring
setwd("~/Dropbox/School/Research/protein_scoring")
source("init_fns.R")
source("mink_dist.R")
library("stringr")

#loading data, removing 1 and 2 counts, normalizing by length
dat <- tbl_df(read.csv("~/Dropbox/School/Research/DUB.csv",
              header = FALSE, stringsAsFactors = FALSE))
names(dat) <- c("Bait", "Prey", "Count")
dat <- group_by(dat, "Bait", "Prey") %.%
  summarise(Count = sum(Count)) %.%
  filter(Bait != Prey, Count > 1) %.%
  ungroup()

data2 <- tbl_df(read.csv("~/Dropbox/School/Research/Minkowski/Chen-new.csv", 
                         header = TRUE,
                         stringsAsFactors = FALSE)) %.%
  rename_col("reference", "Prey") %.%
  group_by(ExpNo, Prey) %.% 
  summarise(Count = sum(Total)) %.%
  apply_col("Prey", str_trim) %.%
  filter(Count > 2) %.%
  ungroup() 

dat <- apply_col(dat, "Count", function(x) {x[x>50] <- 50; x})
n_exp <- length(unique(dat$Bait))
dists <- compute_dist(dat, "Prey", "Count", 0.3) %.%
  group_by(Prey) %.%
  mutate(Specificity = length(Prey) / n_exp) %.%
  ungroup()
qplot(D_i, data = dists)
#poisson scoring
scores <- dists %.%
  select(-D_ik) %.%
  mutate(Pois_score = ppois(Count, D_i * Specificity / n_exp))
arrange(scores, Prey)[1:40,]
write.csv()
qplot(Pois_score, data = scores)

ps <- seq(from = 0.1, to = 0.6, by = 0.02)
diffs <- diffs_pseq(dat, ps, "Prey", "Count", M_diff)
diffs2 <- diffs_pseq(data2, ps, "Prey", "Count", MCV)
qplot(p, MeanDiff, data = diffs, geom = c("point", "line"),
      main = "Average M_i's (DUB data set)",
      xlab = "p (Minkowski power parameter)",
      ylab = "Average M_i")

#looking at prey specificity
spec <- group_by(dat, Prey) %.%
  summarise(occurrences = length(Prey), 
            Length = max(Length),
            TSC = sum(Count)) %.%
  ungroup()
qplot(Length, occurrences, data = spec, geom = "bin2d")
qplot(occurrences, data = spec, binwidth = 1, xlim = c(0, 20))
qplot(log(medTSC), log(meanTSC), data = spec)

#looking at total spectral counts accross baits
bait_sc <- group_by(dat, ExpNo) %.%
  summarise(TSC = sum(Count), meanTSC = mean(Count)) %.%
  ungroup()
qplot(TSC, meanTSC, data = bait_sc, main = "Bait TSC")

#testing Zs
Zs <- function(dists) {
  out <- dists %.% 
    group_by(Prey) %.%
    mutate(Z = (D_ik - mean(D_ik)) / sd(D_ik)) %.%
    ungroup()
}
test <- Zs(compute_dist(dat, "Prey", "Count", 0.3))

#adding CRAPome information

crap <- tbl_df(read.csv("~/Dropbox/School/Research/HIPPO/CRAPome.csv", 
                        header = TRUE, stringsAsFactors = FALSE)) %.%
  rename_col("GENE", "Prey")

scores_crap <- left_join(scores, crap) %.%
  select(-(Num.Expt:Ave.SC))
scores_crap <- scores_crap %.%
  apply_col("Pois_score", round, 4) %.%
  apply_col("Specificity", round, 4) %.%
  apply_col("D_i", round, 4)
write.csv(scores_crap, "scores_june10.csv", row.names = FALSE)
