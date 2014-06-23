#functions for comparing scores to CRAPOME
source("init_fns.R")
library(dplyr)

crap <- tbl_df(read.csv("~/Dropbox/School/Research/HIPPO/CRAPome.csv", 
                        header = TRUE, stringsAsFactors = FALSE)) %.%
  rename_col("GENE", "Prey") %.%
  select(-(Num.Expt:Ave.SC))

crap_fraction <- function(data, prey_col, score_col, 
                       score_thresh, crap_thresh = 20) {
  final_scores <- left_join(data, crap) %.%
    rename_col(score_col, "Score_col") %.%
    rename_col(prey_col, "Prey") %.%
    filter(Score_col > score_thresh) %.%
    apply_col("Frequency", function(x) {x[is.na(x)] <- 0; return(x)})
  ncrap <- nrow(filter(final_scores, Frequency > crap_thresh))
  return(ncrap / nrow(final_scores))
}