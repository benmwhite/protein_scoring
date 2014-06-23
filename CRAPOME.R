#functions for comparing scores to CRAPOME
source("init_fns.R")
require("dplyr")

crap <- tbl_df(read.csv("~/Dropbox/School/Research/HIPPO/CRAPome.csv", 
                        header = TRUE, stringsAsFactors = FALSE)) %.%
  rename_col("GENE", "Prey") %.%
  select(-(Num.Expt:Ave.SC))

crap_fraction <- function(data, prey_col, score_col, 
                       score_thresh, crap_thresh = 20) {
  final_scores <- left_join(data, crap) %.%
    rename_col(score_col, "Score") %.%
    rename_col(prey_col, "Prey") %.%
    filter(Score > score_thresh)
  ncrap <- nrow(filter(final_scores, Frequency > crap_thresh))
  return(ncrap / nrow(final_scores))
}