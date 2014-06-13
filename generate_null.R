#function to generate a reference null proteome given a set of 
#purification experiments
source("init_fns.R")

#the *_col arguments should be strings refering to the appropriate
#variable names in the data frame. nprey is the cap on the number of unique
#preys to appear in each run, TSC is the cap on the total counts 
#to appear in each run. By default nprey is the average number of 
#unique preys found in the original experiments rounded up to the n
#nearest integer. Similarly the default TSC is the average TSC accross
#all the original experiments rounded up to the nearest integer

generate_null <- function(data, bait_col, prey_col, count_col, exp_no,
                          nprey = NULL, TSC = NULL) {
  data <- data %.%
    rename_col(bait_col, "Bait") %.%
    rename_col(prey_col, "Prey") %.%
    rename_col(count_col, "Count")
  bait_info <- group_by(data, Bait) %.%
    summarise(unique_preys = length(Prey), TSC = sum(Count))
  if (is.null(nprey)) {
    nprey <- ceiling(median(bait_info$unique_preys))   
  }
  if (is.null(TSC)) {
    TSC <- ceiling(median(bait_info$TSC))
  }
  #setting up vector of preys and weights
  old_preycounts <- group_by(data, Prey) %.%
    summarise(TSC = sum(Count))
  #picking preys to show up in experiment
  new_preys <- vector("character")
  i <- 1
  while (length(unique(new_preys)) < nprey) {
    #draw
    new_preys[[i]] <- sample(old_preycounts$Prey, size = 1, 
                             prob = old_preycounts$TSC)
    i <- i + 1
  }
  #filling out prey counts until desired TSC is reached
  subset_preycounts <- filter(old_preycounts, Prey %in% new_preys)
  for (i in (length(new_preys) + 1):TSC) {
    new_preys[[i]] <- sample(subset_preycounts$Prey, size = 1,
                             prob = subset_preycounts$TSC)
  }
  out <- tbl_df(data.frame(table(new_preys))) %.%
    rename_col("new_preys", "Prey") %.%
    rename_col("Freq", "Count") %.%
    mutate(ExpNo = exp_no)
  return(out)
}

#function to produce single data frame for multiple runs
sim_data <- function(data, nruns, bait_col, prey_col, count_col,
                     nprey = NULL, TSC = NULL) {
  out <- tbl_df(data.frame())
  for (i in 1:nruns) {
    out <- rbind(out, generate_null(data, bait_col, prey_col, 
                                    count_col, exp_no = i, nprey, TSC))
  }
  return(out)
}