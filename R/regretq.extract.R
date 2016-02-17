regretq.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  regretq=lapply(df, split.task.results, task = "regretq")

  # then apply a the data extraction function across that list where it is not empty
  lapply(regretq, function(regretq){
    if(!is.null(regretq)){
      data.frame(
        regretq.total = sum(c(regretq$rating[1:4],-regretq$rating[5])),
        regretq.curious = regretq$rating[1],
        regretq.seek_info = regretq$rating[2],
        regretq.regret_minimal = regretq$rating[3],
        regretq.missed_oppotunities = regretq$rating[4],
        regretq.never_look_back = regretq$rating[5]
      )
    }
  })
}
