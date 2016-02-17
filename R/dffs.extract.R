dffs.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  dffs=lapply(df, split.task.results, task = "dffs")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(dffs, function(dffs){
    if(!is.null(dffs)){
      dffs2 <- dffs[complete.cases(dffs$score),c("id", "score")]
      data.frame(
        fruit.total	= sum(dffs$fruit, na.rm=TRUE),
        veg.total	= sum(dffs$vegetables, na.rm=TRUE),
        dffs.total	= ifelse(nrow(dffs2) == 26, sum(dffs2$score[1:26]), NA),
        stringsAsFactors = F)
    }
  })
}
