tipi.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  tipi=lapply(df, split.task.results, task = "tipi")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(tipi, function(tipi){
    if(!is.null(tipi) && sum(tipi$responseType!="skip")>0){
      tipi <- tipi[complete.cases(tipi),c("subscale","score")]
      if(nrow(tipi)==10){
        tipim <- reshape::melt(tipi, id="subscale")
        tipi <- cast(tipim, subscale~variable, sum)
        
        data.frame(
          tipi.emotional.stability = tipi$score[1],
          tipi.conscientiousness= tipi$score[2],
          tipi.openness.to.experience = tipi$score[3],
          tipi.agreeableness = tipi$score[4],
          tipi.extroverted = tipi$score[5]
        )
      }
    }
  })
}
