dass.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  dass=lapply(df, split.task.results, task = "dass")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(dass, function(dass){
    if(!is.null(dass)){
      dass <- dass[complete.cases(dass$score),c("subscale","score")]
      if(nrow(dass)==21){
#         dassm <- reshape::melt.data.frame(dass, id="subscale")
#         dass <- reshape::cast(dassm, subscale~variable, sum)
        # spit out the data frame containing the variables of interest
        data.frame(
          dass.score = sum(dass$score),
          dass.depression = sum(dass$score[dass$subscale=="D"]),
          dass.anxiety = sum(dass$score[dass$subscale=="A"]),
          dass.stress = sum(dass$score[dass$subscale=="S"]),
          stringsAsFactors = F)
      }
    }
  })
  
}