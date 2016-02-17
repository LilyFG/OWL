economic.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  economic=lapply(df, split.task.results, task = "economic")     
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(economic, function(economic){
    if(!is.null(economic)){
      economic <- economic[complete.cases(economic$score),c("id", "score")]
      if(nrow(economic)==12){
        data.frame(
          economic.score=sum(economic$score)
        )
      }
    }
  })
}
