life.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  life=lapply(df, split.task.results, task = "life")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(life, function(life){
    if(!is.null(life)){
      if("responseScore" %in% colnames(life)){
        life <- life[complete.cases(life$responseScore),c("id", "responseScore")]
        if(nrow(life)==11){
          data.frame(
            life.score = sum(life$responseScore)
          )
        }
      }
    }
  })
}

