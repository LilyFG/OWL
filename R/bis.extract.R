bis.extract <- function(df){
  # extract the task data into a list of dataframes by subject
  bis <- lapply(df, split.task.results, task = "bis")
  
  # perform whatever operations need to be performed across the list of task data
  lapply(bis, function(bis){
    #if there is data for this task for this subject
    if(!is.null(bis)){
      bis <- bis[complete.cases(bis),c("subscale","score")]
      if(nrow(bis)==30){
        bism <- reshape::melt.data.frame(bis, id="subscale")
        bis <- reshape::cast(bism, subscale~variable, sum)
        bis[7,] <- c("total", sum(bis$score))
        
        # spit out the data frame containing the variables of interest
        data.frame(
          bis.attention	=	as.integer(bis$score[1]),
          bis.cognitiveControl	=	as.integer(bis$score[2]),
          bis.cognitiveInstability	=	as.integer(bis$score[3]),
          bis.motor	=	as.integer(bis$score[4]),
          bis.perseverance	=	as.integer(bis$score[5]),
          bis.selfControl	=	as.integer(bis$score[6]),
          bis.total	=	as.integer(bis$score[7]),
          stringsAsFactors = F)
      }
    }
  })
}