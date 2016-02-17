bisbas.extract <- function(df){
  # extract the task data into a list of dataframes by subject
  bisbas <- lapply(df, split.task.results, task = "bisbas")
  
  # perform whatever operations need to be performed across the list of task data
  lapply(bisbas, function(bisbas){
    #if there is data for this task for this subject
    if(!is.null(bisbas)){
      # use the complete.cases function to remove any rows with (i.e. from skipped questions)
      bisbas <- bisbas[complete.cases(bisbas),c("subscale","score")]
      
      # check that there's a full set of responses
      if(nrow(bisbas)==20){
        
        #split the data by subscale
        bisbas.s <- split(bisbas, f = bisbas$subscale)

        # spit out the data frame containing the variables of interest
        data.frame(
          bisbas.bis	=	sum(bisbas.s[["Worries"]]$score),
          bisbas.drive	=	sum(bisbas.s[["Goals"]]$score),
          bisbas.fun	=	sum(bisbas.s[["Fun"]]$score),
          bisbas.reward	=	sum(bisbas.s[["Rewards"]]$score),
          bisbas.total	=	sum(bisbas$score),
          stringsAsFactors = F)
      }
    }
  })
}
