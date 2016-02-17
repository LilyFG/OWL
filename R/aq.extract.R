aq.extract <- function(df){
  # extract the task data into a list of dataframes by subject
  aq <- lapply(df, split.task.results, task = "aq")
  
  # perform whatever operations need to be performed across the list of task data
  lapply(aq, function(aq){
    if(!is.null(aq)){ #if there is data for this task for this subject
      aq <- aq[complete.cases(aq),c("subscale","score")] # remove incomplete cases (will remove skipped questions)
      if(nrow(aq)==50){ # if there is a complete data set (irrespective of whether questions were skipped once)
        aq.s <- split(aq, aq$subscale)

        # spit out the data frame containing th§e variables of interest
        data.frame(
          aq.communication	=	sum(aq.s[["Expressive"]]$score),
          aq.attention_switching	=	sum(aq.s[["Flexible"]]$score),
          aq.imagination	=	sum(aq.s[["Imaginative"]]$score),
          aq.attention_to_detail	=	sum(aq.s[["Observant"]]$score),
          aq.social_skill	=	sum(aq.s[["Social"]]$score),
          aq.total	=	sum(aq$score),
          stringsAsFactors = F)
      }
    }
  })
}