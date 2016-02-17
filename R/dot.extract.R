dot.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  dot=lapply(df, split.task.results, task = "dot")     
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(dot, function(dot){
    if(!is.null(dot)){
      
      dot$accuracy[dot$response == dot$trial.correctResponse] <- 1
      dot$accuracy[is.na(dot$accuracy) & dot$response %in% c("yes","no")] <- 0 
      dot.acc <- melt(dot[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes', 
                          c("accuracy", "trial.selfOther", "trial.consistency")], 
                      id=c("trial.selfOther", "trial.consistency"),
                      na.rm=TRUE)
      dot.acc <- as.data.frame(cast(dot.acc, trial.selfOther~trial.consistency~variable, mean))
      dot.RT <- melt(dot[dot$phase==3 & dot$accuracy==1 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes',  
                         c("responseTime", "trial.selfOther", "trial.consistency")], 
                     id=c("trial.selfOther", "trial.consistency"),
                     na.rm=TRUE)
      dot.RT <- as.data.frame(cast(dot.RT, trial.selfOther~trial.consistency~variable, mean))
      
      data.frame(dot.other.con.acc=dot.acc["other", "con.accuracy"],
                 dot.other.incon.acc=dot.acc["other", "incon.accuracy"], 
                 dot.self.con.acc=dot.acc["self", "con.accuracy"],
                 dot.self.incon.acc=dot.acc["self", "incon.accuracy"], 
                 dot.other.con.RT=dot.RT["other", "con.responseTime"],
                 dot.other.incon.RT=dot.RT["other", "incon.responseTime"], 
                 dot.self.con.RT=dot.RT["self", "con.responseTime"],
                 dot.self.incon.RT=dot.RT["self", "incon.responseTime"],                               
                 dot.timeouts=length(dot$response[dot$response=="noresponse"])
      )
    }
  })
}