dot.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  dot=lapply(df, split.task.results, task = "dot")     
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(dot, function(dot){
  
    if(!is.null(dot)){
      # recode response data as 1=correct, 0=incorrect and NA=timed out
      dot$accuracy[dot$response == dot$trial.correctResponse] <- 1
      dot$accuracy[is.na(dot$accuracy) & dot$response %in% c("yes","no")] <- 0
      
      # create a new dataframe for the accuracy data including only 'neutral' (not 'filler') trials and where the correct response was 'yes'
      dot.acc <- reshape::melt.data.frame(dot[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes', 
      # include the accuracy variable and the two trial type variables: self/other and consistency
                          c("accuracy", "trial.selfOther", "trial.consistency")], 
      # use the self/other and consistency variables to reshape the data from long into wide format
                      id=c("trial.selfOther", "trial.consistency"),
                      na.rm=TRUE)
      # calculate the mean accuracy including only trials for which a response was made
      dot.acc <- as.data.frame(reshape::cast(dot.acc, trial.selfOther~trial.consistency~variable, mean))
      
      # create a new dataframe for the RT data including only 'neutral' (not 'filler') trials and where the correct response was 'yes' and the response was accurate
      dot.RT <- reshape::melt.data.frame(dot[dot$phase==3 & dot$accuracy==1 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes',  
                         c("responseTime", "trial.selfOther", "trial.consistency")], 
                     id=c("trial.selfOther", "trial.consistency"),
                     na.rm=TRUE)
                     
      
      dot.RT <- as.data.frame(reshape::cast(dot.RT, trial.selfOther~trial.consistency~variable, mean))
      
      data.frame(dot.self.total.acc=length(dot$accuracy[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & "trial.selfOther"=="self"]), 
                 dot.total.timeouts=length(dot$response[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & dot$response=="noresponse"]),
                 dot.other.con.acc=dot.acc["other", "con.accuracy"],
                 dot.other.incon.acc=dot.acc["other", "incon.accuracy"], 
                 dot.self.con.acc=dot.acc["self", "con.accuracy"],
                 dot.self.incon.acc=dot.acc["self", "incon.accuracy"], 
                 dot.other.con.RT=dot.RT["other", "con.responseTime"],
                 dot.other.incon.RT=dot.RT["other", "incon.responseTime"], 
                 dot.self.con.RT=dot.RT["self", "con.responseTime"],
                 dot.self.incon.RT=dot.RT["self", "incon.responseTime"],  
                 
                 # calculate N timeouts for each trial type on critical trials
                 dot.other.con.timeouts=length(dot$response[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & dot$response=="noresponse" & "trial.selfOther"=="other" & "trial.consistency"=="con"]),
                 dot.other.incon.timeouts=length(dot$response[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & dot$response=="noresponse" & "trial.selfOther"=="other" & "trial.consistency"=="incon"]),
                 dot.self.con.timeouts=length(dot$response[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & dot$response=="noresponse" & "trial.selfOther"=="self" & "trial.consistency"=="con"]),
                 dot.self.incon.timeouts=length(dot$response[dot$phase==3 & dot$trial.trialType=='neutral' & dot$trial.correctResponse=='yes' & dot$response=="noresponse" & "trial.selfOther"=="self" & "trial.consistency"=="incon"])
      )
    }
  })
}
