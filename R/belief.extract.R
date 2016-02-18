belief.extract <- function(df){
  # extract the task data into a list of dataframes by subject
  belief <- lapply(df, split.task.results, task = "belief")
  
  # perform whatever operations need to be performed across the list of task data
  
  lapply(belief, function(belief){
    #if there is data for this task for this subject
    if(!is.null(belief)){ #if there is data for this task for this subject
      # determine response accuracy
      belief$accuracy[belief$response == belief$trial.correctResponse] <- 1
      # 
      belief$accuracy[is.na(belief$accuracy) & belief$response %in% c("red","blue")] <- 0
      belief.acc <- reshape::melt.data.frame(belief[belief$phase==3,c("trial.type", "accuracy", "trial.conditionB", 
                                                  "trial.conditionD")], 
                         id=c("trial.type", "trial.conditionB", "trial.conditionD"),
                          measure.vars = "accuracy",
                         na.rm=TRUE)
      belief.acc <- as.data.frame(reshape::cast(belief.acc, trial.type~trial.conditionB~trial.conditionD, mean))
      
      # RTs are trimmed to include correct trials only but no other trimming takes place. Note that responses time is limited at the upper end to 8000ms
      belief.RT <- reshape::melt.data.frame(belief[belief$phase==3 & belief$accuracy==1, c("responseTime", "trial.type", "trial.conditionB", 
                                                                       "trial.conditionD")], 
                        id.vars=c("trial.type", "trial.conditionB", "trial.conditionD"),
                        measure.vars = "responseTime",
                        na.rm=TRUE)
      
      belief.RT$value<-as.numeric(levels(belief.RT$value))[belief.RT$value]
      belief.RT <- as.data.frame(reshape::cast(belief.RT, trial.type~trial.conditionB~trial.conditionD, mean))
      
      # spit out the data frame containing the variables of interest
      data.frame(
        belief.C.Bx.Dx.acc = belief.acc["C","B-.D-"],
        belief.C.B.Dx.acc = belief.acc["C","B+.D-"],
        belief.C.Bx.D.acc = belief.acc["C","B-.D+"],
        belief.C.B.D.acc = belief.acc["C","B+.D+"],
        belief.T.Bx.Dx.acc = belief.acc["T","B-.D-"],
        belief.T.B.Dx.acc = belief.acc["T","B+.D-"],
        belief.T.Bx.D.acc = belief.acc["T","B-.D+"],
        belief.T.B.D.acc = belief.acc["T","B+.D+"],
        belief.C.Bx.Dx.rt = belief.RT[1,1],
        belief.C.B.Dx.rt = belief.RT[1,2],
        belief.C.Bx.D.rt = belief.RT[1,3],
        belief.C.B.D.rt = belief.RT[1,4],
        belief.T.Bx.Dx.rt = belief.RT[2,2],
        belief.T.B.Dx.rt = belief.RT[2,2],
        belief.T.Bx.D.rt = belief.RT[2,3],
        belief.T.B.D.rt = belief.RT[2,4],
        belief.timeouts = length(belief$response[belief$response=="noresponse"]),
        stringsAsFactors = F
      )
    }
  })
}