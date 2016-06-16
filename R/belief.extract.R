belief.extract <- function(df){
  # extract the task data into a list of dataframes by subject
  belief <- lapply(df, split.task.results, task = "belief")

  # perform whatever operations need to be performed across the list of task data

  lapply(belief, function(belief){
    #if there is data for this task for this subject
    if(!is.null(belief)){ #if there is data for this task for this subject
    
      # trim the data to exclude practice 
      belief <- belief[belief$phase==3,]
      
      # recode response time as numeric, for some reason it comes out as text
      belief$responseTime <- as.numeric(belief$responseTime)
      
      # determine response accuracy
      belief$trial.conditionD[belief$trial.type == "C"] <- NA
      belief$accuracy[belief$response == belief$trial.correctResponse] <- 1
      
      # if a response was made but it was not the correct response, accuracy is 0, otherwise, missing
      belief$accuracy[is.na(belief$accuracy) & belief$response %in% c("red","blue")] <- 0
      
      # run the binomial test to determine if performance is above chance
      belief.test <- binom.test(sum(belief$accuracy, na.rm = T), sum(belief$response %in% c("red","blue"), na.rm = T), p=0.5, alternative="greater")
      
      # create a new data frame for calculating mean accuracy by condition
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
        belief.C.Bx.acc = belief.acc["C","B-.NA"],
        belief.C.B.acc = belief.acc["C","B+.NA"],
        belief.T.Bx.Dx.acc = belief.acc["T","B-.D-"],
        belief.T.B.Dx.acc = belief.acc["T","B+.D-"],
        belief.T.Bx.D.acc = belief.acc["T","B-.D+"],
        belief.T.B.D.acc = belief.acc["T","B+.D+"],
        belief.C.Bx.rt = belief.RT["C","B-.NA"],
        belief.C.B.rt = belief.RT["C","B+.NA"],
        belief.T.Bx.Dx.rt = belief.RT["T","B-.D-"],
        belief.T.B.Dx.rt = belief.RT["T","B+.D-"],
        belief.T.Bx.D.rt = belief.RT["T","B-.D+"],
        belief.T.B.D.rt = belief.RT["T","B+.D+"],
        belief.total.timeouts = length(belief$response[belief$response=="noresponse"]),
        belief.C.timeouts = length(belief$response[belief$response=="noresponse" & belief$trial.type == "C"),
        belief.T.Bx.Dx.timeouts = length(belief$response[belief$response=="noresponse" & belief$trial.type == "T" & belief$trial.conditionB == "B-" & belief$trial.conditionD == "D-"]),
        belief.T.B.Dx.timeouts = length(belief$response[belief$response=="noresponse" & belief$trial.type == "T" & belief$trial.conditionB == "B+" & belief$trial.conditionD == "D-"]),
        belief.T.Bx.D.timeouts = length(belief$response[belief$response=="noresponse" & belief$trial.type == "T" & belief$trial.conditionB == "B-" & belief$trial.conditionD == "D+"]),
        belief.T.B.D.timeouts = length(belief$response[belief$response=="noresponse" & belief$trial.type == "T" & belief$trial.conditionB == "B+" & belief$trial.conditionD == "D+"]),
        belief.aboveChance = belief.test$p.value < 0.05,
        belief.Ntrials.sub300ms=sum(belief$responseTime<300, na.rm=T),
        stringsAsFactors = F
      )
    }
  })
}
