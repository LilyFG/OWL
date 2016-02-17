fp2.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  fp2=lapply(df, split.task.results, task = "fp2")     

  # then apply a the data extraction function across that list where it is not empty
  lapply(fp2, function(fp2){
    if(!is.null(fp2)){
      fp2$accuracy[fp2$trial.isCorrect == 'TRUE'] <- 1
      fp2$accuracy[fp2$result.response == 'ok' & fp2$trial.isCorrect == 'FALSE'] <- 0
      fp2$accuracy[fp2$result.response != 'ok' & fp2$trial.isCorrect == 'FALSE'] <- NA
      
      fp2$kind <- ifelse(fp2$phase=="test", fp2[,c("trial.right.kind", "trial.left.kind")][!is.na(fp2[,c("trial.right.kind", "trial.left.kind")])], NA)
      
      # spit out the data frame containing the variables of interest
      data.frame(
        fp2.pres.time = fp2$trial.t[fp2$phase=="test"][1],
        fp2.happy.acc	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'],
                                na.rm=TRUE),
        fp2.neutral.acc	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'],
                             na.rm=TRUE),
        fp2.angry.acc	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'],
                             na.rm=TRUE),
        fp2.happy.acc.early	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'][1:as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'])/2)],
                             na.rm=TRUE),
        fp2.neutral.acc.early	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'][1:as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'])/2)],
                               na.rm=TRUE),
        fp2.angry.acc.early	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'][1:as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'])/2)],
                             na.rm=TRUE),
        fp2.happy.acc.late	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'][as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'])/2)+1:
                                                                                        length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='happy'])],
                                   na.rm=TRUE),
        fp2.neutral.acc.late	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'][as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'])/2)+1:
                                                                                            length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='neutral'])],
                                     na.rm=TRUE),
        fp2.angry.acc.late	= mean(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'][as.integer(length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'])/2)+1:
                                                                                        length(fp2$accuracy[fp2$phase=='test' & fp2$kind=='angry'])],
                                   na.rm=TRUE),
        fp2.happy.endRT	= mean(fp2$result.responseTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'],
                             na.rm=TRUE),
        fp2.neutral.endRT	= mean(fp2$result.responseTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'],
                               na.rm=TRUE),
        fp2.angry.endRT	= mean(fp2$result.responseTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'],
                             na.rm=TRUE),
        fp2.happy.initRT	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'],
                               na.rm=TRUE),
        fp2.neutral.initRT	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'],
                                 na.rm=TRUE),
        fp2.angry.initRT	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'],
                               na.rm=TRUE),
        
        fp2.happy.initRT.early	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'][1:as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'])/2)],
                                   na.rm=TRUE),
        fp2.neutral.initRT.early	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'][1:as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'])/2)],
                                     na.rm=TRUE),
        fp2.angry.initRT.early	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'][1:as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'])/2)],
                                   na.rm=TRUE),
        fp2.happy.initRT.late	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'][as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'])/2)+1:
                                                                                        length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='happy'])],
                                  na.rm=TRUE),
        fp2.neutral.initRT.late	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'][as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'])/2)+1:
                                                                                            length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='neutral'])],
                                    na.rm=TRUE),
        fp2.angry.initRT.late	= mean(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'][as.integer(length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'])/2)+1:
                                                                                        length(fp2$result.outTime[fp2$phase=='test' & fp2$accuracy == 1 & fp2$kind=='angry'])],
                                  na.rm=TRUE),
        fp2.happy.goodCount	= length(fp2$accuracy[!is.na(fp2$accuracy) & fp2$phase=='test' & fp2$kind=='happy']),
        fp2.neutral.goodCount	= length(fp2$accuracy[!is.na(fp2$accuracy) & fp2$phase=='test' & fp2$kind=='neutral']),
        fp2.angry.goodCount	= length(fp2$accuracy[!is.na(fp2$accuracy) & fp2$phase=='test' & fp2$kind=='angry']),
        fp2.actionErrors	= length(fp2$accuracy[is.na(fp2$accuracy) & fp2$phase=='test'])
      )
    }
  })
}


