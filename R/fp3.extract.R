fp3.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  fp3=lapply(df, split.task.results, task = "fp3")     

  # then apply a the data extraction function across that list where it is not empty
  lapply(fp3, function(fp3){
    if(!is.null(fp3)){
      fp3$accuracy[fp3$trial.isCorrect == 'TRUE'] <- 1
      fp3$accuracy[fp3$result.response == 'ok' & fp3$trial.isCorrect == 'FALSE'] <- 0
      fp3$accuracy[fp3$result.response != 'ok' & fp3$trial.isCorrect == 'FALSE'] <- NA
      
      fp3$kind <- ifelse(fp3$phase=="test", fp3[,c("trial.right.kind", "trial.left.kind")][!is.na(fp3[,c("trial.right.kind", "trial.left.kind")])], NA)
      
      # spit out the data frame containing the variables of interest
      data.frame(
        fp3.pres.time = fp3$trial.t[fp3$phase=="test"][1],
        fp3.win.acc	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'],
                                na.rm=TRUE),
        fp3.none.acc	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'],
                             na.rm=TRUE),
        fp3.loss.acc	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'],
                             na.rm=TRUE),
        fp3.win.acc.early	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'][1:as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'])/2)],
                             na.rm=TRUE),
        fp3.none.acc.early	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'][1:as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'])/2)],
                               na.rm=TRUE),
        fp3.loss.acc.early	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'][1:as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'])/2)],
                             na.rm=TRUE),
        fp3.win.acc.late	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'][as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'])/2)+1:
                                                                                        length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='win'])],
                                   na.rm=TRUE),
        fp3.none.acc.late	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'][as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'])/2)+1:
                                                                                            length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='none'])],
                                     na.rm=TRUE),
        fp3.loss.acc.late	= mean(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'][as.integer(length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'])/2)+1:
                                                                                        length(fp3$accuracy[fp3$phase=='test' & fp3$kind=='loss'])],
                                   na.rm=TRUE),
        fp3.win.endRT	= mean(fp3$result.responseTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'],
                             na.rm=TRUE),
        fp3.none.endRT	= mean(fp3$result.responseTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'],
                               na.rm=TRUE),
        fp3.loss.endRT	= mean(fp3$result.responseTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'],
                             na.rm=TRUE),
        fp3.win.initRT	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'],
                               na.rm=TRUE),
        fp3.none.initRT	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'],
                                 na.rm=TRUE),
        fp3.loss.initRT	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'],
                               na.rm=TRUE),
        
        fp3.win.initRT.early	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'][1:as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'])/2)],
                                   na.rm=TRUE),
        fp3.none.initRT.early	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'][1:as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'])/2)],
                                     na.rm=TRUE),
        fp3.loss.initRT.early	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'][1:as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'])/2)],
                                   na.rm=TRUE),
        fp3.win.initRT.late	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'][as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'])/2)+1:
                                                                                        length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='win'])],
                                  na.rm=TRUE),
        fp3.none.initRT.late	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'][as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'])/2)+1:
                                                                                            length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='none'])],
                                    na.rm=TRUE),
        fp3.loss.initRT.late	= mean(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'][as.integer(length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'])/2)+1:
                                                                                        length(fp3$result.outTime[fp3$phase=='test' & fp3$accuracy == 1 & fp3$kind=='loss'])],
                                  na.rm=TRUE),
        fp3.win.goodCount	= length(fp3$accuracy[!is.na(fp3$accuracy) & fp3$phase=='test' & fp3$kind=='win']),
        fp3.none.goodCount	= length(fp3$accuracy[!is.na(fp3$accuracy) & fp3$phase=='test' & fp3$kind=='none']),
        fp3.loss.goodCount	= length(fp3$accuracy[!is.na(fp3$accuracy) & fp3$phase=='test' & fp3$kind=='loss']),
        fp3.actionErrors	= length(fp3$accuracy[is.na(fp3$accuracy) & fp3$phase=='test'])
      )
    }
  })
}


