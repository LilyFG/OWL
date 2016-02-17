fp.extract <- function(df){
  
  # first set up the list of data frames to extract the task variables from
  fp <- lapply(df, split.task.results, task = "fp") 
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(fp, function(fp){
    if(!is.null(fp)){
      # create a new variable called accuracy and use the square brackets to subset certain rows
      fp$accuracy[fp$trial.isCorrect == 'TRUE'] <- 1
      
      # now we need to differentiate between some erroneous responses
      fp$accuracy[fp$trial.staircaseType == 'noGo' & fp$trial.isCorrect == 'FALSE'] <- 0
      fp$accuracy[fp$trial.staircaseType %in% c("selection1","selection2") & fp$result.response == 'ok' & fp$trial.isCorrect == 'FALSE'] <- 0
      fp$accuracy[fp$trial.staircaseType %in% c("selection1","selection2") & fp$result.response != 'ok' & fp$trial.isCorrect == 'FALSE'] <- NA
      
      # spit out the data frame containing the variables of interest
      data.frame(
        fp.selection.acc	= mean(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType %in% c("selection1","selection2")],
                                na.rm=TRUE),
        fp.nogo.acc	= mean(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'],
                           na.rm=TRUE),
        fp.nogo.acc.early	= mean(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'][1:as.integer(length(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'])/2)],
                           na.rm=TRUE),
        fp.nogo.acc.late	= mean(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'][as.integer(length(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'])/2)+1:
                                                                                                   length(fp$accuracy[fp$phase=='test' & fp$trial.staircaseType == 'noGo'])],
                                 na.rm=TRUE),
        fp.selection.endRT	= mean(fp$result.responseTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")],
                               na.rm=TRUE),
        fp.selection.initRT	= mean(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")],
                                  na.rm=TRUE),
        fp.selection.initRT.early	= mean(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")][1:as.integer(length(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")])/2)],
                                   na.rm=TRUE),
        fp.selection.initRT.late	= mean(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")][as.integer(length(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")])/2)+1:
                                                                                                                                                          length(fp$result.outTime[fp$phase=='test' & fp$accuracy==1 & fp$trial.staircaseType %in% c("selection1","selection2")])],
                                         na.rm=TRUE),
        fp.tCritAvg	= mean(c(fp$tCrit1[nrow(fp)], fp$tCrit2[nrow(fp)])),
        fp.tCrit1 = fp$tCrit1[nrow(fp)],
        fp.tCrit2 = fp$tCrit2[nrow(fp)],
        fp.selection1.goodCount	= length(fp$accuracy[!is.na(fp$accuracy) & fp$phase=='test' & fp$trial.staircaseType == "selection1"]),
        fp.selection2.goodCount	= length(fp$accuracy[!is.na(fp$accuracy) & fp$phase=='test' & fp$trial.staircaseType == "selection2"]),
        fp.nogo.goodCount	= length(fp$accuracy[!is.na(fp$accuracy) & fp$phase=='test' & fp$trial.staircaseType == 'noGo']),
        fp.actionErrors	= length(fp$accuracy[is.na(fp$accuracy) & fp$phase=='test'])
      )
    }
  })
}

