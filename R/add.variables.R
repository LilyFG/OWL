add.variables <- function(data, task){
  if(!is.null(data)){
    switch (task,
            "learning" = {
              
              # first we are going to calculate some useful variables and add them to the results data frame
              
              # calculate the trial number from the block number and the trial number within the block
              data$trial.trialNumber.total <- data$trial.trialNumber+1+data$trial.blockNumber*20
              
              # determine the correct response for each trial based on the relative values of the stimuli in each position
              data$correctResp <- ifelse(data$trial.leftRewardValue > data$trial.rightRewardValue, 'left', 'right')
              
              # the response was correct if the choice was to the correctResp location,
              # incorrect if the choice was to the other location, and NA is no choice was recorded
              data$correct <- ifelse(!is.na(data$result.choice) & data$correctResp==data$result.choice, 1,
                                     ifelse(!is.na(data$result.choice) & data$correctResp!=data$result.choice, 0,
                                            NA))
              
              # if a choice was made, points is the points associated with that response
              data$points <- NA
              data$points[!is.na(data$result.choice) & data$phase=="test" & data$result.choice=="left"] <-
                data$trial.leftRewardValue[!is.na(data$result.choice) & data$phase=="test" & data$result.choice=="left"]
              data$points[!is.na(data$result.choice) & data$phase=="test" & data$result.choice=="right"] <-
                data$trial.rightRewardValue[!is.na(data$result.choice) & data$phase=="test" & data$result.choice=="right"]
              
              # the values are jittered across trials so use the findInterval function to assign value categories to the left and right positions
              
              data$leftValue <-
                c("BL",NA,"SL","Z","SW",NA,"BW")[findInterval(data$trial.leftRewardValue,
                                                              c(-110,-90,-30,-20,20,30,90,110))]
              data$rightValue <-
                c("BL",NA,"SL","Z","SW",NA,"BW")[findInterval(data$trial.rightRewardValue,
                                                              c(-110,-90,-30,-20,20,30,90,110))]
              
              
              # sort the left and right values alphabetically and paste with a space between them using the collapse argument to paste the elements of tha same vector
              data$trialType <- as.character(mapply(function(left, right) paste(sort(c(left, right)), collapse = "_"),
                                                    left = data$leftValue, right = data$rightValue))
              data
            },
            'demo' = data,
            'health' = data,
            'bis' = data,
            'aq' = data,
            'dffs' = data,
            'bisbas' = data,
            'dass' = data,
            'life' = data,
            'cape' = data,
            'economic' = data,
            'tipi' = data,
            'regretq' = data,
            'counter' = data,
            'regret' = data,
            'risky' = data,
            'belief' = data,
            'dot' = data,
            'social' = data,
            'fp' = data,
            'fp2' = data,
            'fp3' = data,
            'bonus' = data)
    
  }
}
