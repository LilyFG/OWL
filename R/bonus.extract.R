bonus.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  bonus <- lapply(df, split.task.results, task = "bonus")
  
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(bonus, function(bonus){
    if(!is.null(bonus)){
      
      # split the learning top up and test phases up into separate data frames
      phases <- split(bonus, bonus$phase)
      topup <- phases[["learning-topup"]]
      
      # determine the correct response for each trial based on the relative values of the stimuli in each position
      topup$correctResp <- ifelse(topup$trial.leftRewardValue > topup$trial.rightRewardValue, 'left', 'right')
      
      # the response was correct if the choice was to the correctResp location, 
      # incorrect if the choice was to the other location, and NA is no choice was recorded
      topup$correct <- ifelse(!is.na(topup$result.choice) & topup$correctResp==topup$result.choice, 1, 
                              ifelse(!is.na(topup$result.choice) & topup$correctResp!=topup$result.choice, 0,
                                     NA))
      
      # calculate mean accuracy in learning top up phase
      topup.acc <- mean(topup$correct, na.rm = T)
      
      bonus <- phases[["test"]]
      
      # compute the trial type
      bonus$trialType <- as.character(lapply(bonus$result.rewardValues, function(x){
        ifelse(max(x)==0, "loss", "win")
      }))
      
      # split the data by trial type
      rewards <- split(bonus, bonus$trialType)
      
      # add the complete dataset as a third item in the list
      rewards[["total"]] <- bonus
      
      
      
      bonus_function <- function(bonus){

      
        # create a list of selected pictures on each trial
        selectedPics <- lapply(bonus$result.pictures, function(x)(x$name.id[x$selected]))
        
        # calculated the mean number of pictures reported from the sign side, the goal side and foils
        goal.mean.reported <- mean(mapply(FUN = function(selected, goalPics) sum(selected %in% goalPics),  
                                          selected = selectedPics, 
                                          goalPics = bonus$result.prizeBoxPictures))
        sign.mean.reported <- mean(mapply(FUN = function(selected, signPics) sum(selected %in% signPics),  
                                          selected = selectedPics, 
                                          signPics = bonus$result.luckyDipPictures))
        foil.mean.reported <- mean(mapply(FUN = function(selected, goalPics, signPics) sum(!(selected %in% goalPics | selected %in% signPics)), 
                                          selected = selectedPics, 
                                          goalPics = bonus$result.prizeBoxPictures, 
                                          signPics = bonus$result.luckyDipPictures))
        
        # calculate the proportion of trials when goal and sign pictures from each serial position were recalled
        goal.memory <- apply(mapply(function(goalPics, memoryArray, selected){
          ifelse(goalPics %in% memoryArray$name.id, goalPics %in% selected, NA)
        }, 
        goalPics = bonus$result.prizeBoxPictures, memoryArray = bonus$result.pictures, selected = selectedPics), MARGIN = 1, FUN = mean, na.rm=T)
        names(goal.memory) <- paste("bonus.goal.memory", 1:4, sep=".")
        
        sign.memory <- apply(mapply(function(signPics, memoryArray, selected){
          ifelse(signPics %in% memoryArray$name.id, signPics %in% selected, NA)
        }, 
        signPics = bonus$result.luckyDipPictures, memoryArray = bonus$result.pictures, selected = selectedPics), MARGIN = 1, FUN = mean, na.rm=T)
        names(sign.memory) <- paste("bonus.sign.memory", 1:4, sep=".")
        
        # calculate the proportion correct responses in the value selection task
        reward.choice.accuracy <- sum(bonus$result.rewardResponse.status=="correct")/nrow(bonus)
        
        
        # repeat the process split by trial type (win or loss)
        
  
          
        
        data.frame(
          goal.mean.reported = goal.mean.reported,
          sign.mean.reported = sign.mean.reported,
          foil.mean.reported = foil.mean.reported,
          goal.memory.1 = goal.memory[1],
          goal.memory.2 = goal.memory[2],
          goal.memory.3 = goal.memory[3],
          goal.memory.4 = goal.memory[4],
          sign.memory.1 = sign.memory[1],
          sign.memory.2 = sign.memory[2],
          sign.memory.3 = sign.memory[3],
          sign.memory.4 = sign.memory[4],
          reward.choice.acc = reward.choice.accuracy,
          row.names = NULL
        )
      }
      
      # apply that function across the three data frames in the rewards list
      dfs <- lapply(rewards, bonus_function)
      
      df <- data.frame(dfs, topup.acc)
      
      names(df) = paste("bonus", names(df), sep = ".")
      
      df
    }
  })
}

