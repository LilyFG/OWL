risky.extract <- function(df){
  require(plyr)
  require(reshape)
  
  # first set up the list of data frames to extract the task variables from
  risky=lapply(df, split.task.results, task = "risky")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(risky, function(risky){
    if(!is.null(risky)){
      
      risky$bust[risky$result.safesOpened==risky$trial.thiefIndex+1] <- 1
      risky$bust[risky$result.safesOpened < risky$trial.thiefIndex+1 & risky$result.safesOpened != 0] <- 0
      
      # missed opportunity is the differences between the thief box position and the last safe opened
      # note that we did not need to add 1 to the thiefIndex because we are interested in the 'gap', which is 1 less than the difference 
      risky$missed[risky$result.safesOpened < risky$trial.thiefIndex+1] <- risky$trial.thiefIndex[risky$result.safesOpened < risky$trial.thiefIndex+1] - 
        risky$result.safesOpened[risky$result.safesOpened < risky$trial.thiefIndex+1]
      
      #shift the results for opened, bused and missed down 1 to give the results of the previous trial
      risky$previous.opened[risky$trial.phase=='test' & risky$trial.trialNumber>0] <- risky$result.safesOpened[risky$trial.phase=='test' & risky$trial.trialNumber<79]
      risky$previous.bust[risky$trial.phase=='test' & risky$trial.trialNumber>0] <- risky$bust[risky$trial.phase=='test' & risky$trial.trialNumber<79]
      risky$previous.missed[risky$trial.phase=='test' & risky$trial.trialNumber>0] <- risky$missed[risky$trial.phase=='test' & risky$trial.trialNumber<79]
      
      #calculate whether the previous trial incurred a large or small loss (>2 or <=2)
      risky$previous.missedBinary[risky$previous.missed>2] <- 1
      risky$previous.missedBinary[risky$previous.missed<=2] <- 0
      
      #label keep_keep and bust_keep trials
      risky$trialType[risky$bust==0&risky$previous.bust==0] <- 'keep_keep'
      risky$trialType[risky$bust==0&risky$previous.bust==1] <- 'bust_keep'
      
      #       #create two linear regression models to explain performance on the current trial based on the previous for 'keep_keep' trials and 'bust_keep' trials. See Buchel, 2011.
      #       lm_missed <- lm(result.safesOpened~previous.missedBinary+previous.opened, risky[risky$trialType=="keep_keep",])
      #       c_missed <- coef(lm_missed)["previous.missedBinary"]
      #       c_risk_banked <- coef(lm_missed)["previous.opened"]
      #       
      #       lm_bust <- lm(result.safesOpened~previous.opened, risky[risky$trialType=="bust_keep",])
      #       c_risk_bust <- coef(lm_bust)["previous.opened"]
      
      #set up some handy lists of variable names, and add the empty cariables to the data frame
      safes <- c("safe1", "safe2", "safe3", "safe4", "safe5", "safe6", "safe7", "safe8")
      risky[,safes] <- NA
      safes.times <- paste(safes, "time", sep=".")
      risky[,safes.times] <- NA
      
      # apply a function across each row (signified by the 1 in the apply args)
      # of the risky dataframe and bind the list it creates together into a dataframe
      # this function will put some information about the safe opening events into wide format
      
      risky <- as.data.frame(rbindlist(apply(risky, 1, function(risky){
        # check that it's a proper trial (i.e. that it was either bust or not bust)
        if(!is.na(risky$bust)){
          #if it was not bust (i.e. they banked)
          if(risky$bust==0){
            # each opened box gets 1 and the banked box (i.e. boxes opened + 1) gets 0
            risky[safes][1:risky$result.safesOpened] <- 1
            risky[safes][risky$result.safesOpened + 1] <- 0
            #else if it was a bust trial
          }else if(risky$bust==1){
            # each opened box gets 1
            risky[safes][1:risky$result.safesOpened] <- 1
          }
          # the times for each open event are recorded in a list for each trial.
          # here they are transformed into wide format
          risky[safes.times][1:risky$result.safesOpened] <- 
            as.double(risky$result.safeOpenTimes[[1]])
        }
        # the safe open times variable is now removed as it causes problems 
        # for the binding of the list
        risky$result.safeOpenTimes <- NULL
        
        # return risky
        risky
      })))
      
      # create a long format data frame with safe open data for calculatin the indifference point
      riskySafesLong <- reshape::melt(risky[risky$trial.phase=="test",c("previous.bust","previous.missedBinary",safes)], id.vars=c("previous.bust","previous.missedBinary"))
      riskySafesLong$safe <- as.integer(substr(as.character(riskySafesLong$variable),5,5))
      
      summary <- ddply(riskySafesLong, .(safe), summarise, meanNext=mean(value, na.rm=TRUE))
      riskySafesLong <- merge(riskySafesLong, summary, by=c("safe"))
      
      #calculate the individual indifference point
      g=glm(value~safe,family="binomial",riskySafesLong)
      p <- 0.5
      ip <- (log(p/(1-p)) - coef(g)[1]) / coef(g)[2]
      
      data.frame(
        #calculate some means opened safes after a large miss and a small miss
        risky.mean.opened = mean(risky$result.safesOpened[risky$trial.phase=='test'], na.rm = TRUE),
        risky.mean.largeMiss = mean(risky$result.safesOpened[risky$previous.missed>2 & risky$trialType=="keep_keep"], na.rm = TRUE),
        risky.mean.smallMiss = mean(risky$result.safesOpened[risky$previous.missed<=2 & risky$trialType=="keep_keep"], na.rm = TRUE),
        risky.mean.bust = mean(risky$result.safesOpened[risky$previous.bust==1 & risky$trialType=="bust_keep"], na.rm = TRUE),
        risky.mean.notBust = mean(risky$result.safesOpened[risky$previous.bust==0 & risky$trialType=="keep_keep"], na.rm = TRUE),
        risky.indifferencePoint = ip[[1]]
      )
    }
  })
}

