learning.extract <- function(df){
  
  # first set up the list of data frames to extract the task variables from
  learning=lapply(df, split.task.results, task = "learning")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(learning, function(learning){
    if(!is.null(learning)){
      
      # first we are going to calculate some useful variables and add them to the results data frame
      
      # calculate the trial number from the block number and the trial number within the block
      learning$trial.trialNumber.total <- learning$trial.trialNumber+1+learning$trial.blockNumber*20
      
      # determine the correct response for each trial based on the relative values of the stimuli in each position
      learning$correctResp <- ifelse(learning$trial.leftRewardValue > learning$trial.rightRewardValue, 'left', 'right')
      
      # the response was correct if the choice was to the correctResp location, 
      # incorrect if the choice was to the other location, and NA is no choice was recorded
      learning$correct <- ifelse(!is.na(learning$result.choice) & learning$correctResp==learning$result.choice, 1, 
                                 ifelse(!is.na(learning$result.choice) & learning$correctResp!=learning$result.choice, 0,
                                        NA))

      # if a choice was made, points is the points associated with that response
      learning$points <- NA
      learning$points[!is.na(learning$result.choice) & learning$phase=="test" & learning$result.choice=="left"] <- 
        learning$trial.leftRewardValue[!is.na(learning$result.choice) & learning$phase=="test" & learning$result.choice=="left"]
      learning$points[!is.na(learning$result.choice) & learning$phase=="test" & learning$result.choice=="right"] <- 
        learning$trial.rightRewardValue[!is.na(learning$result.choice) & learning$phase=="test" & learning$result.choice=="right"]
      
      # the values are jittered across trials so use the findInterval function to assign value categories to the left and right positions
      
      learning$leftValue <- 
        c("BL",NA,"SL","Z","SW",NA,"BW")[findInterval(learning$trial.leftRewardValue, 
                                                                               c(-110,-90,-30,-20,20,30,90,110))]
      learning$rightValue <- 
        c("BL",NA,"SL","Z","SW",NA,"BW")[findInterval(learning$trial.rightRewardValue, 
                                                                               c(-110,-90,-30,-20,20,30,90,110))]
      
      
      # sort the left and right values alphabetically and paste with a space between them using the collapse argument to paste the elements of tha same vector
      learning$trialType <- as.character(mapply(function(left, right) paste(sort(c(left, right)), collapse = "_"), 
                                                left = learning$leftValue, right = learning$rightValue))

      #create some new data frames to hold useful information
      # match symbol numbers with names for clarity
      symbols <- 
        data.frame(
          values = c("BL", "SL", "Z", "SW", "BW"),
          symbols = c(learning$trial.leftSymbol[!is.na(learning$trial.leftSymbol) & learning$leftValue=="BL" & learning$phase=="test"][1],
                      learning$trial.leftSymbol[!is.na(learning$trial.leftSymbol) & learning$leftValue=="SL" & learning$phase=="test"][1],
                      learning$trial.leftSymbol[!is.na(learning$trial.leftSymbol) & learning$leftValue=="Z" & learning$phase=="test"][1],
                      learning$trial.leftSymbol[!is.na(learning$trial.leftSymbol) & learning$leftValue=="SW" & learning$phase=="test"][1],
                      learning$trial.leftSymbol[!is.na(learning$trial.leftSymbol) & learning$leftValue=="BW" & learning$phase=="test"][1]),
          stringsAsFactors = F)
      
      
      # create a data frame that holds the liking data
      liking <- data.frame(pre.ratings = learning[1,"symbolRatings"][[1]], 
                           post.ratings=learning[nrow(learning),"symbolRatings"][[1]], 
                           symbol = learning[1,"symbolOrder"][[1]]+1, stringsAsFactors = F)
      
      liking$value <- symbols$values[match(liking$symbol, symbols$symbols)]
      
      
      trialTypes <- as.list(unique(learning$trialType[!is.na(learning$trialType) & learning$trialType != ""]))
      names(trialTypes) <- paste("learning.", unique(learning$trialType[!is.na(learning$trialType) & learning$trialType != ""]), sep="")
      
      trials.to.75 <- as.data.frame(lapply(trialTypes, function(trialType){
        
        df <- data.frame(correct = learning$correct[learning$phase=="test" & learning$trialType==trialType],
                         exposure = seq(1:length(learning$correct[learning$phase=="test" & learning$trialType==trialType])))
        
        fit <- glm(data = df, formula = correct~exposure, family = "binomial")
        
        # here the model predicts the probability of a correct response for each exposure and, if the model predicts any p above 0.75
        # then we take the index of the first instance that it was above 0.75, otherwise we return a missing value
        data.frame(trials.75 = ifelse(sum(predict(object = fit, newdata = data.frame(exposure=seq(1:nrow(df))), type="response")>0.75), 
                               which.max(predict(object = fit, newdata = data.frame(exposure=seq(1:nrow(df))), type="response")>0.75),
                               NA),
                   max.probability = max(predict(object = fit, newdata = data.frame(exposure=seq(1:nrow(df))), type="response")))
        
      }))
      
      
      fit <- glm(data = learning, formula = correct~trial.trialNumber.total, family = "binomial")
      trials.to.75.total <- ifelse(sum(predict(object = fit, newdata = data.frame(trial.trialNumber.total=seq(1:240)), type="response")>0.75), 
                             which.max(predict(object = fit, newdata = data.frame(trial.trialNumber.total=seq(1:240)), type="response")>0.75),
                             NA)
      max.probability <- max(predict(object = fit, newdata = data.frame(trial.trialNumber.total=seq(1:240)), type="response"))
      
      #return variables of interest into a data frame
      data.frame(
        learning.totalScore	=	 sum(learning$points, na.rm=TRUE),
        learning.totalAcc	=	 mean(learning$correct, na.rm=TRUE),
        learning.total.trials.75 = trials.to.75.total,
        learning.total.max.probability <- max.probability,
        learning.actionErrors	=	 length(learning$correct[is.na(learning$correct) & learning$phase=='test']),
        
        trials.to.75, # add the whole trials.to.75 data frame
  
        learning.liking.pre.bigWin = liking$pre.ratings[liking$value=="BW"],
        learning.liking.pre.smallWin = liking$pre.ratings[liking$value=="SW"],
        learning.liking.pre.zero = liking$pre.ratings[liking$value=="Z"],
        learning.liking.pre.smallLoss = liking$pre.ratings[liking$value=="SL"],
        learning.liking.pre.bigLoss = liking$pre.ratings[liking$value=="BL"],
        
        learning.liking.post.bigWin = liking$post.ratings[liking$value=="BW"],
        learning.liking.post.smallWin = liking$post.ratings[liking$value=="SW"],
        learning.liking.post.zero = liking$post.ratings[liking$value=="Z"],
        learning.liking.post.smallLoss = liking$post.ratings[liking$value=="SL"],
        learning.liking.post.bigLoss = liking$post.ratings[liking$value=="BL"],
        
        row.names = NULL
      )
    }
  })
}
