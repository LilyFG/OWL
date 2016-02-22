add.variables <- function(data, task){
  if(!is.null(data)){
    switch (task,
            "learning" = {
              data <- as.data.frame(data)
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

              # add liking data
              # match symbol numbers with names for clarity
              symbols <-
                data.frame(
                  values = c("BL", "SL", "Z", "SW", "BW"),
                  symbols = c(data$trial.leftSymbol[!is.na(data$trial.leftSymbol) & data$leftValue=="BL" & data$phase=="test"][1],
                              data$trial.leftSymbol[!is.na(data$trial.leftSymbol) & data$leftValue=="SL" & data$phase=="test"][1],
                              data$trial.leftSymbol[!is.na(data$trial.leftSymbol) & data$leftValue=="Z" & data$phase=="test"][1],
                              data$trial.leftSymbol[!is.na(data$trial.leftSymbol) & data$leftValue=="SW" & data$phase=="test"][1],
                              data$trial.leftSymbol[!is.na(data$trial.leftSymbol) & data$leftValue=="BW" & data$phase=="test"][1]),
                  stringsAsFactors = F)


              # create a data frame that holds the liking data
              liking <- data.frame(pre.ratings = data$symbolRatings[1][[1]],
                                   post.ratings=data$symbolRatings[nrow(data)][[1]],
                                   symbol = data$symbolOrder[1][[1]]+1, stringsAsFactors = F)

              liking$value <- symbols$values[match(liking$symbol, symbols$symbols)]

              data[,c("BL", "SL", "Z", "SW", "BW")] <- NA
              data[!is.na(data$phase) & data$phase=="pre",c("BL", "SL", "Z", "SW", "BW")] <- liking[order(match(liking$value, c("BL", "SL", "Z", "SW", "BW"))), "pre.ratings"]
              data[!is.na(data$phase) & data$phase=="post", c("BL", "SL", "Z", "SW", "BW")] <- liking[order(match(liking$value, c("BL", "SL", "Z", "SW", "BW"))), "post.ratings"]


              data
            },
            'demo' = data,
            'health' = {
              data <- rbind.fill(data,
                                 data$followup[data$question == "Do you smoke?"][1][[1]],
                                 data$followup[data$question == "Do you drink alcohol?"][1][[1]])
              data
              },
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
            'fp' = {
              data$accuracy[data$trial.isCorrect == 'TRUE'] <- 1

              # now we need to differentiate between some erroneous responses
              data$accuracy[data$trial.staircaseType == 'noGo' & data$trial.isCorrect == 'FALSE'] <- 0
              data$accuracy[data$trial.staircaseType %in% c("selection1","selection2") & data$result.response == 'ok' & data$trial.isCorrect == 'FALSE'] <- 0
              data$accuracy[data$trial.staircaseType %in% c("selection1","selection2") & data$result.response != 'ok' & data$trial.isCorrect == 'FALSE'] <- NA

              data},
            'data' = {
              data$accuracy[data$trial.isCorrect == 'TRUE'] <- 1
              data$accuracy[data$result.response == 'ok' & data$trial.isCorrect == 'FALSE'] <- 0
              data$accuracy[data$result.response != 'ok' & data$trial.isCorrect == 'FALSE'] <- NA

              data$kind <- ifelse(data$phase=="test", data[,c("trial.right.kind", "trial.left.kind")][!is.na(data[,c("trial.right.kind", "trial.left.kind")])], NA)

              data},
            'fp3' = {
              fp3$accuracy[fp3$trial.isCorrect == 'TRUE'] <- 1
              fp3$accuracy[fp3$result.response == 'ok' & fp3$trial.isCorrect == 'FALSE'] <- 0
              fp3$accuracy[fp3$result.response != 'ok' & fp3$trial.isCorrect == 'FALSE'] <- NA

              fp3$kind <- ifelse(fp3$phase=="test", fp3[,c("trial.right.kind", "trial.left.kind")][!is.na(fp3[,c("trial.right.kind", "trial.left.kind")])], NA)

              data},
            'bonus' = data)

  }
}
