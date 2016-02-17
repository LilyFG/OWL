cape.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  cape=lapply(df, split.task.results, task = "cape")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(cape, function(cape){
    if(!is.null(cape)){
      cape <- cape[complete.cases(cape$score),]
      if(nrow(cape)==42){
        # add the subscales
        cape$subscale <- ifelse(cape$id %in% c(1,	9,	12,	14,	19,	38,	39,	40), "depression", 
                                ifelse(cape$id %in% c(3,	4,	8,	16,	18,	21,	23,	25,	27,	29,	32,	35,	36,	37), "negative", 
                                       ifelse(cape$id %in% c(2,	5,	6,	7,	10,	11,	13,	15,	17,	20,	22,	24,	26,	28,	30,	31,	33,	34,	41,	42), "positive", "error")))
        
        # spit out the data frame containing the variables of interest
        data.frame(
          cape.score = sum(cape$score),
          cape.distress.score = sum(cape$scoreF, na.rm=TRUE),
          cape.depression = sum(cape$score[cape$subscale=="depression"]),
          cape.negative = sum(cape$score[cape$subscale=="negative"]),
          cape.positive = sum(cape$score[cape$subscale=="positive"]),
          stringsAsFactors = F)
      }
    }
  })
}

