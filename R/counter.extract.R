counter.extract <- function(df){
  counter=lapply(df, split.task.results, task = "counter")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(counter, function(counter){
    
    if(!is.null(counter)){
      # spit out the data frame containing the variables of interest
      data.frame(counter.score	= length(which(counter$choice==c(1,2,1,2))),
                 counter.close	= length(which(counter$choice[c(1,3)]==c(1,1))),
                 counter.unusual	= length(which(counter$choice[c(2,4)]==c(2,2))),
                 stringsAsFactors = F)
    }
  })
}
