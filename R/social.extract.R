social.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  social=lapply(df, split.task.results, task = "social")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(social, function(social){
    if(!is.null(social)){
      # this is a simple one. We just need the first line of the first and second 
      # columns of the social results
      data.frame(social.7day.contacts=social[1,1], social.30day.contacts=social[1,2])
    }
  })
}
