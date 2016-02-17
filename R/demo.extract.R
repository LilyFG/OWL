demo.extract <- function(df){
  # first set up the list of data frames to extract the task variables from
  demo=lapply(df, split.task.results, task = "demo")
  
  # then apply a the data extraction function across that list where it is not empty
  lapply(demo, function(demo){
    if(!is.null(demo)){
      data.frame(
        demo.gender=tail(demo[demo$id==1,"response"],n=1), # add each variable from the temp dataframe to the task dataframe, 
        demo.age=tail(as.integer(demo[demo$id==2,"age"],n=1)),      # note that the 'tail' function is used with the n arg set at 1. This takes the last item from a list... 
        demo.ethnicity=tail(demo[demo$id==3,"response"],n=1), # this ensures that if this question was skipped on the first go, and answered on the second, the answer is recorded., 
        demo.residence=tail(demo[demo$id==4,"country"],n=1),  # if the question was skipped twice, the variable will record the response which was "skipped", 
        demo.income=tail(demo[demo$id==5,"response"],n=1), 
        demo.education=tail(demo[demo$id==6,"response"],n=1),
        demo.engFirstLang=tail(demo[demo$id==7,"response"],n=1),
        demo.polyLing=tail(demo[demo$id==8,"response"],n=1),
        demo.handed=tail(demo[demo$id==9,"response"],n=1),
        stringsAsFactors = F)
    }
  })
}
