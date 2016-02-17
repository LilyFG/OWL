#' split one participant's results by task
#' 
#' @param df data frame containing one subject's data to be split
#' @param task the name of the task, e.g. "fp"
#' 
#' @return the results data frame from complete sessions of that task along with the date created and the "tags" that tell you the settings version.
#' 
#' @examples
#' split.task.results(df = bySubject[[1]], task = "fp")
#' @export

#function to split the task results up by task and extract the complete data for the chosen task
split.task.results <- function(df, task=task){
  # split the subject's data into a list of dataframes with the task as the data-frame label 
  tasks <- split(df$tasks[[1]], as.factor(df$tasks[[1]]$task))
  
  # if the required task is in the subject's data
  if(task %in% names(tasks) & task != "demo"){
    # check whether there is data, and then whether there is complete data
    if(nrow(tasks[[task]]$sessions[[1]])>0 && 
       nrow(subset(tasks[[task]]$sessions[[1]], state=="complete"))>0){
      # create a data frame with the first complete dataset (there should only ever be one)
      data.frame(subset(tasks[[task]]$sessions[[1]], state=="complete")$results[[1]], 
                 # add when the dataset was created from the sessions level. other information from the 'session' level can be added here too, e.g. setting file tags
                 created = c(subset(tasks[[task]]$sessions[[1]], state=="complete")$created[[1]]),
                 tags = c(subset(tasks[[task]]$sessions[[1]], state=="complete")$tags[[1]][1]))
    }
  }else if (task == "demo") {
    # check whether there is data
    if(!is.null(tasks[[task]]) && nrow(tasks[[task]]$sessions[[1]])>0){
      # create a data frame with the first complete dataset (there should only ever be one)
      data.frame(subset(tasks[[task]]$sessions[[1]])$results[[1]])
    }
  }else NULL
}