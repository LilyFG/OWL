#' get summaries of each of the variables for a given task
#' 
#' @param task is the task to be summarised
#' @param data.sum is the summary data to be summarised further
#' 
#' @return a data frame with summaries of each of the variables in the data
#' 
#' @examples
#' variable.summaries("fp", summary_data)
#' @export
#' 
variable.summaries <- function(task, data.sum){
  # extract variable names that include the task name
  variables <<- grep(paste(task, ".", sep=""), names(data.sum))
  
  #check whether there are more than one variables with each name 
  # (necessary for lapply to work properly)
  if(length(variables)>1){
    
    summary_list <- lapply(summary_data[,variables], function(x){
      if(is.numeric(x)) summary(x) else{
        if(is.character(x)){
          summary(as.factor(x))
        }
      }
    })
    s2 <- lapply(summary_list, function(df) data.frame(as.list(df)))
    data.frame(data.table::rbindlist(s2, fill = T), row.names = names(s2))
  } else data.frame(as.list(summary(summary_data[,grep(paste(task, ".", sep=""), 
                                                       names(summary_data))])), 
                    row.names = names(summary_data)[grep(paste(task, ".", sep=""), names(summary_data))])
}