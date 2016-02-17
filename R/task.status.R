#' check the status of a task
#' 
#' @param task is the task to be summarised
#' @param data is the list of subject data (defaults to buSubject)
#' 
#' @return a data frame of information about the number of participants who have completed, abandoned or not started the task and
#' at which trial they abandoned
#' 
#' @examples
#' task.status("fp", bySubject)
#' @export
#' 

task.status <- function(task, list = bySubject){
  state <- lapply(list, function(df){
    byTasks <- split(df$tasks[[1]], as.factor(df$tasks[[1]]$task))
    complete <- "complete" %in% byTasks[[task]]$sessions[[1]]$state
    abandoned <- "abandoned" %in% byTasks[[task]]$sessions[[1]]$state
    state <- ifelse(abandoned==T & complete==F, "abandoned", ifelse(complete==T, "complete", "not started"))
    trials.abandoned <- ifelse(state=="abandoned",
                               mean(unlist(lapply(byTasks[[task]]$sessions, function(x) nrow(x$results[[1]])))),
                               NA)
    data.frame(state = state, trials.abandoned = trials.abandoned, stringsAsFactors = F)
  })
  
  
  df <- data.frame(rbindlist(state, fill=T), row.names = names(list))
  colnames(df) <- c(paste(task, c(".state", ".trials_abandoned"), sep=""))
  df
}
