#' create a data frame containing all the raw data for the participants
#'
#' @param task is the task of interest
#' @param data is the list of subject data (defaults to bySubject)
#'
#' @return a data frame containing raw data for all participants who have completed the task
#'
#' @examples
#' raw.data(bySubject, "fp")
#' @export
#'
raw.data <- function(data, task){
  raw.list <- lapply(data, split.task.results, task = task)
  raw.list <- lapply(raw.list, add.variables, task)
#   add.demo <- function(data){
#     if(!is.null(data)) {
#     }
#   }
#   if(task!= "demo"){
#     raw.list <- lapply(raw.list, add.demo)
#   }
  add.names <- function(subject, subject.number, data){
    if(!is.null(data)) {
      print(nrow(data))
      data$subject <- rep(subject, nrow(data))
      data$subject.number <- rep(subject.number, nrow(data))
    }
    data
  }
  raw.list <- mapply(FUN = add.names,
                     subject = names(raw.list),
                     subject.number = seq(1, length(raw.list), by = 1),
                     data = raw.list)
  data.table::rbindlist(l = raw.list, fill = T)
}
