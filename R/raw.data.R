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
  add.names <- function(subject, data){
    if(!is.null(data)) data$subject <- rep(subject, nrow(data))
    data
  }
  raw.list <- mapply(FUN = add.names,
                     subject = names(raw.list),
                     data = raw.list)
  data.table::rbindlist(l = raw.list, fill = T)
}
