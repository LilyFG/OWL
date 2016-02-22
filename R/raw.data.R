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
<<<<<<< HEAD
  #   add.demo <- function(data){
  #     if(!is.null(data)) {
  #     }
  #   }
  #   if(task!= "demo"){
  #     raw.list <- lapply(raw.list, add.demo)
  #   }
  add.names <- function(subject, subject.number, data){
    # check for data
    if(!is.null(data)) {
      # create data frame with subject id, subject number and data
      data.frame(subject.id = rep(subject, nrow(data)), subject.number = rep(subject.number, nrow(data)))
    }
=======
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
>>>>>>> 6d9b01796f86941987e75d32dceaf7bc759de0a9
  }

  # apply the add.names function to the raw data taking data, list names and list sequence as arguments
  raw.list <- mapply(FUN = add.names,
                     subject = names(raw.list),
                     subject.number = seq(1, length(raw.list), by = 1),
                     data = raw.list)
  data.frame(data.table::rbindlist(l = raw.list, fill = T))
}
