#' create a data frame containing all the raw data for the participants
#'
#' @param task is the task of interest
#' @param data is the list of subject data (defaults to bySubject)
#' @param demo adds demographic data to the raw data frame. Defaults to TRUE
#'
#' @return a data frame containing raw data for all participants who have completed the task
#'
#' @examples
#' raw.data(bySubject, "fp")
#' @export
#'
raw.data <- function(data, task, demo = T){
  raw.list <- lapply(data, split.task.results, task = task)
  raw.list <- lapply(raw.list, add.variables, task)

  add.names <- function(subject, subject.number, data){
    if(!is.null(data)) {
      data.frame(subject.id = rep(subject, nrow(data)), subject.number = rep(subject.number, nrow(data)), data)
    }

  }
  if(demo){
    demo <- demo.extract(bySubject)

    raw.list <- mapply(FUN = function(raw.data, demo.data){
      if(!is.null(raw.data) & !is.null(demo.data)){
        data.frame(demo.data, raw.data)
      }
    },
    raw.list,
    demo)
  }
  # apply the add.names function to the raw data taking data, list names and list sequence as arguments
  raw.list <- mapply(FUN = add.names,
                     subject = names(raw.list),
                     subject.number = seq(1, length(raw.list), by = 1),
                     data = raw.list)



  data.frame(data.table::rbindlist(l = raw.list, fill = T))
}
