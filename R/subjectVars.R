#' run the source for each task in the list and merge the data
#'
#' @param tasks a character vector containing the names of the tasks to be extracted
#' @param list is a list of subjects' data
#'
#' @return a data frame of summarised data from all the tasks listed with one participant per row
#'
#' @examples
#' subjectVars(c("fp", "learning", "bonus"), bySubject)
#' @export
#'
subjectVars <- function(tasks, list = bySubject){

  # apply the extract tasks function across all the selected tasks
  data <- lapply(tasks, extract_task, data = list)

  # convert the list object 'data' into a data frame 'data2'
  data2 <- data.frame(data, check.names = F)

  # remove duplicated data
  data2 <- data2[, unique(colnames(data2))]

  # rename the column 'x' as subject_id
  data2$subject <- data2$x

  # remove the some excess columns using 'select' argument of the subset function
  data2 <- subset(data2, select=-c(Row.names,x))

  data2 <- data.frame(data2, subject.number = seq(1, nrow(data2)))

  data2
}
