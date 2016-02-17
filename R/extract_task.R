#' source and run task extraction script
#'
#' @param data all data split by subjects
#' @param task the name of the task, e.g. "fp"
#'
#' @return the results data frame from complete sessions of that task along with the date created and the "tags" that tell you the settings version.
#'
#' @examples
#' extract_task(bySubject, "fp")
#' @export
#'
#'
extract_task <- function(data, task){
  subject <- names(data)

  #assign extraction code for that task to the generic name 'func'
  func <- get(paste(task,".extract", sep=""))

  #run the extraction code for that task
  sVars <- func(data)

  # add participant id each data frame in this list
  sVars <- mapply(function(x, y) merge(x, y, by = 0, all = T), x = subject, y = sVars, SIMPLIFY = F)

  # bind all the elements in the list together filling in any blanks with NA
  data.table::rbindlist(sVars, fill = T)
}

