#' import OWL data, flatten and return a list of data split by subject
#'
#' @param choose is a logical where TRUE means you have to select the file and FALSE means it searches for the "results.json" file, defaults to FALSE
#'
#' @return a list contatining all the data for each participant
#'
#' @examples
#' import.owl(choose = FALSE)
#' @export
import.owl <- function(choose = F){
  file <- ifelse(choose, file.choose(), "results.json")
  #import the data file
  dat <- jsonlite::fromJSON(file, flatten = T)

  #split the file by subject
  split(dat, as.factor(dat$`_id`))
}
