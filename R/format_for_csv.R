
# this is done on the assumption that we will deal with these list variables at a later stage
format_for_csv <- function(raw.data){
  flat <- lapply(raw.data, function(x){
    if(is.list(x)){
      x <- as.character(x)
    } else x
  })

  # I've also reformatted character variables in a weird way so that Excel stops reading them as dates
  formatted <- lapply(flat, function(x){
    if(is.character(x)) {
      x <- paste("=", '"', x, '"', sep = "")
    }else x
  })

  as.data.frame(formatted, stringsAsFactors = F)
}
