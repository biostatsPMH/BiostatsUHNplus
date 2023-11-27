#' Modification of the as.numeric function that prints entries that 
#' fail to parse as a message
#' 
#' @param x string or vector to coerce to numeric
#' @keywords as.numeric
#' @export
#' @examples
#' z <- as_numeric_parse(c(1:5, "String1",6:10,"String2"))
#' z
as_numeric_parse <- function(x){
  # Arguments:
  # x = string or vector to coerce to numeric
  
  if (!class(x) %in% c("logical","numeric","double","integer","character","Date")) {
    stop("Invalid data type")
  }
  
  y <- suppressWarnings(as.numeric(x))
  noparse <- x[!is.na(x) & is.na(y)]
  
  if (length(noparse) > 0) {
    noparse_warn <- paste0("Entry ", which(!is.na(x) & is.na(y)), ", '", noparse, "'")
    message("The following entries were converted to NA values:")
    for (i in 1:length(noparse_warn)){
      message(noparse_warn[i])
    }
  }
  return(y)
}
