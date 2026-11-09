#' Attempt to match supplied column names to the standard names in the app
#' 
#' Scans the list of column names in the supplied data frame. If any of those
#' names contains the an exact match for the standard name `std_name` it returns
#' that name as the selected item for the select input.
#' 
#' @param std_name String. The standard name to match against.
#' @param user_cols List. The list of column names in the data supplied.
#' 
#' @return String or NA

match_variables <- function(std_name, user_cols) {
  
  norm_cols <- tolower(gsub("[^[:alnum:]]", "", user_cols))
  pattern   <- paste0("[a-z]*(", std_name, ")[a-z]*")
  score     <- sapply(norm_cols, function(u) { grep(pattern, u) })
  match     <- which.max(score)
  
  if (length(match) != 0) user_cols[match] else NA_character_

}
