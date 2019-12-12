#' Get list of student emails from IDs
#'
#' @param IDs vector of IDs of form a1078881 or 1078881
#' @param form glue string for email address
#'
#' @return single string with all emails seperated by a comma
#' @export
#'
#' @examples
#' IDs  <- c('a1078881', 'a1078886')
#' exam_get_email_list(IDs)
exam_get_email_list  <- function(IDs, form = "a{IDs}@student.adelaide.edu.au"){
  # Remove `a` and then add it - daft but copes with both style of ID
  IDs  <- stringr::str_remove_all(IDs, "a")
  # Add a and also rest of email address
  IDs  <- glue::glue(form)
  # join together
  IDs  <- stringr::str_c(IDs, collapse = ", ")
  return(IDs)
}
# IDs  <- c('a1078881', 'a1078886')
# exam_get_email_list(IDs)