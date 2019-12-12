#' get enrolled
#' 
#' uses the grade roster to get enrolled students. 
#' 
#' @param file  grade roster
#'
#' @return tibble of enrolled
#' @export
#'
#' @examples
#' exam_get_enrolled(file = "inst/grade_example.csv")
exam_get_enrolled  <- function(file){
  # Read in the grade roster
  grade_roster  <- read_lines(file)
  n  <- length(grade_roster)
  data  <- grade_roster[8:n]  
  data  <- read_csv(data)
  data  <- data[, 1:4]
  return(data)
}
# exam_get_enrolled(file = "inst/grade_example.csv")

