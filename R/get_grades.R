#' convert integer marks to grades
#'
#' uses a table of upper and lower points to assign
#' grades
#'
#' @param marks vector of integer marks
#' @param grades table of grade boundaries, needs cols: grade, lower, upper
#'
#' @return grades
#' @export
#' @examples
#' library(examMarking)
#' data_frame(marks = 0:100, grade = get_grades(marks)) %>% 
#'  ggplot(aes(marks, grade, col = grade)) + 
#'  geom_line(size = 4) + 
#'  scale_x_continuous(breaks = seq(0, 100, 5))
get_grades <- function(marks, grade_tab=NULL, trace = FALSE) {
  # round marks to ensure that grades fine
  marks <- round(marks)
  if(is.null(grade_tab)){
    grade_tab <- dplyr::tibble(grade = c("FNS","FNA","FA","P","C","D","HD"),
                         lower = c(0, 1,  45, 50, 65, 75, 85),
                         upper = c(0, 44, 49, 64, 74, 84, 100))
  }
  if(trace){
    print(grade_tab)
  }
  grades <- character(length(marks))
  for(i in seq_along(marks)){
    for(j in seq_along(grade_tab$grade)){
      if(marks[i] < 0){
        stop("mark less than zero")
      }
      if(marks[i] > 100){
        stop("mark greater than 100")
      }
      if(marks[i] >= grade_tab$lower[j] & marks[i] <= grade_tab$upper[j]){
        grades[i] <- grade_tab$grade[j]
        break
      }
    }
  }
  grades <- factor(grades, levels = c("FNS","FNA","FA","P","C","D","HD"))
  return(grades)
}
