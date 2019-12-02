#' Takes column of grades and gives summary
#'
#' @param grades vector of grades
#'
#' @return table of counts and prop. 
#' @export
#'
#' @examples
#' pacman::p_load(examMarking, tidyverse)
#' data("SMI_2018_marks_clean")
#' SMI_2018_marks_clean
#' get_grade_summary(SMI_2018_marks_clean$grade)
get_grade_summary  <- function(grades){
  df  <- tibble(grade = grades)
  GS  <- 
    df %>% 
    count(grade) %>% 
    mutate(
      N = sum(n), 
      Prop = round(n / N, 2))
  return(GS)
}
