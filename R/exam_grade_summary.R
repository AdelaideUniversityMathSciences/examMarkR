#' Takes column of grades and gives summary
#'
#' @param grades vector of grades
#'
#' @return table of counts and prop. 
#' @export
#'
#' @examples
#' marks %>% exam_grade_summary(grade)
exam_grade_summary  <- function(df, grade_col){
  GS  <- 
    df %>% 
    count({{grade_col}}) %>% 
    mutate(
      N = sum(n), 
      Prop = round(n / N, 2))
  return(GS)
}