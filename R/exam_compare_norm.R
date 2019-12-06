#' Compare raw and norm marks
#'
#' @param df marks data frame
#' @param raw column of raw marks
#' @param norm column of norm marks
#' @param ... grading parameters
#'
#' @return table comparing proportions
#' @export
#'
#' @examples
#' marks %>% 
#' mutate(norm = exam_norm_glonek(total, a = 1, b = 1.1)) %>% 
#'   exam_compare_norm(total, norm, first_year = "TRUE")
exam_compare_norm <- function(df, raw, norm, first_year = FALSE) {
  norm_summary  <- 
    df %>% 
    mutate(
      raw_grade = exam_grade({{raw}}, first_year = first_year, trace = FALSE), 
      norm_grade = exam_grade({{norm}}, first_year = first_year, trace = FALSE)) %>% 
    select(raw_grade, norm_grade) %>% 
    pivot_longer(cols = raw_grade:norm_grade) %>% 
    count(name, value) %>% 
    group_by(name) %>% 
    mutate(
      N = sum(n), 
      p = n / N     
    ) %>% 
    select(-n, -N) %>% 
    pivot_wider(values_from = p, values_fill = list(p = 0)) %>% 
    select(Grade = value, 
           `Proportion for raw` = raw_grade,
           `Proportion for norm` = norm_grade
    )
  return(norm_summary)
}

