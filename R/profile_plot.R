#' Plots the exam questions to look for seperation
#'
#' @param df data frame with results
#' @param RE regular expression of exam questions
#' @param grade column with grades
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' library(examMarking)
#' data("SMI_2018_marks_clean")
#' SMI_2018_marks_clean %>% profile_plot("^Q", grade)
profile_plot  <- function(df, RE, grade){
  grade = enquo(grade)
  df %>% 
    select(matches(RE), grade = !!grade) %>% 
    gather(key = "part", value = "value", -grade) %>% 
    group_by(grade, part) %>% 
    summarise(value = mean(value, na.rm = TRUE)) %>% 
    group_by(part) %>% 
    mutate(mean = mean(value, na.rm = TRUE)) %>% 
    ggplot(aes(part, value, col = grade)) + 
    geom_line(aes(group = grade)) + 
    geom_point() + 
    geom_point(col = "black", size = 3, aes(y = mean))
}

# library(examMarking)
# data("SMI_2018_marks_clean")
# SMI_2018_marks_clean %>% profile_plot("^Q", grade)
