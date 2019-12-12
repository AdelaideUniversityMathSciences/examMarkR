#' Plots the assessment for each student
#' 
#' Only good for small coursese
#'
#' @param df data frame of course
#' @param RE regular expression of section totals
#' @param grades vector of grades
#' @param names names of students
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' data("SMI_2018_marks_clean")
#' SMI_2018_marks_clean %>% 
#' exam_plot_assessment(RE = "^A$|^OQ$|^P$|^E$", name = ID, grade = grade)
exam_plot_assessment  <- function(df, RE, name, grade){
  ## Get columns 
  df  <- 
    df %>% 
    dplyr::select(dplyr::matches(RE), name = {{name}}, grade = {{grade}})
  # Get the assessment columns
  # Add total 
  df$total  <- df %>% select(-name, -grade) %>% apply(., 1, sum, na.rm = TRUE)
  df  <- df %>% 
    gather(key = "Assessment", value = "mark",
           -name, -grade, -total) %>% 
    mutate(grade = factor(grade,
                          levels = c("HD", "D", "C", "P", "FA", "FNA", "FNS"))) 
  df %>% 
    ggplot(aes(x = mark, y = forcats::fct_reorder(name, total),
               col = Assessment)) +
    geom_point() +
    geom_point(aes(x = total, col = "Total")) +
    labs(y = "Student") +
    facet_grid(grade ~ ., scales = "free", space = "free")
}

# data("SMI_2018_marks_clean")
# SMI_2018_marks_clean %>% 
# exam_plot_assessment(RE = "^A$|^OQ$|^P$|^E$", name = ID, grade = grade)