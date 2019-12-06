#' Get grade plot
#'
#' @param df data frame of marks
#' @param grade_col column with grades
#'
#' @return ggplot
#' @export
#'
#' @examples
#' marks %>% exam_plot_grades(grade)
exam_plot_grades  <- function(df, grade_col){
  p  <- 
    df %>% 
    exam_grade_summary({{grade_col}}) %>% 
    ggplot(aes(grade, Prop, fill = grade)) + 
    geom_bar(stat = "identity", show.legend = FALSE, col = "black") + 
    geom_text(aes(label = Prop), vjust = 1, col = "black", size = 5) + 
    scale_fill_brewer(palette = "Set1") + 
    labs(x = "Grade", y = "Proportion")
  return(p)
}