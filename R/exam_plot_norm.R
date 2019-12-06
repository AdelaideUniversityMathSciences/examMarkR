#' Plot raw and norm
#' 
#' @param df data frame of data
#' @param raw raw marks column
#' @param norm normalised marks column
#'
#' @return ggplot 
#' @export
#'
#' @examples
#' marks %>% 
#' mutate(norm = round(exam_norm_glonek(total, a = 0.2, b = 1))) %>% 
#'   exam_plot_norm(total, norm) 
exam_plot_norm  <- function(df, raw, norm){
  df  <- df %>% select(raw = {{raw}}, norm = {{norm}})
  df  <- df %>%
    mutate(
      grade = exam_grade(marks = norm, trace = FALSE),
      raw_grade = exam_grade(marks = raw, trace = FALSE),
      grade = fct_rev(grade),
      change_grade = ifelse(raw_grade != grade, "yes", "no")
    )
  tab <- tibble(grade = c("FNA","FA","P","C","D","HD"),
                             lower = c(0,  45, 50, 65, 75, 85),
                             upper = c(45, 50, 65, 75, 85, 100))
  tab$grade  <- factor(tab$grade, levels = c("FNA","FA","P","C","D","HD"))
  tab$grade  <- fct_rev(tab$grade)
  p  <- 
    df %>% 
    ggplot(aes(raw, norm)) + 
    geom_rect(aes(xmin = lower, xmax = upper, ymin = lower, ymax = upper, fill = grade), 
              data = tab, inherit.aes = FALSE, alpha = 0.5) + 
    geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100)) + 
    geom_hline(aes(yintercept = lower), data = tab, col = "lightgrey") + 
    geom_vline(aes(xintercept = lower), data = tab, col = "lightgrey") + 
    geom_point(aes(col = change_grade), size = 2) + 
    scale_fill_brewer(palette = "Set1") + 
    scale_colour_manual(values = c('no' = 'black', 'yes' = 'red')) + 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    labs(x = "Raw mark", y = "Scaled mark", fill = "Grade", col = "Grade change")
  return(p)
}

