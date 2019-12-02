#' Plot raw and norm
#'
#' @param df data frame of data
#' @param raw raw marks
#' @param norm normalised marks
#' @param ID student ID column
#'
#' @return ggplot 
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse)
#' df  <- data_frame(
#'   raw = 0:100, 
#'   norm = norm_glonek(raw, a = 0.2, b = 2), 
#'   ID = seq_along(raw))
#'   plot_norm(df, raw, norm, ID)
plot_norm  <- function(df, raw, norm, ID){
  df  <- df %>% select(raw = {{raw}}, norm = {{norm}}, ID = {{ID}})
  df  <- df %>% 
    mutate(
      ID = factor(ID),
      ID = fct_reorder(ID, norm), 
      grade = get_grades(norm), 
      grade = fct_rev(grade))
  df_long  <- df %>% pivot_longer(-c(ID, grade))
  p  <- df_long %>% 
    ggplot(aes(value, ID, col = name)) + 
    geom_point() + 
    labs(y = "Student") + 
    facet_grid(grade ~ ., scales = "free", space = "free")
  return(p)
}