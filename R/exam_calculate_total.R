#' takes data frame and regular expression and calculates the totals for the given 
#' columns
#'
#' @param df data frame of marks
#' @param RE regular expression to choose columns to total
#'
#' @return vector of totals
#' @export
#'
#' @examples
# data("raw_data")
# SMI_2018_marks %>% exam_calculate_total("^Q")
exam_calculate_total  <- function(df, RE){
  # Select columns for total
  df  <- df %>% dplyr::select(dplyr::matches(RE))
  total  <- apply(df, 1, sum, na.rm = TRUE)
  return(total)
}

