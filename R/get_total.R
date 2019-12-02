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
#' library(examMarking)
#' data("SMI_2018_marks")
#' SMI_2018_marks$ET  <- SMI_2018_marks %>% get_total("^Q")
get_total  <- function(df, RE){
  # Select columns for total
  df  <- df %>% dplyr::select(dplyr::matches(RE))
  total  <- apply(df, 1, sum, na.rm = TRUE)
  return(total)
}

