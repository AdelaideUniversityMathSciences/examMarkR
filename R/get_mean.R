#' takes data frame and regular expression and calculates the mean for the given 
#' columns. Will take into account exemmptions
#'
#' @param df data frame of marks
#' @param RE regular expression to choose columns to average
#'
#' @return vector of means
#' @export
#'
#' @examples
#' library(examMarking)
#' data("SMI_2018_marks")
#' SMI_2018_marks <- 
#'   SMI_2018_marks %>% 
#'   clean_marks_df("^A") %>% 
#'   get_prop_df("^A", 1)
#' SMI_2018_marks %>% 
#'   get_mean("^A")
get_mean  <- function(df, RE){
  # Select columns for mean
  df  <- df %>% dplyr::select(dplyr::matches(RE))
  # get means
  mean  <- apply(df, 1, mean, na.rm = TRUE)
  return(mean)
}



