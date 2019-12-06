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
#' raw_data %>% 
#'   exam_canvas_clean("^A") %>% 
#'   exam_calculate_prop("^A", 1) %>% 
#'   exam_calculate_mean("^A")
exam_calculate_mean  <- function(df, RE){
  # Select columns for mean
  df  <- df %>% dplyr::select(dplyr::matches(RE))
  # get means
  mean  <- apply(df, 1, mean, na.rm = TRUE)
  return(mean)
}



