#' Calculates the proportion for each mark in a vector. 
#' 
#' Assumes that colname has total after _
#'
#' @param x column of marks with total in name of vector
#'
#' @return vector of proportions.
get_prop_vec  <- function(x, total_row){
  x  <- as.numeric(x)
  total  <- x[total_row]
  x  <- x / total
  return(x)
}

#' Get proportions for dataframe.
#' 
#' Identifies columns by regular expression
#'
#' @param df data frame of marks
#' @param assess_RE regular expression to choose columns
#'
#' @return data frame with proportions
#' @export
#'
#' @examples
#' data("SMI_2018_marks")
#' SMI_2018_marks  <-
#'   SMI_2018_marks %>%
#'   exam_canvas_clean("^A")
#' SMI_2018_marks
#' SMI_2018_marks %>%
#'   exam_calculate_prop("^A", 1)
exam_calculate_prop  <- function(df, assess_RE, total_row){
  df  <- dplyr::mutate_at(df, dplyr::vars(dplyr::matches(assess_RE)), get_prop_vec, total_row)
  return(df)
}
