#' takes a vector of marks and converts missing to zero and EX to NA
#'
#' @param x marks 
#'
#' @return vector of marks
exam_clean_marks  <- function(x){
  x[is.na(x)]  <- 0
  x[x=="EX"]  <- NA
  x  <- as.numeric(x)
  return(x)
}
#' Takes a data frame of marks and regular expression and converts 
#' the columns whose names match regular expression
#'
#' @param df data frame of marks
#' @param RE regular expression used to identify the columns
#'
#' @return data frame with exemptions converted to value.
#' @export
#'
#' @examples
#' data("raw_data")
#' SMI_2018_marks
#' exam_canvas_clean(SMI_2018_marks, "^A")
exam_canvas_clean  <- function(df, assess_RE){
  df  <- dplyr::mutate_at(df, dplyr::vars(dplyr::matches(assess_RE)), exam_clean_marks)
  return(df)
}

