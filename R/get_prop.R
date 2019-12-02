#' Calculates the proportion for each mark in a vector. 
#' 
#' Assumes that colname has total after _
#'
#' @param x column of marks with total in name of vector
#'
#' @return vector of proportions.
#' @export
#'
#' @examples
#' library(examMarking)
#' data("SMI_2018_marks")
#' SMI_2018_marks  <- SMI_2018_marks %>% clean_marks_df("^A")
#' get_prop_vec(SMI_2018_marks$`Assignment 1 (77417)`, 1)
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
#' library(examMarking)
#' data("SMI_2018_marks")
#' SMI_2018_marks  <- 
#'   SMI_2018_marks %>% clean_marks_df("^A")
#' SMI_2018_marks
#' SMI_2018_marks %>% get_prop_df("^A", 1)
get_prop_df  <- function(df, assess_RE, total_row){
  df  <- dplyr::mutate_at(df, dplyr::vars(dplyr::matches(assess_RE)), get_prop_vec, total_row)
  return(df)
}
