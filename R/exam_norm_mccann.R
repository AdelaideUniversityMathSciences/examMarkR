#' Add McCann spike to data
#'
#' @param x totals
#' @param spikes place to add one
#'
#' @return new totals with no one off a boundary
#' @export
#'
#' @examples
#' exam_norm_mccann(1:100)
#' exam_norm_mccann(NA)
exam_norm_mccann  <- function(x, spikes = c(49, 64, 74, 84)){
  x  <- ifelse(
    x %in% spikes, 
    x + 1, 
    x
  )
  return(x)
}

