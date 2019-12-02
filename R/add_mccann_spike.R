#' Add McCann spike to data
#'
#' @param x totals
#' @param spikes place to add one
#'
#' @return new totals with no one off a boundary
#' @export
#'
#' @examples
#' add_mccann_spike(1:100)
#' add_mccann_spike(NA)
add_mccann_spike  <- function(x, spikes = c(49, 64, 74, 84)){
  x  <- ifelse(
    x %in% spikes, 
    x + 1, 
    x
  )
  return(x)
}

# add_mccann_spike(1:100)
# add_mccann_spike(NA)
