#' Linear interpolation norm
#'
#' @param x marks
#' @param boundary_pts gives mapping from raw to norm. Always add (0,0) and (100, 100)
#'
#' @return norm marks
#' @export
#'
#' @examples
#' boundary_pts  <- tribble(
#'   ~x, ~y, 
#'   25, 50, 
#'   75, 90
#' )
#' df  <- tibble(
#'   x = 0:100
#' ) %>% 
#'   mutate(
#'     y = norm_linear_inter(x, boundary_pts = boundary_pts)
#' )
#' df %>% ggplot(aes(x, y)) + geom_point()
norm_linear_inter  <- function(x, boundary_pts){
  # Check that we have columns x and y
  stopifnot(
    "x" %in% colnames(boundary_pts), 
    "y" %in% colnames(boundary_pts))
  x1 = c(0, boundary_pts$x)
  x2 = c(boundary_pts$x, 100)
  y1 = c(0, boundary_pts$y)
  y2 = c(boundary_pts$y, 100)
  # Check increasing slope
  for(i in 1:length(x1)){
    slope  <- (y2[i] - y1[i]) / (x2[i] - x1[i])
    if(slope <= 0){
      stop("Must have postive normalisation slope")
    }
  }
  y  <- numeric(length(x))
  for(i in 1:length(x)){
    for(j in 1:length(x1)){
      if(x1[j] <= x[i] & x[i] <= x2[j]){
        y[i]  <- y1[j] + (y2[j] - y1[j]) / (x2[j] - x1[j]) * (x[i] - x1[j])
      }
    }
  }
  return(y)
}
