#' Normalizes using Glonek function
#'
#' Takes marks and normalizes using Glonek function - linear
#' scale on logit scale
#'
#' @param x marks
#' @param a normalization parameter
#' @param b normalization parameter
#' @return normalized marks
#' @author Jono Tuke <simon.tuke@@adelaide.edu.au>
#' @export
#' @examples
#' df  <- tibble(
#'   raw = 0:100, 
#'   `a = 0.5, b = 1.1` = exam_norm_glonek(raw, 0.5, 1.1),
#'   `a = 0.5, b = 1.5` = exam_norm_glonek(raw, 0.5, 1.5),
#'   `a = 0.5, b = 0.9` = exam_norm_glonek(raw, 0.5, 0.9)
#' )
#' df %>% 
#'   pivot_longer(-raw) %>% 
#'   ggplot(aes(raw, value, col = name)) + 
#'   geom_point() + 
#'   geom_abline(intercept = 0, slope = 1)
exam_norm_glonek <- function (x,a=0,b=1){
  y <- log(x/(100-x))
  y <- a + b * y
  x <- 100 * exp(y) / (1 + exp(y))
  for(i in 1:length(x)){
    if(is.na(x[i])) x[i] <- 100
  }
  return(x)
}

