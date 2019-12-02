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
#' data_frame(raw = 0:100, norm = norm_glonek(raw, a = 0.5, b = 2)) %>% 
#'   ggplot(aes(raw, norm)) + geom_point()
norm_glonek <- function (x,a=0,b=1){
  y <- log(x/(100-x))
  y <- a + b * y
  x <- 100 * exp(y) / (1 + exp(y))
  for(i in 1:length(x)){
    if(is.na(x[i])) x[i] <- 100
  }
  return(x)
}
#' compare norm 
#' 
#' gives some basic comparions of raw and normed marks
#'
#' @param raw vector of raw marks
#' @param norm vector of normalised marks
#'
#' @return list of various comparisons
#' @export
#'
#' @examples
#' df  <- data_frame(raw = 0:100, norm = norm_glonek(raw, a = 0.2, b = 2))
#' norm_compare(df$raw, df$norm)
norm_compare <- function(raw, norm) {
  stopifnot(length(raw) == length(norm))
  df  <- tibble(
    raw = raw, 
    norm = norm, 
    raw_grade = get_grades(raw),
    norm_grade = get_grades(norm), 
    grade_change = ifelse(raw_grade != norm_grade, "yes", "no")
  )
  conf_M  <- df %>% 
    count(raw_grade, norm_grade) %>% 
    spread(norm_grade, n, fill = 0)
  return(conf_M)
}


