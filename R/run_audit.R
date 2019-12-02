#' Run audit
#'
#' @param df data frame of marks
#' @param mark_col colname of column with final marks
#' @param width width of band to filter on
#' @param boundary vector of boundary points. 
#'
#' @return nothing - just prints to screen cases to check. 
#' @export
#'
#' @examples
#' data(SMI_2018_marks_clean)
#' SMI_2018_marks_clean
#' run_audit(SMI_2018_marks, Total)
run_audit  <- function(df, mark_col, width = 1, boundary = c(50, 65, 75, 85)){
  audit  <- NULL
  for(i in boundary){
    tmp  <- 
      df %>% 
      filter({{mark_col}} <= i + width, {{mark_col}} >= i - width) 
    audit  <- bind_rows(audit, tmp)
  }
  return(audit)
}
# data(SMI_2018_marks_clean)
# SMI_2018_marks_clean
# run_audit(SMI_2018_marks_clean, Total)