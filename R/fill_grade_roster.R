#' fill_grade_roster
#' 
#' Still need to open, check and save
#' 
#' @param IDs vector of IDs
#' @param total vector of marks - order corresponds to IDs
#' @param infile empty grade roster
#' @param outfile lace to save complete grade roster
#'
#' @return VOID
#' @export
#'
#' @examples
#' pacman::p_load(tidyverse, examMarking)
#' set.seed(2019)
#' df  <- tibble(
#'   ID = 1:7,
#'   total = c(sample(80:100, 4), 0, 49, 44)
#' )
#' df
#' fill_grade_roster(df$ID, df$total,
#'                   infile = "inst/grade_example.csv",
#'                   outfile = "inst/example_output.csv")
#' read_lines("inst/example_output.csv")
#' fill_grade_roster(df$ID, df$total,
#'                   infile = "inst/grade_example.csv",
#'                   outfile = "inst/example_output_2.csv",
#'                   supp = 40:49)
#' read_lines("inst/example_output_2.csv")
#' fill_grade_roster(df$ID, df$total,
#'                   infile = "inst/grade_example.csv",
#'                   outfile = "inst/example_output_3.csv",
#'                   RP = c(2, 5))
#' read_lines("inst/example_output_3.csv")
fill_grade_roster  <- function(IDs, total, 
                               infile, outfile, 
                               supp = 45:49, supp_code = "US10", 
                               RP = NULL){
  # Read in the grade roster
  grade_roster  <- read_lines(infile)
  # Get header
  header  <- grade_roster[1:7]
  # Write out header
  write_lines(header, outfile)
  # data 
  n  <- length(grade_roster)
  data  <- grade_roster[8:n]  
  data  <- read_csv(data)
  # Add totals
  for(i in 1:nrow(data)){
    ID  <- data$EmplID[i]
    index  <- which(IDs == ID)
    if(length(index) > 0){
      data$`Mark/Grade Input`[i]  <- total[index]
      if(total[index] %in% supp){
        data$`Transcript Note ID`[i]  <- supp_code
      }
      if(total[index] == 0){
        data$`Mark/Grade Input`[i]  <- "FNS"
      }
      if(ID %in% RP){
        data$`Mark/Grade Input`[i]  <- "RP"
      }
    }
  }
  write_csv(data, outfile, append = TRUE, na = "", col_names = TRUE)
}
# pacman::p_load(tidyverse, examMarking)
# set.seed(2019)
# df  <- tibble(
#   ID = 1:7,
#   total = c(sample(80:100, 4), 0, 49, 44)
# )
# df
# fill_grade_roster(df$ID, df$total,
#                   infile = "inst/grade_example.csv",
#                   outfile = "inst/example_output.csv")
# read_lines("inst/example_output.csv")
# fill_grade_roster(df$ID, df$total,
#                   infile = "inst/grade_example.csv",
#                   outfile = "inst/example_output_2.csv",
#                   supp = 40:49)
# read_lines("inst/example_output_2.csv")
# fill_grade_roster(df$ID, df$total,
#                   infile = "inst/grade_example.csv",
#                   outfile = "inst/example_output_3.csv",
#                   RP = c(2, 5))
# read_lines("inst/example_output_3.csv")