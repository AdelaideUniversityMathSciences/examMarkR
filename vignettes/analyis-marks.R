## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, 
  fig.width = 6, 
  fig.asp = 0.618, 
  out.width = "70%",
  fig.align = "center", 
  warning = FALSE, 
  message = FALSE
)
library(tidyverse)

## ---- message=FALSE-----------------------------------------------------------
library(tidyverse)
library(examMarkR)

## -----------------------------------------------------------------------------
data("SMI_2018_marks")
SMI_2018_marks

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  filter(str_detect(`Assignment 1 (77417)`, "EX"))

## -----------------------------------------------------------------------------
colnames(SMI_2018_marks)  <- c("ID", 
                               str_c("A", 1:5), 
                               "P", 
                               str_c("OQ0", 1:8), 
                               str_c("Q", 1:6))

## -----------------------------------------------------------------------------
SMI_2018_marks
SMI_2018_marks  <- 
  SMI_2018_marks %>% 
  clean_marks_df("^A") %>% 
  clean_marks_df("^P$") %>% 
  clean_marks_df("^OQ") %>% 
  clean_marks_df("^Q")

## -----------------------------------------------------------------------------
SMI_2018_marks <- 
  SMI_2018_marks %>% 
  get_prop_df("^A", total_row = 1) %>% # Assignments
  get_prop_df("^P$", total_row = 1) %>% # project
  get_prop_df("^OQ", total_row = 1) # Online quizzes
SMI_2018_marks

## -----------------------------------------------------------------------------
SMI_2018_marks$E  <- 
  SMI_2018_marks %>% 
  get_total("^Q")

## -----------------------------------------------------------------------------
SMI_2018_marks  <- 
  SMI_2018_marks %>% 
  get_prop_df("^E$|^Q", total_row = 1)
SMI_2018_marks

## -----------------------------------------------------------------------------
SMI_2018_marks$A  <- 
  SMI_2018_marks %>% 
  get_mean("^A")
SMI_2018_marks$OQ  <- 
  SMI_2018_marks %>% 
  get_mean("^OQ")

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  select(ID, A, OQ, P, E)

## -----------------------------------------------------------------------------
SMI_2018_marks  <- 
  SMI_2018_marks %>% 
  mutate(
    A = A * 15, 
    OQ = OQ * 5,
    P = P * 10, 
    E = E * 70
  )

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  select(ID, A, OQ, P, E)

## -----------------------------------------------------------------------------
SMI_2018_marks$Total  <- 
  SMI_2018_marks %>% 
  get_total("^A$|^OQ$|^P$|^E$")

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  select(ID, A, OQ, P, E, Total)

## -----------------------------------------------------------------------------
SMI_2018_marks$grade  <- get_grades(SMI_2018_marks$Total, trace = TRUE)

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  select(ID, A, OQ, P, E, Total, grade)

## -----------------------------------------------------------------------------
SMI_2018_marks  <- SMI_2018_marks[-1, ]
SMI_2018_marks

## -----------------------------------------------------------------------------
get_grade_summary(SMI_2018_marks$grade)

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  plot_assessment(RE = "^A$|^OQ$|^P$|^E$", 
                  name = ID, 
                  grade = grade)

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  profile_plot(RE = "^Q", 
               grade = grade)

## -----------------------------------------------------------------------------
SMI_2018_marks %>% 
  profile_plot(RE = "^A\\d", 
               grade = grade)

## -----------------------------------------------------------------------------
tibble(raw = 0:100, 
           norm = norm_glonek(raw, a = 0.5, b = 2)) %>% 
  ggplot(aes(raw, norm)) + 
  geom_point()

## -----------------------------------------------------------------------------
SMI_2018_marks  <- 
  SMI_2018_marks %>% 
  mutate(norm = norm_glonek(Total, a = 0.2, b = 2))

## -----------------------------------------------------------------------------
boundary_pts  <- tribble(
  ~x, ~y, 
  25, 50, 
  75, 90
)

## -----------------------------------------------------------------------------
df  <- tibble(
  x = 0:100
) %>% 
  mutate(
    y = norm_linear_inter(x, boundary_pts = boundary_pts)
)

## -----------------------------------------------------------------------------
df %>% ggplot(aes(x, y)) + geom_point() + 
  geom_point(data = boundary_pts, aes(col = "BP"))

## -----------------------------------------------------------------------------
norm_compare(SMI_2018_marks$Total, SMI_2018_marks$norm)

## -----------------------------------------------------------------------------
df  <- tibble(
  total = 1:100
)
df  <- df %>% 
  mutate(norm_total = add_mccann_spike(total))
df %>% ggplot(aes(total, norm_total)) + geom_point()
norm_compare(df$total, df$norm_total)

## -----------------------------------------------------------------------------
grade_roster  <- read_lines("../inst/grade_example.csv")
grade_roster
get_enrolled("../inst/grade_example.csv")

## -----------------------------------------------------------------------------
grade_roster  <- read_lines("../inst/grade_example.csv")
grade_roster

## -----------------------------------------------------------------------------
SMI_2018_marks  <- 
  SMI_2018_marks %>% 
  mutate(Total = round(Total))

## -----------------------------------------------------------------------------
fill_grade_roster(SMI_2018_marks$ID, SMI_2018_marks$Total, 
                  infile = "../inst/grade_example.csv", 
                  outfile = "../inst/example_output.csv")

## -----------------------------------------------------------------------------
grade_roster  <- read_lines("../inst/example_output.csv")
grade_roster

## -----------------------------------------------------------------------------
run_audit(SMI_2018_marks, Total)

