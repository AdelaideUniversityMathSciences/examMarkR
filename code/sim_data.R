## get libs ----
library(tidyverse)
library(stringr)

## set names and ability ----
students <- data_frame(ID = 1:30, ability = rep(c(50, 60, 80), 10))
students

## set assessements ----
assessments <- data_frame(name = c(paste0("A",1:5),paste0("Q",1:6)),
                          difficulty = c(50,50,50,30,80,30,50,50,40,60,90), 
                          total = c(rep(20, 5), 10,20,20,5,15,7))
assessments  <- assessments %>% 
  mutate(name = str_c(name, "_", total))

## set marks parameters ----
marks <- expand.grid(ID = students$ID, name = assessments$name, stringsAsFactors = FALSE)
marks <- tbl_df(marks)
marks  <- marks %>% left_join(students, by = "ID") %>% 
  left_join(assessments, by = "name")
marks

## simulate marks ----
set.seed(2016)
marks  <- marks %>% 
  mutate(average = 0.5 - (difficulty - ability)/100)
marks  <- marks %>% 
  mutate(mark = rbinom(nrow(marks), size = total, prob = average), 
         ID = as.character(ID))
marks  <- marks %>% 
  select(ID, name, mark) %>% 
  spread(name, mark)
marks

## Add exemptions ----
marks$A1_20[3]  <- 'EX'
marks$A4_20[5]  <- 'EX'
marks

## Add missing data ----
marks$A2_20[5]  <- NA
marks$A5_20[4]  <- NA


## Save data ----
devtools::use_data(marks, overwrite = TRUE)