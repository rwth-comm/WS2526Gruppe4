library(jmv)
library(tidyverse)
library(dataforsocialscience)

df <- robo_care

df.short <- df[,c(2:3,28:29)]

df.short %>% 
  filter(job_type == "employee" | job_type == "student" ) %>% 
  filter(gender != "rather not say") %>% 
  droplevels() -> df.short

jmv::mancova(df.short, deps = c(cse, diff_pref), factors = c(gender, job_type), multivar = "wilks")
