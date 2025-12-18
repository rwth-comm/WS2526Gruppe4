 install.packages("tidyverse")
 install.packages("psych")
 
 library(tidyverse)
 library(psych)
 
 raw <-read_csv("data/datacleaning_Beispieldaten.csv")
raw <- raw[c(-1,-2),] 
  
raw <- filter(raw, Progress == 100)

raw.short <- raw[,c(6,9,18:54)] 

dput(names(raw.short))

names(raw.short) <- c("Duration", "ResponseId", "Age", "Gender", "Edu", "JobType",
                      "bf_1n", "bf_2", "bf_3n", "bf_4n", "bf_5n",
                      "bf_6", "bf_7", "bf_8", "bf_9", "bf_10",
                      "ati_1", "ati_2", "ati_3n", "ati_4", "ati_5",
                      "ati_6n", "ati_7", "ati_8n", "ati_9",
                      "wrfq_1", "wrfq_2", "wrfq_3", "wrfq_4", "wrfq_5",
                      "wrfq_6", "wrfq_7", "wrfq_8", "wrfq_9", 
                      "svi_1n", "svi_2n", "svi_3", "svi_4n", "svi_5")



