# Pakete installieren

#install.packages("tidyverse")
#install.packages("psych")
#install.packages("plotrix")
#install.packages("remotes")
#remotes::install_github("statisticsforsocialscience/dataforsocialscience")

# Pakete aktivieren
library(tidyverse)
library(psych)
library(plotrix)
library(dataforsocialscience)

# Daten einlesen
df <- robo_care
table(df$gender)
library(ggplot2)
qplot(df$gender)
median(df$age)
quantile(df$age)
boxplot(df$age)

quantile(df$technical_knowledge)
boxplot(df$technical_knowledge)
mean(df$technical_knowledge)

describe(df)
