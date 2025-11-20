# Pakete installieren

install.packages("tidyverse")
install.packages("psych")
install.packages("plotrix")
install.packages("remotes")
remotes::install_github("statisticsforsocialscience/dataforsocialscience")

# Pakete aktivieren
library(tidyverse)
library(psych)
library(plotrix)
library(dataforsocialscience)

# Daten einlesen
df <- robo_care
