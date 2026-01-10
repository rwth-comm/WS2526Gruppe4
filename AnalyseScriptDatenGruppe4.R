# Pakete aktivieren ----
library(tidyverse)
library(psych)
#library(hcictools)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/Mastermethoden+WS2526_9.+Januar+2026_11.10.csv")

# Rohdaten filtern ----
raw %>%
  filter(Progress == 100) %>%
  filter(Status == 0) -> raw

# Überflüssige Variablen entfernen ----
# Was wir brauchen (Deomgraphie: gender, age, edu, urban; 
# Skalen: ATI(Technikaffinität), TP (Technophobie), TIA (Vertrauen in Techonologie);
# AV: Nutzungsbereitschaft pro Szenario: Q21_1-Q24_3 (Q23 hat Qualtrics-Bug))
raw.short <- raw[,c(19:21, 24, 46:52, 71:79, 89:100)]

# Variblen umbenennen ----
generate_codebook(raw.short, "data/Mastermethoden+WS2526_9.+Januar+2026_11.10.csv", "data/codebook1a.csv")
codebook <- read_codebook("data/codebook_final1a.csv")
names(raw.short) <- codebook$variable

# Korrekte Daten zuordnen ----
raw.short$Gender %>% 
  as.character() %>%
  recode(`1` = "Männlich", `2` = "Weiblich", `3` = "Divers", `4` = "Keine Angaben") %>% 
  as.factor() -> raw.short$Gender

raw.short$Age %>% 
  as.character() %>%
  readr::parse_number() %>%
  as.numeric() -> raw.short$Age

raw.short$Edu %>% 
  as.character() %>%
  ordered(levels = as.character(1:5),
          labels = c("(noch) keinen Schulabschluss",
                     "Hauptschulabschluss",
                     "Realschulabschluss",
                     "Abitur",
                     "Hochschulabschluss")) -> raw.short$Edu

raw.short$Urban %>% 
  as.character() %>%
  recode(`1` = "Ländlich",
         `2` = "Vorort/Kleinstadt",
         `3` = "Großstadt") %>%
  as.factor() -> raw.short$Urban

# Qualitätskontrolle ----

raw.short.quality <- careless_indices(raw.short, likert_vector = c(5:ncol(raw.short)), speeder_analysis = "median/2")

raw.short.quality %>% 
  filter(speeder_flag == FALSE) %>% 
  filter(careless_longstr < 20) %>% 
  filter(careless_psychsyn > 0) %>% 
  filter(careless_psychant < 0) %>% 
  filter(careless_mahadflag == FALSE) -> raw.short.quality

# Skalenwerte berechnen ----

schluesselliste <- list(
  ATI          = c("ATI_1","ATI_2","-ATI_3n","-ATI_4n"),
  TrustTech    = c("-TIA_1n","TIA_2","TIA_3"),
  Technophobie = c("TP_1","TP_2","TP_3","TP_4","TP_5","TP_6","-TP_7n","-TP_8n","-TP_9n"),
  
  NB_Baseline  = c("Q21_1","Q21_2","Q21_3"),
  NB_Public    = c("Q23_1","Q23_2","Q23_3"),
  NB_Private   = c("Q22_1","Q22_2","Q22_3"),
  NB_Coop      = c("Q24_1","Q24_2","Q24_3"),
  
  NB_General   = c("Q21_1","Q21_2","Q21_3",
                   "Q23_1","Q23_2","Q23_3",
                   "Q22_1","Q22_2","Q22_3",
                   "Q24_1","Q24_2","Q24_3")
)

scores <- scoreItems(schluesselliste, items = raw.short, min = 1, max = 6)
scores.quality <- scoreItems(schluesselliste, items = raw.short.quality, min = 1, max = 6)
scores$alpha
scores.quality$alpha
  
data <- bind_cols(raw.short.quality, scores.quality$scores)
  
# Daten exportieren ----
write_rds(data, "data/data1a.rds")