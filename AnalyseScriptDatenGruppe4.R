# Pakete aktivieren ----
library(tidyverse)
library(psych)
library(hcictools)
source("qualtricshelpers.R")

# Daten einlesen ----
raw <- load_qualtrics_csv("data/Mastermethoden+WS2526_9.+Januar+2026_11.10.csv")

# Rohdaten filtern ----
raw %>%
  filter(DSGVO == 1) %>%
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


raw.short$q23_1 %>% 
  recode(`43` = 1,
         `51` = 2,
         `44` = 3,
         `45` = 4,
         `47` = 5,
         `48` = 6) %>%
  as.numeric() -> raw.short$q23_1

raw.short$q23_2 %>% 
  recode(`43` = 1,
         `51` = 2,
         `44` = 3,
         `45` = 4,
         `47` = 5,
         `48` = 6) %>%
  as.numeric() -> raw.short$q23_2

raw.short$q23_3 %>% 
  recode(`43` = 1,
         `51` = 2,
         `44` = 3,
         `45` = 4,
         `47` = 5,
         `48` = 6) %>%
  as.numeric() -> raw.short$q23_3

# Qualitätskontrolle ----


# Skalenwerte berechnen ----

schluesselliste <- list(
  ATI          = c("ati_1","ati_2","-ati_3n","-ati_4n"),
  TrustTech    = c("-tia_1n","tia_2","tia_3"),
  Technophobie = c("tp_1","tp_2","tp_3","tp_4","tp_5","tp_6","-tp_7n","-tp_8n","-tp_9n"),
  
  NB_Basic = c("q21_1","q21_2","q21_3"),
  NB_Public  = c("q23_1","q23_2","q23_3"),
  NB_Private    = c("q22_1","q22_2","q22_3"),
  NB_Coop      = c("q24_1","q24_2","q24_3"),
  
  NB_General    = c("q21_1","q21_2","q21_3",
                   "q23_1","q23_2","q23_3",
                   "q22_1","q22_2","q22_3",
                   "q24_1","q24_2","q24_3")
)

scores <- scoreItems(schluesselliste, items = raw.short, min = 1, max = 6)
scores$alpha
  

data <- bind_cols(raw.short, scores$scores)
  
# Daten exportieren ----
write_rds(data, "data/data1a.rds")

