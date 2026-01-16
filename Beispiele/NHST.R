library(dataforsocialscience)
library(tidyverse)
library(lsr)

df <- robo_care
df_male <- filter(df, df$gender == "male")
df_female <- filter(df, df$gender == "female")

# KUT
t.test(df_male$cse, df_female$cse)
lsr::cohensD(df_male$cse, df_female$cse)

# Präferenz für schwierige Aufgaben
t.test(df_male$diff_pref, df_female$diff_pref)
cohensD(df_male$diff_pref, df_female$diff_pref)

# Technikwissen
t.test(df_male$technical_knowledge, df_female$technical_knowledge)

# Privacybedenken
t.test(df_male$privacy_concerns, df_female$privacy_concerns)

# Automatisierungstendenz
t.test(df_male$automation_tendency, df_female$automation_tendency)

# Pflegeerfahrung
t.test(df_male$care_experience, df_female$care_experience)
