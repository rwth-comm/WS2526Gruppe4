install.packages("pwr")
library(pwr)

# Variante 1: Stichprobengröße gesucht ----
pwr::pwr.t.test(n = NULL, sig.level = 0.05 , d = 0.8, power = 0.8)

# Variante 2: Signifikanzniveau gesucht ----
pwr::pwr.t.test(n = 200, sig.level = NULL , d = 0.2, power = 0.8)

# Variante 3: Effektstärke gesucht ----
pwr::pwr.t.test(n = 110, sig.level = 0.05 , d = NULL, power = 0.8)

# Variante 4: Power gesucht ----

library(dataforsocialscience)
library(tidyverse)
library(lsr)
df <- robo_care
df_male <- filter(df, df$gender == "male")
df_female <- filter(df, df$gender == "female")

t.test(df_male$privacy_concerns, df_female$privacy_concerns)
cohensD(df_male$privacy_concerns, df_female$privacy_concerns)

pwr::pwr.t.test(n = 122, sig.level = 0.05, d = 0.04400775, power = NULL)
