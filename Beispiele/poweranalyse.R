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


# Variante 1: Wie viele ProbandInnen schwachen Effekt
pwr::pwr.t.test(n = NULL, sig.level = 0.05 , d = 0.2, power = 0.8)
Für einen schwachen Effekt benötigt man rund 788 Proband:innen (pro Gruppe 394 VP)

# Variante 1: Wie viele ProbandInnen mittleren Effekt ----
pwr::pwr.t.test(n = NULL, sig.level = 0.05 , d = 0.5, power = 0.8)
Für einen mittleren Effekt benötigt man rund 126 Proband:innen (pro Gruppe 64 VP)

# Variante 1: Wie viele ProbandInnen starken Effekt 
pwr::pwr.t.test(n = NULL, sig.level = 0.05 , d = 0.8, power = 0.8)
Für einen starken Effekt benötigt man rund 52 Proband:innen (pro Gruppe 26 VP)

# Variante 3:Wie müssen Effekte in Realität mindestens sein, damit Sie diese detektieren können? 
pwr::pwr.t.test(n = 180, sig.level = 0.05 , d = NULL, power = 0.8)
Mit 180 Personen pro Gruppe, einem Signifikanzniveau von α = 0.05 und einer Teststärke von 0.8 können in der Realität nur Effektstärken von mindestens d ≈ 0.30 zuverlässig detektiert werden.
