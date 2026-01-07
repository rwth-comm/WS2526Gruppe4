## Diagramme
# Pakete laden und Daten einlesen:
library(dataforsocialscience)
library(hcictools)
library(tidyverse)
library(plotrix)
library(jmv)
library(ggthemes)

rwthcolor <- hcictools::rwth.colorpalette()
df <- dataforsocialscience::robo_care


## Balken- und Punktdiagramm für T-Test:
# Diesen T-Test möchten wir visualisieren:
t.test( filter(df, gender == "male")$cse, filter(df, gender == "female")$cse)


# Balkendiagramm. ----
robo_care %>% 
  filter(gender != "rather not say") %>%
  group_by(gender) %>% 
  summarise(mean_cse = mean(cse)-1, sem_cse = std.error(cse)) %>%
  ggplot() +
  aes(x = gender, fill = gender, weight = mean_cse, ymin = mean_cse - sem_cse, ymax = mean_cse + sem_cse ) +
  geom_bar( width = 0.5) +
  scale_fill_manual(values=c(rwthcolor$lightblue, rwthcolor$red)) + 
  geom_errorbar(width = 0.2) +
  ylim(0,6) +
  theme_dark() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Balkendiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [0 - 5]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("examplebar.png", width = 6, height = 6)


# Punktdiagramm. ----
# Hier kann die Skala auf 1-6 bleiben. Aufpassen bei der Skalenbeschriftung.
# Hinweis: Bei Punkten heißen die Eigenschaften anders, z.B. size statt width
df %>% 
  filter(gender != "rather not say") %>%
  group_by(gender) %>% 
  summarise(mean_cse = mean(cse), sem_cse = std.error(cse)) %>%
  ggplot() +
  aes(x = gender, y = mean_cse, weight = mean_cse, colour = gender, ymin = mean_cse - sem_cse, ymax = mean_cse + sem_cse ) +
  geom_errorbar(width = 0.2, colour = rwthcolor$black) +
  geom_point(size = 5) +
  scale_colour_manual(values=c(rwthcolor$lightblue, rwthcolor$red)) + 
  ylim(3.5,5) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können", 
       subtitle = "Punktdiagramm: KUT im Vergleich zwischen Männern und Frauen ", 
       x = "Geschlecht",
       y = "KUT [1 - 6]",
       fill = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("examplepoint.png", width = 6, height = 6)


# Punktdiagramm für ANOVA ----
# Berechnung der ANOVA:
df %>% 
  filter(gender != "rather not say") %>%
  filter(job_type == "employee" | job_type == "student" ) %>% 
  droplevels() %>% 
  jmv::ANOVA(dep = "cse", factors = c("job_type", "gender"), emMeans = list(c("job_type", "gender")) )




# Profildiagramm in ggplot. Neu: geom_line als Marker für die Gruppen.
df %>% 
  filter(gender != "rather not say") %>%
  filter(job_type == "employee" | job_type == "student" ) %>% 
  group_by(gender, job_type) %>% 
  summarise(mean_cse = mean(cse), sem_cse = std.error(cse)) %>%
  ggplot() +
  aes(x = job_type, y = mean_cse, colour = gender, ymin = mean_cse - sem_cse, ymax = mean_cse + sem_cse ) +
  geom_errorbar(width = 0.2, colour = rwthcolor$black) +
  geom_point(size = 3) +
  geom_line(aes(group = gender)) +
  scale_colour_manual(values=c(rwthcolor$blue, rwthcolor$red), labels = c("männlich", "weiblich")) + 
  scale_x_discrete(labels = c("berufstätig", "studierend")) +
  ylim(3,6) +
  theme_gray() +
  labs(title = "Männer glauben eher daran, Technik kontrollieren zu können, Schüler und Studierende auch!", 
       subtitle = "Punktdiagramm: KUT im Vergleich zwischen ArbeitnehmerInnen und Studierenden, gruppiert nach Geschlecht", 
       x = "Beruf",
       y = "KUT [1 - 6]",
       colour = "Geschlecht",
       caption = "Fehlerbalken zeigen Standardfehler des Mittelwertes") +
  NULL
ggsave("exampleInteraction.png", width = 9, height = 6)
# Punktdiagramm für Korrelarion / Lineare Regression. ----


# Punktdiagramm für Korrelation: ----

ggplot(df) +
  aes(x = cse, y = technical_knowledge) +
  geom_point(shape = "circle", size = 1.5, colour = rwthcolor$turquois) +
  geom_smooth(method = "lm", color = rwthcolor$lightblue) +
  scale_x_continuous(breaks = c(1:6), limits = c(1,6)) +
  scale_y_continuous(breaks = c(1:6), limits = c(1,6)) +
  labs(x = "KUT [1-6]", 
       y = "Technikwissen [1-6]", 
       title = "KUT und Technikwissen hängen zusammen.", 
       subtitle = "Punktdiagramm mit Regressionsgeraden.", 
       caption = "Schatten der Regressionsgraden zeigt 95% Konfidenzintervall") +
  theme_minimal()
ggsave("corrpoint.png", width = 9, height = 6)


# Jitterplot für Korrelation mit ordinalskalierten Variablen: ----

ggplot(df) +
  aes(x = robo_bed, y = human_bed) +
  geom_jitter(shape = "circle", size = 1.5, colour = rwthcolor$turquois, width = 0.2, height = 0.2, alpha = 0.5) +
  geom_smooth(method = "lm", color = rwthcolor$lightblue) +
  scale_x_continuous(breaks = c(1:6), limits = c(0.5,6.5)) +
  scale_y_continuous(breaks = c(1:6), limits = c(0.5, 6.5)) +
  labs(x = "Bettlegebereitschaft durch Roboter [1-6]", 
       y = "Bettlegebereitschaft durch Mensch [1-6]", 
       title = "Von Roboter oder Mensch ins Bett legen lassen? \nKein Zusammenhang in der Bereitschaft", 
       subtitle = "Jitterplot mit Regressionsgeraden.", 
       caption = "Schatten der Regressionsgraden zeigt 95% Konfidenzintervall. Jitter = 20%") +
  theme_minimal()
ggsave("jitter.png", width = 9, height = 6)
