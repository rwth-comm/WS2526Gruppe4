library(tidyverse)
library(jmv)
library(AachenColorPalette)
library(truncnorm)
library(plotrix)

### Daten würfeln: ----

testdaten_female <- rtruncnorm(100, 0, 10, 7, 4)
testdaten_male <- rtruncnorm(100, 0, 10, 4, 4)

testdata <- data.frame(gender = c(rep("female", 100), rep("male", 100)),
                       dance = c(testdaten_female, testdaten_male))
testdata$gender <- as.factor(testdata$gender)

testdata %>% 
  filter(testdata$gender == "female" | testdata$gender == "male") %>% 
  group_by(gender) %>% 
  summarise(M = mean(dance), lower = t.test(dance)$conf.int[1], upper = t.test(dance)$conf.int[2]) %>% 
  ggplot() +
  aes(x = gender, y = M, ymin=lower, ymax=upper) +
  geom_errorbar(width = 0.2) +
  geom_point(color = c(aachen_color("red75", "blue75")), size = 3) +
  scale_y_continuous(limits = c(0,10)) +
  labs(x = "Gender", 
       y = "Lust zu Tanzen [0-10]", 
       title = paste0("Frauen haben mehr Lust zu Tanzen als Männer."), 
       subtitle = "Mittelwertvergleich im Punktdiagramm.", 
       caption = "Fehlerbalken zeigen 95%-Konfidenzintervall.") +
  theme_gray() + 
  NULL



## Kein Interaktionseffekt:

male_alk <- rtruncnorm(50, 0, 10, 5, 2)
male_noalk <- rtruncnorm(80, 0, 10, 2, 2)
female_alk <- rtruncnorm(75, 0, 10, 7, 2)
female_noalk <- rtruncnorm(70, 0, 10, 4, 2)

## Ordinale Interaktion:
#male_alk <- rtruncnorm(50, 0, 10, 9, 2)
#male_noalk <- rtruncnorm(80, 0, 10, 4, 2)
#female_alk <- rtruncnorm(75, 0, 10, 4, 2)
#female_noalk <- rtruncnorm(70, 0, 10, 2, 2)

## Hybride Interaktion:

#male_alk <- rtruncnorm(50, 0, 10, 7, 2)
#male_noalk <- rtruncnorm(80, 0, 10, 5.5, 1)
#female_alk <- rtruncnorm(75, 0, 10, 3, 2)
#female_noalk <- rtruncnorm(70, 0, 10, 4, 1)

## Disordinale Interaktion:

#male_alk <- rtruncnorm(50, 0, 10, 5, 1)
#male_noalk <- rtruncnorm(80, 0, 10, 3, 2)
#female_alk <- rtruncnorm(75, 0, 10, 2, 2)
#female_noalk <- rtruncnorm(70, 0, 10, 8, 2)

### Datensatz zusammensetzen: ----

testdata <- data.frame(gender = c(rep("male", 130), rep("female", 145)),
                       alkohol = c(rep("no_alk", 80), rep("yes_alk", 50), 
                                   rep("no_alk", 70), rep("yes_alk", 75)),
                       dance = c(male_noalk,male_alk,female_noalk, female_alk ))
testdata$gender <- as.factor(testdata$gender)
testdata$alkohol <- as.factor(testdata$alkohol)

### Daten testen: ----

testdata %>% 
  group_by(gender, alkohol) %>% 
  summarise(n = n(), mean = mean(dance))


jmv::ANOVA(testdata, dep = dance, factors = c("gender", "alkohol"))

jmv::ANOVA(testdata, dep = dance, factors = c("gender", "alkohol"), 
           emmPlots = TRUE, emmTables = TRUE, emMeans = list(c("gender"), c("alkohol"),
                                                             c("gender", "alkohol"),
                                                             c("alkohol", "gender")))

result <- jmv::ANOVA(testdata, dep = dance, factors = c("gender", "alkohol"), 
                     emmPlots = TRUE, emmTables = TRUE, emMeans = list(c("gender"), c("alkohol"),
                                                                       c("gender", "alkohol"),
                                                                       c("alkohol", "gender")))

haupteffekt1 <- result$emm[[1]]$emmTable$asDF
haupteffekt2 <- result$emm[[2]]$emmTable$asDF
interaktionseffekt <- result$emm[[3]]$emmTable$asDF

haupteffekt1 %>%
  ggplot() +
  aes(x = gender, y = mean, colour = gender, ymin = lower, ymax = upper ) +
  geom_errorbar(width = 0.2, colour = aachen_color("black")) +
  geom_point(size = 3) +
  scale_colour_manual(values=aachen_color("black", "black")) + 
  scale_x_discrete() +
  ylim(0,10) +
  theme_gray() +
  labs(title = "Faktor: Gender", 
       subtitle = "", 
       x = "",
       y = "Lust zu Tanzen [0 - 10]",
       colour = "",
       caption = "") +
  NULL

haupteffekt2 %>%
  ggplot() +
  aes(x = alkohol, y = mean, colour = alkohol, ymin = lower, ymax = upper ) +
  geom_errorbar(width = 0.2, colour = aachen_color("black")) +
  geom_point(size = 3) +
  scale_colour_manual(values=aachen_color("black", "black")) + 
  scale_x_discrete() +
  ylim(0,10) +
  theme_gray() +
  labs(title = "Faktor: Alkohol", 
       subtitle = "", 
       x = "",
       y = "Lust zu Tanzen [0 - 10]",
       colour = "",
       caption = "") +
  NULL

interaktionseffekt %>% 
  ggplot() +
  aes(x = gender, y = mean, colour = alkohol, ymin = lower, ymax = upper, group = alkohol ) +
  geom_errorbar(width = 0.2, colour = aachen_color("black")) +
  geom_point(size = 3) +
  geom_line() +
  scale_colour_manual(values=aachen_color("red","blue")) + 
  ylim(0,10) +
  theme_gray() +
  labs(title = "Faktor: Gender", 
       subtitle = "", 
       x = "",
       y = "Lust zu Tanzen [0 - 10]",
       colour = "",
       caption = "") +
  theme(title = element_text(face = "bold", size = 20), 
        axis.text.x = element_text(face = "bold", size = 20))
#ggsave("anovaplotimage/C_IE1.png", width = 15, height = 10, units = "cm")

interaktionseffekt %>% 
  ggplot() +
  aes(x = alkohol, y = mean, colour = gender, ymin = lower, ymax = upper, group = gender) +
  geom_errorbar(width = 0.2, colour = aachen_color("black")) +
  geom_point(size = 3) +
  geom_line() +
  scale_colour_manual(values=aachen_color("red","blue")) + 
  ylim(0,10) +
  theme_gray() +
  labs(title = "Faktor: Alkohol", 
       subtitle = "", 
       x = "",
       y = "Lust zu Tanzen [0 - 10]",
       colour = "",
       caption = "") +
  theme(title = element_text(face = "bold", size = 20), 
        axis.text.x = element_text(face = "bold", size = 20))
#ggsave("anovaplotimage/C_IE2.png", width = 15, height = 10, units = "cm")
