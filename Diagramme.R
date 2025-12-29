install.packages("tidyverse")
install.packages("ggthemes")
remotes::install_github("christianholland/AachenColorPalette")
install.packages("esquisse")

library(tidyverse)
library(ggthemes)
library(AachenColorPalette)

df <- readRDS("data/data.rds")

display_aachen_colors()

library(ggplot2)

ggplot(df) +
  aes(x = Age) +
  geom_histogram(bins = 30L, fill = aachen_color("orange")) +
  geom_vline(xintercept = mean(df$Age, na.rm = TRUE)) +
  geom_text(x = mean(df$Age, na.rm = TRUE), y = 50, label = paste0("M = ", round(mean(df$Age, na.rm = TRUE), 2)), angle = 90 , vjust = 1.5) +
  labs(x = "Alter in Jahren", 
       y = "Anzahl", 
       title = paste0("Studentische Altersverteiliung n = (", nrow(df), ")"), 
       subtitle = "Histogramm der Altersverteilung", 
       caption = "30 Bins") +
  theme_minimal()

ggsave(filename = "histogram.png", width = 15, height = 12, units = "cm")

library(ggplot2)


library(ggplot2)

df %>% 
  filter(Gender == "Männlich" | Gender == "Weiblich") %>% 
  ggplot() +
  aes(x = ATI, y = Gender) +
  geom_boxplot(fill = aachen_color("blue75", "red75")) +
  labs(x = "ATI [1-6]", 
       y = "Geschlecht", 
       title = "Männer haben deskriptiv einen höheren KUT als Frauen", 
       subtitle = "Deskriptiver Vergleich im Boxplot", 
       caption = "Punkte zeigen Ausreißer") +
  coord_flip() +
  scale_x_continuous(limits = c(1,6), breaks = c(1:6)) +
  theme_minimal()

ggsave(filename = "boxplot.png", width = 15, height = 12, units = "cm")
