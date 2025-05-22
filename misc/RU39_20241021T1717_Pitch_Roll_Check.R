rm(list = ls())

library(tidyverse)
library(readxl)
library(rstatix)

pitch = read.csv("C:/Users/Delphine/Box/Glider Data/ru39-20241021T1717/Echoview CSV Import Files/data.pitch.csv")
roll = read.csv("C:/Users/Delphine/Box/Glider Data/ru39-20241021T1717/Echoview CSV Import Files/data.roll.csv")

pitch_roll = cbind(pitch, roll$Roll_angle)
names(pitch_roll)[5] = "Roll_angle"

pitch_roll = pitch_roll %>% 
  dplyr::filter(Pitch_angle > -55 & Pitch_angle < -15)

ggplot(data = pitch_roll, aes(x = Pitch_date)) +
  geom_boxplot(aes(y = Pitch_angle),
               color = "red", fill = "orange", show.legend = T) +
  geom_boxplot(aes(y = Roll_angle),
               color = "blue", fill = "purple", show.legend = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Date", y = "Angle") +
  theme(legend.position = "right")

pitch_roll %>%
  group_by(Pitch_date) %>%
  summarize(mean(Pitch_angle), min(Roll_angle), max(Roll_angle), median(Roll_angle))

pitch_roll_both_wings = pitch_roll %>% filter(Pitch_date == "2024-10-21" | Pitch_date == "2024-10-22")
mean(pitch_roll_both_wings$Roll_angle)
sd(pitch_roll_both_wings$Roll_angle)
median(pitch_roll_both_wings$Roll_angle)
     
pitch_roll_single_wing = pitch_roll %>% filter(Pitch_date != "2024-10-21" & Pitch_date != "2024-10-22" & Pitch_date != "2024-10-23")
mean(pitch_roll_single_wing$Roll_angle)
sd(pitch_roll_single_wing$Roll_angle)
median(pitch_roll_single_wing$Roll_angle)

var.test(pitch_roll_both_wings$Roll_angle, pitch_roll_single_wing$Roll_angle)

t.test(pitch_roll_both_wings$Roll_angle, pitch_roll_single_wing$Roll_angle)

## Pitch/roll for a deployment that didn't have a wing loss


pitch_good = read.csv("C:/Users/Delphine/Box/Glider Data/ru39-20250226T1700/Echoview CSV Import Files/data.pitch.csv")
roll_good = read.csv("C:/Users/Delphine/Box/Glider Data/ru39-20250226T1700/Echoview CSV Import Files/data.roll.csv")

pitch_roll_good = cbind(pitch_good, roll_good$Roll_angle)
names(pitch_roll_good)[5] = "Roll_angle"

pitch_roll_good = pitch_roll_good %>% 
  dplyr::filter(Pitch_angle > -55 & Pitch_angle < -15)

ggplot(data = pitch_roll_good, aes(x = Pitch_date)) +
  geom_boxplot(aes(y = Pitch_angle),
               color = "red", fill = "orange", show.legend = T) +
  geom_boxplot(aes(y = Roll_angle),
               color = "blue", fill = "purple", show.legend = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Date", y = "Angle") +
  theme(legend.position = "right")

mean(pitch_roll_good$Roll_angle)
sd(pitch_roll_good$Roll_angle)
median(pitch_roll_good$Roll_angle)
