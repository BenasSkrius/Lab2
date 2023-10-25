#Benas Skirius EEI-0/1

library(tidyverse)
library(dplyr)
library(ggplot2)

lab<-read.csv("D:/Downloads/lab_sodra.csv")

#Isrenkami tik reikiami duomenys
filtered_df <- subset(lab, lab[,5] == 452000)

#Isimami stulpeliai su daugiau nei 50% trukstamu reiksmiu
remove_columns_with_many_nas <- function(df, threshold = 0.5) {
  max_nas <- nrow(df) * threshold
  df_filtered <- df[, colSums(is.na(df)) <= max_nas]
  return(df_filtered)
}
df_filtered <- remove_columns_with_many_nas(filtered_df, threshold = 0.5)


#awgWage stulpelyje trukstamos vertes uzpildomos vidutinemis vertemis pagal imone
df_filtered <- df_filtered %>%
  group_by(name) %>%
  filter(!all(is.na(avgWage)))  
  mutate(avgWage = ifelse(is.na(avgWage), mean(avgWage, na.rm = TRUE), avgWage)) %>%
  ungroup()

#tax stulpelyje trukstamos vertes uzpildomos vidutinemis vertemis pagal imone
df_filtered <- df_filtered %>%
  group_by(name) %>%
  filter(!all(is.na(tax)))  
  mutate(tax = ifelse(is.na(tax), mean(tax, na.rm = TRUE), tax)) %>%
  ungroup()
  filter(!all(is.na(tax)))

#1 uzduotis
  
lab <- df_filtered
  
alga <- subset(lab, lab[,3] == "UŽDAROJI AKCINĖ BENDROVĖ STOP SERVIS")
    
my_bar<-barplot(table(alga[,8]),xlab="AvgWage",ylab="Count")
title(main = "UŽDAROJI AKCINĖ BENDROVĖ STOP SERVIS Vidutinių atligynimų historama")


#2 Uzduotis

company_avg_wages <- lab %>%
  group_by(name) %>%
  summarize(AverageAvgWage = mean(avgWage, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(AverageAvgWage))

top_5_companies <- head(company_avg_wages, 5)
filtered_df <- lab %>%
  filter(name %in% top_5_companies$name)

ggplot(filtered_df, aes(x = month, y = avgWage, color = name)) +
  geom_line() +
  labs(title = "5 didžiausią darbo užmokestį turinčių kompanijų darbo užmokestis metų eigoje",
       x = "Month",
       y = "Average Wage") +
  theme_minimal()


#3 Uzduotis
max_numInsured <- filtered_df %>%
  group_by(name) %>%
  summarize(MaxNumInsured = max(numInsured, na.rm = TRUE)) %>%
  ungroup()


ggplot(max_numInsured, aes(x = reorder(name, -MaxNumInsured), y = MaxNumInsured)) +
  geom_bar(stat = "identity") +
  labs(title = "5 Komapniju su dydziasiais atlyginimais apdraustu darbuotuju skaicius",
       x = "Name",
       y = "Max numInsured") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




