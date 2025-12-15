# | Étape de la mission                      | Série utilisée                                     | Période                 |
#   | ---------------------------------------- | -------------------------------------------------- | ----------------------- |
#   | Traitement, nettoyage, transformation    | Série d’entraînement                               | 22/11/2018 → 31/08/2024 |
#   | Moyennes mobiles, tendances, régressions | Série d’entraînement                               | 22/11/2018 → 31/08/2024 |
#   | Coefficients saisonniers, décomposition  | Série d’entraînement                               | 22/11/2018 → 31/08/2024 |
#   | Désaisonnalisation                       | Série d’entraînement                               | 22/11/2018 → 31/08/2024 |
#   | Analyse des résidus                      | Série d’entraînement                               | 22/11/2018 → 31/08/2024 |
#   | Prévisions 1/09/2024 → 23/01/2025        | Modèles ajustés sur série d’entraînement           | 22/11/2018 → 31/08/2024 |
#   | Évaluation des prévisions (EQM)          | Série complète (pour récupérer les vraies valeurs) | 01/09/2024 → 27/01/2025 |
#   


# Environnement de travail ----

rm(list = ls())
setwd("D:/devoir de Enzo/IUT 2eme annee/SAE Données temporelles")
.libPaths("Packages")

library(tidyverse)
library(readr)
library(zoo)
library(forecast)

# Importation ----
chatelet_2013_2020 <- read_delim(file = "Donnees/chatelet_2013_2020.csv") %>% 
  select(`DATE/HEURE`,CO2) %>% 
  rename(DATE_HEURE = `DATE/HEURE`)


chatelet_2020_2025 <- read_delim(file = "Donnees/chatelet_2020_2025.csv")%>% 
  select(`DATE/HEURE`,CO2) %>% 
  rename(DATE_HEURE = `DATE/HEURE` )

# Création table de la série complète ----

chatelet_2013_2025 <-  chatelet_2020_2025 %>% 
  bind_rows(chatelet_2013_2020)%>%
  mutate(CO2 = as.numeric(ifelse(CO2 == "ND", NA, CO2)))%>% 
  arrange(DATE_HEURE)

# Création table de la série entraînée ----
chatelet_2018_2024 <- chatelet_2013_2025 %>%
  filter(DATE_HEURE > as.POSIXct("2018-11-22 00:00:00"),
         DATE_HEURE < as.POSIXct("2024-09-01 00:00:00"))


sum(is.na(chatelet_2018_2024$CO2))

# ==========
#
#  Fin première partie
# 
# ==========


# Création des séries temporelles ----



# Création de la série temporelle entrâinée
co2.ts <- ts(chatelet_2018_2024$CO2,
             start = 1,
             frequency = 24)


plot(co2.ts, 
     main = "Concentration de CO2 (22 novembre 2018–2025)",
     xlab = "Temps",
     ylab = "CO2",
     col = "blue")


dates <- seq(from = as.POSIXct("2018-11-22 00:00:00"),
             by = "hour",
             length.out = length(co2.ts))

plot(dates, co2.ts, type = "l",
     main = "Série CO2 avec les NA", 
     xlab = "Date", ylab = "CO2",
     xaxt = "n")  # Désactive l'axe x automatique

axis.POSIXct(1, at = seq(from = as.POSIXct("2019-01-01"), 
                         to = as.POSIXct("2024-12-31"), 
                         by = "1 year"),
             format = "%Y", las = 1)




#### Création de la table de NA gaps_df

# Trouver tous les gaps de NA
na_rle <- rle(is.na(co2.ts))

# Extraire les informations sur les gaps
if(any(na_rle$values)) {
  # Positions de début et fin de chaque gap
  gap_lengths <- na_rle$lengths[na_rle$values]  # Tailles des gaps
  gap_starts <- cumsum(c(0, na_rle$lengths))[which(na_rle$values)] + 1
  
  # Créer un data.frame avec tous les gaps
  gaps_df <- data.frame(
    gap_id = 1:sum(na_rle$values),
    start_index = gap_starts,
    end_index = gap_starts + gap_lengths - 1,
    length_hours = gap_lengths,
    length_days = round(gap_lengths / 24, 2)
  )
  
  # Ajouter les dates si disponibles
  dates <- seq(from = as.POSIXct("2018-11-22 00:00:00"),
               by = "hour",
               length.out = length(co2.ts))
  
  gaps_df$start_date <- dates[gaps_df$start_index]
  gaps_df$end_date <- dates[gaps_df$end_index]
  
  print(head(gaps_df, 10))
  cat("\nNombre total de gaps:", nrow(gaps_df), "\n")
}


sum(is.na(chatelet_2018_2024$CO2))

#####

#Imputation par Interpolation linéaire pour les gaps courts (<100h)

# 1. Identifier les gaps courts (<100h)
short_gaps <- gaps_df[gaps_df$length_hours < 100, ]

# 2. Créer une copie de la série originale
co2_interpolated <- co2.ts

# 3. Pour CHAQUE gap court individuellement, appliquer l'interpolation
for(i in 1:nrow(short_gaps)) {
  start_idx <- short_gaps$start_index[i]
  end_idx <- short_gaps$end_index[i]
  
  # Extraire le segment avec le gap
  segment_indices <- (start_idx - 1):(end_idx + 1)
  segment <- co2_interpolated[segment_indices]
  
  # Appliquer l'interpolation uniquement sur ce segment
  segment_interpolated <- na.approx(segment, na.rm = FALSE)
  
  # Remplacer uniquement la partie interpolée (sans les bordures)
  co2_interpolated[start_idx:end_idx] <- segment_interpolated[2:(length(segment_interpolated)-1)]
}


sum(is.na(co2_interpolated))
cat("il reste encore  4509 NA apres avoir bouché les gaps courts")





#### Imputation des gaps longs qui sont supérieurs à 100


# Données des gaps longs triés
long_gapsMain <- data.frame(
  start_index = c(2174, 5162, 10760, 11235, 13175, 30171, 40349, 42011, 49902),
  end_index = c(2434, 5297, 10885, 13042, 13285, 30321, 40726, 43305, 50144)
)

long_gaps <- gaps_df[gaps_df$length_hours >= 100, c("start_index", "end_index","length_hours")]

# Trier par start_index croissant
long_gaps <- long_gaps[order(long_gaps$start_index), ]

# Initialiser la série (supposée déjà avec NA pour les gaps longs)
co2_final <- co2_interpolated

# Pour chaque gap, prédire avec Holtz winston 

for(i in 1:nrow(long_gaps)) {
  
  gap_start <- long_gaps$start_index[i]
  gap_end <- long_gaps$end_index[i]
  
  # Données d'entraînement : tout avant le gap
  train_data <- co2_final[1:(gap_start-1)]
  
  cat("==== DEBUT de Traitement du", i ,"eme gap======\n")
  cat("Longueur de gap :", long_gaps[i,"length_hours"], "\n")
  cat("start_index :", long_gaps[i,"start_index"], "\n")
  cat("end_index :", long_gaps[i,"end_index"], "\n")
  cat("Taille du jeu de donnee d'entrainement :", length(train_data), "\n\n")
  
  # Holtz winston 
  ts_train <- ts(train_data, frequency = 24)
  
  hw_model <- HoltWinters(ts_train, seasonal = "additive")
  
  cat("==== Holtz winston fini du", i ,"eme gap======", "\n\n")
  
  # Prédiction
  gap_length <- gap_end - gap_start + 1
  predictions <- forecast(hw_model, h = gap_length)
  
  
  cat("==== Prediction fini du", i ,"eme gap======", "\n\n")
  
  # Remplacer
  co2_final[gap_start:gap_end] <- as.numeric(predictions$mean)
  
  cat("==== Imputation fini du", i ,"eme gap======", "\n\n\n\n\n")
}

sum(is.na(co2_final))



plot(co2_final, 
     main = "Imputation NA via Holt-Winters",
     xlab = "Temps",
     ylab = "CO2",
     col = "blue")

