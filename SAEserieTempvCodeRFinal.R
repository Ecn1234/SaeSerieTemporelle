# Environnement de travail ----

rm(list = ls())
setwd("D:/devoir de Enzo/IUT 2eme annee/SAE Données temporelles")
.libPaths("Packages")



library(tidyverse)
library(readr)
library(zoo)
library(forecast)
library(dplyr)
library(lubridate)




# ============================ #
#
#  I) PREPARATION DES DONNEES -----
# 
# ============================ #


# ============================ #
#
#  I)a) Importation des deux fichiers csv -----
# 
# ============================ #


chatelet_2013_2020 <- read_delim(file = "Donnees/chatelet_2013_2020.csv") %>% 
  select(`DATE/HEURE`,CO2) %>% 
  rename(DATE_HEURE = `DATE/HEURE`)


chatelet_2020_2025 <- read_delim(file = "Donnees/chatelet_2020_2025.csv")%>% 
  select(`DATE/HEURE`,CO2) %>% 
  rename(DATE_HEURE = `DATE/HEURE`)


# Création dataframe de la série complète 2013 à 2025----

chatelet_2013_2025 <-  chatelet_2020_2025 %>% 
  bind_rows(chatelet_2013_2020)%>%
  mutate(CO2 = as.numeric(ifelse(CO2 == "ND" | CO2 == "", NA, CO2)))%>% 
  arrange(DATE_HEURE)

chatelet_2013_2025$DATE_HEURE <- lubridate::with_tz(
  chatelet_2013_2025$DATE_HEURE,
  tzone = "Europe/Paris")



# Création dataframe de la série entraînée du 22 novembre 2018 au 31 août 2024----

chatelet_2018_2024 <- chatelet_2013_2025 %>%
  filter(
    DATE_HEURE > as.POSIXct("2018-11-22 00:00:00", tz = "Europe/Paris"),
    DATE_HEURE <= as.POSIXct("2024-08-31 23:00:00", tz = "Europe/Paris")
  )


sum(is.na(chatelet_2018_2024$CO2))
length(chatelet_2018_2024$CO2)



# Création dataframe de la série de test du 1er septembre 2024 au 23 janvier 2025----


chatelet_2024_2025 <- chatelet_2013_2025 %>%
  filter(
    DATE_HEURE > as.POSIXct("2024-09-01 00:00:00", tz = "Europe/Paris"),
    DATE_HEURE <= as.POSIXct("2025-01-23 00:00:00", tz = "Europe/Paris")
  )

sum(is.na(chatelet_2024_2025$CO2))




# ============================ #
#
#  I)b) Traitement des NA -----
# 
# ============================ #

# ============================ #
#
#  Transformation de la série de données horraire en une série journalière sans les heures afin de réduire les NA ----
# 
# ============================ #


#  Création d'une colonne pour la DATE sans l'heure (format aa-mm-jj)
chatelet_2018_2024_journalier <- chatelet_2018_2024 %>%
  mutate(DATE = as.Date(DATE_HEURE))

# Création de la colonne MEDIANE_CO2 et  MOYENNE_CO2 pour chaque jour
chatelet_2018_2024_journalier <- chatelet_2018_2024_journalier %>%
  group_by(DATE) %>%
  summarise(MEDIANE_CO2 = median(CO2, na.rm = TRUE),
            MOYENNE_CO2 = mean(CO2, na.rm = TRUE))


sum(is.na(chatelet_2018_2024_journalier$MEDIANE_CO2))/length(chatelet_2018_2024_journalier$MEDIANE_CO2)
sum(is.na(chatelet_2018_2024_journalier$MOYENNE_CO2))/length(chatelet_2018_2024_journalier$MOYENNE_CO2)



# On a retrouvé 6 dates qui n'ont pas été entré dans le jeu de donnée

dates_completes <- seq(as.Date("2018-11-22"), as.Date("2024-08-31"), by   = "day")
length(dates_completes)   




dates_presentes <- chatelet_2018_2024_journalier$DATE
length(dates_presentes)   




dates_manquantes <- as.Date(setdiff(dates_completes, dates_presentes))
dates_manquantes  # on obtient 6 dates non entrées


df_datemanquante <- data.frame(DATE = dates_manquantes,
                               MOYENNE_CO2 = NA,
                               MEDIANE_CO2 = NA)


chatelet_2018_2024_journalier <- chatelet_2018_2024_journalier %>%
  bind_rows(df_datemanquante) %>% arrange(DATE)


# ============================ #
#
#  IMPUTATION DES NA PAR MEDIANE OU MOYENNE # # Remplacer les NA par la médiane de ce jour sur toutes les années ----
# 
# ============================ #



# Création de la colonne JOUR_MOIS, pour avoir la date sans année seulement le jour et le mois
chatelet_2018_2024_journalier <- chatelet_2018_2024_journalier %>%
  mutate(JOUR_MOIS = format(DATE, "%m-%d"))

# Création d'un dataframe qui contient la médiane et la moyenne pour chaque jour/mois sur toutes les années
mediane_jour_mois <- chatelet_2018_2024_journalier %>%
  group_by(JOUR_MOIS) %>%
  summarise(MEDIANE_JOUR = median(MEDIANE_CO2, na.rm = TRUE),
            MOYENNE_JOUR = mean(MEDIANE_CO2, na.rm = TRUE))

# Remplacer les NA par la médiane correspondante
chatelet_2018_2024_journalier <- chatelet_2018_2024_journalier %>%
  left_join(mediane_jour_mois, by = "JOUR_MOIS") %>%
  mutate(MEDIANE_CO2 = ifelse(is.na(MEDIANE_CO2), MEDIANE_JOUR, MEDIANE_CO2),
         MOYENNE_CO2 = ifelse(is.na(MOYENNE_CO2), MOYENNE_JOUR, MOYENNE_CO2))




chatelet_2018_2024_journalier <- chatelet_2018_2024_journalier %>% 
  distinct(DATE, .keep_all = TRUE)




sum(is.na(chatelet_2018_2024_journalier$MEDIANE_CO2))
sum(is.na(chatelet_2018_2024_journalier$MOYENNE_CO2))





# ============================================================== #
#
# II) Analyse série temporelle : tendance, saisonnalité et résidu    #
# 
# ===============================================================#


# Création de la série temporelle d'entrainememnt du 22 novembre 2018 au 31 août 2024 ----


# 1. Transformer la série de CO2 en objet ts (time series)
co2.ts <- ts(chatelet_2018_2024_journalier$MOYENNE_CO2, 
             start = c(year(min(chatelet_2018_2024_journalier$DATE)), 
                       yday(min(chatelet_2018_2024_journalier$DATE))),
             frequency = 365) # fréquence 365 pour données journalières, on a pas utilisé 365.25 car cela provoque des erreurs pour les modèles de prédictions HW et polynomial



# ============================================================== #
#
# II) a) Analyse série temporelle : COMPOSANTE TENDANCIELLE -----
# 
# ===============================================================#

########################
# Moyenne Mobile d'ordre 365
#
########################

plot(co2.ts, ylab = "CO2", xlab = "Temps (année)", main = "Concentration de CO2 - Châtelet")


MM365 <- stats::filter(co2.ts, filter = rep(1/365, 365))  # P impair pas besoin de centrer


plot(co2.ts, ylab = "CO2", xlab = "Temps (année)", main = "Moyenne mobile d'ordre 365 Concentration de CO2 - Châtelet")
lines(MM365, col = "green", lwd = 2) 




########################
# Moyenne journalière
#
########################

# Série de base
dates <- chatelet_2018_2024_journalier$DATE
co2 <- chatelet_2018_2024_journalier$MOYENNE_CO2


plot(dates, co2, type = "l", ylab = "CO2 (en ppm)", xlab = "Date",
     main = "Courbe de régréssion de moyennes journalières sur la concentration de CO2 à Châtelet", xaxt = "n", col = "#1f77b4")


axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))


chatelet_2018_2024_journalier$DATE[which.min(chatelet_2018_2024_journalier$MOYENNE_CO2)] # Date du CO2 minimum


########################
# Moyenne hebdomadaire
########################

# Tracer la série complète en bleu
plot(dates, co2, type = "l", ylab = "CO2 (en ppm)", xlab = "Date",
     main = "Courbe de régréssion de moyennes hebdomadaires sur la concentration de CO2 à Châtelet", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))

# Moyenne hebdomadaire
semaine <- floor_date(dates, "week")
mean_hebdo <- tapply(co2, semaine, mean, na.rm = TRUE)

# Milieu de semaine
semaines_milieu <- as.Date(names(mean_hebdo)) + 3

# Courbe hebdomadaire en orange
lines(semaines_milieu, mean_hebdo, col = "#ff7f0e", lwd = 2)  # orange



########################
# Moyenne mensuelle
########################


# Tracer la série complète en bleu
plot(dates, co2, type = "l", ylab = "CO2 (en ppm)", xlab = "Date",
     main = "Courbe de régréssion de moyennes mensuelles sur la concentration de CO2 à châtelet", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))

# Moyenne mensuelle
mois <- floor_date(dates, "month")
mean_mensuel <- tapply(co2, mois, mean, na.rm = TRUE)

# Milieu approximatif du mois
mois_date <- as.Date(names(mean_mensuel)) + 15

# Courbe mensuelle en orange
lines(mois_date, mean_mensuel, col = "#ff7f0e", lwd = 2)  # orange




########################
# Moyenne annuelle
########################

# Tracer la série complète en bleu
plot(dates, co2, type = "l", ylab = "CO2", xlab = "Date",
     main = "Courbe de régréssion de moyennes annuelles Concentration de CO2 - Châtelet", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))
# Moyenne annuelle
annee <- year(dates)
mean_annual <- tapply(co2, annee, mean, na.rm = TRUE)

# Milieu de l'année (1er juillet)
milieu_annee <- as.Date(paste0(names(mean_annual), "-07-01"))

# Courbe annuelle en orange
lines(milieu_annee, mean_annual, col = "#ff7f0e", lwd = 2)  # orange



## Représentation des 3 courbes de régréssion en même temps:

# Tracer la série complète en bleu
plot(dates, co2, type = "l", ylab = "CO2 (en ppm)", xlab = "Date",
     main = "Évolution de la concentration de CO2 à Châtelet : quatre courbes de régression", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))



# Courbe hebdomadaire 
lines(semaines_milieu, mean_hebdo, col = "red", lwd = 2)  # orange


# Courbe mensuelle 
lines(mois_date, mean_mensuel, col = "purple", lwd = 2)  # orange


# Courbe annuelle 
lines(milieu_annee, mean_annual, col = "green", lwd = 2)  # orange




legend("topright",
       legend = c("Série complète (Moyenne quotidienne)", "Moyenne hebdomadaire", "Moyenne mensuelle", "Moyenne annuelle"),
       col = c("#1f77b4", "red", "purple", "green"),
       lwd = 2,
       cex = 0.8)





# Tracer la série complète en bleu
plot(dates, co2, type = "l", ylab = "CO2 (en ppm)", xlab = "Date",
     main = "Évolution de la concentration de CO2 à Châtelet : comparaison courbe de régréssion annuelle et moyenne moyenne mobile d'ordre 365", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))


# Courbe annuelle 
lines(milieu_annee, mean_annual, col = "green", lwd = 2)  # orange


lines(dates, MM365, col = "black", lwd = 2)



legend("topright",
       legend = c("Moyenne annuelle","Moyenne mobile d'ordre 365" ),
       col = c("green","black"),
       lwd = 2,
       cex = 0.8)







# ============================================================== #
#
# II) b) Analyse série temporelle : COMPOSANTE SAISONNIERE  -----
# 
# ===============================================================#



# ================================
# Décomposition saisonnalité
# ================================



# 1. Décomposition de la série
decomposition <- decompose(co2.ts, type = "additive")  # additive ou multiplicative selon les données
plot(decomposition)

# 2. Coefficients saisonniers journaliers
# Extraire la saisonnalité
saisonnalite <- decomposition$seasonal



# Tracer la saisonnalité sur une année (365 jours) avec axes personnalisés
plot(saisonnalite[1:365], type = "l", xlab = "Jour de l'année", ylab = "Effet saisonnier",
     main = "Saisonnalité sur une année (CO2)", xaxt = "n")

# Ajouter l'axe des x avec des repères tous les 30 jours
axis(1, at = seq(10, 365, by = 30), labels = seq(10, 365, by = 30))



# ================================
#  série désaisonnalisée
# ================================


# 2. Extraire la série désaisonnalisée
CVS <- co2.ts - decomposition$seasonal

# Tracer la série complète en bleu
plot(dates, CVS, type = "l", ylab = "CO2", xlab = "Date",
     main = "Série désaisonnalisée (CVS)", xaxt = "n",
     col = "#1f77b4")   # bleu

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))




# ============================================================== #
#
# II) c) Analyse série temporelle : COMPOSANTE RESIDUELLE  -----
# 
# ===============================================================#


decomposition <- decompose(co2.ts, type = "additive")  # additive ou multiplicative
residus <- decomposition$random

dates <- chatelet_2018_2024_journalier$DATE
mois <- month(dates)  # extraire le mois



#  Courbe  des résidus

sd_residus <- sd(decomposition$random, na.rm = TRUE)

plot(dates, decomposition$random, type = "l", ylab = "CO2", xlab = "Date",
     main = "Graphique des résidus - CO2 Châtelet", xaxt = "n",
     col = "#1f77b4")   # bleu


axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))


# Ajouter les lignes à ±2*écart-type
abline(h = 2*sd_residus, col = "red", lty = 2)
abline(h = -2*sd_residus, col = "red", lty = 2)



# 3. Boxplot des résidus
boxplot(decomposition$random, main = "Boxplot des résidus - CO2 Châtelet",
        ylab = "Résidus", col = "lightblue")


valeurs_extremes <- boxplot(decomposition$random)$out


indices <- which(decomposition$random%in%valeurs_extremes == TRUE)
indices


co2.ext <- data.frame(Date=time(co2.ts)[indices], CO2_extreme=co2.ts[indices],
                      prediction = decomposition$trend[indices] + decomposition$seasonal[indices], residus=decomposition$random[indices])
co2.ext






# ============================================================== #
#
# III) PREDICTION  -----
# 
# ===============================================================#




# ============================================================== #
#
# III) a) PREDICTION  ARIMA-----
# 
# ===============================================================#





# 2️⃣ Modèle ARIMA automatique
auto_sarma <- forecast::auto.arima(co2.ts)

# 3️⃣ Horizon de prévision correct : 145 jours
h <- as.numeric(as.Date("2025-01-23") - as.Date("2024-09-01")) + 1
hFin2025 <- as.numeric(as.Date("2025-12-31") - as.Date("2024-09-01")) + 1


# 4️⃣ Prévision
prediction_auto_sarma <- forecast(auto_sarma, h = h)

prediction_auto_sarmaFin2025 <- forecast(auto_sarma, h = hFin2025)

# 5️⃣ Extraction des valeurs prédites
prediction_sarma <- prediction_auto_sarma$mean

prediction_sarmaFin2025 <- prediction_auto_sarmaFin2025$mean

# 6️⃣ Tracé


debut <- c(2018, yday(as.Date("2018-11-22")))
fin   <- c(2026, yday(as.Date("2026-01-23")))

plot(co2.ts, 
     col = "black", lwd = 1.5,
     main = "Prévisions ARIMA pour la période du 23 janvier au 31 décembre 2025 – Station Châtelet",
     xlab = "Temps", ylab = "CO2",
     xlim = c(debut[1] + (debut[2]-1)/365,
              fin[1]   + (fin[2]-1)/365))

# Axe des dates
axis(1, at = seq(min(dates), max(dates), by = "month"),
     labels = format(seq(min(dates), max(dates), by = "month"), "%Y-%m"))





lines(prediction_sarma, col="red", lwd=2)
lines(prediction_sarmaFin2025, col="orange", lwd=2)







# ============================================================== #
#
# III) b) PREDICTION  HoltWinters-----
# 
# ===============================================================#





# 2️⃣ Ajuster le modèle Holt-Winters (avec tendance et saisonnalité)
HW <- HoltWinters(co2.ts)

# 3️⃣ Définir la période de prévision


n_pred <- as.numeric(as.Date("2025-01-23") - as.Date("2024-09-01")) + 1


n_predFin2025 <- as.numeric(as.Date("2025-12-31") - as.Date("2024-09-01")) + 1


# 4️⃣ Calculer la prévision
prediction_hw <- predict(HW, n.ahead = n_pred)

prediction_hwFin2025 <- predict(HW, n.ahead = n_predFin2025)



# 6️⃣ Tracer la série et la prévision




debut <- c(2018, yday(as.Date("2018-11-22")))
fin   <- c(2026, yday(as.Date("2026-01-23")))

plot(co2.ts, 
     col = "black", lwd = 1.5,
     main = "Prévisions Holt Winter pour la période du 23 janvier au 31 décembre 2025 – Station Châtelet",
     xlab = "Temps", ylab = "CO2",
     xlim = c(debut[1] + (debut[2]-1)/365,
              fin[1]   + (fin[2]-1)/365))



lines(prediction_hw, col = "red", lwd = 2)

lines(prediction_hwFin2025, col = "orange", lwd = 2)

legend("topleft", legend = c("Série historique", "Prévision HW"), col = c("blue", "red"), lty = 1, lwd = c(1,2))







# ============================================================== #
#
# III) c) PREDICTION  Polynome degré 2-----
# 
# ===============================================================#




# ============================
# a Décomposition & CVS
# ============================

decomposition <- decompose(co2.ts, type = "additive")
CVS <- co2.ts - decomposition$seasonal

dates <- chatelet_2018_2024_journalier$DATE
t <- time(co2.ts)


# ============================
# b. Modèles polynomiaux (sans boucle)
# ============================

# Degré 1
mod1 <- lm(CVS ~ t)

# Degré 2
mod2 <- lm(CVS ~ t + I(t^2))

# Degré 3
mod3 <- lm(CVS ~ t + I(t^2) + I(t^3))

# Degré 4
mod4 <- lm(CVS ~ t + I(t^2) + I(t^3) + I(t^4))




tend_2 <-  mod2$coefficients[1] + mod2$coefficients[2]*t + mod2$coefficients[3]*t^2


plot(CVS, xlab="Années", main="Modèle polynomial de degré 2 ajustée sur la CVS", lty=3, col="red")
lines(tend_2, col="black")

legend("topright", legend = c("CVS", "Degré 2"),
       col = c("red", "black"), lwd = 2, bty = "n")

# ============================
# c. Sélection du meilleur via AIC
# ============================

AIC_values <- c(
  "Degré 1" = AIC(mod1),
  "Degré 2" = AIC(mod2),
  "Degré 3" = AIC(mod3),
  "Degré 4" = AIC(mod4)
)

best_deg <- which.min(AIC_values)

cat("Meilleur degré polynomial :", best_deg, "\n")

best_model <- list(mod1, mod2, mod3, mod4)[[best_deg]]

# ============================
# d. Tendance polynomiale observée
# ============================

tend_poly <- predict(best_model, newdata = data.frame(t = t))

# Reconstruction : tendance + saison
hat.yp <- tend_poly + decomposition$seasonal



# Partie Prédiction ----

# ============================
# 1️⃣ Définir la période future
# ============================

n_pred <- as.numeric(as.Date("2025-01-23") - as.Date("2024-09-01")) + 1


n_predFin2025 <- as.numeric(as.Date("2025-12-31") - as.Date("2024-09-01")) + 1

# ============================
# 2️⃣ Temps futurs
# ============================

t_last <- as.numeric(tail(t, 1))
t_future <- t_last + (1:n_pred)/365


t_futureFin2025 <- t_last + (1:n_predFin2025)/365

# ============================
# 3️⃣ Prévision tendance future
# ============================

tend_future <- predict(best_model, newdata = data.frame(t = t_future))

tend_futureFin2025 <- predict(best_model, newdata = data.frame(t = t_futureFin2025))

# ============================
# 4️⃣ Saison future
# ============================

season <- decomposition$seasonal
freq <- frequency(co2.ts)

season_future <- season[((length(season)+1):(length(season)+n_pred) - 1) %% freq + 1]

season_futureFin2025 <- season[((length(season)+1):(length(season)+n_predFin2025) - 1) %% freq + 1]

# ============================
# 5️⃣ Prévision finale
# ============================

prediction_poly <- tend_future + season_future

prediction_poly_ts <- ts(prediction_poly,
                         start =  c(year(as.Date("2024-09-01")), yday(as.Date("2024-09-01"))),
                         frequency = 365)



prediction_polyFin2025 <- tend_futureFin2025 + season_futureFin2025

prediction_poly_tsFin2025 <- ts(prediction_polyFin2025,
                                start =  c(year(as.Date("2024-09-01")), yday(as.Date("2024-09-01"))),
                                frequency = 365)



# ============================
# TRACÉ GLOBAL
# ============================

debut <- c(2018, yday(as.Date("2018-11-22")))
fin   <- c(2026, yday(as.Date("2026-01-23")))

plot(co2.ts, 
     col = "black", lwd = 1.5,
     main = "Prévision polynomiale pour la période du 23 janvier au 31 décembre 2025 – Station Châtelet",
     xlab = "Temps", ylab = "CO2",
     xlim = c(debut[1] + (debut[2]-1)/365,
              fin[1]   + (fin[2]-1)/365))



# Modélisation sur données observées
lines(hat.yp, col = "blue", lwd = 2)

# Prévision polynomiale future
lines(prediction_poly_ts, col = "red", lwd = 2)


# Prévision polynomiale future jusqu'à Fin 2025
lines(prediction_poly_tsFin2025, col = "orange", lwd = 2)

legend("topleft",
       legend = c("Observations", 
                  "Modélisation polynomiale", 
                  "Prévision polynomiale"),
       col    = c("black", "blue", "orange"),
       lty    = c(1, 1, 1),
       lwd    = c(1.5, 2, 2),
       bg = "white")






# ============================================================== #
#
# III) d) PREDICTION  COMPARATION DES TROIS MODELES-----
# 
# ===============================================================#




##Il est difficile de dire quel est le meilleur modèle. On decidera avec les EQM :


# Création dataframe de la série de test du 1er septembre 2024 au 23 janvier 2025----


chatelet_2024_2025 <- chatelet_2013_2025 %>%
  filter(
    DATE_HEURE > as.POSIXct("2024-09-01 01:00:00", tz = "Europe/Paris"),
    DATE_HEURE <= as.POSIXct("2025-01-23 23:00:00", tz = "Europe/Paris")
  )

sum(is.na(chatelet_2024_2025$CO2))




# ============================ #
#
#  Transformation de la série de données horraire en une série journalière sans les heures afin de réduire les NA
# 
# ============================ #


#  Création d'une colonne pour la DATE sans l'heure (format aa-mm-jj)
chatelet_2024_2025_journalier <- chatelet_2024_2025 %>%
  mutate(DATE = as.Date(DATE_HEURE))

# Création de la colonne MEDIANE_CO2 et  MOYENNE_CO2 pour chaque jour
chatelet_2024_2025_journalier <- chatelet_2024_2025_journalier %>%
  group_by(DATE) %>%
  summarise(MEDIANE_CO2 = median(CO2, na.rm = TRUE),
            MOYENNE_CO2 = mean(CO2, na.rm = TRUE))


sum(is.na(chatelet_2024_2025_journalier$MEDIANE_CO2))/length(chatelet_2024_2025_journalier$MEDIANE_CO2)
sum(is.na(chatelet_2024_2025_journalier$MOYENNE_CO2))/length(chatelet_2024_2025_journalier$MOYENNE_CO2)


# 1. Transformer la série de CO2 en objet ts (time series)
co2.tsTest <- ts(chatelet_2024_2025_journalier$MOYENNE_CO2, 
                 start = c(year(min(chatelet_2024_2025_journalier$DATE)), 
                           yday(min(chatelet_2024_2025_journalier$DATE))),
                 frequency = 365) # fréquence 365 pour données journalières


EQM_polynomial <- mean((co2.tsTest - prediction_poly_ts)^2, na.rm = TRUE)
EQM_SARMA <- mean((co2.tsTest - prediction_sarma)^2, na.rm = TRUE)
EQM_HW <- mean((co2.tsTest - prediction_hw)^2, na.rm = TRUE)

data.frame("EQM_polynomial"=EQM_polynomial, "EQM_SARMA"=EQM_SARMA, "EQM_HW"=EQM_HW)




################### COMPARAISON FINALE

# 1. Tracer la série d'entraînement
plot(co2.ts,
     col = "black",
     lwd = 1,
     ylim = c(min(co2.ts, co2.tsTest, prediction_sarma, prediction_hw, prediction_poly_ts, na.rm=TRUE),
              max(co2.ts, co2.tsTest, prediction_sarma, prediction_hw, prediction_poly_ts, na.rm=TRUE)),
     xlim = c(debut[1] + (debut[2]-1)/365,
              fin[1]   + (fin[2]-1)/365),
     main = "Comparaison des trois méthodes de prévision",
     ylab = "CO2(en ppm)",
     xlab = "Temps")

# 2. Ajouter la série de test
lines(co2.tsTest,
      col = adjustcolor("red", alpha.f = 0.5),
      lwd = 1)



# 2. Ajouter la série de test
lines(prediction_sarma,
      col = "green",
      lwd = 2)


# 2. Ajouter la série de test
lines(prediction_hw,
      col = adjustcolor("orange", alpha.f = 0.5),
      lwd = 2)

# 2. Ajouter la série de test
lines(prediction_poly_ts,
      col = adjustcolor("purple", alpha.f = 0.5),
      lwd = 2)



# 3. Ajouter une légende
legend("topleft",
       legend = c("Série Entrainée (2018–2024)", "Série de test (2024–2025)","ARIMA","Holt-Winters","Polynome degré 2"),
       col = c("black", "red","green","orange","purple"),
       lwd = 2)




