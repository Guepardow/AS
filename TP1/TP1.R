# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 3A - Apprentissage statistique
# TP1 - Mardi 25 octobre 2016
# Chargé de TD : Arnak Dalalyan & Vincent Cottet
#
# Etudiants : Mehdi Miah & Ulrich Mpeli
# Descriptif : analyse descriptive de algae et modèles prédictifs
# Remarques : Après nettoyage des données manquantes, prédiction avec une
#   régressioon linéaire et un arbre de décision
#
# TODO  : Finir le TP et le rendre sous Drive avant le 30 octobre
#       : On peut modifier certaines variables en prenant le log
#
# BUG :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# == Préambule ============================================================
rm(list=ls())
cat("\014")

setwd("C:/Users/Mehdi Miah/Desktop/AS/TP1")

library(DMwR)
library(rpart) #pour les arbres de décision
library(stargazer)

# == Analyse descriptive ==================================================

head(algae) # il y a 18 (= 3+8+7) variables et 200 observations

summary(algae)
colSums(is.na(algae))
# données manquantes : 33 (soit 0.91%)
# les 8 variables des composantes chimiques possèdent des valeurs manquantes

# Affichage de la répartition du pH : histogramme, densité et QQplot
#library(car)
op = par(mfrow=c(1,2))
hist(algae$Chla, 
     prob = TRUE,
     xlab = "",
     main = "Histogram of maximum pH value", 
     ylim = c(0,1)
     )
lines(density(log(algae$Chla), na.rm = TRUE), col = 'red')
rug(jitter(algae$Chla))
qqnorm(algae$Chla, 
       main = "Normal QQ plot of maximum pH")
abline(a=0, b=1, lty= 2)
par(op)

# Boxplot conditionelle par rapport à size (catégorielle !)
bwplot(season ~ a1, data = algae,
       ylab = "Season", 
       xlab  = "Algae a1")

# == Traitement des données manquantes ======================

#Observations avec des données manquantes

algae[!complete.cases(algae), ]
nrow(algae[!complete.cases(algae), ])

# 1ere technique : les supprimer de l'échantillon
algae1 = na.omit(algae)
nrow(algae)
nrow(algae1)

# 2e technique : remplir par la médiane / le mode
algae2 = centralImputation(algae)
nrow(algae[!complete.cases(algae), ])
nrow(algae2[!complete.cases(algae2), ])

#centralImputation (fournie par DMwR) remplace les valeurs manquantes : 
# - d'une variable quantitative par la médiane de cette varaible ;
# - d'une variable catégorielle par le mode de cette variable.

# 3e technique : utiliser les similitudes entre les observations
algae3 = knnImputation(algae, k = 10, meth = "median")
nrow(algae[!complete.cases(algae), ])
nrow(algae3[!complete.cases(algae3), ])

#knnImputation (fournie par DMwR) remplace les valeurs manquantes :
# (avec les choix k = 10 et meth = "median")
# - d'une variable quantitative par la médiane des 10 valeurs de cette variable
#     correspondante aux observations les plus proches de celle qui contient la 
#     valeur manquante ; 
# - d'une variable catégorielle par le mode parmi les 10 observations les plus
#     proches.

# == Transformation logarithmqiue ==========================

op = par(mfrow=c(1,2))
hist(algae$NO3, 
     prob = TRUE,
     xlab = "",
     main = "Histogram of NO3",
     ylim = c(0,0.2)
)
lines(density(algae$NO3, na.rm = TRUE), col = 'red')
hist(log(algae$NO3), 
     prob = TRUE,
     xlab = "",
     main = "Histogram of log(NO3)", 
     ylim = c(0, 0.5)
)
lines(density(log(algae$NO3), na.rm = TRUE), col = 'red')
par(op)

# == Modèle prédictif ======================================

# Nous utiliserons dans la suite la troisième technique pour gérer les 
# données manquantes
algae = knnImputation(algae, k = 10, meth = "median")

# Transformation logarithmique pour les concentrations chimiques
algae$lmnO2 = log(algae$mnO2)
algae$lCl = log(algae$Cl)
algae$lNO3 = log(algae$NO3)
algae$lNH4 = log(algae$NH4)
algae$loPO4 = log(algae$oPO4)
algae$lPO4 = log(algae$PO4)
algae$lChla = log(algae$Chla)

# == Premier modèle : la régression linéaire multiple ==
#calcul des coefficients de la régression
lm.a1 = lm(a1 ~ size + speed + mxPH + lmnO2 + lCl + lNH4 + loPO4 + lChla, 
           data = algae)
summary(lm.a1)

#Réponse : str(algae) => il y a 3 variables catégorielles (factor). La commande R crée pour 
# pour chacune d'elles des dummy variables (variables booléennes). Si une variable admet
# m modalités, il y (m-1) nouvelles variables qui sont créées, en prenant comme référence 
# l'autre modalité.

#Réponse : dans une régression linéaire, l'une des métriques les plus utilisées pour 
# caractériser la qualité d'ajustement est le coefficient R^2.
# Mais il en existe une autre plus pertinente quand il s'agit de prédiction : le R^2
# ajusté.

#choisir les paramètres pertinentes
anova(lm.a1)

#Réponse : les variables inutiles sont season, lPO4, lNO3 
#sous-modèle
final.lm = step(lm.a1)

summary(final.lm)

# == Second modèle : les arbres de décision ==

#calcul du modèle
rt.a1 = rpart(a1 ~ ., data = algae[, 1:12])
rt.a1

#affichage de l'arbre dé décision
par(lwd = 2, col = "red")
plot(rt.a1, compress = TRUE)
text(rt.a1, use.n = TRUE, col = "blue")

#ou

par(lwd = 2)
prettyTree(rt.a1, col = "navy")

# == Evaluer la qualité de prévision ==
lm.predictions.a1 = predict(final.lm, algae)
rt.predictions.a1 = predict(rt.a1, algae)

regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[, "a1"])
regr.eval(algae[, "a1"], lm.predictions.a1, train.y = algae[, "a1"])

#affichage des erreurs
par(mfrow = c(1, 2), col="navy")
plot(lm.predictions.a1, algae[, "a1"], 
     main = "Linear Model",
     xlab = "Predictions", 
     ylab = "True Values", 
     xlim=c(-15,62)
     )
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae[, "a1"], 
     main = "Regression Tree",
     xlab = "Predictions", 
     ylab = "True Values", 
     xlim=c(-15,62))
abline(0, 1, lty = 2)

# == Prévisions sur le second dataset ==

summary(test.algae)

# -- TODO - - - - - - - - - - - 
# Nettoyage
#test.algae = 
#
# Modèle linéaire
# lm.predictions.a1 = predict()
#
# Modèle d'arbre de décision
# rt.predictions.a1 = predict()
#
# Evaluation de la modélisation
# regr.eval(true, pred, train.y)
# - - - - - - - - - - - - - - -
