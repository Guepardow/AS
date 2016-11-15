# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# ENSAE - 3A - Apprentissage statistique
# Créé le 2016-11-15
#
# Etudiants : Mehdi Miah & Ulrich Mpeli
# Descriptive : TP2
# Remarques : librairies utilisées : class, DMwR, quantmod, randomForest
#
# TODO  : 
# BUG :
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# == Préambule ============================================================

rm(list=ls())
cat("\014")

# == Tests avec knn =========================================================

library(class)
data(iris)
head(iris)
summary(iris)
# iris a 150 lignes et 5 colonnes

train = iris[c(1:30,51:80,101:130),1:5]
test = iris[c(31:50,81:100,131:150),1:5]

# prédiction
pred = knn(train[,1:4], test[,1:4], train[,5], k = 3)

# display the confusion matrix
table(pred,test[,5])

# == Cross validation ======================

# 5-fold cross-validation to select k from the set {1,...,10}
fold = sample(rep(1:5,each=18)) # creation des groupes B_v
cvpred = matrix(NA,nrow=90,ncol=10) # initialisation de la matrice des prédicteurs

for (k in 1:10){
  for (v in 1:5){
    sample1 = train[which(fold!=v),1:4] #train
    sample2 = train[which(fold==v),1:4] #test
    class1 = train[which(fold!=v),5]
    cvpred[which(fold==v),k] = knn(sample1,sample2,class1, k=k)
  }
}

class = as.numeric(train[ ,5])
# display misclassificati on rate s for k=1:10
apply(cvpred, 2, function(x) sum(class!=x) ) # calcule l’erreur de classif.
    
# == Test avec stock market returns ===================
rm(list=ls())

library(DMwR)
data(GSPC)

# == Indicator =================================
library(quantmod)
# C'est ce package qu'ill faut charger, cf ?candleChart

T.ind = function(quotes, tgt.margin = 0.025 , n.days = 10){
  v = apply(HLC(quotes), 1, mean)
  r = matrix(NA, ncol = n.days, nrow = NROW( quotes))
  for (x in 1:n.days){
    r[, x] = Next(Delt(v, k = x), x)
  }
  x = apply(r, 1, function(x) sum(x[x > tgt.margin | x <
                                      -tgt.margin] ))
  
  if(is.xts(quotes)){
    xts(x, time(quotes))
  }else{
    x
  }
}

# ccc = T.ind(GSPC)

# == Plot with candleChart ==============================

candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice = function(p) apply(HLC(p), 1, mean)
addAvgPrice = newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind = newTA(FUN = T.ind, col = "red", legend = "tgtRet")

get.current.chob<-function(){quantmod:::get.current.chob()}
candleChart(last(GSPC, "3 months"), theme = "white", TA = "addAvgPrice(on=1)")
candleChart(last(GSPC, "3 months"), theme = "white", TA = "addT.ind(on=1) ; addAvgPrice(on=1)")

# Notre fonction qui donne la valeur médiane
medPrice = function(p) apply(HLC(p), 1, median)
addMedPrice = newTA(FUN = medPrice, col = 1, legend = "MedPrice", lty = 2)
candleChart(last(GSPC, "3 months"), theme = "white", TA = "addT.ind() ; addAvgPrice(on=1) ; addMedPrice(on=1)")

# Dans HLC, la valeur médiane correspond toujours à la valeur de C car : H > C > L ...

# L'argument (on=1) permet la superposition des graphes sur le candleChart

# Rq : l'indicateur T.ind étant construit sur 10 jours, on ne 
# dispose pas de sa valeur sur les 10 derniers jours ? Quid d'une prédiction ?
# == Financial indicators =====================================================

myATR = function(x) ATR(HLC(x))[, "atr"]
mySMI = function(x) SMI(HLC(x))[, "SMI"]
myADX = function(x) ADX(HLC(x))[, "ADX"]
myAroon = function(x) aroon(x[ , c("High", "Low")])$oscillator
myBB = function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol = function(x) Delt(chaikinVolatility(x[ ,c("High","Low")]))[, 1]
myCLV = function(x) EMA(CLV(HLC(x)))[, 1]
myEMV = function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,2]
myMACD = function(x) MACD(Cl(x))[, 2]
myMFI = function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
mySAR = function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat = function(x) volatility(OHLC(x), calc = "garman")[,1]

# Cours sur arbre de décision : sur chaque partition, on prédit une même valeur
# on obtient un pavage de l'espace avec un découpage vertical et horizontale (cf slide TP2 n° 5)
# on obtient un pavage avec des rectangles
# la prédiction est rapide mais ça peut prendre du temps pour le construire

# pour les KNN, il faut tout sauvegarder, déterminer k_opt, la prédiction est assez couteuse car 

# elle nécessite le calcul de toutes les distances
# Les forêts : avoir pleins de prédicteurs et prendre la moyenne
# Pleins d'arbres avec des sous-ensembles, puis on en prend la moyenne

# En général, random forest > arbre de décision

data(GSPC)
library(randomForest)
data.model = specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
                             myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                             myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)

rf = buildModel(data.model, method="randomForest", training.per = c(start(GSPC),index(GSPC["1999-12-31"])),
                ntree=50, importance=T)
# Question 2 : 

varImpPlot(rf@fitted.model, type =1)
# Question 3 : myATR, mySMI, runMean, myVolat
# runSD, myMACD, mySAR, myADX

data.model = specifyModel(T.ind(GSPC) ~ myATR(GSPC) + mySMI(GSPC) + myADX(GSPC)+ myVolat(GSPC) +
                            myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

Tdata.train = as.data.frame(modelData(data.model,
                                      data.window=c("1970-01-02","1999-12-31")))
Tdata.eval = na.omit(as.data.frame(modelData(data.model,
                                             data.window=c("2000-01-01","2009-09-15"))))
