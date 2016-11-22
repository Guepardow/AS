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

library(class)
library(rpart)

library(DMwR)
library(quantmod)
# C'est ce package qu'ill faut charger, cf ?candleChart

library(randomForest)
# == Tests avec knn =========================================================

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

# == k optimal pour les iris ===============================================

Niter = 1
mat_res = matrix(NA, nrow = Niter, ncol = 10)

for(n in 1:Niter){
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
  mat_res[n, ] = apply(cvpred, 2, function(x) sum(class!=x) ) # calcule l’erreur de classif.
}

vect_res = colMeans(mat_res)
plot(vect_res, type = 'l', col = "red", 
     main = "Determination of the optimal k", 
     xlab = "k", ylab = "number of errors")

# == Test avec stock market returns ===================
rm(list=ls())

data(GSPC)

# == Indicator =================================

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

# Function that gives the median of the serie for each day
medPrice = function(p) apply(HLC(p), 1, median)
addMedPrice = newTA(FUN = medPrice, col = 1, legend = "MedPrice", lty = 2)
candleChart(last(GSPC, "3 months"), theme = "white", 
TA = "addT.ind();addAvgPrice(on=1) ; addMedPrice(on=1)")

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
data.model = specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                            myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
                            myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                            CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                            myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                            mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

data.model = specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                            myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                            CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                            myMFI(GSPC) + RSI(Cl(GSPC)) +
                            runSD(Cl(GSPC)))
#volat - macd - aroon - adx - atr - runmean - sar - smi
set.seed(1234)
rf = buildModel(data.model, method="randomForest", training.per = c(start(GSPC), index(GSPC["1999-12-31"])),
                ntree=50, importance=T)
# Question 2 : 

data.model_v2 = specifyModel(T.ind(GSPC) ~ myVolat(GSPC) + myMACD(GSPC) + myAroon(GSPC)+ myADX(GSPC) +
                               myATR(GSPC) + runMean(Cl(GSPC)) + mySAR(GSPC) + mySMI(GSPC))
rf_v2 = buildModel(data.model_v2, method="randomForest", training.per = c(start(GSPC), index(GSPC["1999-12-31"])),
                   ntree=50, importance=T)
varImpPlot(rf_v2@fitted.model, type = 1)

# == Classification au lieu d'une régression ===============================

Tdata.train = as.data.frame(modelData(data.model_v2,
                                      data.window=c("1970-01-02","1999-12-31")))
Tdata.eval = na.omit(as.data.frame(modelData(data.model_v2,
                                             data.window=c("2000-01-01","2009-09-15"))))

Tdata.train[, 1] = trading.signals(Tdata.train[, 1], 0.1, -0.1)
names(Tdata.train)[1] = "signal"
summary(Tdata.train)

# == Méthode des plus proches voisins =========================================

#Choix optimal de k
Niter = 100
nb_k = 10

mat_res = matrix(NA, nrow = Niter, ncol = nb_k)

for(n in 1:Niter){
  # 5-fold cross-validation to select k from the set {1,...,10}
  fold = sample(c(rep(1:5,each=1508), 1, 2)) #1508*5+2 = 7542 = # observations in train
  cvpred = matrix(NA,nrow=nrow(Tdata.train),ncol=nb_k) # initialisation de la matrice des prédicteurs
  
  for (k in 1:nb_k){
    for (v in 1:5){
      sample1 = Tdata.train[which(fold!=v),2:9] #train
      sample2 = Tdata.train[which(fold==v),2:9] #test in cross-validation
      class1 = Tdata.train[which(fold!=v),1]
      cvpred[which(fold==v),k] = knn(sample1,sample2,class1, k=k)
    }
  }
  
  class = as.numeric(Tdata.train[ ,1]) #true values
  # display misclassification rates for k=1:10
  mat_res[n, ] = apply(cvpred, 2, function(x) sum(class!=x) ) # calcule l’erreur de classif.
}

vect_res = colMeans(mat_res)
plot(vect_res, type = 'l', col = "red", 
     main = "Determination of the optimal k", 
     xlab = "k", ylab = "number of errors")

# Prediction on signal
pSignal_knn = knn(Tdata.train[,2:ncol(Tdata.train)], Tdata.eval[,2:ncol(Tdata.eval)], Tdata.train[,1], k = 1)            
table_knn = table(pSignal_knn,trading.signals(Tdata.eval[, 1], 0.1, -0.1))

#calcul du taux d'erreur:
taux_erreur_knn = 1 - (sum(diag(table_knn))/sum(table_knn))
taux_erreur_knn

# arbre de décision
tree_Signal = rpart(formula = signal ~ .,method='class', data = Tdata.train[,1:ncol(Tdata.train)], cp = 0.006)
prettyTree(tree_Signal, col = "navy", cex = 0.8)

#Prediction on signal eval
pSignal_dt = predict(tree_Signal, Tdata.eval, "class")
table_dt = table(pSignal_dt,trading.signals(Tdata.eval[, 1], 0.1, -0.1))

#calcul du taux d'erreur:
taux_erreur_dt = 1 - (sum(diag(table_dt))/sum(table_dt))
taux_erreur_dt

