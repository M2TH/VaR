# CODE VaR du projet 1
#Matthieu Dhenain

rm(list = ls()) #pour travailler avec une memoire vide

library(xts)
library(forecast)
library(moments)
library(yfR)
library(scales)


my_ticker <- 'ABEV3.SA'   # MODIFIEE
first_date <- "2010-01-04"
last_date <-"2023-09-25" # MODIFIEE
# préparation des données
df_yf <- yf_get(tickers = my_ticker, first_date = first_date, last_date = last_date, freq_data='daily',type_return='log')
pt<-df_yf$price_adjusted
dpt=diff(pt)
datesp<-df_yf$ref_date
dates<-datesp[-1]
rt=df_yf$ret_adjusted_prices[-1]
N<-length(rt)
rte<-rt[1:2232] # MODIFIEE
T<-length(rte)
rtt<-rt[2232:N] # MODIFIEE


op<-par(mfrow=c(3,1))
plot(datesp,pt,type='l',ylab="Action AmBev",col=3)
plot(dates,dpt,type='l',col=2,ylab="variations de AmBev")
plot(dates,rt,type='l',col=1,ylab="rendement de AmBev")
par(op)

###----------------------------

# Est ce que cette moyenne empirique tr`es proche de 0 est statistiquement nulle ?
# On test H0 : E(rte)=µ=0 avec la statistique t

rbar<-mean(rte) #moyenne empirique
rbar
# [1] 0.0005792723
s=sd(rte) #´ecart type estim´e
rbar/(s/sqrt(T))#calcul de la statistique t à la main
# [1] 1.895198

#On peut aussi utilise la fonction t.text pour faire le test statistique
# t.test(rte) #fonction R pour faire le test


###-------------------------------------------------------
###     1 : Asymétrie Perte/Gain   (skewness)
###-------------------------------------------------------

# Test d'Agostino 
agostino.test(rte) #par defaut bilatéral

# skew = 0.11743, z = 2.26469, p-value = 0.02353


###-------------------------------------------------------
###     2 : Queues de distribution épaisse  (kurtosis)
###-------------------------------------------------------

# Test d'Anscombe modifié
anscombe.test(rte) #par defaut bilatéral

#kurt = 6.5182, z = 13.7170, p-value < 2.2e-16


###-------------------------------------------------------
###     3 : Autocorrélations fortes et faibles
###-------------------------------------------------------

#Diagramme ACF

library(FinTS)
op<-par(mfrow=c(2,1)) #paramètres d'affichage
Acf(rte,main='ACF du rendement logarithmique')
Acf(rte^2,main='ACF du rendement logarithmique au carré')
par(op)

# FAUT Il METTRE l'ACF et son interprétation dans le rapport ? (Ou que Ljung-Box ??)



#Test de Ljung-Box

pvaluesrt =rep(0,20)
pvaluesrt2 =rep(0,20)
for (i in 1:25 ) {
pvaluesrt[i] = Box.test(rte,lag=i,type="Ljung-Box")$p.value
pvaluesrt2[i] = Box.test(rte^2,lag=i,type="Ljung-Box")$p.value
}

pvaluesrt2

# Ancien : 
# [1] 1.740497e-01 3.124772e-06 1.302194e-05 5.868257e-06 9.327016e-06 9.119891e-07
# [7] 2.263433e-06 5.507277e-06 1.210790e-05 2.518251e-05 5.166277e-05 8.891678e-05
# [13] 1.616718e-04 1.740360e-04 8.887767e-09 1.406554e-08 2.209264e-08 4.604517e-08
# [19] 9.355175e-08 1.841401e-07 3.536086e-07 3.336867e-07 5.748131e-07 9.880263e-07
# [25] 1.580001e-06

#Nouveau 11/10
# [1] 2.370526e-01 1.918177e-07 8.766096e-07 4.095387e-07 8.849146e-07 2.072995e-07 5.499993e-07
# [8] 1.428928e-06 3.032061e-06 6.870694e-06 1.343847e-05 2.508890e-05 4.457560e-05 6.392865e-05
# [15] 2.676651e-08 2.548620e-08 4.858510e-08 9.385920e-08 1.866431e-07 3.644409e-07 6.936276e-07
# [22] 6.100067e-07 1.073613e-06 1.502041e-06 2.660269e-06

pvaluesrt

#Ancien 
# [1] 0.30425932 0.39379068 0.12252453 0.01282864 0.02287402 0.04133692 0.06946988 0.10236957
# [9] 0.12311435 0.05961623 0.07921472 0.07303282 0.10297534 0.08278680 0.09903425 0.13207856
# [17] 0.17132380 0.21163574 0.21042833 0.19097777 0.19472873 0.22459625 0.25405273 0.19941659
# [25] 0.16245368

#Nouveau 11/10
# [1] 0.29286000 0.42043950 0.23483567 0.02802472 0.04565595 0.07885987 0.12190944 0.17970930
# [9] 0.24214799 0.13132183 0.16438772 0.13434954 0.18050251 0.14679363 0.18402567 0.23375735
# [17] 0.28843448 0.33506292 0.35131214 0.33331906 0.33064289 0.37992067 0.42732359 0.32070212
# [25] 0.24532717

###:::::::::::::
### ARMA
###:::::::::::::

# 1. Détermination de p et q du ARMA(p,q) via l’eacf
library(TSA)
library(lmtest)
eacf(rte)



#Nouvea du 11/10 : 
# AR/MA
# 0 1 2 3 4 5 6 7 8 9 10 11 12 13
# 0 o o o x o o o o o o o  o  o  o 
# 1 x o o x o o o o o o o  o  o  o 
# 2 x x o x o o o o o o o  o  o  o 
# 3 x x x x o o o o o o o  o  o  o 
# 4 x x o o o o o o o o o  o  o  o 
# 5 x o o o o o o o o o o  o  o  o 
# 6 x x o o o x o o o o o  o  o  o 
# 7 x o x o o x o o o o o  o  o  o 

# On trouve MA(4)


# 2. Estimation du mod`ele ARMA(p,q) avec les valeurs trouv´ees via l’eacf Estimation d’un MA(4) avec la fonction Arima() du package forecast.

library(forecast)
#reg<-Arima(rte, order=c(0,0,5))
#Il faut avoir des coeffs significatifs, dont au moins celui d'ordre le plus élévé. 
reg<-Arima(rte, order=c(0,0,4))
# reg<-Arima(rte, order=c(0,0,3))
# reg<-Arima(rte, order=c(0,0,15))
coeftest(reg)

#fixation à 0 des coefficients non-significatifs :
#reg<-Arima(rte, order=c(0,0,5),fixed=c(NA,NA,NA,0,NA,NA))
# reg<-Arima(rte, order=c(0,0,4),fixed=c(NA,0,NA,NA,NA))
# coeftest(reg)
# reg<-Arima(rte, order=c(0,0,4),fixed=c(0,0,NA,NA,NA))
# coeftest(reg)
reg<-Arima(rte, order=c(0,0,4),fixed=c(0,0,0,NA,NA))
coeftest(reg)

#On trouve que les coeffs ma1, ma2 et ma3 sont nuls. ma4 et intecpt sont non nuls




# 3. Test d’espérance nulle des aléas du modèle MA sans la constante

residu<-reg$res
t.test(residu)
# 
# One Sample t-test
# 
# data:  residu
# t = -0.0043066, df = 2231, p-value = 0.9966
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.0005998180  0.0005971893
# sample estimates:
#   mean of x 
# -1.314384e-06 

# La p.value= 0.99 est >0.05 donc on ne rejette pas H0 donc l’esp´erance des al´eas est nulle et on passe au test d’absence d’autocorr´elation

# Test d’absence d’autocorr´elation dans les al´eas du mod`ele ARMA

library(tseries)
residuv=(residu-mean(residu))/sd(residu)
K<-20
tmp<-rep(0,K)
for(i in 1:K){
  tmp[i]<-Box.test(residuv,lag=i,type="Ljung-Box")$p.value
  }
tmp


# [1] 0.2675477 0.3799019 0.1999706 0.3260184 0.4189761 0.5461192 0.6590141 0.7551910 0.8249853
# [10] 0.5960112 0.6450476 0.5440773 0.6254199 0.5481709 0.6018622 0.6673172 0.7307158 0.7664317
# [19] 0.7666130 0.7181860

#Tte les valeurs sont >0.05, donc les résidus ne sont pas auto-corrélé : c'est bon, MA(4) est valide!


###-------------------------------------------------------
###     4 : Clusters de volatilité
###-------------------------------------------------------


# LM1<-ArchTest(as.numeric(rte),lag=1)
# LM1

#LM2<-ArchTest(as.numeric(rte),lag=2)
# LM2

for (i in seq(1, 20)) {
  LMi <- ArchTest(as.numeric(rte), lag = i)
  cat("LM d'ordre :", i, "\n")
  print(LMi)
}


# Hormis pour le premier test, toutes les p-values sont < 0.05. On peut rejeter l'hypothèse nulle en faveur de l'hypothèse alternative. Cela indique la présence d'effets ARCH dans les résidus, montrant ainsi une hétéroscédasticité conditionnelle. 





###-------------------------------------------------------
###     5 : Queues épaisses conditionnelles
###-------------------------------------------------------


volat<-garch(residuv,order=c(1,1))
summary(volat)

# Il faut sur les coefficients des p value < 0.05

#faut il regarder autres choses que la p-value des coefficients a0, a1 et b1 ? 
#faut il faire le test d'Arch pour tous les lag entre 1 et 20 ? 


# ? Dans la suite, il faut ici des p-values >0.05 pour dire qu'on a réussi à prendre en compte toute l’hétéroscédasticité conditionnelle

ArchTest(volat$res,lag=1)
for (i in seq(2, 20)) {
  volati <- ArchTest(volat$res, lag = i)
  cat("LM d'ordre :", i, "\n")
  print(volati)
}
# ArchTest(volat$res,lag=20)

#pas d'effet ARCH pour i<15. A partir de 15 : effet ARCH. 
# On essaie ensuite d'augmenter les paramètres m et n du GARCH(m,n) et on reprend la même étude : 

# volat<-garch(residuv,order=c(2,1))
# summary(volat)
# 2,1 ne fonctionne pas : p-value de 1 pour b2

# volat<-garch(residuv,order=c(1,2))
# summary(volat)
#volat<-garch(residuv,order=c(1,3))
#summary(volat)
# coeffs non significatifs

# volat<-garch(residuv,order=c(2,2))
# summary(volat)
# # coeffs non significatifs
# volat<-garch(residuv,order=c(3,2))
# summary(volat)
# # coeffs b non significatifs
# volat<-garch(residuv,order=c(3,1))
# summary(volat)
# # coeffs b non significatifs
# 
# #volat<-garch(residuv,order=c(2,3))  # coeffs b non significatifs
# volat<-garch(residuv,order=c(3,3))
# summary(volat)
# volat<-garch(residuv,order=c(4,3))
# summary(volat)



volat<-garch(residuv,order=c(1,15))
summary(volat)

ArchTest(volat$res,lag=1)
ArchTest(volat$res,lag=20)



anscombe.test(volat$res)




###-------------------------------------------------------
###     6 : Effet de levier
###-------------------------------------------------------

sig<-rep(0,T)
for(t in 1:T)
{
sig[t]<-sqrt(sum(rte[t-22]-(sum(rte[t-22]/22)))^2/22)
}
sigma=sig[24:T]*100
plot(log(pt[24:length(rte)]),type='l',col=2,axes=F,xlab="", ylab="",lwd=3)
axis(2,at=seq(1,3.5,by=0.25)) #axe de gauche
par(new=T)
plot(sigma, col="grey",type='l',axes = F,xlab="", ylab="")
axis(4,at=seq(0,2.5,by=0.25))#axe de droite
legend("topleft", c("log(pt)","sigma"),col = c(2, 1),lty=c(1,1))




###-------------------------------------------------------
###     7 : La saisonnalité
###-------------------------------------------------------


### EFFET WEEK-END

#mise en forme du tableau : rangées et colonnes
jour=format(dates[1:T], format = "%A")
tableaures <- data.frame(matrix(NA,ncol=5,nrow=4))
colnames(tableaures) <- c("lundi","mardi","mercredi","jeudi","vendredi")
rownames(tableaures) <- c("moyenne en %","écart-type annuel en %","skewness","kurtosis")

# Données du mardi : remplissage du tableau
rtmar<-as.numeric(rte[jour=="mardi"])
mardi<-mean(rtmar) #moyenne journaliere
tableaures[1,2] <- mardi*100 #moyenne journaliere en %
tableaures[2,2] <- sd(rtmar)*100*sqrt(252) #ecart-type annualise en %
tableaures[3,2] <- skewness(rtmar)
tableaures[4,2] <- kurtosis(rtmar)

rtmer<-as.numeric(rte[jour=="mercredi"])
mer<-mean(rtmer)
tableaures[1,3] <- mer*100
tableaures[2,3] <- sd(rtmer)*100*sqrt(252)
tableaures[3,3] <- skewness(rtmer)
tableaures[4,3] <- kurtosis(rtmer)

rtjeu<-as.numeric(rte[jour=="jeudi"])
jeudi<-mean(rtjeu)
tableaures[1,4] <- jeudi*100
tableaures[2,4] <- sd(rtjeu)*100*sqrt(252)
tableaures[3,4] <- skewness(rtjeu)
tableaures[4,4] <- kurtosis(rtjeu)

rtven<-as.numeric(rte[jour=="vendredi"])
ven<-mean(rtven)
tableaures[1,5] <- ven*100
tableaures[2,5] <- sd(rtven)*100*sqrt(252)
tableaures[3,5] <- skewness(rtven)
tableaures[4,5] <- kurtosis(rtven)

rtlun<-as.numeric(rte[jour=="lundi"])
lundi<-mean(rtlun)
tableaures[1,1] <- lundi*100
tableaures[2,1] <- sd(rtlun)*100*sqrt(252)
tableaures[3,1] <- skewness(rtlun)
tableaures[4,1] <- kurtosis(rtlun)

tableaures



### EFFET JANVIER

monthplot(rte, ylab="rendement",main="", cex.main=1,col.base=2,lwd.base=3)




###-------------------------------------------------------
###     8 : Stationnarité
###-------------------------------------------------------


# De ce que je comprends : les aléas sont non-autocorrélés d'apres le test LB de la partie 3 ?


library(urca)
summary(ur.df(rte,type= "trend",lags=0))


# Pour tester si les résidus de DF sont autocorrélés, on fait le graphique et le corrélogramme des résidus suivant : 
plot(ur.df(rte,lag=0,type="trend"))
# Les résidus ne sont pas corrélés, SAUF à l'ordre 4, de façon tout juste significative. 


# On passe au modèle contenant une constante mais pas de tendance.
summary(ur.df(rte,type= "drift",lags=0))

# la valeur calculée du t est > à la valeur tabulée donc on ne rejette pas H0 : (rho-1) = 0 et beta0 = 0

plot(ur.df(rte,lag=0,type="drift"))
# Même CCL que précedemment : Les résidus ne sont pas corrélés, SAUF à l'ordre 4


summary(ur.df(rte,type= "none",lags=0))

### On trouve t = -48 < -1.95. donc on rejette H0 donc le PGD n’est pas DS. On en conclut que le PGD est stationnaire. Cette
## conclusion n’est valide que si les aléas de la régression de Dickey et Fuller ne sont pas auto-corrélés



# Verifions si les aléas de la régression de Dickey et Fuller ne sont pas auto-corrélés : 

plot(ur.df(rte,lag=0,type="none"))

# Ce graphique indique que les aléas sont auto-corrélés à l'ordre 4 et donc notre conclusion concernant l’absence
# de RU dans le rendement logartithmique de AmBev n’est pas valide. Nous devons effectuer un test de RU dans le cadre de la
# régression ADF.



#TEStT de LB à l'ordre 4



### test de Dickey Fuller Augmenté (ADF)

# On introduit k variables explicatives
# On determine k avec la partie entière de la formule de Schwert
Schwert<-as.integer(12*(T/100)^(0.25))
# Schwert = 26

summary(ur.df(rte,type= "none",lags=Schwert))

# On diminue la valeur de lag jusqu’à avoir tous les gamma avec une valeur de la statistique t calculée supérieure en valeur absolue à 1.6

#summary(ur.df(rte,type= "none",lags=20))
#summary(ur.df(rte,type= "none",lags=15))
#summary(ur.df(rte,type= "none",lags=10))
#summary(ur.df(rte,type= "none",lags=5))
#summary(ur.df(rte,type= "none",lags=4))
summary(ur.df(rte,type= "none",lags=3))

# Pour k=3, toutes les valeurs t sont supérieures en valeur absolue à 1.6. 
# Value of test-statistic is: -25.7566  < −1.95 donc le test ADF permet de conclure à la stationnarité du PGD qui a généré notre série du rendement


# CODE EXECUTE









