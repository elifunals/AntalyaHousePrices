set.seed(1024)

library(dplyr)
library(tidyverse)
library(Hmisc)
library(cluster)
library(fpc)
library(ggplot2)
library(DMwR2)
library(dbscan)
library(rpart) 
library(rpart.plot) 
library(varhandle)

load("house.data")

summary(antalya_train)
str(antalya_train)
dataHouse<-antalya_train

summary <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}
summary(dataHouse)

colSums(is.na(antalya_train))
new_DF <- antalya_train[rowSums(is.na(antalya_train)) > 0,] #dataset with just NA's
dataHouse<-na.omit(antalya_train)
dataHouse$Id<-NULL

dataHouse$MustakilMi <- as.factor(ifelse(dataHouse$MustakilMi == 0, "Hayir", "Evet"))
dataHouse$CepheBati <- as.factor(ifelse(dataHouse$CepheBati == 0, "Hayir", "Evet"))
dataHouse$CepheDogu <- as.factor(ifelse(dataHouse$CepheDogu == 0, "Hayir", "Evet"))
dataHouse$CepheGuney <- as.factor(ifelse(dataHouse$CepheGuney == 0, "Hayir", "Evet"))
dataHouse$CepheKuzey <- as.factor(ifelse(dataHouse$CepheKuzey == 0, "Hayir", "Evet"))
dataHouse$ManzaraSehir <- as.factor(ifelse(dataHouse$ManzaraSehir == 0, "Hayir", "Evet"))
dataHouse$ManzaraDoga <- as.factor(ifelse(dataHouse$ManzaraDoga == 0, "Hayir", "Evet"))
dataHouse$ManzaraGol <- as.factor(ifelse(dataHouse$ManzaraGol == 0, "Hayir", "Evet"))
dataHouse$ManzaraDeniz <- as.factor(ifelse(dataHouse$ManzaraDeniz == 0, "Hayir", "Evet"))
dataHouse$ManzaraBogaz <- as.factor(ifelse(dataHouse$ManzaraBogaz == 0, "Hayir", "Evet"))
dataHouse$Mahalle<-unfactor(dataHouse$Mahalle)
dataHouse$Ilce<-unfactor(dataHouse$Ilce)

describe(dataHouse)

colnames(dplyr::select_if(dataHouse,is.numeric))

find_outliers <- function(x)
{
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  Vl <- Q1 - 1.5 * IQR
  Vr <- Q3 + 1.5 * IQR
  return (which(x < Vl | x > Vr))
}

alanOutliers <- dataHouse[find_outliers(dataHouse$OrijinalAlan),4]
metreKareOutliers <- dataHouse[find_outliers(dataHouse$AlanMetrekare),5] 
noBanyoOutliers <- dataHouse[find_outliers(dataHouse$BanyoSayisi),6] 
fiyatOutliers <- dataHouse[find_outliers(dataHouse$FiyatTL),7]
noOdaOutliers <- dataHouse[find_outliers(dataHouse$OdaSayisi),8]
noSalonOutliers <-dataHouse[find_outliers(dataHouse$SalonSayisi),9] 
noToplamOdaOutliers <- dataHouse[find_outliers(dataHouse$ToplamOdaSayisi),10] 
toplamKatOutliers <- dataHouse[find_outliers(dataHouse$ToplamKat),11] 
katOutliers <- dataHouse[find_outliers(dataHouse$BulunduguKat),12] 
yasOutliers <- dataHouse[find_outliers(dataHouse$GercekYas) ,13]

numData<- select_if(dataHouse, is.numeric)
dbscan::kNNdistplot(numData, k =  1)

res.fpc <- fpc::dbscan(numData, eps = 30, MinPts = 100)
res.db <- dbscan::dbscan(numData, 15, 15)

dbscan.outliers <- function(data, ...) 
{ 
  require(fpc, quietly=TRUE)
  cl <- dbscan(data,...)
  posOuts <- which(cl$cluster == 0) 
  list(positions = posOuts,outliers = data[posOuts,],dbscanResults = cl) 
}

outs<-dbscan.outliers(numData, 100,3)
outs$outliers

numData$outlier = 0 
numData$outlier[outs$positions] = 1
outlier<-numData$outlier

max_Fiyat <- max(dataHouse$FiyatTL)
min_Fiyat <- min(dataHouse$FiyatTL)
width <- median(dataHouse$FiyatTL)
dataHouse$FiyatTL<-cut(dataHouse$FiyatTL, breaks= c(17000,170000,250000,400000,max_Fiyat))
fiyat<-cut(dataHouse$FiyatTL, breaks= c(17000,170000,250000,400000,max_Fiyat))

fit<-rpart(FiyatTL~.,data=numData, cp = 0.005,  maxdepth=4)
rpart.plot(fit, main="Single Rule Model")


boxplot(FiyatTL ~AlanMetrekare, data=numData, main="Ozone reading across months")

x <- numData$FiyatTL
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

numData$YeniIlce=0
numData$YeniIlce<-dataHouse$Ilce
numData$Isıtma=0
numData$Isıtma<-dataHouse$Isitma
numData

