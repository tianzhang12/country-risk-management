prdata = read.csv("PR.csv")
erdata = read.csv("ERisk.csv")
lrdata = read.csv("LR.csv")
lcddata = read.csv("LCD.csv")
crdata = read.csv("Countryrisk.csv")
prdata
erdata
lrdata
crdata

#making variables

er1 = as.numeric(erdata[,2])
er2 = as.numeric(erdata[,3])
er3 = as.numeric(erdata[,4])
er4 = as.numeric(erdata[,5])
er5 = as.numeric(erdata[,6])
er6 = as.numeric(erdata[,7])
er7 = as.numeric(erdata[,8])
er8 = as.numeric(erdata[,9])
er9 = as.numeric(erdata[,10])
er10 = as.numeric(erdata[,11])
er11 = as.numeric(erdata[,12])

pr1 = as.numeric(prdata[,2])
pr2 = as.numeric(prdata[,3])
pr3 = as.numeric(prdata[,4])
pr4 = as.numeric(prdata[,5])

lr1 = as.numeric(lrdata[,2])

cr = as.numeric(crdata[,2])

#check colinearity 
total=cbind(er1,er2,er3,er4,er5,er6,er7,er8,er9,er10,er11,pr1,pr2,pr3,pr4,lr1)
cor(total)

#############################################################################################

#generate principle components for PR (including pr and lr variables)

pr = cbind(pr1,pr2,pr3,pr4,lr1)
pr
pcapr = prcomp(pr)
summary(pcapr)
pcapr$rotation
pcapr$x
prindex1 = pcapr$x[,1]
prindex2 = pcapr$x[,2]

#############################################################################################

#generate principle components for ER

er = cbind(er1,er2,er3,er4,er5,er6,er7,er8,er9,er10,er11)
er
pcaer= prcomp(er)
summary(pcaer)
pcaer$rotation
pcaer$x
erindex1 = pcaer$x[,1]

#check colinearity of both PCA index for PR and ER
tr = cbind(prindex1,prindex2, erindex1)
cor(tr)

#############################################################################################

#linear regression model for y1- PCA of both, y2- PCA of PR and LR and all economic variables
install.packages("olsrr")
library(olsrr)


y1<-lm(cr~prindex1 + prindex2 + erindex1)
y2<-lm(cr~prindex1 + prindex2 + er1 + er2 + er3 + er4 + er5 + er6 +er7 + er8 + er9 + er10 + er11)
summary(y1)
summary(y2)

ols_vif_tol(y)
ols_vif_tol(lc1)
ols_vif_tol(lc2)

#############################################################################################

install.packages("Rcmdr")
library(MASS)
library(Rcmdr)

#Backward selection

2. fit the full model
fit.full<-lm(cr~prindex1 + prindex2 + er1 + er2 + er3 + er4 + er5 + er6 +er7 + er8 + er9 + er10 + er11)
summary(fit.full)

3. variable selection

dropterm(fit.full, test="F")
fit1<-update(fit.full, .~.-er10-er11-er3)

dropterm(fit1, test="F")
fit2<-update(fit1, .~.-er9-er4-er1)

dropterm(fit2, test="F")
fit3<-update(fit2, .~.-er5)

dropterm(fit3, test="F")
fit4<-update(fit3, .~.-er2)

dropterm(fit4, test="F")
fit5<-update(fit4, .~.-er7)

dropterm(fit5, test="F")

# We stop the backward regression here

fit.reduce<-lm(cr ~ prindex1 + prindex2 + er6 + er8)
summary(fit.reduce)

anova(fit.full,fit1,fit2,fit3,fit4,fit5,fit.reduce, test="F")

#############Stepwise 
resstep<-stepwise(fit.full)

#############Forward
step(lm(cr~1), direction="forward",scope=~prindex1+prindex2+er1 + er2 + er3 + er4 + er5 + er6 +er7 + er8 + er9 + er10 + er11)

forward<-lm(cr ~ er2 + er7 + er5 + prindex2 + er3 + er6 + prindex1 + er8)
summary(forward)
#################plot
totalrisk = cbind(prdata,erdata,lrdata)

barplot(pr1, names.arg=prdata[,1],main="Physical Violence")
barplot(pr2, names.arg=prdata[,1],main="Violence Ranking")
barplot(pr3, names.arg=prdata[,1],main="Corruption Score")
barplot(pr4, names.arg=prdata[,1],main="Corruption, Ranking")
barplot(lr1, names.arg=prdata[,1],main="Legal Risk")
barplot(er1, names.arg=prdata[,1],main="GDP")
barplot(er2, names.arg=prdata[,1],main="GDP per cap")
barplot(er3, names.arg=prdata[,1],main="Gross national saving")
barplot(er4, names.arg=prdata[,1],main="Unemployment rate")
barplot(er5, names.arg=prdata[,1],main="Taxed and other income")
barplot(er6, names.arg=prdata[,1],main="Public Debt")
barplot(er7, names.arg=prdata[,1],main="Inflation rate")
barplot(er8, names.arg=prdata[,1],main="reserve of foreign currency and gold")
barplot(er9, names.arg=prdata[,1],main="central bank discount rate")
barplot(er10, names.arg=prdata[,1],main="commerical prime lending rate")
barplot(er11, names.arg=prdata[,1],main="current balance")

