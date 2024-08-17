# An-empirical-study-of-influencing-factors-of-translanguaging
R script
#import and view data
> library(readxl)
> TST <- read_excel("C:/Desktop/spreadsheet.xlsx")
> View(TST)
> str(TST)
 
#install packages
install.packages("dplyr")
library("dplyr")
install.packages("mosaic")
library("mosaic")
install.packages("foreign")
library("foreign")
install.packages("Hmisc")
library("Hmisc")
install.packages("rcompanion")
library(rcompanion)
install.packages("ggplot2")
library(ggplot2)
 
#descriptive analysis
summary(TST$trans.speak)
sd(TST$trans.speak)
d1<-density(TST$trans.speak)
plot(d1,main="Kernel Density of Trans-speaking")
polygon(d1,col="purple",border = "yellow")
hist(TST$trans.speak)
 
summary(TST$trans.think)
sd(TST$trans.think)
d2<-density(TST$trans.think)
plot(d2,main="Kernel Density of Trans-thinking")
polygon(d2,col="blue",border = "yellow")
hist(TST$trans.think)
 
summary(TST$competence)
sd(TST$competence)
 
summary(TST$confidence)
sd(TST$confidence)
 
 
#correlation analysis
corr1 <- c("trans.speak", "trans.think", "confidence","competence")
data1 <- TST[corr1]
View(corr1)
corr.matrix <- rcorr(as.matrix(data1))
corr.matrix
 
install.packages("corrplot")
libraryC"corrplot")
corrplot(.corr.matrix$r,type="upper",order="hclust",tl.col="black",tl.srt=140)
 
#multiple regression
#trans-speaking
Model1.1 <- lm(trans.speak ~ confidence, data=TST)
summary(Model1.1)
Model1.2 <- lm(trans.speak ~ confidence+competence, data=TST)
summary(Model1.2)
Model1.3 <- lm(trans.speak ~ confidence+competence+gender, data=TST)
summary(Model1.2)
Model1.4 <- lm(trans.speak ~ confidence+competence+gender+native.language, data=TST)
summary(Model1.3)
Model1.5 <- lm(trans.speak ~ confidence+competence+gender+native.language+education.level, data=TST)
summary(Model1.4)
 
#trans-thinking
Model2.1 <- lm(trans.think ~ confidence, data=TST)
summary(Model2.1)
Model2.2 <- lm(trans.think ~ confidence+competence, data=TST)
summary(Model1.2)
Model2.3 <- lm(trans.think ~ confidence+competence+gender, data=TST)
summary(Model2.2)
Model2.4 <- lm(trans.think ~ confidence+competence+gender+native.language, data=TST)
summary(Model2.3)
Model2.5 <- lm(trans.think ~ confidence+competence+gender+native.language+education.level, data=TST)
summary(Model2.4)
 
