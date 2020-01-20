fpath<-"C:/Users/rollt/Documents/R/Bootcamp/Bootcamp Files/Data Analytics Script Files/Day 3/practicum-for-data-analytcs/datasets/wine/wine.data.csv"
a<-read.csv(fpath)
#view(a)
plot(a)
psych::pairs.panels(a)
#https://vita.had.co.nz/papers/gpp.pdf

#library(ggplot2)
#ggpairs()

components<-objects(a)

componentsContinuous<-c("fixed.acidity","volatile.acidity", "citric.acid", "residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol")
componentsCategorical<-c("quality","color")

library(dplyr)
#Recode red to 1 and white to 2----------------------------------------------------------
colorCategory <- a %>% 
  mutate(
    color2 = case_when(
      color == "RED" ~ "1",
      color == "WHITE" ~ "2"
    )
  )
#-------------------------------------------------------------------------------------------Visualize data

par(mfrow=c(3,4))

#for (i in 1:length(componentsContinuous)){
#  boxplot(a[componentsContinuous[i]])
#}

for (i in componentsContinuous){
  boxplot(colorCategory[i], main=i)
}

for (i in componentsContinuous){
  hist(colorCategory[i], main=i)
}


for (i in componentsContinuous){
  boxplot(colorCategory[i], main=i)
}


#------------------------------------------------------------------------------------Logistic Regression for quality
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

#https://www.analyticsvidhya.com/blog/2016/02/multinomial-ordinal-logistic-regression/

#---------------------------------------------------------Logistic Regression for quality (All Factors)
m<-polr(as.factor(quality)~ color+alcohol+chlorides+citric.acid+color+density+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates+total.sulfur.dioxide+volatile.acidity, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#---------------------------------------------------------Logistic Regression for quality (removed Density)
m<-polr(as.factor(quality)~ color+alcohol+chlorides+citric.acid+color+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates+total.sulfur.dioxide+volatile.acidity, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#---------------------------------------------------------Logistic Regression for quality (removed Density and volatile.acidity)
m<-polr(as.factor(quality)~ color+alcohol+chlorides+citric.acid+color+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates+total.sulfur.dioxide, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#---------------------------------------------------------Logistic Regression for quality (removed Density, volatile.acidity, total.sulfur.dioxide)
m<-polr(as.factor(quality)~ color+alcohol+chlorides+citric.acid+color+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#---------------------------------------------------------Logistic Regression for quality (removed Density, volatile.acidity, total.sulfur.dioxide, chlorides)
m<-polr(as.factor(quality)~ color+alcohol+citric.acid+color+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#---------------------------------------------------------Logistic Regression for quality (removed Density, volatile.acidity, total.sulfur.dioxide, chlorides,fixed.acidity)
#This is the best model by t value, but the worst by AIC.  Since AIC is so high, switching to k nearest neighbors.
m<-polr(as.factor(quality)~ color+alcohol+citric.acid+color+free.sulfur.dioxide+pH+residual.sugar+sulphates, data = colorCategory, Hess=TRUE)
summary(m)
ctable<-coef(summary(m))
p<-pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
ctable<-cbind(ctable, "pvalue"=p)
ctable

xtabs(quality~ color+alcohol+citric.acid+color+free.sulfur.dioxide+pH+residual.sugar+sulphates, data = colorCategory)

#build confidence intervals (this takes a while)
ci<-confint(m, level = 0.95)
ci
exp(coef(m))
exp(cbind(OR=coef(m),ci))

#for scatterplots: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

#Information on AIC: https://www.r-bloggers.com/how-do-i-interpret-the-aic/



#---------------------------------------------------------Shifting to KNN because of the very high AIC (14,672)
#If the KNN produces somewhat good results, then subset and try again.  Because the logistic regression model
#didn't work, I am going to try to do discriminant analysis as the secondary model.

#recode the quality so the model runs faster
colorCategory$qualitynum=colorCategory$quality
colorCategory$quality[which(colorCategory$quality %in% c(3,4,5))]='low'
colorCategory$quality[which(colorCategory$quality %in% c(6,7))]='medium'
colorCategory$quality[which(colorCategory$quality %in% c(8,9))]='high'

features=colnames(colorCategory)[1:11]


library(caret)

#make subsets
train.set=createDataPartition(colorCategory$quality, p=0.7, list = FALSE)
exclude=which(names(colorCategory) %in% c('color','qualitynum'))
train=colorCategory[train.set,-exclude]
test=colorCategory[train.set, -exclude]  #possible error here

#cross validation
ctrl=trainControl(method="repeatedcv",repeats = 5, classProbs = TRUE)

#train the model
knn.mod=train(quality~., data=train, method='knn', preProc=c("center", "scale"), metric="Accuracy", trControl=ctrl, tuneLength=75)

print(knn.mod$finalModel$k)
print(knn.mod$finalModel$xNames)

trngData<-knn.mod$trainingData
head(trngData)

finalModel<-knn.mod$finalModel$tuneValue
finalModel

plot(knn.mod)
ggplot(data=knn.mod,aes(x="#Neighbors", y="Accuracy (Repeated Cross-Validation"))+geom_line()

summary(knn.mod)
knn.mod
#Running out of ram...  may need to shift to new path of separating red and white wines.

#------------------------------------------------------------------------------------Discriminant Analysis
#http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/
library(tidyverse)
library(caret)

preproc.param<-train%>%
  preProcess(method=c("center","scale"))
train.transformed<-preproc.param%>%predict(train)
test.transformed<-preproc.param%>%predict(test)

# it looks like the best option is the mixed discriminant analysis due to the underlying properties of the data.
# The data is full of reasons why linear and quadratic would not give the optimum results.
#This processs does assume an equality of covariance matrix among the classes.

library(mda)
#develop the model
model<-mda(quality~., data=train.transformed)
model

#build the predictions
predicted.classes<-model%>%predict(test.transformed)

#check the model accuracy
mean(predicted.classes==test.transformed$quality)

#Results
#plot(model)

#confusion matrix


#ggplot
test.transformed$quality=test.transformed$quality
test.transformed$quality[which(test.transformed$quality %in% c("low"))]=1
test.transformed$quality[which(test.transformed$quality %in% c("medium"))]=2
test.transformed$quality[which(test.transformed$quality %in% c("high"))]=3

head(as.dataframe(predictions))

mda.data<-cbind(train.transformed,predict(model)$x)



#------------------------------------------------------------------------------------Dealing with outliers
componentsContinuousToAdjust<-componentsContinuous[!componentsContinuous %in% c("fixed.acidity","volatile.acidity","chlorides","total.sulfur.dioxide","density")]

CleanFactors<-c("color","alcohol","citric.acid","color","free.sulfur.dioxide","pH","residual.sugar","sulphates")

winsorizeFactors<-a[a %in% c("sulphates","pH","free.sulfur.dioxide","residual.sugar","citric.acid")]
winsorizeFactors<-select(a, matches("sulphates","pH","free.sulfur.dioxide","residual.sugar","citric.acid"))
#Working here, the lines above aren't working____________________________________________________________________________________________

trimFactors<-componentsContinuousToAdjust[componentsContinuousToAdjust %in% c("alcohol")]

winsorizeFactors<-psych::winsor(winsorizeFactors, trim = 0.05)

#par(mfrow=c(3,4))

for (i in componentsContinuousToAdjust){
  boxplot(colorCategory[i], main=i)
}

for (i in componentsContinuousToAdjust){
  hist(colorCategory[i], main=i)
}



#------------------------------------------------------------------------------------transformations

#for chlorides, we do a log transform to make the data set more normal, 
#but there are still significant outliers because the stdev is still the same
chlorideLog<-log(a$chlorides)
hist(chlorideLog)
boxplot(chlorideLog)
#now testing for normality.  Shapiro-Wilk needs 3 to 5000 observations, so we take a random sample
chloridesSample<-sample(chlorideLog,5000)

#now we can run the shapiro-Wilk.  For the data to be normally distributed, it needs to have a p-value >0.05
shapiro.test(chloridesSample)

#checking for kurtosis to see what type of transformation to do.  The kurtosis should be around 3
e1071::kurtosis(chloridesSample)
#with such a high kurtosis, this data is leptokurtic so we need to do a second transformation
e1071::skewness(chloridesSample)
#After looking at skewness, doing the boxcox transformation.


#using Lambert WxF transformation
library(LambertW)
MLE_LambertW(chlorideLog, distname = "normal", type = "h")

library(MASS)
chlorideBoxCox<-boxcox(a$chlorides)

mean(chloridesSample)
median(chloridesSample)

#citric acid
hist(log(a$citric.acid))
boxplot(log(a$citric.acid))
hist(a$citric.acid)

#------------------------------------------------------------------------------------

