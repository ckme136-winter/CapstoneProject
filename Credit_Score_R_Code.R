# import Dataset in R and display summary
credit_score_data <- read.csv("C:/Users/cs-training.csv", stringsAsFactors = FALSE,
strip.white = TRUE, na.strings= c("NA","") )

nrow(credit_score_data) # 150000 observations of 12 variables

summary(credit_score_data)

# find number of observations where credit_score_data$SeriousDlqin2yrs is coded as 0
sum(credit_score_data$SeriousDlqin2yrs == 0) # 139974 observations (93.3%)   

# find number of observations where credit_score_data$SeriousDlqin2yrs is coded as 1
sum(credit_score_data$SeriousDlqin2yrs == 1) # 10026 observations (6.7%) 

# clean missing data 

sum(is.na(credit_score_data$MonthlyIncome)) # 29731 observations
sum(is.na(credit_score_data$NumberOfDependents)) # 3924 observations

# remove rows which contains NA in credit_score_data$MonthlyIncome 
goodCredit_score_data <- credit_score_data[!is.na(credit_score_data$MonthlyIncome),]
nrow(goodCredit_score_data) # (150000-29731=) 120269 observations 

# observed from 'summary(goodCredit_score_data)' that rows containing NA values in  
# MonthlyIncome and NumberOfDependents columns have been removed  
sum(is.na(goodCredit_score_data)) # 0 observation

# **** Data Cleaning - First Pass

# find number of observations where goodCredit_score_data$MonthlyIncome = 0
sum(goodCredit_score_data$MonthlyIncome == 0) # 1634 observations

# 1.35% [(1634/120269)*100] is bad data and these observations has been removed as it will 
# not impact significantly goodCredit_score_data of 120269 observations 

# remove row where goodCredit_score_data$MonthlyIncome = 0 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$MonthlyIncome > 0,]
nrow(goodCredit_score_data) # 118635 observations <- 120269 - 1634


# find number of observations where goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines greater than 1
sum(goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines > 1) # 2749 observations

# there are 2749 rows where goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines is > 1
# 2.31% [(2749/118635)*100] is bad data and these observations will be removed 

# remove row where goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines is > 1 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines <= 1,]
nrow(goodCredit_score_data) # 115886 observations <- 118635 - 2749

# find number of observations where goodCredit_score_data$DebtRatio greater than 1
sum(goodCredit_score_data$DebtRatio > 1) # 5561 observations

# there are 5561 rows where goodCredit_score_data$DebtRatio is > 1
# 4.7% [(5561/115886)*100] is bad data and these observations will be removed 

# remove row where goodCredit_score_data$DebtRatio is > 1 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$DebtRatio <= 1,]
nrow(goodCredit_score_data) # 110325 observations <-  115886 - 5561

# **** Data Cleaning - Second Pass

# range of goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines
range(goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines) # 0 1, no further correction required 
nrow(goodCredit_score_data) # 110325 observations

# range of goodCredit_score_data$age
range(goodCredit_score_data$age) # 0 103

sort(as.factor(unique(unlist(goodCredit_score_data$age))), decreasing=TRUE)
# unique values of age:
# 0 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 ... 103

# find number of observations where goodCredit_score_data$age > 80
sum(goodCredit_score_data$age > 80) #  2981 observations

# there are 2981 rows where goodCredit_score_data$age is > 80
# 2.6% [(2981/110325)*100] is bad data and these observations will be removed 

# remove row where goodCredit_score_data$age is > 80 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$age <= 80,]
nrow(goodCredit_score_data) #  107344 observations <-  110325 - 2981

# find number of observations where goodCredit_score_data$age > 20
sum(goodCredit_score_data$age < 20) #  1

goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$age > 20,]
nrow(goodCredit_score_data) # 107343 observations 

# range of goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse
range(goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse) # 0 98

sort(as.factor(unique(unlist(goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse))), decreasing=TRUE)
# unique values of NumberOfTime30.59DaysPastDueNotWorse: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 96 98

# find number of observations where goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse > 13
sum(goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse > 13) #  141 observations

# remove row where goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWors is > 13 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse <= 13,]
nrow(goodCredit_score_data) #  107202 observations 

# range of goodCredit_score_data$DebtRatio
range(goodCredit_score_data$DebtRatio) # 0 1 -> no further correction required

# range of goodCredit_score_data$MonthlyIncome
range(goodCredit_score_data$MonthlyIncome) # 1 3008750

# find number of observations where goodCredit_score_data$MonthlyIncome < 1500
sum(goodCredit_score_data$MonthlyIncome < 1500) # 3165 observations

# remove row where goodCredit_score_data$MonthlyIncome is < 1500 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$MonthlyIncome >= 1500,]
nrow(goodCredit_score_data) #  104037 observations 

# range of goodCredit_score_data$NumberOfOpenCreditLinesAndLoans
range(goodCredit_score_data$NumberOfOpenCreditLinesAndLoans) # 0 57

sort(as.factor(unique(unlist(goodCredit_score_data$NumberOfOpenCreditLinesAndLoans))), decreasing=TRUE)
# unique values of NumberOfOpenCreditLinesAndLoans:
# 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 ... 57

# find number of observations where goodCredit_score_data$NumberOfOpenCreditLinesAndLoans >= 15
sum(goodCredit_score_data$NumberOfOpenCreditLinesAndLoans >= 15) # 10525

# remove row where goodCredit_score_data$NumberOfOpenCreditLinesAndLoans >= 15 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$NumberOfOpenCreditLinesAndLoans < 16,]
nrow(goodCredit_score_data) #  93512 observations 

# range of goodCredit_score_data$NumberOfTimes90DaysLate
range(goodCredit_score_data$NumberOfTimes90DaysLate) # 0 17

sort(as.factor(unique(unlist(goodCredit_score_data$NumberOfTimes90DaysLate))), decreasing=TRUE)
# unique values of NumberOfTimes90DaysLate:
# 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 17

# find number of observations where goodCredit_score_data$NumberOfTimes90DaysLate > 10
sum(goodCredit_score_data$NumberOfTimes90DaysLate >= 10) # 8 

# remove row where goodCredit_score_data$NumberOfTimes90DaysLate > 10 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$NumberOfTimes90DaysLate <= 10,]
nrow(goodCredit_score_data) # 93504 observations 

# range of goodCredit_score_data$NumberRealEstateLoansOrLines
range(goodCredit_score_data$NumberRealEstateLoansOrLines) # 0 11

sort(as.factor(unique(unlist(goodCredit_score_data$NumberRealEstateLoansOrLines))), decreasing=TRUE)
# unique values of NumberRealEstateLoansOrLines:
# 0 1 2 3 4 5 6 7 8 9 10 11

# find number of observations where goodCredit_score_data$NumberRealEstateLoansOrLines > 5
sum(goodCredit_score_data$NumberRealEstateLoansOrLines > 5) # 173

# remove row where goodCredit_score_data$NumberRealEstateLoansOrLines > 5 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$NumberRealEstateLoansOrLines < 6,]
nrow(goodCredit_score_data) # 93331 ovservations 

# range of goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse
range(goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse) # 0 7

sort(as.factor(unique(unlist(goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse))), decreasing=TRUE)
# unique values of NumberOfTime60.89DaysPastDueNotWorse:
# 0 1 2 3 4 5 6 7

# find number of observations where goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse > 5
sum(goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse > 5) # 10

# remove row where goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse > 5 
goodCredit_score_data <- goodCredit_score_data[goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse < 6,]
nrow(goodCredit_score_data) # 93321 observations 

# range of goodCredit_score_data$NumberOfDependents
range(goodCredit_score_data$NumberOfDependents) # 0 20, no further correction required 

summary(goodCredit_score_data)

nrow(goodCredit_score_data) #  93321 observations

# Copy goodCredit_score_data to goodCredit_score_data_Final and skip column 'X' and SeriousDlqin2yrs
goodCredit_score_data_Final <- goodCredit_score_data[c(3,4,5,6,7,8,9,10,11,12)]

# Rename variable names to short variable names for corrplot  
colnames(goodCredit_score_data_Final) <- 
  c('RU', 'age', '30-59D', 'DR', 'MI', 'OCL', '90L', 'REL', '60-89D', 'Dep')

library(corrplot)

# Find Spearman coorelation between columns in goodCredit_score_data_Final 
cor(goodCredit_score_data_Final, method="spearman")
corrplot(cor(goodCredit_score_data_Final), method = "ellipse")


# Find Pearson coorelation between columns in goodCredit_score_data_Final
cor(goodCredit_score_data_Final, method="pearson")
corrplot(cor(goodCredit_score_data_Final), method = "ellipse")

# Histogram (a very common plot which plots the frequencies that data appears 
# within certain ranges
# A histogram consists of parallel vertical bars that graphically shows the frequency 
# distribution of a quantitative variable. The area of each bar is equal to the frequency of
# items found in each class.
hist(goodCredit_score_data$RevolvingUtilizationOfUnsecuredLines, col="red")
hist(goodCredit_score_data$age, col="red")
hist(goodCredit_score_data$NumberOfTime30.59DaysPastDueNotWorse, col="red")
hist(goodCredit_score_data$DebtRatio, col="red")
hist(goodCredit_score_data$MonthlyIncome, col="red")
hist(goodCredit_score_data$NumberOfOpenCreditLinesAndLoans, col="red")
hist(goodCredit_score_data$NumberOfTimes90DaysLate, col="red")
hist(goodCredit_score_data$NumberRealEstateLoansOrLines, col="red")
hist(goodCredit_score_data$NumberOfTime60.89DaysPastDueNotWorse, col="red")
hist(goodCredit_score_data$NumberOfDependents, col="red")


range(goodCredit_score_data$SeriousDlqin2yrs)
sum(goodCredit_score_data$SeriousDlqin2yrs == 1) # 5598 observations
sum(goodCredit_score_data$SeriousDlqin2yrs == 0) # 87723 observation


goodCredit_score_data_new <- subset(goodCredit_score_data[c(2,3,4,5,6,7,8,9,10,11,12)])


# Split dataset into training and test sets
rn_train <- sample(nrow(goodCredit_score_data_new),floor(nrow(goodCredit_score_data_new)*0.7))
train <- goodCredit_score_data_new[rn_train,] # nrow(train) -> 65324
test <- goodCredit_score_data_new[-rn_train,] # nrow(test)  -> 27997

# Build full Model with all variables 
formula_SerDlq <- paste(names(goodCredit_score_data)[2] ,"~",
                        paste(names(goodCredit_score_data[c(3,4,5,6,7,8,9,10,11,12)]), collapse="+"))

formula_SerDlq
formula <- as.formula(formula_SerDlq)

system.time(fit.full <- glm(formula,data=train,family=binomial()))
summary(fit.full) 


pr <- predict(fit.full, newdata = test, type = "response")

for (i in 1:length(pr))
  ifelse(pr[i] > 0.5, pr[i]<- 1, pr[i] <- 0 ) 

confusion_matrix <- table(test$SeriousDlqin2yrs, pr, dnn = c("actual", "predicted"))
confusion_matrix

accuracy <- (26172 + 252)/( 26172+182+1391+252)
recall <- 26172/(26172+ 182)
precision <- 26172/(26172 + 1391)

accuracy
recall 
precision 


library(ROCR)

# ROC
test_glm_roc <- test

system.time(test_glm_roc$score <- predict(fit.full, newdata=test, type='response'))
pred <- prediction(test_glm_roc$score, test_glm_roc$SeriousDlqin2yrs)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

# AUC
library(AUC)
library(ggplot2)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc 

# RMSE
prediction <- predict(fit.full, interval="prediction", newdata=test)
# head(prediction)
rmse <- sqrt(sum((prediction - test$SeriousDlqin2yrs)^2)/nrow(test))
rmse


# Run Naive Bayes
library("RCurl") 
library("klaR") 
library("caret") 

system.time(modelNB <- NaiveBayes(as.factor(SeriousDlqin2yrs)~., data=train))
plot(modelNB)

# make predictions
predictionsNB <- predict(modelNB, test)

caret::confusionMatrix(test$SeriousDlqin2yrs,predictionsNB$class)

accuracy <- (24782 + 749)/( 24782 +1572+894+749)
recall <- 24782 /(24782 + 1572)
precision <- 24782 / (24782  + 894)

accuracy
recall 
precision 


# Random Forest

library(randomForest)

# Train data, Tree size = 500 
system.time(model.crf <- randomForest(as.factor(SeriousDlqin2yrs) ~ RevolvingUtilizationOfUnsecuredLines + age + 
                                        NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                                        MonthlyIncome+NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                                        NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                                        NumberOfDependents, data=train, mtry=3, importance=TRUE, ntree=500, do.trace=100))

print(model.crf)
nrow(train)
table(predict(model.crf),train$SeriousDlqin2yrs)
plot(model.crf)
importance(model.crf)
varImpPlot(model.crf)  

PredRF<-predict(model.crf,newdata=test)

table(PredRF, test$SeriousDlqin2yrs)

caret::confusionMatrix(PredRF,test$SeriousDlqin2yrs)

accuracy <- (26155 + 749)/( 26155 +1429+199+214)
recall <- 26155 /(26155 + 1429)
precision <- 26155 / (26155  + 199)

accuracy 
recall 
precision 

# Train data, Tree size = 1000
system.time(model.crf2 <- randomForest(as.factor(SeriousDlqin2yrs) ~ RevolvingUtilizationOfUnsecuredLines + age + 
                                        NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + 
                                        MonthlyIncome+NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + 
                                        NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + 
                                        NumberOfDependents, data=train, mtry=3, importance=TRUE, ntree=1000, do.trace=100))

print(model.crf2)
table(predict(model.crf2),train$SeriousDlqin2yrs)
plot(model.crf2)
importance(model.crf2)
varImpPlot(model.crf2)

PredRF2<-predict(model.crf2,newdata=test)
table(PredRF2, test$SeriousDlqin2yrs)
caret::confusionMatrix(PredRF2,test$SeriousDlqin2yrs)

accuracy <- (26155 + 749)/( 26155 +1432+199+211)
recall <- 26155 /(26155 + 1432)
precision <- 26155 / (26155  + 199)

accuracy 
recall 
precision 

# k fold cross validation
require(boot)

cv.err <- rep(0, 10)

system.time(for (i in 1:10) {
  cv.glm.fit <- glm(SeriousDlqin2yrs~poly(RevolvingUtilizationOfUnsecuredLines + 
                                            NumberOfTime30.59DaysPastDueNotWorse + 
                                            NumberOfOpenCreditLinesAndLoans +
                                            NumberOfTimes90DaysLate +
                                            NumberOfTime60.89DaysPastDueNotWorse, i), data=test)
  cv.err[i] <- cv.glm(test, cv.glm.fit, K=10)$delta[1]
})














  
