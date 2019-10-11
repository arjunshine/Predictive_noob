german_credit<-read.csv("C:/Users/Arjun/Documents/Trimester 4/PREDICTIVE ANALYTICS/german_credit.csv")

library(randomForest)
library(Metrics)
library(rpart)
library(rpart.plot) 
library(dplyr)
library(caret)
library(e1071)
library(ROCit)

#Partition the data
names(german_credit)
str(german_credit)

set.seed(123)
german_mixed<-german_credit[order(runif(1000)),]

#Variable Conversion [into factor and numeric]
german_mixed$Creditability<-as.factor(german_mixed$Creditability)
german_mixed$Account.Balance<-as.factor(german_mixed$Account.Balance)
german_mixed$Payment.Status.of.Previous.Credit<-as.factor(german_mixed$Payment.Status.of.Previous.Credit)
german_mixed$Purpose<-as.factor(german_mixed$Purpose)
german_mixed$Value.Savings.Stocks<-as.factor(german_mixed$Value.Savings.Stocks)
german_mixed$Length.of.current.employment<-as.factor(german_mixed$Length.of.current.employment)
german_mixed$Instalment.per.cent<-as.factor(german_mixed$Instalment.per.cent)
german_mixed$Guarantors<-as.factor(german_mixed$Guarantors)
german_mixed$Duration.in.Current.address<-as.factor(german_mixed$Duration.in.Current.address)
german_mixed$Most.valuable.available.asset<-as.factor(german_mixed$Most.valuable.available.asset)
german_mixed$Concurrent.Credits<-as.factor(german_mixed$Concurrent.Credits)
german_mixed$Type.of.apartment<-as.factor(german_mixed$Type.of.apartment)
german_mixed$No.of.Credits.at.this.Bank<-as.factor(german_mixed$No.of.Credits.at.this.Bank)
german_mixed$Occupation<-as.factor(german_mixed$Occupation)
german_mixed$No.of.dependents<-as.factor(german_mixed$No.of.dependents)
german_mixed$Telephone<-as.factor(german_mixed$Telephone)
german_mixed$Foreign.Worker<-as.factor(german_mixed$Foreign.Worker)
str(german_mixed)

#splitting the data
german_training<-german_mixed[1:700,]
german_testing<-german_mixed[701:1000,]
str(german_training)
str(german_testing)
names(german_testing)

##Model set up
control_improved_german <- trainControl(method="repeatedcv", number=10, repeats=10)
grid_rf_german <- expand.grid(.mtry = c(2, 4, 8, 16))
set.seed(200)
m_rf_german <- train(Creditability ~ ., data =german_training, method = "rf",metric = "Kappa", trControl = control_improved_german,tuneGrid = grid_rf_german)
m_rf_german

set.seed(400)
finamodel_rf_smarket<-randomForest(Creditability~.,data=german_training,mtry=2,importance=TRUE,ntree=500)
finamodel_rf_smarket

#Validate on test data
german_prediction<-predict(finamodel_rf_smarket,newdata=german_testing,type="prob")
head(german_prediction)

# saving the file # the path should be mentioned to the file to which the code is to be appended 
write.csv(german_prediction,"C:/Users/Arjun/Documents/PREDICTIVE ANALYTICS/german_credit_finalout1.csv")
german_probability<-read.csv("C:/Users/Arjun/Documents/PREDICTIVE ANALYTICS/german_credit_finalout.csv")
german_probability

