library(caret)
set.seed(1)

data=read.csv("C:\\Users\\Windows\\Desktop\\rushi\\projects\\loan-status\\train.csv")
str(data)
anyNA(data)
library(DMwR)
data<- knnImputation(data)
anyNA(data)
dt=sort(sample(nrow(data),nrow(data)*.75))
train=data[dt,]
test=data[-dt,]
dim(train)
dim(test)
#Defining the training controls for multiple models
fitcontrol=trainControl(method="cv",number=5,savePredictions = 'final',classProbs =T)
##Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome")
outcomeName<-'Loan_Status'

#Training RandomForest
mrf=train(train[,predictors],train[,outcomeName],method='rf',trcontrol=fitcontrol,tuneLength=3)
#predicting using rf
test$pred_rf=predict(object=mrf,test[,predictors])
#checking the accuracy of rf 
confusionMatrix(test$Loan_Status,test$pred_rf)
#training the KNN Model
mknn<-train(train[,predictors],train[,outcomeName],method='knn',trControl=fitcontrol,tuneLength=3)
#prediction using Knn 
test$pred_knn=predict(mknn,test[,predictors])
#Accuracy of Knn
confusionMatrix(test$Loan_Status,test$pred_knn)
#Training the logistic regression model
mlog<-train(train[,predictors],train[,outcomeName],method='glm',trControl=fitcontrol,tuneLength=3)
#predict using logistic
test$pred_lg=predict(mlog,test[,predictors])
#accuracy of logistic 
confusionMatrix(test$Loan_Status,test$pred_lg)

##predicting the Probabilities 
test$pred_rf_prob=predict(mrf,test[,predictors],type='prob')
test$pred_knn_prob=predict(mknn,test[,predictors],type='prob')
test$pred_mlog_prob=predict(mlog,test[,predictors],type='prob')
##taking avg of predictions
test$pred_avg=(test$pred_rf_prob$Y+test$pred_knn_prob$Y+test$pred_mlog_prob$Y)/3
##splitting Into binary classes at 0.5
test$pred_avg=as.factor(ifelse(test$pred_avg>0.5,'Y','N'))
##The majority Vote 
test$pred_majority=as.factor(ifelse(test$pred_rf=='Y' & test$pred_knn=='Y','Y',
ifelse(test$pred_rf=='Y' & test$pred_lg=='Y','Y',
ifelse(test$pred_knn=='Y' & test$pred_lg=='Y','Y','N'))))

#taking Weighted Avg
test$pred_weighted_avg=(test$pred_rf_prob$Y*0.25)+(test$pred_knn_prob$Y*0.25)+(test$pred_mlog_prob$Y*0.25)

#splitting into binary classes at 0.5
test$pred_weighted_avg=as.factor(ifelse(test$pred_weighted_avg>0.5,'Y','Y'))
####stacking 
##Defining the training control
fitcontrol=trainControl(method="cv",number=10,
savePredictions ='final',## To save out of fold predictions for best parameter combinantions
classProbs =T )##To save the class probabilities of the out of fold predictions

##Defining the predictors and outcome 
#training each mofel(rf,knn,glm)
##predict using each base layer model for training data and test data  

##predicting the out of fold prediction probabilities for train data

train$OOF_pred_rf<-mrf$pred$Y[order(mrf$pred$rowIndex)]
train$OOF_pred_knn<-mknn$pred$Y[order(mknn$pred$rowIndex)]
train$OOF_pred_lg<-mlog$pred$Y[order(mlog$pred$rowIndex)]


#Predicting probabilities for the test data
test$OOF_pred_rf<-predict(mrf,test[predictors],type='prob')$Y
test$OOF_pred_knn<-predict(mknn,test[predictors],type='prob')$Y
test$OOF_pred_lg<-predict(mlog,test[predictors],type='prob')$Y

#Predictors for top layer models 
predictors_top<-c('OOF_pred_knn','OOF_pred_lg') 
tes
#GBM as top layer model 
fitcontrol=trainControl(method="cv",number=5,savePredictions = 'final',classProbs =T)
mgbm<- 
  train(train[,predictors_top],train[,outcomeName],method='gbm',trControl=fitcontrol,tuneLength=3)


