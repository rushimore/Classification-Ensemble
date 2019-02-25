train=read.csv("C:\\Users\\Windows\\Desktop\\rushi\\New folder\\train_u6lujuX_CVtuZ9i.csv")
test=read.csv("C:\\Users\\Windows\\Desktop\\rushi\\New folder\\test_Y3wMUE5_7gLdaTN.csv")
train$Loan_ID=as.numeric(train$Loan_ID)
train$Gender =as.numeric(train$Gender)
train$Married=as.numeric(train$Married)
train$Dependents=as.numeric(train$Dependents)
train$Education=as.numeric(train$Education)
train$Self_Employed=as.numeric(train$Self_Employed)
train$ApplicantIncome=as.numeric(train$ApplicantIncome)
train$CoapplicantIncome=as.numeric(train$CoapplicantIncome)
train$LoanAmount=as.numeric(train$LoanAmount)
train$Loan_Amount_Term=as.numeric(train$Loan_Amount_Term)
train$Credit_History=as.numeric(train$Credit_History)
train$Property_Area=as.numeric(train$Property_Area)
Loan_Status=as.factor(train$Loan_Status)




##If you need NA count Column wise -
sapply(train, function(x) sum(is.na (x)))
colSums(is.na(train))
###If you need NA count Row wise - 
rowSums(is.na (train))
###imputation by MICS
install.packages("mice")
library(mice)
md.pattern(train)## mising values by row wise and coloumwise (exact number)
###visualization of missing values 
install.packages("VIM")
library(VIM)
mice_plot <- aggr(train, col=c('navyblue','Red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

### impute missing value

imputed_Data <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
#check imputed values
imputed_Data$imp$Loan_ID
imputed_Data$imp$Gender
imputed_Data$imp$Married
imputed_Data$imp$Dependents
imputed_Data$imp$Education
imputed_Data$imp$Self_Employed
imputed_Data$imp$ApplicantIncome
imputed_Data$imp$CoapplicantIncome
imputed_Data$imp$LoanAmount
imputed_Data$imp$Loan_Amount_Term
imputed_Data$imp$Credit_History
imputed_Data$imp$Property_Area
imputed_Data$imp$Loan_Status
#get complete data
completeData <- complete(imputed_Data,2)
###Model
fit=glm(Loan_Status~.,data=completeData,family=binomial)
summary(fit)

###confusition Matrix
par=predict(fit,type = 'response')
a=table(train$Loan_Status,par>0.5)
print(a)

###Roc curve
library(ROCR)
ROCRpred <- prediction(par, train$Loan_Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
 plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
 par=predict(fit,type = 'response')

#### imputting  for test data
 
test$Loan_ID=as.numeric(test$Loan_ID)
 test$Gender =as.numeric(test$Gender)
 test$Married=as.numeric(test$Married)
 test$Dependents=as.numeric(test$Dependents)
 test$Education=as.numeric(test$Education)
 test$Self_Employed=as.numeric(test$Self_Employed)
 test$ApplicantIncome=as.numeric(test$ApplicantIncome)
 test$CoapplicantIncome=as.numeric(test$CoapplicantIncome)
 test$LoanAmount=as.numeric(test$LoanAmount)
 test$Loan_Amount_Term=as.numeric(test$Loan_Amount_Term)
 test$Credit_History=as.numeric(test$Credit_History)
 test$Property_Area=as.numeric(test$Property_Area)
###
 
 md.pattern(test)
 mice_plot <- aggr(test, col=c('navyblue','Red'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(train), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))
 imputed_Data <- mice(test, m=5, maxit = 50, method = 'pmm', seed = 500)
 summary(imputed_Data)
 #check imputed values
 imputed_Data$imp$Loan_ID
 imputed_Data$imp$Gender
 imputed_Data$imp$Married
 imputed_Data$imp$Dependents
 imputed_Data$imp$Education
 imputed_Data$imp$Self_Employed
 imputed_Data$imp$ApplicantIncome
 imputed_Data$imp$CoapplicantIncome
 imputed_Data$imp$LoanAmount
 imputed_Data$imp$Loan_Amount_Term
 imputed_Data$imp$Credit_History
 imputed_Data$imp$Property_Area
 imputed_Data$imp$Loan_Status
 
 

 test1 <- complete(imputed_Data,2) 
 ###predicting the test data 
 pre_test=predict(fit,newdata=test1)

 