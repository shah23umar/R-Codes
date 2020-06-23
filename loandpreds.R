######Set Working Directory & Import train & Test Data########
setwd('F:\\Training\\DSP20\\Loan Prediction')
train <- read.csv('train_ctrUa4K.csv', na.strings = c(""))
test <- read.csv('test_lAUu6dG.csv', na.strings = c(""))

###########Audit The Data##############

str(train)

#################Data Preparation############
train$Credit_History <- as.factor(train$Credit_History)

train$Dependents <- as.character(train$Dependents)
train$Dependents <- ifelse(train$Dependents=='3+', '3', train$Dependents)
train$Dependents <- as.factor(train$Dependents)
table(train$Dependents)

####################Data Imputation using Amelia##########

#install.packages("Amelia")
library(Amelia)

newimpute <- amelia(train, m=5, 
                    idvars = c("Loan_ID","Education","ApplicantIncome",
                               "CoapplicantIncome","Property_Area","Loan_Status"), 
                    noms = c("Gender","Married","Self_Employed","Credit_History"),
                    ords = c("Dependents"))

write.amelia(newimpute, file.stem = "imputed_data_set")

train_1 <- read.csv('imputed_data_set1.csv', na.strings = c(""))
sum(is.na(train_1))

str(train_1)

train_1$Credit_History <- as.factor(train_1$Credit_History)
train_1$Dependents <- as.factor(train_1$Dependents)

summary(train_1)
###################Univariate & Bi-Variate Analysis############
library(psych)
describe(train_1)

train_1$Log_ApplicantIncome <- log(train_1$ApplicantIncome)
library(e1071)
skewness(train_1$Log_ApplicantIncome)

train_1$Log_CoapplicantIncome <- log(train_1$CoapplicantIncome)
skewness(train_1$Log_CoapplicantIncome)
train_1$Log_CoapplicantIncome <- ifelse(train_1$Log_CoapplicantIncome=='-Inf',0,train_1$Log_CoapplicantIncome)
train_1$Log_LoanAmount <- log(train_1$LoanAmount)
skewness(train_1$Log_LoanAmount)

train_1$Log_Loan_Amount_Term <- log(train_1$Loan_Amount_Term)
skewness(train_1$Log_Loan_Amount_Term)
train_1$Log_Loan_Amount_Term <- NULL
###########################################Logistic Regression######################
names(train_1)
model_lr <- glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+
                  Log_ApplicantIncome+Log_CoapplicantIncome+Log_LoanAmount+
                  Loan_Amount_Term+Credit_History+Property_Area,
                family = 'binomial', data = train_1)
summary(model_lr)
train_1$PredsProbs <- model_lr$fitted.values
train_1$Preds_Loan_Status <- ifelse(train_1$PredsProbs>0.50, 'Y', 'N')
table(train_1$Loan_Status, train_1$Preds_Loan_Status)


train_1$Log_LoanAmount <- ifelse(train_1$LoanAmount==0,0,log(train_1$LoanAmount))


