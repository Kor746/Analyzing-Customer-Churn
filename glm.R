library(Amelia)
library(pROC)
library(reshape2)
library(ROCR)
library(caret)
library(rsample)
library(ggplot2)
library(ggcorrplot)

#Read CSV into R
churn_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/WA_Fn-UseC_-Telco-Customer-Churn.csv", header=TRUE, sep=",")
#Delete rows that have empty values
toDel <- which(! complete.cases(churn_data))
churn_data <- churn_data[-toDel,]


#Drop first column
churn_data$customerID <- NULL

#Data preprocessing step
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)

#Correlation analysis
# Must be numeric
df_cor <- subset(churn_data, select=c("SeniorCitizen","tenure","MonthlyCharges","TotalCharges"))
x_cor <- cor(df_cor, use="everything")
lapply(churn_data,length)

#Check data
is.na(churn_data)
head(churn_data)
summary(churn_data)
lapply(churn_data,class)
missmap(churn_data,main="Missing values vs observed",legend=FALSE,x.cex=0.6)

#Train model
fit <- glm(churn_data$Churn~.,data=churn_data,family=binomial((link="logit")))

#Check model
summary(fit)

prediction=predict(fit,type=c("response"))
#churn_data$prediction=prediction
roc_obj <- roc(churn_data$Churn, prediction)
auc(roc_obj)

#ROC figure
plot.roc(roc_obj,xlab="False Positive Rate",ylab="True Positive Rate")

#ANOVA - variance analysis
churn_anova <- anova(fit, test='Chisq')
sorted_non_sig_features <- order(var$Deviance)

#Create copy of original churn dataframe
churn_data2 <- churn_data

#Bootstrap sampling
set.seed(13)
resample <- bootstraps(churn_data2,time=25,strata=NULL,apparent=FALSE)
resample$splits

#Sampling train, test
x <- initial_split(churn_data,prop=3/4,strata=NULL)
train_x <- training(x)
test_x <- testing(x)
fit2 <- glm(train_x$Churn ~ .,data=train_x,family=binomial((link="logit")))
pdata <- predict(fit2, newdata = test_x, type="response")

prediction=predict(fit2,type=c("response"))
#churn_data$prediction=prediction
roc_obj <- roc(test_x, prediction)
auc(roc_obj)
roc_obj <- roc(test_x, pdata)
auc(roc_obj)
#Create confusion matrix
df_cm <- confusionMatrix(data = as.factor(as.numeric(pdata>0.5) + 1), reference = as.factor(as.numeric(test_x$Churn)))

#Compute precision, recall, f-measure
tp <- df_cm$table[1,1]
fp <- df_cm$table[1,2]
fn <- df_cm$table[2,1]
precision <- tp / (tp + fp)
recall <- tp/(tp+fn)
f_measure <- 2*((precision*recall)/(precision+recall))

a <- churn_data$TotalCharges
b <- churn_data$Contract
plot(a~b , ylim = rev(range(a)) ,lwd=4 , type="l" , bty="n", xlab="Contracts", ylab="Total Charges" , col=rgb(0.2,0.4,0.6,0.8) )
abline(v=seq(0,900,100) , col="grey" , lwd=0.6)

lines(churn_data2$tenure,churn_data2$SeniorCitizen)

precision
recall
f_measure
#############################
# Week 7 Data Mining - GLM

#Read CSV into R
churn_data <- read.csv(file="/Users/admin/Documents/Queens_Masters_Courses/CISC873/Assignment_GLM/WA_Fn-UseC_-Telco-Customer-Churn.csv", header=TRUE, sep=",")
#Delete rows that have empty values
toDel <- which(! complete.cases(churn_data))
churn_data <- churn_data[-toDel,]

#We use these features
#We remove the features with low independence in ANOVA
churn_data$SeniorCitizen <- as.factor(churn_data$SeniorCitizen)
churn_data$customerID <- NULL
churn_data$TotalCharges <- NULL
churn_data$gender <- NULL
churn_data$PhoneService <- NULL
churn_data$OnlineBackup <- NULL
churn_data$DeviceProtection <- NULL
churn_data$MonthlyCharges <- NULL

colnames(churn_data[1:13])


#Drop first column
churn_data$customerID <- NULL

#Make factors into numeric 
churn_data$gender <- as.numeric(churn_data$gender)
churn_data$SeniorCitizen <- as.numeric(churn_data$SeniorCitizen) + 1
churn_data$Partner <- as.numeric(churn_data$Partner)
churn_data$tenure <- as.numeric(churn_data$tenure)
churn_data$Dependents <- as.numeric(churn_data$Dependents)
churn_data$PhoneService <- as.numeric(churn_data$PhoneService)
churn_data$PaperlessBilling <- as.numeric(churn_data$PaperlessBilling)
churn_data$MultipleLines <- as.numeric(churn_data$MultipleLines)
churn_data$InternetService <- as.numeric(churn_data$InternetService)
churn_data$OnlineSecurity<- as.numeric(churn_data$OnlineSecurity)
churn_data$TechSupport <- as.numeric(churn_data$TechSupport)
churn_data$StreamingTV<- as.numeric(churn_data$StreamingTV)
churn_data$StreamingMovies <- as.numeric(churn_data$StreamingMovies)
churn_data$Contract <- as.numeric(churn_data$Contract)
churn_data$PaperlessBilling <- as.numeric(churn_data$PaperlessBilling)
churn_data$PaymentMethod <- as.numeric(churn_data$PaymentMethod)
churn_data$OnlineBackup <- as.numeric(churn_data$OnlineBackup)
churn_data$DeviceProtection <- as.numeric(churn_data$DeviceProtection)
#churn_data$InternetService <- as.numeric(churn_data$InternetService)


churn_data$OnlineSecurity<- as.numeric(churn_data$OnlineSecurity)


churn_data$customerID <- NULL
churn_data$MultipleLines <- NULL
churn_data$InternetService <- NULL
churn_data$OnlineSecurity <- NULL
churn_data$OnlineBackup <- NULL
churn_data$DeviceProtection <- NULL
churn_data$TechSupport <- NULL
churn_data$StreamingTV <- NULL
churn_data$StreamingMovies <- NULL
churn_data$Contract <- NULL
churn_data$PaymentMethod <- NULL
churn_data$Churn <- NULL


#Chi-square test of independence
chisq.test(churn_data)

#Correlation analysis
corr <- cor(churn_data[1:19])
ggcorrplot(corr)

#Checking class of all vector features
for(i in churn_data) {
  print(class(i))
}

# Check correlation between tenu and total charges
plot(churn_data$tenure~churn_data$TotalCharges)
cor(churn_data$tenure,churn_data$TotalCharges)
#churn_data$PhoneService <- as.numeric(churn_data$PhoneService)

#Train model
fit <- glm(churn_data$Churn~.,data=churn_data,family=binomial((link="logit")))
summary(fit)


#Bagging with GLM
#random sample of y to be our "test" set, rest is training
set.seed(10)
positions <- sample(nrow(churn_data),size=floor((nrow(churn_data)/4)*3))
xytraining <- churn_data[positions,]
#training$Churn <- NULL
xytesting <- churn_data[-positions,]

xTrain <- xytraining[,-5]
yTrain <- xytraining[,5]
#testing$Churn <- NULL
y <- churn_data$Churn
library(randomGLM)
RGLM <- randomGLM(xTrain,yTrain, nCandidateCovariates = ncol(training), classify= TRUE,nBags=100,keepModels=TRUE,replace=TRUE,nThreads=4)
prediction <- predict(RGLM, newdata=)

#y <- churn_data$Churn
#hurn_data$Churn <- NULL
glm_fit <- glm(training$Churn~.,data=training)
prediction <- predict(glm_fit,newdata=testing)
error <- sqrt((sum((testing$y-predictions^2))/nrow(testing)))


  
library(foreach)
length_divisor <- 4
iterations <- 1000
predictions <- foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos <- 1:nrow(training) %in% training_positions
  glm_fit <- glm(training$Churn~.,data=training[train_pos,],family=binomial((link="logit")))
  predict(glm_fit,newdata=testing)
}

predictions <- rowMeans(predictions)
error <- sqrt((sum((testing$y-predictions)^2))/nrow(testing))
#Train model
fit <- glm(churn_data$Churn~.,data=churn_data,family=binomial((link="logit")))

test_bob <- c()
#Test client Bob
prediction <- predict(fit,test_bob)


# Lol
churn_data2 <- churn_data
set.seed(10)
indx = sample(nrow(churn_data2), 5626, replace=TRUE)
xyTrain = churn_data2[indx,]
xyTest = churn_data2[-indx,]
xTrain = xyTrain[1:19]
yTrain = xyTrain$Churn

xTest = xyTest[1:19]
yTest = xyTest$Churn

RGLM = randomGLM(xTrain, yTrain, nCandidateCovariates=ncol(xTrain), nBags=500,keepModels=TRUE, nThreads=4)
predicted <- predict(RGLM, newdata = xTest, type = "class")
lol_matrix <- table(predicted,yTest)

#Compute precision, recall, f-measure
tp <- lol_matrix[1,1]
fp <- lol_matrix[1,2]
fn <- lol_matrix[2,1]
precision <- tp / (tp + fp)
recall <- tp/(tp+fn)
f_measure <- 2*((precision*recall)/(precision+recall))

prediction=predict(fit,type=c("response"))
roc_obj <- roc(as.numeric(yTest), as.numeric(predicted))
df_cm <- confusionMatrix(data = predicted, 
                         reference = yTest)
auc(roc_obj)
churn_anova <- anova(RGLM, test='Chisq')

lol1 <- churn_data$InternetService

boxplot(split(churn_data$TechSupport,churn_data$Contract)
)

#regularization

pred_L2_reg=data.table(predict(fit,as.matrix(housingData[-indexTrain,-c('price'),with=F])))
RMSE_L2=sqrt(apply(pred_L2_reg[,(.SD-housingData[-indexTrain]$price)^2,.SD=1:ncol(pred_L2_reg)],2,mean))
DF_plot=data.frame(lambda=fit$lambda,rmse=RMSE_L2)
plotCoeffEvolution(fit,'L2')
require(ggplot2)
ggplot(DF_plot,aes(x=lambda,y=rmse))+geom_line()+ggtitle("Evolution of test error vs lambda value")+scale_x_log10()


a <- churn_data$InternetService
b <- churn_data$Contract
plot(a~b , ylim = rev(range(a)) ,lwd=4 , type="l" , bty="n", xlab="Contracts", ylab="Internet Service" , col=rgb(0.2,0.4,0.6,0.8) )
abline(v=seq(0,900,100) , col="grey" , lwd=0.6)
