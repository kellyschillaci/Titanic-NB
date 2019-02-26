library(readr)
trainset <- read.csv("titanic-train.csv")	
trainset$Survived=factor(trainset$Survived)	
trainset$Pclass=ordered(trainset$Pclass)	

testset <- read.csv("titanic-test.csv")	
testset$Survived=factor(testset$Survived)	
testset$Pclass=ordered(testset$Pclass)	

myVars=c("Pclass", "Sex", "Age", "SibSp", "Fare", "Survived")	
newtrain=trainset[myVars]	
newtest=testset[myVars]	

Partition <- createDataPartition(y = newtrain$Survived,p = 0.7, list = FALSE)
Training = newtrain[Partition,] # Create the training sample
dim(Training)
Test = newtrain[-Partition,] # Create the test sample
dim(Test)

library(e1071)	

nb=naiveBayes(Survived~., data = Training, laplace = 1, na.action = na.pass)

pred=predict(nb, newdata=Test, type=c("class"))	
pred2=predict(nb, newdata=newtest, type=c("class"))
myids=c("PassengerId")	
id_col=testset[myids]	
newpred=cbind(id_col, pred)	

colnames(newpred)=c("Passengerid", "Survived")

write.csv(newpred, file="titanic-NB-pred.csv", row.names=FALSE)

table(pred, Test$Survived,dnn=c("Prediction","Actual"))
