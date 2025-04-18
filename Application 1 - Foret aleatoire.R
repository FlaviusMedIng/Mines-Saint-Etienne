ptm <- proc.time()

data=read.csv("income.csv",sep=";")
data=data[1:1000,]

#Forets aleatoires avec package caret
library(caret)
library(e1071)
# decoupage partie train et partie test
set.seed(1234)
index <- sample(1:nrow(data),round(0.70*nrow(data)))
train <- data[index,]
test <- data[-index,]
nrow(train)
nrow(test)

ranf=train(x=train[,names(train)!="incomePy"],y=train$incomePy, data=train,method="rf",importance=T)

proc.time() - ptm

# modele optimal
print(ranf$finalModel)
# variable importnce des Forets aleatoires
varImp(ranf)
plot(varImp(ranf))
#matrice de confusion partie test et erreur de classement.
prf=predict.train(ranf,newdata = test)
mrf=table(test$incomePy, prf, dnn=c("reelle", "Predicted"))
mrf
err=1-((mrf[1,1]+mrf[2,2])/sum(mrf))
err
#Courbe Roc et AUC des forets par Caret
library(pROC)
prf1=predict(ranf,test, type="prob")
RCad= roc(test$incomePy, prf1[,2])
rocAD = plot.roc(test$incomePy, prf1[,2],
                 main = " ROC de Random Forest avec Caret", 
                 type="l", col="blue", grid=c(0.1,0.2),print.auc = TRUE)# print the AUC (will contain the CI)

