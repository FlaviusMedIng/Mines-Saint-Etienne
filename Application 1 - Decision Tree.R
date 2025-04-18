library(rpart)
library(caret)

data=stagec

# decoupage partie train et partie test
set.seed(1234)
index <- sample(1:nrow(data),round(0.70*nrow(data)))
train <- data[index,]
test <- data[-index,]
nrow(train)
nrow(test)

### Construction de l'arbre
#fulltree<-rpart(pgstat~age+eet+grade+gleason+ploidy, data=data, method="class")
fulltree<-rpart(pgstat~age+eet+grade+gleason+ploidy, data=train, method="class")
rpart.plot::rpart.plot(fulltree)
#summary(fulltree)
printcp(fulltree)
varImp(fulltree)


####### prediction.
predicted <- predict(fulltree, test, type="class")
error1=sum(test$pgstat != predicted)/length(predicted)
predicted <- ordered(predicted, levels = c(1, 0))
actual<- ordered(test$pgstat, levels = c(1, 0))
mc=table(predicted,actual, dnn=c( "Predicted","reelle")) #matrice de confusion
error2=1-((mc[1,1]+mc[2,2])/sum(mc))
error2

sensitivity(mc)
specificity(mc)
accuracy_Test <- sum(diag(mc)) / sum(mc) ## ou 1-error


####Predictions as probability
Predprob <- predict(fulltree, newdata = test,type = "prob")
Predprob = as.data.frame(Predprob)
# taking the cut-off probability 50%
pred.DT <- ifelse(Predprob$"1" > 0.5, 1, 0)
# saving predicted vector as factor 
Pred <- as.factor(pred.DT)
# ordering the vectors
Pred <- ordered(Pred, levels = c(1, 0))
Actual <- ordered(test$pgstat,levels = c(1, 0))
# making confusion matrix
cm1 <-confusionMatrix(table(Pred,Actual))
cm1

# loading the package for ROC curve and AUC
library(ROCR)
Prediction <- prediction(Predprob[2],test$pgstat)
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve
plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

# area under curve
aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT

###########Courbe LIFT
library(rpart)
library(ROCR)
score<-predict(fulltree, type="prob", train)
pred<-prediction(score[,2],train$pgstat)
perf<-performance(pred,'tpr','rpp')
plot(perf,col="blue")
perf<-performance(pred,"lift","rpp")
plot(perf,col="red")

