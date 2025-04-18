library(ISLR)
data(Hitters)
Hitters=Hitters[,c(7,2,19)]
cart_fit <- rpart::rpart(Salary ~ Years + Hits, data = Hitters,minsplit=6,minbucket=2)
min_ind <- which.min(cart_fit$cptable[, "xerror"])
min_cp <- cart_fit$cptable[min_ind, "CP"]
pruned_fit <- rpart::prune(cart_fit, cp = min_cp)
rpart.plot::rpart.plot(cart_fit)
rpart.plot::rpart.plot(pruned_fit)

#################################################### Max Tree performance
salary_predicted = predict(cart_fit)
SSres = sum((salary_predicted-na.omit(Hitters$Salary))^2)
SStot = sum((na.omit(Hitters$Salary)-mean(na.omit(Hitters$Salary)))^2)
r_sq_max = 1 - (SSres/SStot)

#################################################### Pruned Tree performance
salary_predicted = predict(pruned_fit)
SSres = sum((salary_predicted-na.omit(Hitters$Salary))^2)
SStot = sum((na.omit(Hitters$Salary)-mean(na.omit(Hitters$Salary)))^2)
r_sq_pruned = 1 - (SSres/SStot)

################################################### Linear Model Performance
linear_model=lm(Salary ~ Years + Hits, data = Hitters)
summary(linear_model)


