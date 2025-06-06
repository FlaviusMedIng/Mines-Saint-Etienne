# Chargement de la librairie
library(randomForest)
library(plotly)

# Chargement des donn�es 
data(iris)

# description des donn�es 
summary(iris)

# Algorithme Random Forest  
iris_RandomForest <- randomForest(Species~.,data=iris, ntree = 100, 
                                  mtry = 2,na.action = na.roughfix)

# R�sultats de l'algoritme
print(iris_RandomForest)

#Variables d'importance 
iris_RandomForest$importance[order(iris_RandomForest$importance[, 1], 
                                   decreasing = TRUE), ]

# Impact de Petal.Length et de Petal.Width sur Species
plot_ly(data = iris, x = ~Species, y = ~Petal.Length, type ='bar')
plot_ly(data = iris, x = ~Species, y = ~Sepal.Width,type ='bar')
