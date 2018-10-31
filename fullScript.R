install.packages("tree")
install.packages("randomForest")

library("tree")
library(randomForest)

##Functions
#Make function to calculate MSE
MSE = function(estPred,test){
  squaredError = rep(NA,length(estPred))
  for (i in 1:length(estPred)) {
    squaredError[i] = (estPred[i]-test[i,1])^2
  }
  return(mean(squaredError))
}


#Import Training Set
trainingSet = read.csv("C:/Users/James Moore/Documents/Random_Forrest/dataFile/smogData_train.csv")
trainingSet$X = NULL

#format data into training and test set
train = trainingSet[-seq(from = 5,to=255,by=5),]
test = trainingSet[seq(from = 5, to= 255,by = 5),]

#run tree with all predictors
full_tree = tree(O3~.,data = train)
plot(full_tree)
text(full_tree)
summary(full_tree)

#Evaluate Performance
performanceResult = predict(full_tree,test)
MSE(performanceResult,test)

#run a pruned tree
tree_control = tree(O3~.,data = train, control = tree.control(nobs = 204,mincut = 2,minsize = 12,mindev = 0.001))

#visualize results
plot(tree_control)
text(tree_control)
summary(tree_control)

#Evaluate Performance
performanceResult = predict(tree_control,test)
MSE(performanceResult,test)

##Move to Random Forest
set.seed(1234)
#Default Random Forest
forest = randomForest(O3~.,data = train, ntree = 1000, importance = TRUE)

#Evaluate Performance
performanceResult = predict(forest,test)
MSE(performanceResult,test)

#visualize
plot(forest,main = "MSE on Number of Trees in Forest")
abline(v = 200, col="red", lwd=3, lty=2)
varImpPlot(forest)

set.seed(1234)
#Improved random forest
improve_rf = randomForest(O3~.,data = train, ntree = 925, importance=TRUE,
                          mtry=2,
                          nodesize=3,
                          maxnodes = 100,
                          nPerm = 3,
                          corr.bias = TRUE)

#Evaluate Performance
performanceResult = predict(improve_rf,test)
MSE(performanceResult,test)
varImpPlot(improve_rf)

###Make Final Predictions
testSet = read.csv("C:/Users/James Moore/Documents/Random_Forrest/dataFile/smogDataPredict.csv")
testSet$X = NULL

finalPredictions = predict(improve_rf,testSet)
#print final predictions
finalPredictions
#write predictions to file
write.table(finalPredictions, "predicted_O3_Values.txt",row.names = F)

