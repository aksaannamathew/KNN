install.packages("readr")
install.packages("caret")
install.packages("mice")
install.packages("DataExplorer")
install.packages("class")
install.packages("psych")
install.packages("corrplot")
library(readr)
library(caret)
library(mice)
library(DataExplorer)
library(class)
library(psych)
library(corrplot)

#Importing the DataSet
Zoo <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\7 - KNN\\Zoo.csv")
attach(Zoo)
View(Zoo)

sum(is.na(Zoo))

#EDA
summary(Zoo)
str(Zoo)

Zoo <- Zoo[,-1]
Zoo$type <- as.factor(Zoo$type)
View(Zoo)

#Graphical Representation
pairs.panels(Zoo)
plot_histogram(Zoo)

#Creating Training and Testing Data
table(Zoo$type)
round(prop.table(table(Zoo$type))*100, 1)

set.seed(123)
split <-sample(1:nrow(Zoo), size = nrow(Zoo)*0.7, replace = F)
Zoo.train <- Zoo[split,]
Zoo.test <- Zoo[-split,]

#KNN Model 1
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
fit <- train(type~., data = Zoo.train, method = "knn", tuneLength=20, trControl=trControl, preProc=c("center", "scale"))
fit
plot(fit)
pred <- predict(fit, newdata = Zoo.test)
confusionMatrix(pred, Zoo.test$type)

#Model Building 2
set.seed(222)
model <- knn(Zoo.train, Zoo.test, Zoo.train$type, k=5)
model
accuracy <- sum(Zoo.test$type==model)/NROW(Zoo.test) #0.8709677 accuracy
accuracy
confusionMatrix(model, Zoo.test$type)

i=1
k.optm=i
for (i in 1:28) {
  model <- knn(Zoo.train, Zoo.test, Zoo.train$type, k=i)
  k.optm[i] <- 100*sum(Zoo.test$type==model)/NROW(Zoo.test)
  k=i
  cat(k, "=", k.optm[i], "")
}
plot(k.optm, type = "b", xlab = "K-value", ylab = "Accuracy Level")
