install.packages("readr")
install.packages("DataExplorer")
install.packages("class")
install.packages("caret")
install.packages("mice")
install.packages("psych")
library(readr)
library(DataExplorer)
library(class)
library(caret)
library(mice)
library(psych)

#Read the DataSEt
glass <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\7 - KNN\\glass.csv")
attach(glass)
View(glass)
head(glass)

str(glass)

#EDA
glass$Type <- factor(glass$Type)
table(glass$Type)
summary(glass)

round(prop.table(table(glass$Type))*100, 1)

#Graphical Representation
plot_histogram(glass)
pairs.panels(glass)
par(mfrow=c(3, 3))
for (i in 1:8) {
  boxplot(glass[,i], main=names(glass[i]), col="blue")
}

sum(is.na(glass))

#Data Normalization
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
glass_norm <- as.data.frame(lapply(glass[1:9], norm))
View(glass_norm)
summary(glass_norm)
glass.bind <- cbind(glass_norm, Type)
View(glass.bind)
head(glass.bind)
str(glass.bind)
glass.bind$Type <- as.factor(glass.bind$Type) #Changing into Factor

#Creating Training andTesting Data Sets
set.seed(123)
split <- sample(1:nrow(glass_norm), size = nrow(glass_norm)*0.7, replace = FALSE)
train_glass <- glass_norm[split,] #70% Training Data
test_glass <- glass_norm[-split,] #30% Testing Data

train_glass.label <- glass[split,10]
test_glass.label <- glass[-split, 10]
head(train_glass.label)
  
#Model Building
NROW(train_glass.label)
sqrt(149)

glass_model <- knn(train = train_glass, test = test_glass, cl = train_glass.label, k=1 )
confusionMatrix(table(glass_model, test_glass.label))

i=1
k.optm=1
for (i in 1:28) {
  knn_model <- knn(train = train_glass, test = test_glass, cl=train_glass.label, k=i)
  k.optm[i] <- 100*sum(test_glass.label==knn_model)/NROW(test_glass.label)
  k=i
  cat(k, "=", k.optm[i],'
      ')
}
plot(k.optm, type = "b", xlab = "k-value", ylab = "Accuaracy level")

##Data Spliting into Train and Test
set.seed(123)
split_2 <- createDataPartition(glass.bind$Type, p=0.7, list = FALSE)
train_glass_2 <- glass.bind[split_2,]
test_glass_2 <- glass.bind[-split_2,]

#Model Building on Original Dataset
glass_model_2 <- knn(train = train_glass_2, test = test_glass_2, cl = train_glass_2$Type, k=21)
confusionMatrix(table(glass_model_2, test_glass_2$Type)) #Accuracy=96.72%

i=1
k.optm=1
for (i in 1:28) {
  knn_model_2 <- knn(train =train_glass_2, test = test_glass_2, cl=train_glass_2$Type, k=i)
  k.optm[i] <- 100*sum(test_glass_2$Type==knn_model_2)/NROW(test_glass_2)
  k=i
  cat(k, "=", k.optm[i],'
      ')
}
plot(k.optm, type = "b", xlab = "k-value", ylab = "Accuarcy Level")

#Model Evaluation
trclt <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
knn_fit <- train(Type~., data = train_glass_2, method="knn", trControl=trclt, tuneLength=10, preProc=c("center", "scale"))
plot(knn_fit)
knn_pred <- predict(knn_fit, newdata=test_glass_2)
knn_pred
confusionMatrix(knn_pred, test_glass_2$Type)