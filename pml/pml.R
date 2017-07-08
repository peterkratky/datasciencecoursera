setwd("C:/Users/short/Documents/pml")

data = read.csv("pml-training.csv")
data = subset(data, new_window == "no")

cols = c()
for (i in 8:(ncol(data)-1)) {
  if (!is.na(mean(data[,i])))
    cols = c(cols, i)
}

data = data[, c(cols,ncol(data))]

correlationMatrix <- cor(data[,8:(ncol(data)-1)], use = "everything")
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
data = data[, -highlyCorrelated]


train = createDataPartition(y=data$classe, p=0.7, list=FALSE)

training = data[train,]
testing = data[-train,]


control <- trainControl(method="repeatedcv", number=10, repeats=1)
model <- train(classe~., data=training, method="rf", preProcess=c("BoxCox"), trControl=control)

tunegrid <- expand.grid(.mtry=c(1:15))
model_search <- train(classe~., data=training, method="rf", tuneGrid=tunegrid, preProcess=c("BoxCox"), trControl=control)
print(model_search)

pr = predict(model, testing)
m = confusionMatrix(pr, testing$classe)


test = read.csv("pml-testing.csv")
test = test[, c(cols)]
test = test[, -highlyCorrelated]
prall = predict(model, test)





library(ggplot2)
library(reshape2)
dataForPlot <- melt(training[,1:8], value.name="variable")

ggplot(dataForPlot, aes(value)) + 
  geom_bar() + 
  facet_wrap(~ variable, scales="free", ncol=4)

