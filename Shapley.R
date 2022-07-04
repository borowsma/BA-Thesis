getwd()
setwd("/Users/10002292ergo.com/Desktop/мои/uni/dissertation")

# uploading libraries

library(rpart)
library(ranger)
library(iml)
library(dplyr)
library(caret)

# data set upload
credit <- read.csv(file='Taiwan.csv', header = TRUE, sep = ';', as.is = 3)

# changing the type of variables to factors with levels
names <- c(1, 2, 10, 24)
credit[,names] <- lapply(credit[,names], factor)
colnames(credit)[which(names(credit) == "PAY_0")] <- "PAY_1"

# quick outlook to data
head(credit)
str(credit)



# training the model
credit.x <- credit[names(credit) != 'default']


model <- caret::train(credit.x,
                      credit$default,
                     method = 'rf', ntree=30, maximise = FALSE)


predictor = Predictor$new(model, class = "1", data = credit.x, type = "prob")
avg.prediction = mean(predict(model, type = 'prob')[,'1'])


# Person no 1 with default = 1
actual.prediction = predict(model, newdata = credit.x[1,], type = 'prob')['1']
diff.prediction = actual.prediction - avg.prediction

shapley2 = Shapley$new(predictor, x.interest = credit.x[1,], sample.size = 100)
plot(shapley2) +
  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Person No 1 with default= %.2s\nActual prediction: %.2f\nAverage prediction: %.2f\nDifference: %.2f", '1', actual.prediction, avg.prediction, diff.prediction)) +
  scale_x_discrete("")


# Person no 2 with default = 1
actual.prediction = predict(model, newdata = credit.x[2,], type = 'prob')['1']
diff.prediction = actual.prediction - avg.prediction

shapley2 = Shapley$new(predictor, x.interest = credit.x[2,], sample.size = 100)
plot(shapley2) +
  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Person No 2 with default= %.2s\nActual prediction: %.2f\nAverage prediction: %.2f\nDifference: %.2f", '1', actual.prediction, avg.prediction, diff.prediction)) +
  scale_x_discrete("")



#Person no 3 with default = 0
actual.prediction = predict(model, newdata = credit.x[3,], type = 'prob')['1']
diff.prediction = actual.prediction - avg.prediction

shapley2 = Shapley$new(predictor, x.interest = credit.x[3,], sample.size = 100)
plot(shapley2) +
  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Person No 3 with default= %.2s\nActual prediction: %.2f\nAverage prediction: %.2f\nDifference: %.2f", '0', actual.prediction, avg.prediction, diff.prediction)) +
  scale_x_discrete("")


#Person no 4 with default = 0
actual.prediction = predict(model, newdata = credit.x[4,], type = 'prob')['1']
diff.prediction = actual.prediction - avg.prediction

shapley2 = Shapley$new(predictor, x.interest = credit.x[4,], sample.size = 100)
plot(shapley2) +
  scale_y_continuous("Feature value contribution") +
  ggtitle(sprintf("Person No 4 with default= %.2s\nActual prediction: %.2f\nAverage prediction: %.2f\nDifference: %.2f", '0', actual.prediction, avg.prediction, diff.prediction)) +
  scale_x_discrete("")







