##Source: http://machinelearningmastery.com/non-linear-classification-in-r/

##############################################################################

library(mda)
data(iris)
# fit model
fit <- mda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)


library(MASS)
data(iris)
# fit model
fit <- qda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)


library(klaR)
data(iris)
# fit model
fit <- rda(Species~., data=iris, gamma=0.05, lambda=0.01)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])$class
# summarize accuracy
table(predictions, iris$Species)

library(nnet)
data(iris)
# fit model
fit <- nnet(Species~., data=iris, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)


library(mda)
data(iris)
# fit model
fit <- fda(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)



library(kernlab)
data(iris)
# fit model
fit <- ksvm(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="response")
# summarize accuracy
table(predictions, iris$Species)


library(caret)
data(iris)
# fit model
fit <- knn3(Species~., data=iris, k=5)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)


library(e1071)
data(iris)
# fit model
fit <- naiveBayes(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

##############################################################################


library(nnet)
data(iris)
# fit model
fit <- nnet(newozGPP~Tair, data=newDF2, size=4, decay=0.0001, maxit=500)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, data = newDF2)
# summarize accuracy

#plot(newDF2$newozGPP, predictions)
plot(newozGPP ~ Tair, data = newDF2, type = "p", ylab = y.lab, xlab = x.lab,
     ylim = c(v.low,v.high), col = "grey")

points(newDF2$Tair, predictions, col = "blue")
tmpDF <- as.data.frame(cbind(newDF2$Tair, predictions))
colnames(tmpDF) <- c("Tair", "predictions")
topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)


