library(ISLR)
library(MASS)
library(class)

rm(list = ls())

bos<-na.omit(Boston)

names(bos)

bos$crim


med<-median(bos$crim)

output_classifier=function(input_v){
  if(input_v<med){
    return(0)
  }
  
  else{
    return(1)
  }
}

bos$crim_med<-sapply(bos$crim,output_classifier)

train <- sample(1:nrow(bos), .66*nrow(bos))
bos_train <- bos[train,]
bos_test <- bos[-train,]

crim_med.test<-bos_test$crim_med

crim_med.test


#########################################
# Logistic Regression
#########################################

fit.glm <- glm(crim_med ~ . - crim_med - crim, data = bos_train, family = binomial)

pred.test <- predict(fit.glm, bos_test, type = "response")

pred.test<-round(pred.test)

table(pred.test, crim_med.test)

test_err.lr <- sum(abs(pred.test- crim_med.test))/length(crim_med.test)

test_err.lr


fit.glm1 <- glm(crim_med ~ . - crim_med - crim- chas - nox, data = bos_train, family = binomial)

pred.test1 <- predict(fit.glm1, bos_test, type = "response")

pred.test1<-round(pred.test1)

table(pred.test1, crim_med.test)

test_err.lr1 <- sum(abs(pred.test1- crim_med.test))/length(crim_med.test)

test_err.lr1

#########################################
# LDA
#########################################

fit.lda1 <- lda(crim_med ~ . - crim_med - crim , data = bos_train)


pred.lda1 <- predict(fit.lda1, bos_test)

table(pred.lda1$class, crim_med.test)

test_err_lda1<-mean(pred.lda1$class != crim_med.test)

test_err_lda1


fit.lda <- lda(crim_med ~ . - crim_med - crim - chas - nox, data = bos_train)


pred.lda <- predict(fit.lda, bos_test)

table(pred.lda$class, crim_med.test)

test_err_lda<-mean(pred.lda$class != crim_med.test)

test_err_lda

#########################################
# KNN
#########################################

train.X <-bos_train[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)]

test.X <- bos_test[,c(2,3,4,5,6,7,8,9,10,11,12,13,14)]

train.Y<-bos_train[,c(15)]

test.Y<-bos_test[,c(15)]

pred.knn <- knn(train.X, test.X, train.Y, k = 1)

table(pred.knn, crim_med.test)

mean(pred.knn != crim_med.test)

pred.knn10 <- knn(train.X, test.X, train.Y, k = 10)

table(pred.knn10, crim_med.test)

mean(pred.knn10 != crim_med.test)

summary(fit.glm)


