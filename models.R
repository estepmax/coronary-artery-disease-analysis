library(InformationValue)
library(PCAmixdata)
library(FactoMineR)
library(regclass)
library(e1071)
library(MASS)
library(glmnet)
library(boot)
library(ggplot2)

#quantitative/qualitative features
features <- c(1:13)

#PCAmixdata Procedure
trainsplit <- splitmix(train[,features])
X1 <- trainsplit$X.quanti
X2 <- trainsplit$X.quali
train.pcamix <- PCAmix(X.quanti=X1,X.quali=X2,rename.level=T,graph=T)
train.pcarot <- PCArot(train.pcamix,dim=2,graph=T)

testsplit <-splitmix(test[,features])
X1test <- testsplit$X.quanti
X2test <- testsplit$X.quali
test.pcamix <- predict(train.pcamix,X.quanti=X1test,X.quali=X2test)
test.pcarot <- predict(train.pcarot,X.quanti=X1test,X.quali=X2test)

#factor scores
factors <- c('f_1','f_2','f_3','f_4','f_5')
factors_rot <- c('f_1','f_2')

train_scores <- data.frame(train.pcamix$scores)
test_scores <- data.frame(test.pcamix)
train_scores_rot <- data.frame(train.pcarot$scores)
test_scores_rot <- data.frame(test.pcarot)

#train_scores <- data.frame(heart.famd$ind['coord'])
#test_scores <- data.frame(heart.famd.test$coord)

colnames(train_scores) <- factors
colnames(test_scores) <- factors
colnames(train_scores_rot) <- factors_rot
colnames(test_scores_rot) <- factors_rot

train_scores$target <- train$target
test_scores$target <- test$target
train_scores_rot$target <- train$target
test_scores_rot$target <- test$target

#--------------------------------------------------------
#FAMD + logistic regression models (No Rotation)
#--------------------------------------------------------

#Full Model
full.model <- glm(target ~ .,data=train_scores,family=binomial)
#Reduced Model
reduced.model <- glm(target ~ f_1 + f_2 + f_3 + f_4,data=train_scores,family=binomial)

reduced.predict <- predict(reduced.model,data=train_scores,type="response")

#Find optimal cutoff for cross-validation (Based on Youden's Index)
cutoff <- optimalCutoff(actuals = train_scores$target, predictedScores = reduced.predict,optimiseFor = "Both")
cost <- function(r, pi = 0) mean(abs(r-pi) > cutoff)

#cross-validation (LOOCV)
full.err_ = NULL
reduced.err_ = NULL

full.err_ <- cv.glm(train_scores,full.model,cost=cost)$delta[1]
reduced.err_ <- cv.glm(train_scores,reduced.model,cost=cost)$delta[1]

c(mean(full.err_),mean(reduced.err_))

#--------------------------------------------------------
#FAMD + logistic regression models (Rotation)
#--------------------------------------------------------

#Full Model
full.model.rot <- glm(target ~ .,data=train_scores_rot,family=binomial)
#Reduced Model
reduced.model.rot <- glm(target ~ f_1,data=train_scores_rot,family=binomial)

full.predict.rot <- predict(full.model.rot,data=train_scores_rot,type="response")
reduced.predict.rot <- predict(reduced.model.rot,data=train_scores_rot,type="response")

#Find optimal cutoff for cross-validation (Based on Youden's Index)
cutoff_ <- optimalCutoff(actuals = train_scores_rot$target, predictedScores = reduced.predict.rot,optimiseFor = "Both")
cost <- function(r, pi = 0) mean(abs(r-pi) > cutoff_)

#cross-validation (LOOCV)
full.err = NULL
reduced.err = NULL

#LOOCV

full.err <- cv.glm(train_scores_rot,full.model.rot,cost=cost)$delta[1]
reduced.err <- cv.glm(train_scores_rot,reduced.model.rot,cost=cost)$delta[1]


c(mean(full.err),mean(reduced.err))

#--------------------------------------------------------
# Non Parametric Bootstrap
#--------------------------------------------------------

logit.bootstrap <- function(data,indicies){
  d <- data[indicies,]
  fit <- glm(target ~ .,data=d,family=binomial)
  return(coef(fit))
}
logit.boot <- boot(data=train_scores,statistic=logit.bootstrap,R=1000)


#--------------------------------------------------------
# Accuracy for Unrotated Models
#--------------------------------------------------------

full.predict.test <- predict(full.model,newdata=test_scores,type="response")
confusionMatrix(test_scores$target,full.predict.test,cutoff)

reduced.predict.test <- predict(reduced.model,newdata=test_scores,type="response")
confusionMatrix(test_scores$target,reduced.predict.test,cutoff)

#--------------------------------------------------------
# Accuracy for Rotated Models
#--------------------------------------------------------

full.rot.predict.test <- predict(full.model.rot,newdata=test_scores_rot,type="response")
confusionMatrix(test_scores_rot$target,full.rot.predict.test,cutoff_)

reduced.rot.predict.test <- predict(reduced.model.rot,newdata=test_scores_rot,type="response")
confusionMatrix(test_scores_rot$target,reduced.rot.predict.test,cutoff_)

#-------------------------------------------------------
# Checking For Linearity between predictors and odds
#-------------------------------------------------------

#full model

probabilities <- predict(full.model, type = "response")

# Select only numeric predictors
mydata <- train_scores[,1:5] %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#reduced model

probabilities <- predict(reduced.model, type = "response")

# Select only numeric predictors
mydata <- train_scores[,1:4] %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


#full/reduced model rotated

probabilities <- predict(reduced.model.rot, type = "response")

# Select only numeric predictors
mydata <- train_scores_rot[,1:2] %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


probabilities <- predict(full.model.rot, type = "response")

# Select only numeric predictors
mydata <- train_scores_rot[,1:2] %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")







