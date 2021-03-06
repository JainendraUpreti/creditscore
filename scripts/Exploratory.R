#Credit Scoring data: Variable Selection and study model performances

#loading the required libraries

library(tidyr)
library(dplyr)
library(ggplot2)
library(ROCR)
library(rpart)

# fetching the data
german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

# Adding column names as per data specifications
colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")

View(head(german_credit))

# orginal response coding 1= good, 2 = bad; we need 0 = good, 1 = bad
# Changing the data accordingly

german_credit$response = german_credit$response - 1 # Run this only once
str(german_credit)

View(head(german_credit)) # changes reflected

# setting a seed and creating training and testing samples
# train dataset: 70%; testdata: 30%

set.seed(6)

subset = sample(nrow(german_credit), nrow(german_credit) * 0.7)
german_credit_train = german_credit[subset, ]
german_credit_test = german_credit[-subset, ]

## Running a logistic model on our training data set

#glm full model using logit, probit and cloglog ####
german_credit_train.glm0 <- glm(response ~ ., family = binomial(link = "logit"),
                                     german_credit_train)
summary(german_credit_train.glm.logit)

german_credit_train.glm.probit <- glm(response ~ ., family = binomial(link = "probit"), 
                                      german_credit_train)
summary(german_credit_train.glm.probit)

german_credit_train.glm.complementaryloglog <- glm(response ~ ., family = binomial(link = "cloglog"),
                                                   german_credit_train)
summary(german_credit_train.glm.complementaryloglog)

## Applying modelling techniques

### running a NULL model with no dependent variables

german_credit_train.glm_Null <- glm(response ~ 1, family = binomial(link = "logit"), 
                                    german_credit_train)
summary(german_credit_train.glm_Null )

# Backward Selection Method: starting with 0 dependent variables #####
german_credit_train.step_Backward <- step(german_credit_train.glm0)  # AIC
AIC(german_credit_train.step_Backward)

german_credit_train.step_Backward_BIC <- step(german_credit_train.glm0, 
                                              k = log(nrow(german_credit_train))) # BIC
BIC(german_credit_train.step_Backward)

# Forward Selection Method: Starting with all the dependent variables #####
german_credit_train.step_forward <- step(german_credit_train.glm_Null, 
                                        scope = list(lower = german_credit_train.glm_Null, 
                                                     upper = german_credit_train.glm0),
                                                      direction = "forward")  # AIC

AIC(german_credit_train.step_forward)

german_credit_train.step_forward_BIC <- step(german_credit_train.glm_Null, 
                                            scope = list(lower = german_credit_train.glm_Null, 
                                                         upper = german_credit_train.glm0),
                                                        direction = "forward", 
                                                k = log(nrow(german_credit_train)))  # BIC

BIC(german_credit_train.step_forward_BIC)

# stepwise Selection Method #####
german_credit_train.step_both <- step(german_credit_train.glm_Null, 
                                     scope = list(lower = german_credit_train.glm_Null, 
                                                  upper = german_credit_train.glm0),
                                                  direction = "both")  # AIC

AIC(german_credit_train.step_both)


german_credit_train.step_both_BIC <- step(german_credit_train.glm_Null, 
                                         scope = list(lower = german_credit_train.glm_Null, 
                                                      upper = german_credit_train.glm0),
                                         direction = "both", k = log(nrow(german_credit_train)))  # BIC

BIC(german_credit_train.step_both_BIC)

# Predict on insample Training Data #### model choose to use
### model chosen for seed(6)
german_credit_train.trn_fit <- glm(response ~ age  + installment_rate + foreign + 
                                    amount  + other_debtor + credit_his + n_credits  + 
                                    n_people + telephone + present_resid + other_install  + sex  + 
                                    housing + property + present_emp  + saving_acct + 
                                    job + duration + purpose + chk_acct,  
                                  family = binomial(link = "logit"), 
                                  data =  german_credit_train)

#german_credit_train.trn_fit = glm(response ~ chk_acct + duration + credit_his,
#                                  family = binomial(link = "logit"), data =  german_credit_train)

german_credit_train.prob.insample <- predict(german_credit_train.trn_fit, type = "response")
german_credit_train.prob.insample  <- german_credit_train.prob.insample > (1/6)
german_credit_train.prob.insample  <- as.numeric(german_credit_train.prob.insample)


# Train data set - ROCR ####

pred <- prediction(german_credit_train.prob.insample, german_credit_train$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main="In sample ROCR for GLM")

as.numeric(performance(pred,'auc')@y.values) # AUC


# Train Misclassification Rate ####

table(german_credit_train$response, german_credit_train.prob.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(german_credit_train$response != german_credit_train.prob.insample, 1, 0))

## Predicting on the test dataset
# Test Data Prediction ######

german_credit_test.prob.outsample <- predict(german_credit_train.trn_fit, german_credit_test, type = "response")
german_credit_test.prob.outsample  <- german_credit_test.prob.outsample  > (1/6)
german_credit_test.prob.outsample   <- as.numeric(german_credit_test.prob.outsample )

# Test Data Misclassification Rate ####
table(german_credit_test$response, german_credit_test.prob.outsample, dnn = c("Truth", "Predicted"))
mean(ifelse(german_credit_test$response != german_credit_test.prob.outsample, 1, 0))

##### The prediction is almost as good MSE - Train dataset: 0.337; Test dataset: 0.346###


# Test Data ROC curve and AUC ####

pred <- prediction(german_credit_test.prob.outsample, german_credit_test$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main="Out-of-sample ROCR for GLM")

as.numeric(performance(pred,'auc')@y.values) # AUC

### AUC for training was 73% and AUC for test is 70.71%

########### Finding the best classification tree ######################

german_credit.rpart <- rpart(formula = response ~ ., data = german_credit_train, method = "class", 
                             parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)), 
                             cp=0.0000001) #method='anova' is default for regression

plotcp(german_credit.rpart)  #cp value 0.0085 tree size 24

## sequential search to find the value of cp

cp <- seq(0.0005, 0.013, by=0.001)

result <- cbind(cp, NA)
k <- 1

for (i in cp){
  german_credit.rpart <- rpart(formula = response ~ ., data = german_credit_train, method = "class", 
                               parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)), cp=i) #method='anova' is default for regression
  credit.test.pred.tree1 = predict(german_credit.rpart, german_credit_test)[ ,2]
  credit.test.pred.tree1 = as.numeric( credit.test.pred.tree1 > (1/6) )
  result[k, 2] <- mean(ifelse(german_credit_test$response != credit.test.pred.tree1, 1, 0))
  k <- k+1
}
result[which.min(result[,2]), 1]  ##cp value which minimizes the misclassification rate
plot(cp, result[,2], ylab="misclassification rate", main="Find the best tree",type="b")

## Also gives value 0.0085, we will use this value

##best tree for seed(6)
german_credit.rpart <- rpart(formula = response ~ ., data = german_credit_train, method = "class", 
                             parms = list(loss = matrix(c(0, 5, 1, 0), nrow = 2)), 
                             cp=0.0085) #method='anova' is default for regression


german_credit.rpart
plot(german_credit.rpart)
text(german_credit.rpart) #tree model

########## Classification tree complete #############




