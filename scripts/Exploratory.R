#Credit Scoring data: Variable Selection and study model performances

#loading the required libraries

library(tidyr)
library(dplyr)
library(ggplot2)

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






