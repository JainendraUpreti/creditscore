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





