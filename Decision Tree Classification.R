#EXTRACT DATA
credit <- read.csv("credit.csv",stringsAsFactors = TRUE)

#REVIEW DATA
View(credit)
str(credit)
credit$default
credit[17]
#Extract from row and column
credit[2,4]

#REVIEW CATEGORICAL VARIABLES
table(credit$checking_balance)
table(credit$savings_balance)

#EXPLORE NUMERICAL VARIABLES
summary(credit$months_loan_duration)
summary(credit$amount)
hist(credit$months_loan_duration)
hist(credit$amount)

table(credit$default)

#SPLIT THE DATA, 90% to train the data and 10% to evaluate it
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)
credit_train <- credit[train_sample, ]
credit_test <- credit[-train_sample, ]
#Check that the ratio of defaults (70 to 30) is kept approx. in the split data.
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


#TRAIN THE MODEL
#Package
install.packages("C50")
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)
#SEE THE MODEL
C5.0.default(x = credit_train[-17], y = credit_train$default)
summary(credit_model)


#EVALUATE THE MODEL
#Package
install.packages("gmodels")
library(gmodels) #(For the CrossTable function)

credit_pred <- predict(credit_model,credit_test)
#Comparison
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE,
           prop.r = FALSE,
           dnn = c('actual default','predicted default'))


###LET'S IMPROVE THE MODEL: ADAPTIVE BOOSTING AND COST MATRICES###

##ADAPTATIVE BOOSTING##
credit_boost10 <- C5.0(credit_train[-17],credit_train$default, trials = 10)
summary(credit_boost10)


#EVALUATE NEW MODEL
credit_boost_pred10 <- predict(credit_boost10, credit_test)
#COMPARISON
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#The model improved, compare the results


##COST MATRIX##
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

#Filling the matrix
empty_matrix <- matrix(c("true negative","false positive",
                         "false negative","true positive"),
                       nrow = 2, dimnames = matrix_dimensions)
empty_matrix
#Suppose, loan default (false negative) costs the bank four times as much as a missed opportunity (false positive)
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)
error_cost

#Improve the model with cost matrix
credit_cost <- C5.0(credit_train[-17],credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
#Evaluate and Analyse
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
s
