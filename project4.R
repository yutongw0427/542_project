installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car","xgboost","lime"))

# Helper functions =========================================
oneHotEncoding <- function(train.data, test.data){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  train.matrix <- train.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  test.matrix <- test.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  n.train <- nrow(train.data)
  n.test <- nrow(test.data)
  for(var in categorical.vars){
    mylevels <- sort(unique(train.data[, var]))
    m <- length(mylevels)
    tmp.train <- matrix(0, n.train, m)
    tmp.test <- matrix(0, n.test, m)
    col.names <- NULL
    for(j in 1:m){
      tmp.train[train.data[, var]==mylevels[j], j] <- 1
      if(mylevels[j] %in% levels(test.data[, var])){
        tmp <- as.character(mylevels[j])
        tmp.test[test.data[, var]== tmp, j] <- 1
      }
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.train) <- col.names
    colnames(tmp.test) <- col.names
    train.matrix <- cbind(train.matrix, tmp.train)
    test.matrix <- cbind(test.matrix, tmp.test)
  }
  return(list(train = as.matrix(train.matrix), test = as.matrix(test.matrix)))
}

# Function of removing unnecessary variables
RemoveVariable <- function(data, var){
  data <- data[, !colnames(data) %in% var]
  return(data)
}

# Function of winsorization
ProcessWinsorization <- function(train.var, test.var, quant){
  threshold <- quantile(train.var, quant)
  train.var[train.var > threshold] <- threshold
  test.var[test.var > threshold] <- threshold
  return(list(train.v = train.var, test.v = test.var))
}

# Change NA to specific value
changeNA <- function(column, repl){
  is.fac <- 0
  if(is.factor(column)){
    column <- as.character(column)
    is.fac <- 1
  }
  ind <- c(which(is.na(column)), which(column == "n/a"))
  column[ind] <- repl
  if(is.fac == 0)
    return(column)
  else
    return(as.factor(column))
}
                                                                                                         
# Function of log loss**********************delete*****************************************
logLoss = function(y, p){
  if (length(p) != length(y)){
    stop('Lengths of prediction and labels do not match.')
  }
  
  if (any(p < 0)){
    stop('Negative probability provided.')
  }
  
  p = pmax(pmin(p, 1 - 10^(-15)), 10^(-15))
  mean(ifelse(y == 1, -log(p), -log(1 - p)))
}


# Data Processing =========================================
# setwd("/Users/Meana/Documents/4.STAT542/Project_4")
setwd("~/Desktop/542/project/Project4")
split <- read.csv("Project4_test_id.csv")
data <- read.csv("LoanStats_2007_to_2018Q2.csv")

set.seed(5798)

library(car)
processData <- function(data){
  # recode y label
  data[,"loan_status"] <- Recode(data[,"loan_status"],
                                   "c('Fully Paid') = 0;
                                   c('Default','Charged Off') = 1")
  
  # remove unecessary variables
  rm <- c("zip_code","emp_title", "title", "grade")
  data <- data[ , !(names(data) %in% rm)]
  
  # recode missing values
  naVraibles <- c("dti", "revol_util")
  for(i in naVraibles){
    data[, i] <- changeNA(data[, i], median(data[, i], na.rm = T))
  }
  tmp <- 100000
  data$mort_acc <- changeNA(data$mort_acc, tmp)
  data$emp_length <- changeNA(data$emp_length, "missing")
  
  # Take the year of earliest credit line
  data[,"earliest_cr_line"] <- as.factor(substr(data[,"earliest_cr_line"],5,8))
  
  # Perform grouping in some variables
  ## Group: 0,1,2,3,4,5,6
  data$pub_rec[data$pub_rec > 5] <- 6
  data$pub_rec <- as.factor(data$pub_rec)
  
  ## Group: 0,1,2,3,4,5,6,7,10000 (10000 is missing values)
  data$mort_acc[data$mort_acc >= 7 & data$mort_acc < tmp] <- 7
  data$mort_acc <- as.factor(data$mort_acc)
  
  ## Group: 0, 1, 10000 (2 is missing values)
  data$pub_rec_bankruptcies[data$pub_rec_bankruptcies > 0] <- 1
  data$pub_rec_bankruptcies[is.na(data$pub_rec_bankruptcies)] <- tmp
  data$pub_rec_bankruptcies <- as.factor(data$pub_rec_bankruptcies)
  
  ## Group: RENT, MORTGAGE, OWN, OTHER
  data[, "home_ownership"] <- Recode(data[, "home_ownership"], 
                                       "c('OTHER', 'NONE', 'ANY') = 'OTHER'")
  return(data)
}

data <- processData(data)

# Building Classifiers =================================
library(xgboost)
rem.var <- c("id","loan_status")
lloss_xgboost <- rep(NA, 3)
i<-1
testid <- split[, i]
ind <- which(data$id %in% testid)
train.y <-data[-ind,"loan_status"] 
test.y <- data[ind,"loan_status"]
tmp <- oneHotEncoding(RemoveVariable(data[-ind,],c("id","loan_status")), 
                                 RemoveVariable(data[ind,],c("id","loan_status")))
train.x <- tmp$train
test.x <- tmp$test
xgb.model <- xgboost(data = train.x, label = (as.numeric(train.y)-1), 
                     nrounds =120,subsample = 0.6,eval_metric="logloss",
                     max_depth = 3, eta = 0.4, verbose=1, objective = "binary:logistic" )

#Calculate the logloss on the three splits
for(i in 1:ncol(split)){
  testid <- split[, i]
  ind2 <- which(data$id %in% testid)
  test.y <- data[ind2,"loan_status"]
  tmp <- oneHotEncoding(RemoveVariable(data[-ind,],c("id","loan_status")), 
                                   RemoveVariable(data[ind2,],c("id","loan_status")))
  test.x <- tmp$test
  xgboost.prob <- predict(xgb.model, test.x, type="prob")
  output<- cbind(testid, xgboost.prob)
  colnames(output) <- c("id","prob")
  lloss_xgboost[i] <- logLoss(test.y, output[,"xgboost.prob"])
  # outname <-  paste("mysubmission_test",i,".txt",sep="")
  # write.table(output, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
}

#lloss_xgboost
#[1] 0.4491859 0.4486139 0.4472861
#mean(lloss_xgboost)
#[1] 0.448362                                                        
                                                        
# Build the final classifier ==============================================================================
rem.var <- c("id", "loan_status")
train.y <- data[, "loan_status"]

process <- function(train, test){
  vars <- colnames(train)
  test <- test[, colnames(test) %in% vars]
  
  test$revol_util <- as.numeric(sub("%", "", test$revol_util))
  test$int_rate <- as.numeric(sub("%", "", test$int_rate))
  test$term <- as.factor(sub(" ", "", as.character(test$term)))
  
  test <-processData(test)
  test.y <- test[, "loan_status"]
  test.id <- test[, "id"]
  tmp <- oneHotEncoding(RemoveVariable(train, rem.var),
                        RemoveVariable(test, rem.var))
  train.x <- tmp$train
  test.x <- tmp$test
  return(list("train.x" = train.x,"test.x" = test.x, "test.id" = test.id，"test.y" = test.y))
}

testq3 <- read.csv("LoanStats_2018Q3.csv")
testq4 <- read.csv("LoanStats_2018Q4.csv")

tmpQ3 <- process(data, testq3)
tmpQ4 <- process(data, testq4)

train.x <- tmpQ3$train.x

xgb.model <- xgboost(data = train.x, label = (as.numeric(train.y)-1), 
                     nrounds =120,subsample = 0.6,eval_metric="logloss",
                     max_depth = 3, eta = 0.4, verbose=1, objective = "binary:logistic" )
#train-logloss:0.447171 
#Calculate by the given function :0.4471651
                                                        
# Output the results of test data sets ==============================================================================
for (i in (3:4)){
  i <-3
  tmp <- get(paste("tmpQ",i, sep=""))
  pred.prob<- predict(xgb.model, tmp$test.x, type = "prob")
  out <- cbind("id"=tmp$test.id, "prob"=pred.prob)
  outname <-  paste("mysubmission_2018Q",i,".txt",sep="")
  write.table(out, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
}

#Analyze the results(white box)=======================================================================================
set.seed(5798)
library(lime)
for (i in (3:4)){
tmp <- get(paste("tmpQ",i, sep=""))
test.y <- tmp$test.y
ind <- which(test.y %in% c("0", "1"))
test.x <- as.data.frame(tmp$test.x[ind,])
sample <- sample(c(1:dim(test.x)[1]), 4) 
explainer <- lime(test.x[-sample,], xgb.model, bin_continuous = TRUE, n_bins = 2, quantile_bins = FALSE)
explanation <- explain(test.x[sample, ],explainer, labels = "1", n_features = 5)
# # Only showing part of output for better printing
# explanation[2:9]
abc <-plot_features(explanation)
print(abc)
}

#Test the logloss on the new test data
  # test <- testq4
  # tmp <- tmpQ4
  # test[,"loan_status"] <- Recode(test[,"loan_status"],
  #                              "c('Fully Paid') = 0;
  #                              c('Default','Charged Off') = 1")
  # ind <- which(test[,"loan_status"] %in% c("0", "1"))
  # test.x <- tmp$test.x[ind,]
  # test.y <- test$loan_status[ind]
  # prob <- predict(xgb.model, test.x, type = "prob")
  # logLoss(test.y, prob)

#===================Other models==============================================
##Random forest
#********** model3: Random Forest ******************* 
# library(randomForest)
# i <- 1
# testid <- split[, i]
# ind <- which(data$id %in% testid)
# train <-data[-ind, !colnames(train) %in% "id"] 
# test <- data[ind,]
# # tmp <- oneHotEncoding(RemoveVariable(data[-ind,],c("id","loan_status")), 
# #                       RemoveVariable(data[ind,],c("id","loan_status")))
# # train.x <- tmp$train
# # test.x <- tmp$test
# # train <-as.data.frame(cbind(train.x,train.y))
# library(randomForest)
# rf_fit3 = randomForest(loan_status ~., 
#                        data = train, 
#                        ntree = 100, 
#                        mtry = 5, 
#                        nodesize = 3, 
#                        sampsize = 10000, 
#                        importance = TRUE)
# rm.var <- c("id")
# y <- "loan_status"
# for(i in 1:ncol(split)){
#   testid <- split[, i]
#   ind <- which(data$id %in% testid)
#   test.y <- data3[ind, y]
#   train <- RemoveVariable(data3[-ind, ], rm.var)
#   test.x <- RemoveVariable(data3[ind, ], c(rm.var, y))
#   rf_fit3 = randomForest(loan_status ~., 
#                          data = train, 
#                          ntree = 100, 
#                          mtry = 5, 
#                          nodesize = 3, 
#                          sampsize = 10000, 
#                          importance = TRUE)
#   pred3 = predict(rf_fit3, test.x, type = "prob")
#   logLoss(test.y, pred3[,2])
# }
# 
# #Logistic regression
# result_lr <- rep(NA, 3)
# for(i in 1:3) {
#   testid <- split[, i]
#   ind <- which(data$id %in% testid)
#   train.y <-data[-ind,"loan_status"] 
#   test.y <- data[ind,"loan_status"]
#   train.x <- RemoveVariable(data[-ind,],c("id","loan_status"))
#   test.x <- RemoveVariable(data[ind,],c("id","loan_status"))
#   
#   tmp <- oneHotEncoding(RemoveVariable(data[-ind,],c("id","loan_status")),
#                         RemoveVariable(data[ind,],c("id","loan_status")))
#   train_x <- tmp$train
#   test_x <- tmp$test
#   
#   library(glmnet)
#   library(doParallel)
#   registerDoParallel(4)
#   # lr.model <- cv.glmnet(train_x, train.y, family="binomial", alpha=1, parallel = TRUE)
#   lr.model <- glmnet(train_x, train.y, family="binomial", alpha=0, lambda = 0.002, standardize = TRUE)
#   lr.probs <- predict(lr.model, newx=test_x, type="response")
#   output.lr <- cbind(testid, lr.probs)
#   colnames(output.lr) <- c("id","prob")
#   logLoss(test.y, output.lr[,"prob"])
#   result_lr[i] <- logLoss(test.y, output.lr[,"prob"])
# }
# 
# 

