installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car"))

# Helper functions =========================================
oneHotEncoding <- function(train.data, test.data){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  print(categorical.vars)
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
      print(paste(var, j))
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

#Function of Calculating the month
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1940-01-01")); 
lt$year*12 + lt$mon } 

# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1)}


# Function of log loss
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


oneHotOnTraining <- function(train.data){
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  train.matrix <- train.data[, !colnames(train.data) %in% categorical.vars, drop=FALSE]
  n.train <- nrow(train.data)
  for(var in categorical.vars){
    mylevels <- sort(unique(train.data[, var]))
    m <- length(mylevels)
    tmp.train <- matrix(0, n.train, m)
    col.names <- NULL
    for(j in 1:m){
      tmp.train[train.data[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.train) <- col.names
    train.matrix <- cbind(train.matrix, tmp.train)
  }
  return(train.matrix)
}
                                                        
                                                        
                                                        
# Data Processing =========================================
# setwd("/Users/Meana/Documents/4.STAT542/Project_4")
setwd("~/Desktop/542/project/Project4")
split <- read.csv("Project4_test_id.csv")
data <- read.csv("LoanStats_2007_to_2018Q2.csv")
     
set.seed(5798)
                                                        
library(car)
processData <- function(datacl){
# recode y label
    datacl[,"loan_status"] <- Recode(datacl[,"loan_status"],
                                     "c('Fully Paid') = 0;
                                     c('Default','Charged Off') = 1")
    # if(lenght(unique(datacl[, "loan_status"])) < 3){
    #   others <- c(levels(datacl[, "loan_status"])[-(1:2)])
    #   datacl[,"loan_status"] <- Recode(datacl[,"loan_status"],
    #                                    "c('Current', 'In Grace Period', '') = 0")
    # }
  # remove unecessary variables
  rm <- c("zip_code","emp_title", "title", "grade")
  datacl <- datacl[ , !(names(datacl) %in% rm)]
  
  # recode missing values
  naVraibles <- c("dti", "revol_util")
  for(i in naVraibles){
    datacl[, i] <- changeNA(datacl[, i], median(datacl[, i], na.rm = T))
  }
  tmp <- 100000
  datacl$mort_acc <- changeNA(datacl$mort_acc, tmp)
  datacl$emp_length <- changeNA(datacl$emp_length, "missing")
  
  #calculate the month
  # date <- as.Date(paste("1-", datacl[,"earliest_cr_line"], sep=""),format="%d-%b-%Y")
  # datacl[,"earliest_cr_line"] <- mondf(date, "2015-04-01")
  datacl[,"earliest_cr_line"] <- as.factor(substr(datacl[,"earliest_cr_line"],5,8))
  
  
  
  # Perform grouping in some variables
  ## Group: 0,1,2,3,4,5,6
  datacl$pub_rec[datacl$pub_rec > 5] <- 6
  datacl$pub_rec <- as.factor(datacl$pub_rec)
  
  ## Group: 0,1,2,3,4,5,6,7,10000 (10000 is missing values)
  datacl$mort_acc[datacl$mort_acc > 7 & datacl$mort_acc < tmp] <- 7
  datacl$mort_acc <- as.factor(datacl$mort_acc)
  
  ## Group: 0, 1, 10000 (2 is missing values)
  datacl$pub_rec_bankruptcies[datacl$pub_rec_bankruptcies > 0] <- 1
  datacl$pub_rec_bankruptcies[is.na(datacl$pub_rec_bankruptcies)] <- tmp
  datacl$pub_rec_bankruptcies <- as.factor(datacl$pub_rec_bankruptcies)
  
  ## Group: RENT, MORTGAGE, OWN, OTHER
  datacl[, "home_ownership"] <- Recode(datacl[, "home_ownership"], 
                                       "c('OTHER', 'NONE', 'ANY') = 'OTHER'")
  return(datacl)
}

datacl <- processData(data)
                                                        

                                                        
# Building Classifiers =================================

#********** model3 ******************* 
library(xgboost)
data2 <- datacl
rem.var <- c("id","loan_status")
lloss_xgboost <- rep(NA, 3)
  i<-1
  testid <- split[, i]
  ind <- which(data2$id %in% testid)
  train.y <-data2[-ind,"loan_status"] 
  test.y <- data2[ind,"loan_status"]
  tmp <- oneHotEncoding(RemoveVariable(data2[-ind,],c("id","loan_status")), 
                                   RemoveVariable(data2[ind,],c("id","loan_status")))
  train.x <- tmp$train
  test.x <- tmp$test
  xgb.model <- xgboost(data = train.x, label = (as.numeric(train.y)-1), 
                       nrounds = 120,num_parallel_tree = 3, 
                       colsample_bytree = 0.6, subsample = 0.6, 
                       max_depth = 3, eta = 0.4, verbose=1, objective = "binary:logistic" )
for(i in 1:ncol(split)){
  testid <- split[, i]
  ind2 <- which(data2$id %in% testid)
  test.y <- data2[ind2,"loan_status"]
  tmp <- PreProcessingMatrixOutput(RemoveVariable(data2[-ind,],c("id","loan_status")), 
                                   RemoveVariable(data2[ind2,],c("id","loan_status")))
  test.x <- tmp$test
  xgboost.prob <- predict(xgb.model, test.x, type="prob")
  lloss_xgboost[i] <- logLoss(test.y, xgboost.prob)
}


# Build the final classifier======================
#Model Evaluation=================================
                                                                                                                
vars <- colnames(datacl)
y <- "loan_status"
rem.var <- c("id", "loan_status", outname)
predicting <- funciton(train, test){
  test <- testq3
  test <- test[, colnames(test) %in% vars]
  
  test$revol_util <- as.numeric(sub("%", "", test$revol_util))
  test$int_rate <- as.numeric(sub("%", "", test$int_rate))
  test$term <- as.factor(sub(" ", "", as.character(test$term)))
  
  test <-processData(test)
  test.id <- test[, "id"]
  train.y <- train[, y]
  tmp <- oneHotEncoding(RemoveVariable(train, rem.var),
                        RemoveVariable(test, rem.var))
  train.x <- tmp$train
  test.x <- tmp$test
  
  xgb.model <- xgboost(data = train.x, label = (as.numeric(train.y)-1), 
                       nrounds = ,num_parallel_tree = 3, 
                       colsample_bytree = 0.6, subsample = 0.6, 
                       max_depth = 3, eta = 0.4, verbose=1, 
                       objective = "binary:logistic" )
  xgboost.prob <- predict(xgb.model, test.x, type = "prob")
  out <- cbind(test.id, xgboost.prob)
  write.table(out, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
}

testq3 <- read.csv("LoanStats_2018Q3.csv")
testq4 <- read.csv("LoanStats_2018Q4.csv")

predicting(datacl, testq3, "mysubmission_2018Q3.txt")
predicting(datacl, testq3, "mysubmission_2018Q4.txt")


# write.table(mysubmission_2018Q3.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
# write.table(mysubmission_2018Q4.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
