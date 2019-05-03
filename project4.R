installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car"))

# Helper functions =========================================
PreProcessingMatrixOutput <- function(train.data, test.data, y){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  categorical.vars <- categorical.vars[!categorical.vars %in% y]
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
      tmp.test[test.data[, var]==mylevels[j], j] <- 1
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
  ind <- which(is.na(column))
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
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

                                                          # log-loss function
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
setwd("/Users/Meana/Documents/4.STAT542/Project_4")

split <- read.csv("Project4_test_id.csv")
data <- read.csv("loan_stat542.csv")
datacl <- data

# recode y label
library(car)
datacl[,"loan_status"] <- Recode(datacl[,"loan_status"],
                               "c('Fully Paid') = 0;
                               c('Default','Charged Off') = 1")

# remove unecessary variables
rm <- c("zip_code","emp_title", "title")
datacl <- datacl[ , !(names(datacl) %in% rm)]

# recode missing values
naVraibles <- c("mort_acc", "dti", "revol_util", "pub_rec_bankruptcies")
for(i in naVraibles){
  datacl[, i] <- changeNA(datacl[, i], mean(datacl[, i], na.rm = T))
}
datacl$emp_length <- changeNA(datacl$emp_length, "missing")

#calculate the month
date <- as.Date(paste("1-", data[,"earliest_cr_line"], sep=""),format="%d-%b-%Y")
datacl[,"earliest_cr_line"] <- mondf(date, "2015-04-01")




# Building Classifiers =================================

#********** model1: Lasso Logistic Regression ******************* 
result_lr <- rep(NA, 3)
data1 <- datacl
#
  # prepare the train/test splits
  for(i in 1:3){
  testid <- split[, i]
  ind <- which(data1$id %in% testid)
  train.y <-data1[-ind,"loan_status"] 
  test.y <- data1[ind,"loan_status"]
  train.x <- RemoveVariable(data1[-ind,],c("id","loan_status"))
  test.x <- RemoveVariable(data1[ind,],c("id","loan_status"))


  
  # One hot encoding
  b <- PreProcessingMatrixOutput(train, test)
  train_x <- b$train
  test_x <- b$test
 
  set.seed(100)
  library(glmnet)
  lr.model <- cv.glmnet(train_x, train.y, family="binomial", alpha=1)
  lr.probs <- predict(lr.model, newx=test_x, type="response")
  output.lr <- cbind(testid, lr.probs)
  colnames(output.lr) <- c("id","prob")
  

  result_lr[i] <- logLoss(test.y,lr.re[,"prob"])
}
  
}



#********** model2 ******************* 
library(xgboost)
data2 <- datacl
rem.var <- c("id","loan_status")

for(i in 1:ncol(split)){
  testid <- split[, i]
  ind <- which(data2$id %in% testid)
  train.y <-data2[-ind,"loan_status"] 
  test.y <- data2[ind,"loan_status"]
  tmp <- PreProcessingMatrixOutput(RemoveVariable(data2[-ind,],c("id","loan_status")), 
                                   RemoveVariable(data2[ind,],c("id","loan_status")))
  train.x <- tmp$train
  test.x <- tmp$test
  xgb.model <- xgboost(data = train.x, label = as.factor(train.y), 
                       nrounds = 120, max_depth = 5, eta = 0.2, num_parallel_tree = 3,
                       colsample_bytree = 0.6, subsample = 0.6, verbose=1)
  xgboost.prob <- predict(xgb.model, test.x) - 1
  ind <- which(xgboost.prob > 0)
  logLoss(test.y[ind], xgboost.prob[ind])
}
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
                                                        
# outname <-  paste("mysubmission_test",i,".txt",sep="")
# write.table(output, file=outname, row.names = FALSE, sep=",", col.names = TRUE)

# Build the final classifier======================


#Model Evaluation=================================
testq3 <- read.csv ("LoanStats_2018Q3.csv")
testq4 <- read.csv ("LoanStats_2018Q4.csv")

# write.table(mysubmission_2018Q3.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
# write.table(mysubmission_2018Q4.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)





