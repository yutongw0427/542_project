installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car","xgboost"))

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
  data$mort_acc[data$mort_acc > 7 & data$mort_acc < tmp] <- 7
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
tmp <- PreProcessingMatrixOutput(RemoveVariable(data[-ind,],c("id","loan_status")), 
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
  tmp <- PreProcessingMatrixOutput(RemoveVariable(data[-ind,],c("id","loan_status")), 
                                   RemoveVariable(data[ind2,],c("id","loan_status")))
  test.x <- tmp$test
  xgboost.prob <- predict(xgb.model, test.x, type="prob")
  output<- cbind(testid, xgboost.prob)
  colnames(output) <- c("id","prob")
  lloss_xgboost[i] <- logLoss(test.y, output[,"xgboost.prob"])
  # outname <-  paste("mysubmission_test",i,".txt",sep="")
  # write.table(output, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
}

                                                        
                                                        
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
  test.id <- test[, "id"]
  tmp <- oneHotEncoding(RemoveVariable(train, rem.var),
                        RemoveVariable(test, rem.var))
  train.x <- tmp$train
  test.x <- tmp$test
  return(list("train.x" = train.x,"test.x" = test.x, "test.id" = test.id))
}

testq3 <- read.csv("LoanStats_2018Q3.csv")
testq4 <- read.csv("LoanStats_2018Q4.csv")

tmpQ3 <- process(data, testq3)
tmpQ4 <- process(data, testq4)

train.x <- tmpQ3$train.x

xgb.model <- xgboost(data = train.x, label = (as.numeric(train.y)-1), 
                     nrounds =120,subsample = 0.6,eval_metric="logloss",
                     max_depth = 3, eta = 0.4, verbose=1, objective = "binary:logistic" )
                                                        

# Build the final classifier ==============================================================================
pred.prob.Q3 <- predict(xgb.model, tmpQ3$test.x, type = "prob")
pred.prob.Q4 <- preodct(xgb.model, tmpQ4$test.x, type = "prob")
  
outQ3 <- cbind(tmpQ3$test.id, pred.prob.Q3)
outQ4 <- cbind(tmpQ4$test.id, pred.prob.Q4)

write.table(outQ3, file="mysubmission_2018Q3.txt", row.names = FALSE, sep=",", col.names = TRUE)
write.table(outQ4, file="mysubmission_2018Q4.txt", row.names = FALSE, sep=",", col.names = TRUE)



# write.table(mysubmission_2018Q3.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
# write.table(mysubmission_2018Q4.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
