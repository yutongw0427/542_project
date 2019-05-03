installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car"))

# Helper functions =========================================
PreProcessingMatrixOutput <- function(train.data, test.data){
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


# Data Processing =========================================
setwd("/Users/Meana/Documents/4.STAT542/Project_4")

split <- read.csv("Project4_test_id.csv")
data <- read.csv("loan_stat542.csv")
datacl <- data

# recode y label
library(car)
datacl[,"loan_status"] <- Recode(datacl[,"loan_status"],
                               "c('Fully Paid') = 0;
                               c('Default','Charged Off') = 1"
)

# remove unecessary variables
rm <- c("zip_code","emp_title", "title")
datacl <- datacl[ , !(names(datacl) %in% rm)]

# recode missing values
datacl$mort_acc <- changeNA(datacl$mort_acc, mean(datacl$mort_acc, na.rm = T))
datacl$emp_length <- changeNA(datacl$emp_length, "missing")

#calculate the month
date <- as.Date(paste("1-", data[,"earliest_cr_line"], sep=""),format="%d-%b-%Y")
datacl[,"earliest_cr_line"] <- mondf(date, "2015-04-01")




# Building Classifiers =================================
result <- matrix(NA, ncol=3, nrow=3)
colnames(result) <-c("m1","m2","m3")

#********** model1: Lasso Logistic Regression ******************* 
result_lasso <- rep(NA, 3)

for(i in 1:ncol(split)){
  # prepare the train/test splits
  testid <- split[, i]
  ind <- which(datacl$id %in% testid)
  test <- data[ind,]
  train <- data[-ind,]
  
  # One hot encoding
  



}








#********** model2 ******************* 



#********** model3 ******************* 

# colnames(output) <- c("id", "prob")
# outname <-  paste("mysubmission_test",i,".txt",sep="")
# write.table(output, file=outname, row.names = FALSE, sep=",", col.names = TRUE)

# Build the final classifier======================


#Model Evaluation=================================
testq3 <- read.csv ("LoanStats_2018Q3.csv")
testq4 <- read.csv ("LoanStats_2018Q4.csv")

# write.table(mysubmission_2018Q3.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)
# write.table(mysubmission_2018Q4.txt, file=outname, row.names = FALSE, sep=",", col.names = TRUE)





