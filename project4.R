installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car"))

# Helper Functions ==================================
# One-Hot encoding
PreProcessingMatrixOutput <- function(train.data, test.data){
  # generate numerical matrix of the train/test
  # assume train.data, test.data have the same columns
  categorical.vars <- colnames(train.data)[which(sapply(train.data, 
                                                        function(x) is.factor(x)))]
  categorical.vars <- c(categorical.vars, "Mo_Sold", "Year_Sold")
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
                                                        
                                                        
                                                        
                                                        
# prepare the Data======================
setwd("~/Desktop/542/project/Project4")
split <- read.csv("Project4_test_id.csv")
data <- read.csv("LoanStats_2007_to_2018Q2.csv")
                                                        
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


                                                        
                                                        
                                                                                                          
# prepare the train/test splits======================
#Prepare the data
i <- 1
testid <- split[,i]
test <- data[testid,]
train <- data[-testid,]
train.y <- train$loan_status
test.y <- test$loan_status

# Pick a classification method====================
result <- matrix(NA, ncol=3, nrow=3)
colnames(result) <-c("m1","m2","m3")
#********** model1 ******************* 
library(glmnet)

lr.model <- cv.glmnet(train[,-13], train.y, family="binomial", alpha=1)
lr.probs <- predict(lr.model, newx=dtm_test, type="response")
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
