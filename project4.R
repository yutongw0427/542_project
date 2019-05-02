installIfNeeded <- function(cliblist){
  libsNeeded <- cliblist
  libsNeeded <- libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}
installIfNeeded(c("car"))
# prepare the Data======================
setwd("~/Desktop/542/project/Project4")
split <- read.csv("Project4_test_id.csv")
data <- read.csv("LoanStats_2007_to_2018Q2.csv")
library(car)
data[,"loan_status"] <- Recode(data[,"loan_status"],
                   "c('Fully Paid') = 0;
                    c('Default','Charged Off') = 1")
rm <- c("zip_code","emp_title", "title")
datacl <- data[ , !(names(data) %in% rm)]

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


