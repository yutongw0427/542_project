setwd("~/Desktop/542/project/Project4")

split <- read.csv("Project4_test_id.csv")
data <- read.csv("LoanStats_2007_to_2018Q2.csv")
# prepare the train/test splits======================
i <- 1
testid <- split[,i]
test <- data[testid,]
train <- data[-testid,]

# Pick a classification method====================
result <- matrix(NA, ncol=3, nrow=3)
colnames(result) <-c("m1","m2","m3")
#********** model1 ******************* 
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





