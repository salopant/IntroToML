library(ISLR)
data (OJ)
smp_siz = floor(0.8*nrow(OJ))
set.seed(23)   # set seed to ensure you always have same random numbers generated
train_ind = sample(seq_len(nrow(OJ)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =OJ[train_ind,] #creates the training dataset with row numbers stored in train_ind
test=OJ[-train_ind,]
svmModel <- svm(Purchase~., data = train, kernel = "linear", cost = 0.01)
(testPurch <- predict(svmModel,test))
(sum(testPurch == test$Purchase)/nrow(train))