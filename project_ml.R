pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


tr_l <- read.csv('pml-training.csv')

#we have to clean the data first
#not all row, columns are similar
#from exploratory data analysis, we can eliminate columns with NAs in it.

#eliminate rows with new_window = yes (averages of other cols are calculated here)
yes_rows <- which(tr_l$new_window %in% 'yes')
tr_l <- tr_l[-x,]

#now make the training and data sets


#let's subsample the data (a fifth of the sample)
library(caret)
set.seed(1000)
Index_mini <- createDataPartition(tr_l$classe, p = 1,list=FALSE)
mini_tr_l <- tr_l[Index_mini,]
#now do new mini training (60%) and test
set.seed(2000)
inTrain_index_mini <- createDataPartition(mini_tr_l$classe,p=0.6,list=FALSE)
training_mini <- mini_tr_l[inTrain_index_mini,]
testing_mini <- mini_tr_l[-inTrain_index_mini,]

#pruning: looking for the non-zero variables
nzv_training_mini <- nearZeroVar(mini_tr_l, saveMetrics=T)
cols_yes <- NULL
counter <- 0 
for (i in 8:160){
  if (nzv_training_mini$nzv[i]==F){
   yy <- table(is.na(training_mini[,i]))#check for those columns with NA values (are avergaes)
   if (dim(yy)==1){
    counter <- counter + 1
    cols_yes[counter] <- i 
    }
  }
}

#now extract those columns
head
training_mini<- training_mini[,cols_yes]
testing_mini <- testing_mini[,cols_yes]

#PCA
preProc <- preProcess(training_mini[,-53],method='pca',thresh=0.8)
trainPC <- predict(preProc,training_mini[,-53])

#random forest method
modFit <- train(training_mini$classe ~ ., data=trainPC,prox=T,method='rf',do.trace=T)#,ntrees=100)

#get accuracy in test sample
testPC <- predict(preProc,testing_mini[,-53])
classe_test <- predict(modFit,testPC)
confusionMatrix(classe_test,testing_mini$classe)

#now get the tests
tr_t <- read.csv('pml-testing.csv')
#clean up the columns
tr_t <- tr_t[,cols_yes]
tr_t <- tr_t[,-53]

questPC <- predict(preProc,tr_t)
classe_quest <- predict(modFit,questPC)

answers <- NULL
for (i in 1:20){
  
}


