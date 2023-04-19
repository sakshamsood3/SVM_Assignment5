library(kernlab)
#Loading the dataset
data=read.csv("D:\\R Programming\\Data Science\\sensor_readings_24.csv")
#Checking for missing values
sum(is.na(data))
#Displaying the top 5 rows
head(data)
#Getting a summary of entire dataset
summary(data)
#Sampling
dim(data)
# we have around 5450 instances and we need 10 samples so we will take sample size say n to be=545
#Let's sample the data using Simple Random Sampling



turns<-c("Move-Forward","Slight-Right-Turn","Sharp-Right-Turn","Slight-Left-Turn")
data$Class=match(data$Class,turns)

for (i in 1:10) {
  set.seed(i) 
  vec<-sample(x=nrow(data), size=round(nrow(data)/10,2),replace=FALSE)
  varname<-paste('sample',as.character(i),sep="_")
  assign(varname,data[vec,])
}

print(sample_1)
print(sample_2)
print(sample_3)
print(sample_4)
print(sample_5)
print(sample_6)
print(sample_7)
print(sample_8)
print(sample_9)
print(sample_10)


dim(sample_1)





# Now for the SVM parameter Optimisation


testParam=function(sample,k,n,e){
  model=ksvm(sample$Class~.,data=sample,kernel=k,nu=n,epsilon=e,kpar=list(),type="C-svc")
  return(model)
  
}



paramOpt=function(sample){
  train<-sample(x=nrow(sample), size=round(nrow(sample)*0.7),replace=FALSE)
  train_df <- sample[train, ]
  test_df <- sample[-train, ]
  
  bestAcc=0
  bestKernel=""
  bestNu=0
  bestEpsilon=0
  iteration=1000
  accuracy=0
  kernels=c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')
  results <- data.frame(iteration = numeric(), accuracy = numeric())
  for(i in 1:1000){
    k=kernels[sample(length(kernels),1)]
    n=runif(1)
    e=runif(1)
    print(paste("i:",i," k:",k," n:",n," e:",e))
    model=testParam(train_df,k,n,e)
    predicted=round(predict(model,test_df))
    accuracy=round(mean(as.numeric(test_df$Class==predicted))*100,2)
    
    if(accuracy  > bestAcc ){
      bestKernel=k
      bestNu=n
      bestEpsilon=e
      bestAcc=accuracy
    }
    print(bestAcc)
    results <- rbind(results, data.frame(iteration = i, accuracy = bestAcc))
  }
  write.csv(results, file = "finalres.csv", row.names = FALSE)
  print(bestKernel)
  print(bestAcc)
  print(bestEpsilon)
  print(bestNu)
  return(bestAcc)
  
}

print(paramOpt(sample_1))


print(paramOpt(sample_2))

print(paramOpt(sample_3))

print(paramOpt(sample_4))

print(paramOpt(sample_5))

print(paramOpt(sample_6))

print(paramOpt(sample_7))

print(paramOpt(sample_8))

print(paramOpt(sample_9))

print(paramOpt(sample_10)) 



