library(e1071)
library(parallel)
library(nnet)
samples <- read.csv("agaricus-lepiota.data",sep=",",header=F)
features <- read.csv("feature_names",header=F)
colnames(samples) <- c("Type",as.character(features[[1]]))
mshr <- samples[,-which(colnames(samples)=="stalk-root")] # with missing values
mshr <- mshr[,-which(colnames(mshr)=="veil-type")] # same in all samples
for(i in 1:length(mshr)){mshr[,i] <- as.factor(as.numeric(mshr[,i]))} # label encoding

# cross-validation
crossvalidate <- function(k,input,equation,Model,Model.arg=NULL,predict.arg=NULL){
  #k: k-fold
  #Model.arg: other arguments in model
  #predict.arg: other arguments in predict()
  y <- list()
  len <- nrow(input)
  order <- sample(1:len,replace=F)
  acc <- c()
  
  for(i in 1:k){
    if(i<=(k-len%%k)){
      test_index <- order[((i-1)*(len%/%k)+1):(i*len%/%k)]
      last <- i*len%/%k
    }else{
      test_index <- order[(last+1):(last+len%/%k+1)]
      last <- last+len%/%k+1
    }
    myData <- input[-test_index,] # training set
    model <- eval(parse(text=paste0("Model(formula=equation,data=myData,",Model.arg,")")))
    #lm_model <- Model(formula = equation,data=myData)
    
    myData <- input[test_index,] # testing set
    result <- list(predict=c(),ans=c())
    #result$predict <- predict(model,myData)
    result$predict <- eval(parse(text=paste0("predict(model,myData,",predict.arg,")")))
    result$ans <- myData[[as.character(equation[[2]])]]
    result <- as.data.frame(result)
    y[[i]] <- result
  }
  return(y)
}
Modeltest<- function(i,k,factors){
  y <- crossvalidate(k=k,mshr,equation=as.formula(paste("Type~odor+`spore-print-color`+habitat+population",factors,"",sep="`")),Model=svm,Model.arg="kernel='linear',scale=F")
  # y <- crossvalidate(k=k,mshr,equation=Type~.,Model=nnet,predict.arg = "type='class'",Model.arg="size = 3, rang = 0.1,decay = 5e-1, maxit = 500,trace=FALSE")
  acc.fold <- c()
  for(j in 1:k){
    acc.fold[j] <- length(which(y[[j]]$predict==y[[j]]$ans))/nrow(y[[j]])
  }
  return(mean(acc.fold))
}

#forward selection
selecfea <- colnames(mshr)[-c(1,6,19,21,20)] # remove "Type" & "odor" & "spore-print-color" & "habitat" & "population"
acc <- c(); acc.temp <- c()
for(i in 1:length(selecfea)){
  acc.temp <- mcmapply(Modeltest,1:100,k=10,factors=selecfea[i],mc.cores=45) # repeat xvalidation 100 times
  acc[i] <- mean(acc.temp)
}

# acc_1 <- acc  # odor+
# acc_2 <- acc  # odor+spore-print-color+
# acc_3 <- acc  # odor+spore-print-color+habitat+
# acc_4 <- acc  # odor+spore-print-color+habitat+population+
