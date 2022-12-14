ExNRule <- function(xtrain, xtest, ytrain, k=3, r=500, p=round(sqrt(ncol(xtrain)))){

  Mode <- function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  #################
  EKNN <- function(xtrain, ytrain, xtest, k=k){

    ytrain <- as.numeric(as.factor(ytrain))-1
    knn1 <- get.knnx(xtrain, xtest, k=1)$nn.index
    knnk <- get.knn(xtrain, k=k)$nn.index

    j <- 1
    class <- c()
    repeat{
      NN1 <- knn1[j]
      myN1 <- knn1[j]
      i <- 1
      myN <- c()
      repeat{
        myN2 <- c(NN1, myN, knnk[myN1,1:(i+1)])
        myN <- (myN2[which(!duplicated(myN2))])[1:(i+1)]
        myN1 <- myN[(i+1)]
        i <- i+1
        if(i == k){
          break
        }
      }
      class[j] <- Mode(ytrain[myN[1:k]])
      j <- j+1
      if(j > nrow(xtest)){
        break
      }
    }

    return(class)
  }
  ###################
  mod <- function(xtrain, xtest, ytrain, k=k, p=p){
    boot <- sample(1:nrow(xtrain), nrow(xtrain), replace = T)
    feat <- sample(1:ncol(xtrain), p, replace = F)
    my.pred <- EKNN(xtrain[boot,feat], ytrain[boot], xtest[,feat], k=k)
    return(my.pred)
  }
  yy3 <- replicate(r, mod(xtrain, xtest, ytrain, k=k, p=p))
  res = list()
  res$class <- factor(apply(yy3, 1, Mode), levels = c("0", "1"))
  res$class.prob <- apply(yy3, 1, function(x){length(x[which(x==1)])/length(x)})
  return(res)
}
