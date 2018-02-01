# Don't forget to set working directory!
# Put this code file and PhillyCrime.csv file under the same working directory




# Question 1
set.seed(100)
download.file("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/docv_classification.R",
              destfile = "docv_classification.R")
source("docv_classification.R")
PhillyCrime <- read.csv("lecture1/TA1/PhillyCrime.csv")
library(MASS) 
library(kknn)
par(mfrow = c(1,2))
thefts = PhillyCrime[PhillyCrime$Category=="Thefts",]
plot(thefts$X, thefts$Y, cex = 0.1, xlab = "X", ylab = "Y", 
     main = "Thefts", col = "blue")
vandalism = PhillyCrime[PhillyCrime$Category=="Vandalism",]
plot(vandalism$X, vandalism$Y, cex = 0.1, xlab = "X", ylab = "Y", 
     main = "Vandalisim", col = "blue")



# Question 2

# 2.a
compute_error = function(train, test, k_vec){
  error_vec = c()
  for(i in 1:length(k_vec)){
    fit = kknn(Category ~ X + Y, train, test, k = k_vec[i], kernel = "rectangular")
    # create confusion matrix
    results = table(fit$fitted.values, test$Category)
    # compute the probability of error
    error_vec[i] = (results[1,2] + results[2,1]) / dim(test)[1]
  }
  return(error_vec)
}

N = dim(PhillyCrime)[1]
train_ratio = 0.5
train_index = sample(N,train_ratio*N)
train = PhillyCrime[train_index,]
test = PhillyCrime[-train_index,]
k_vec =  1:40
error_vec = compute_error(train, test, k_vec)
plot(k_vec, error_vec)



# 2.b
best_k = k_vec[which.min(error_vec)]
best_k


# 2.c
knn = kknn(Category~X+Y,train,test, k=best_k, kernel = "rectangular")
plot(test$X, test$Y, col = as.numeric(knn$fitted.values), cex = 0.1)
legend("bottomright", c("Thefts","Vandalism"), col = c(1,2), pch = c(1,1))



# Question 3


# Warning. It takes a long time to run


train_ratio = 0.5
error_mat = c()
for(j in 1:20){
  train_index = sample(N,train_ratio*N)
  train = PhillyCrime[train_index,]
  test = PhillyCrime[-train_index,]
  k_vec =  1:40
  error_vec_temp = compute_error(train, test, k_vec)
  error_mat = rbind(error_mat, error_vec_temp)
}

best_k_1 = c()
min_error_1 = c()
for(i in 1:dim(error_mat)[1]){
  best_k_1[i] = k_vec[which.min(error_mat[i,])]
  min_error_1[i] = min(error_mat[i,])
}



# 3.a
best_k_1


# 3.b
min_error_1
mean(min_error_1)
sd(min_error_1)



# Question 4

# 4.a
train_ratio = 0.9
N = dim(PhillyCrime)[1]
train_index = sample(N,train_ratio*N)
train = PhillyCrime[train_index,]
test = PhillyCrime[-train_index,]
k_vec =  1:40
error_vec2 = compute_error(train, test, k_vec)
plot(k_vec, error_vec2)



# 4.b
best_k = k_vec[which.min(error_vec2)]
best_k
knn = kknn(Category~X+Y,train,test, k=best_k, kernel = "rectangular")
plot(test$X, test$Y, col = as.numeric(knn$fitted.values), cex = 0.1)
legend("bottomright", c("Thefts","Vandalism"), col = c(1,2), pch = c(1,1))



# 4.c

# Warning. It takes a long time to run


train_ratio = 0.9
error_mat_2 = c()
for(j in 1:20){
  train_index = sample(N,train_ratio*N)
  train = PhillyCrime[train_index,]
  test = PhillyCrime[-train_index,]
  k_vec =  1:40
  error_vec_temp = compute_error(train, test, k_vec)
  error_mat_2 = rbind(error_mat_2, error_vec_temp)
}

best_k_2 = c()
min_error_2 = c()
for(i in 1:dim(error_mat_2)[1]){
  best_k_2[i] = k_vec[which.min(error_mat_2[i,])]
  min_error_2[i] = min(error_mat_2[i,])
}


best_k_2



# 4.d
mean(min_error_2)
sd(min_error_2)



# Question 6
library(ROCR)
train_ratio = 0.5
train_index = sample(N,train_ratio*N)
train = PhillyCrime[train_index,]
test = PhillyCrime[-train_index,]
knn = kknn(Category~X+Y,train,test, k=25, kernel = "rectangular")
pred = prediction(as.numeric(knn$fitted.values), as.numeric(test$Category))
perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(c(0,1),c(0,1),xlab='False Positive Rate',
     ylab='True Positive Rate',main="ROC curve",cex.lab=1,type="n")
lines(perf@x.values[[1]], perf@y.values[[1]])
