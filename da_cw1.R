library(ISLR)
data("Auto")
attach(Auto)
sigmoid12 = function(z){
  g= (1+exp((-1)*z))
  return(1/g)
}
orig_1 = as.numeric(origin<2)
orig_3 = as.numeric(origin>2)
origin = as.numeric(origin==2)
data = data.frame(horsepower,weight,year,origin,orig_1,orig_3)
data=scale(data)
high = as.matrix(as.numeric(ifelse(mpg>=23,1,0)))
set.seed(0412)
dimx = dim(data)
rand = floor(runif(dimx[1],1,dimx[1]))
train = data[rand[1:(dimx[1]/2)],]
test = data[rand[((dimx[1]/2)+1):dimx[1]],]
y_train = high[rand[1:(dimx[1]/2)], ]
y_test = high[rand[((dimx[1]/2)+1):dimx[1]],]
dimx = dim(as.matrix(train))
theta = runif(dimx[2],-0.7,0.7)
thetabias = 0
m = dimx[1]
ex6 = c()
for (i in 1:1000){
  h = sigmoid12((as.matrix(train)%*%as.matrix(theta))+thetabias)
  mse = sum((as.matrix(y_train) - h)**2) * (1/m)
  #ex6[i] = mse
  #print(mse)
  #htest = sigmoid12((as.matrix(test)%*%as.matrix(theta))+thetabias)
  #msetest = sum((as.matrix(y_test) - htest)**2) * (1/m)
  #cat(sprintf("\"%f\" \"%f\"\n", mse, msetest))
  one = 2*(as.matrix(y_train)-h)
  two = h*(1-h)
  three= one*two
  theta = theta + ((0.01/m)*(t(as.matrix(train))%*%as.matrix(three)))
  if(mse<0.05){
    break
  }
  
}
#print(ex6)
#boxplot(ex6)
htest = sigmoid12((as.matrix(test)%*%as.matrix(theta))+thetabias)
msetest = sum((as.matrix(y_test) - htest)**2) * (1/m)
hnew = ifelse(htest<=0.5,0,1)
acc = mean(y_test==hnew)
print(paste0("Accuracy of training set: ",acc))
print(paste0("MSE of test set with updated weights: ",msetest))

