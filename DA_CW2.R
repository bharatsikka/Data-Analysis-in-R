library(MASS)
data("Boston")
attach(Boston)
set.seed(0412)
bos = data.frame(lstat,rm)
train = sample(1:nrow(bos), nrow(bos)/2)
test = -train
xtrain = bos[train,]
xtest = bos[test,]
ytrain = as.matrix(medv)[train,]
ytest = as.matrix(medv)[test,]

#Minimum value of lstat 
slstat = seq(1.8,37.9,by = 0.1)
srm  = seq(3.6,8.7,by = 0.1)
xtrainless = c()
xtrainmore = c()

rsslstat = c()
rssrm = c()
#Minimum value of lstat
for(j in 1:length(slstat)){
  yoxtrainless = c()
  yoxtrainmore = c()
  k=1
  h=1
  lowindex = c()
  moreindex = c()
  for(i in 1:length(xtrain$lstat)){
    if(xtrain$lstat[i]<slstat[j]){
      yoxtrainless[k] = ytrain[i]
      lowindex[k] = i
      k = k+1
    }
    else{
      yoxtrainmore[h] = ytrain[i]
      moreindex[h]= i
      h=h+1
    }
  }
  lowlstatmean = sum(yoxtrainless)/length(yoxtrainless)
  highlstatmean = sum(yoxtrainmore)/length(yoxtrainmore)
  low = 0
  high = 0
  for(i in lowindex){
    low = low+(ytrain[i]-lowlstatmean)^2
  }
  for(i in moreindex){
    high=high+(ytrain[i]-highlstatmean)^2
  }
  rsslstat[j]=low+high
}
#Minimum value of rm
for(j in 1:length(srm)){
  yoxtrainless = c()
  yoxtrainmore = c()
  k=1
  h=1
  lowindex = c()
  moreindex = c()
  for(i in 1:length(xtrain$rm)){
    if(xtrain$rm[i]<srm[j]){
      yoxtrainless[k] = ytrain[i]
      lowindex[k] = i
      k = k+1
    }
    else{
      yoxtrainmore[h] = ytrain[i]
      moreindex[h]= i
      h=h+1
    }
  }
  lowrmmean = sum(yoxtrainless)/length(yoxtrainless)
  highrmmean = sum(yoxtrainmore)/length(yoxtrainmore)
  low = 0
  high = 0
  for(i in lowindex){
    low = low+(ytrain[i]-lowrmmean)^2
  }
  for(i in moreindex){
    high=high+(ytrain[i]-highrmmean)^2
  }
  rssrm[j]=low+high
}

minlstat = min(rsslstat)
minrm = min(rssrm)
MSErm = minrm/nrow(xtrain)
print(paste0("MSE of training set: ",MSErm))
indexlstats = which.min(rsslstat)
print(paste0("minimum lstat rss: ",minlstat))
print(paste0("for value of S: ", slstat[indexlstats]))
indexrms = which.min(rssrm)
print(paste0("minimum lstat rss: ",minrm))
print(paste0("for value of S: ", srm[indexrms]))
minS = pmin(slstat[indexlstats],srm[indexrms])
#Minimum RSS is from RM, thereby we will utilise S from minimum RSS value from RM

#Evaluating RSS and MSE for test set
f=1
g=1
yoxtestless = c()
yoxtestmore = c()
lowindex = c()
moreindex = c()

for(i in 1:length(xtest$rm)){
  if(xtest$rm[i]<minS){
    yoxtestless[g] = ytest[i]
    lowindex[g] = i
    g = g+1
  }
  else{
    yoxtestmore[f] = ytest[i]
    moreindex[f]= i
    f=f+1
  }
}
lowrmmn = sum(yoxtestless)/length(yoxtestless)
highrmmn = sum(yoxtestmore)/length(yoxtestmore)
low1 = 0
high1 = 0
for(i in lowindex){
  low1 = low1+(ytest[i]-lowrmmn)^2
}

for(i in moreindex){
  high1= high1+(ytest[i]-highrmmn)^2
}
rsstotal = low1+high1

print(paste0("MSE of test set: ",rsstotal/nrow(xtest)))


#BDS
r = ytrain
fx = c(length(r))
B = 1000
n = 0.01
bdmes = c()
for(j in 1:length(srm)){
  yoxtrainless = c()
  yoxtrainmore = c()
  k=1
  h=1
  lowindex = c()
  moreindex = c()
  for(i in 1:length(xtrain$rm)){
    if(xtrain$rm[i]<minS){
      yoxtrainless[k] = ytrain[i]
      lowindex[k] = i
      k = k+1
    }
    else{
      yoxtrainmore[h] = ytrain[i]
      moreindex[h]= i
      h=h+1
    }
  }
  lowrmmean = sum(yoxtrainless)/length(yoxtrainless)
  highrmmean = sum(yoxtrainmore)/length(yoxtrainmore)
  low = 0
  high = 0
  y=0
  if(xtrain$rm[i]<minS){
    fx[y] = lowrmmean
  }
  else{
    fx[y] = highrmmean
  }
  r[i] = r[i] - n*fx[i]
}
r = Filter(Negate(is.na),r)
msetab = 0
sum(r)/length(ytrain)



for(j in 1:length(srm)){
  yoxtrainless = c()
  yoxtrainmore = c()
  k=1
  h=1
  lowindex = c()
  moreindex = c()
  for(i in 1:length(xtest$rm)){
    if(xtest$rm[i]<minS){
      yoxtrainless[k] = ytest[i]
      lowindex[k] = i
      k = k+1
    }
    else{
      yoxtrainmore[h] = ytest[i]
      moreindex[h]= i
      h=h+1
    }
  }
  lowrmmean = sum(yoxtrainless)/length(yoxtrainless)
  highrmmean = sum(yoxtrainmore)/length(yoxtrainmore)
  low = 0
  high = 0
  y=0
  if(xtest$rm[i]<minS){
    fx[y] = lowrmmean
  }
  else{
    fx[y] = highrmmean
  }
  r[i] = r[i] - n*fx[i]
}
r = Filter(Negate(is.na),r)
msetab = 0
sum(r)/length(ytrain)
