x = read.table('C:/Users/bhara/Downloads/nci.data.txt')

x = t(x)
yup = read.table('C:/Users/bhara/Downloads/label.txt')
y = nrow(unique(yup))


distmatrix = as.matrix(dist(x))
#distmatrix[distmatrix-0] = NA

#for (item in 1:nrow(x))(clusters[[item]]=item)
#check = c(rep(1,nrow(x)))
N=nrow(distmatrix)
#new = matrix(NA,2*nrow(x)-y,2*nrow(x)-y)
#new[1:nrow(distmatrix),1:ncol(distmatrix)] = distmatrix
diag(distmatrix)=Inf
#dmx1 = new
n= -(1:N)
new = matrix(0,nrow = N-1,ncol = 2)
clusters = rep(0,N-1)
#single
for(j in 1:(nrow(distmatrix)-1)){
  clusters[j] = min(distmatrix)
  minimum = which(distmatrix - clusters[j] == 0,arr.ind=TRUE)
  minimum = minimum[1,,drop=FALSE]
  p = n[minimum]
  p= p[order(p)]
  new[j,]=p
  group = c(minimum,which(n %in% n[minimum[1,n[minimum]>0]]))
  n[group] = j
  #Specifiy min for single
  r = apply(distmatrix[minimum,],2,min)
  distmatrix[min(minimum),]=distmatrix[,min(minimum)]=r
  distmatrix[min(minimum),min(minimum)] = Inf
  distmatrix[max(minimum),] = distmatrix[,max(minimum)]=Inf
}
#[1] 38.23033 38.59604 39.10562 45.15158 45.35338 45.44295 51.43823 56.78015 56.96831 57.91726
#[11] 60.35052 60.49651 60.80441 61.55425 61.63750 61.92928 62.17806 62.42947 63.26414 63.42863
#[21] 63.57608 63.74588 64.19223 64.37262 65.12892 65.29849 65.47271 65.70067 65.77107 65.91205
#[31] 65.93815 66.48708 66.90878 67.83789 68.16588 68.20512 68.58010 68.81308 69.02726 69.98456
#[41] 69.98967 70.51728 70.93760 71.27390 71.74471 72.55173 72.85035 73.56083 74.00466 74.17665
#[51] 74.90674 76.67937 77.13962 78.72363 78.90728 79.50676 80.19941 80.35100 80.49364 81.30138
#[61] 81.66619 83.23252 93.06565

#complete
for(j in 1:(nrow(distmatrix)-1)){
  clusters[j] = min(distmatrix)
  minimum = which(distmatrix - clusters[j] == 0,arr.ind=TRUE)
  minimum = minimum[1,,drop=FALSE]
  p = n[minimum]
  p= p[order(p)]
  new[j,]=p
  group = c(minimum,which(n %in% n[minimum[1,n[minimum]>0]]))
  n[group] = j
  #Specifiy max for complete
  r = apply(distmatrix[minimum,],2,max)
  distmatrix[min(minimum),]=distmatrix[,min(minimum)]=r
  distmatrix[min(minimum),min(minimum)] = Inf
  distmatrix[max(minimum),] = distmatrix[,max(minimum)]=Inf
}

#[1]  38.23033  39.10562  39.99990  45.15158  45.35338  48.01163  51.43823  56.78015  57.91726
#[10]  60.49651  61.55425  61.63750  62.17806  62.78163  63.26414  63.67441  65.76414  66.23110
#[19]  67.07313  67.83789  68.60602  68.99669  70.62273  70.93760  71.59949  72.56828  73.56083
#[28]  73.67670  74.62701  74.80702  75.92471  76.63179  77.06838  77.32029  78.42222  78.55289
#[37]  78.90728  80.11985  80.19941  80.28908  80.35100  80.97092  81.89804  82.16220  83.83983
#[46]  84.13402  84.32319  86.31141  88.85328  89.33675  90.13573  92.58579  92.65795  92.70719
#[55]  97.94831  99.76405  99.99019 101.82696 105.92310 106.43871 111.51307 118.25973 138.15045

#average
for(j in 1:(nrow(distmatrix)-1)){
  clusters[j] = min(distmatrix)
  minimum = which(distmatrix - clusters[j] == 0,arr.ind=TRUE)
  minimum = minimum[1,,drop=FALSE]
  p = n[minimum]
  p= p[order(p)]
  new[j,]=p
  group = c(minimum,which(n %in% n[minimum[1,n[minimum]>0]]))
  n[group] = j
  #Specifiy mean for average
  r = apply(distmatrix[minimum,],2,mean)
  distmatrix[min(minimum),]=distmatrix[,min(minimum)]=r
  distmatrix[min(minimum),min(minimum)] = Inf
  distmatrix[max(minimum),] = distmatrix[,max(minimum)]=Inf
}

#[1]  38.23033  39.10562  39.29797  45.15158  45.35338  46.72729  51.43823  56.78015  57.91726
#[10]  60.32136  60.49651  61.55425  61.63750  62.17806  62.60555  63.26414  65.36187  65.74220
#[19]  65.76414  65.81495  67.22052  67.37998  67.46270  69.12749  69.95764  70.61731  70.84073
#[28]  70.93760  71.27390  71.99333  72.85917  73.10218  73.56083  73.56432  74.91283  75.21517
#[37]  75.61619  76.27664  77.03851  77.06838  78.14310  79.03982  79.16951  79.78302  80.19941
#[46]  80.35100  80.97092  81.11220  82.76091  84.42031  85.36822  85.48046  89.60358  89.94723
#[55]  91.15119  93.00696  93.35947  95.70737  96.13087  97.75827  99.59912 104.11134 109.34401

#centroid
for(j in 1:(nrow(distmatrix)-1)){
  clusters[j] = min(distmatrix)
  minimum = which(distmatrix - clusters[j] == 0,arr.ind=TRUE)
  minimum = minimum[1,,drop=FALSE]
  p = n[minimum]
  p= p[order(p)]
  new[j,]=p
  group = c(minimum,which(n %in% n[minimum[1,n[minimum]>0]]))
  n[group] = j
  #Specifiy median for centroid
  r = apply(distmatrix[minimum,],2,median)
  distmatrix[min(minimum),]=distmatrix[,min(minimum)]=r
  distmatrix[min(minimum),min(minimum)] = Inf
  distmatrix[max(minimum),] = distmatrix[,max(minimum)]=Inf
}

#[1]  38.23033  39.10562  39.29797  45.15158  45.35338  46.72729  51.43823  56.78015  57.91726
#[10]  60.32136  60.49651  61.55425  61.63750  62.17806  62.60555  63.26414  65.36187  65.74220
#[19]  65.76414  65.81495  67.22052  67.37998  67.46270  69.12749  69.95764  70.61731  70.84073
#[28]  70.93760  71.27390  71.99333  72.85917  73.10218  73.56083  73.56432  74.91283  75.21517
#[37]  75.61619  76.27664  77.03851  77.06838  78.14310  79.03982  79.16951  79.78302  80.19941
#[46]  80.35100  80.97092  81.11220  82.76091  84.42031  85.36822  85.48046  89.60358  89.94723
#[55]  91.15119  93.00696  93.35947  95.70737  96.13087  97.75827  99.59912 104.11134 109.34401

set.seed(1204)
datacluster = kmeans(x,14)
datacluster
#V1  V2  V3  V4  V5  V6  V7  V8  V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 
#12  12   6   6   6   6  11   6  11   6  14  14  14  14  14  14  14  11  11 
#V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30 V31 V32 V33 V34 V35 V36 V37 V38 
#11  11  11  11   2   2   2   2   2   2   2   2   2   2   8   3   3   3   8 
#V39 V40 V41 V42 V43 V44 V45 V46 V47 V48 V49 V50 V51 V52 V53 V54 V55 V56 V57 
#4   4   8   5   5   5   5   5  10   5   9   9   9   9   2   7   7   1   1 
#V58 V59 V60 V61 V62 V63 V64 
#1  13  13  13  13  13  13



