x = read.table('C:/Users/bhara/Downloads/nci.data.txt')

x = t(x)
yup = read.table('C:/Users/bhara/Downloads/label.txt')
y = nrow(unique(yup))

#single
distmatrix = as.matrix(dist(x))
#distmatrix[distmatrix--0] = NA
cluster = list()
for (item in 1:nrow(distmatrix)){cluster[[item]]=item}

status = c(rep(1,nrow(distmatrix)))

clustermatrix = matrix(NA,2*nrow(distmatrix)-y,2*nrow(distmatrix)-y)
clustermatrix[1:nrow(distmatrix),1:ncol(distmatrix)] = distmatrix
fmclustermatrix = clustermatrix

for (item in 1:(nrow(distmatrix)-y)){
  minimum = which(fmclustermatrix-min(fmclustermatrix,na.rm = TRUE)==0,arr.ind = TRUE)
  cluster[[nrow(distmatrix)+item]] = c(cluster[[minimum[nrow(minimum),1]]],cluster[[minimum[nrow(minimum),2]]])
  status[minimum[nrow(minimum),]] = 0
  status[nrow(distmatrix)+item] = 1
  #single
  cluslength = length(cluster)
  newcluster = cluster[[length(cluster)]]
  for (i in c(1:(cluslength-1))[-newcluster]){
    temp = c()
    temp.c = 1
    for (j in cluster[[i]]){
      for (k in newcluster){
        temp[temp.c] = distmatrix[j,k]
        temp.c = temp.c+1
      }
    }
    clustermatrix[cluslength,i] = min(temp)
    clustermatrix[i,cluslength] = min(temp)
  }
  #end of single
  fmclustermatrix[minimum[nrow(minimum),],] = NA
  fmclustermatrix[,minimum[nrow(minimum),]] = NA
  fmclustermatrix[(nrow(distmatrix)+item),1:nrow(distmatrix)+item] = clustermatrix[(nrow(distmatrix)+item),1:nrow(distmatrix)+item]
  fmclustermatrix[1:(nrow(distmatrix)+item),1:nrow(distmatrix)+item] = clustermatrix[1:(nrow(distmatrix)+item),(nrow(distmatrix)+item)]
}
final = cluster[status==1]
final1 = c()
for (item in 1:length(final)) final1[final[[item]]]

print(final1)
