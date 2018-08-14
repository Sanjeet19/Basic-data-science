a <- readline()
trde=read.csv(a, row.names = 1)

k.max=nrow(trde)/2
wss=rep(NA, k.max)
nClust=list()
for(i in 1:k.max){
  driveClass=kmeans(trde,i)
  wss[i]=driveClass$tot.withinss
  nClust[[i]]=driveClass$size
}

plot(1:k.max,wss,type='b',pch=19,xlab="no of clusters",ylab="total within")

z <- readline('input numbeer of clusters : ')
z <- as.integer(z)
kmc= kmeans(trde,z)
print(kmc)