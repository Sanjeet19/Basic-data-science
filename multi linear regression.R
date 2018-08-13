a=readline()
datas = read.csv(a)
# View(datas)
plot(datas, main='pair wise scatter plot')

l=lm(Price~.,data=datas);# change Price to the name of your dependant coloumn name
round(cor(datas),3)

datac = datas
a=l

repeat{
  View(colnames(datac))
  print(summary(a))
  z <- readline()
  z <- as.integer(z)
  if(z==0){
    break
  } else{
    datac=datac[,-z]
    a=lm(Price~.,data=datac)# change Price to the name of your dependant coloumn name

  }

}
plot(a)
