a=readline()
datas<-read.delim(a,row.names=1)
# View(datas)
# str(datas)
plot(datas[,1],datas[,2],xlab = row.names(2),ylab = row.names(1))

l=lm(datas[,2]~datas[,1])
plot(datas[,1],datas[,2],xlab = row.names(2),ylab = row.names(1))
abline(l)

sse = sum((datas[,2]-l$fitted.values)^2)
ssr = sum((l$fitted.values-mean(datas[,2]))^2)
n =nrow(datas)
Fstat= (ssr/sse)*(n-2)

plot(l$fitted.values,rstandard(l),xlab = row.names(2),ylab = row.names(1))
abline(h=-2)
abline(h=2)


l1 = l
repeat{
  identify(l1$fitted.values,rstandard(l1),main='residual analysis', xlab='predicted price values', ylab='standardized residuals')
  z <- readline()
  z <- as.integer(z)
  if(z==0){
    break
  } else{
    datas = datas[-z,]
    l1 = lm(datas[,2]~datas[,1])
    
    plot(l1$fitted.values,rstandard(l1),main='residual analysis', xlab='predicted price values', ylab='standardized residuals')
    abline(h=2,lty=2)
    abline(h=-2,lty=2)
  }

}

plot(datas$CouponRate,datas$BidPrice,xlab = row.names(2),ylab = row.names(1))
abline(l1)
summary(l1)
