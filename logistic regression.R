traid=read.csv("C:/Users/Sanjeet/Documents/R/code/crashTest_1.csv",row.names = 1)
testd=read.csv("C:/Users/Sanjeet/Documents/R/code/crashTest_1_TEST.csv",row.names = 1)
library(caret)

lr =glm(traid$CarType~., family = 'binomial', data=traid)    # change CarType to the name of your dependant coloumn name
lrprob= predict(lr, type = 'response')
plot(lrprob)
tapply(lrprob, traid$CarType, mean)    # change CarType to the name of your dependant coloumn name

lrprob2 =predict(lr, newdata = testd ,type = 'response')
plot(lrprob2)
tapply(lrprob2, testd$CarType, mean)    # change CarType to the name of your dependant coloumn name

testd[lrprob2<0.5,"lrprob2"] <- "Hatchback"
testd[lrprob2>0.5,"lrprob2"] <- "SUV"

#View(testd)
cm = confusionMatrix(table(testd[,7],testd[,6]), positive = "Hatchback")
print(cm)
