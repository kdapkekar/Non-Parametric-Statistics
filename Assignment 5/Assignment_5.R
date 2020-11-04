# For 'No', numeric value is 0 and for 'yes' it is 1
df1 = data.frame(asthma)
library(stringr)
df1$Asthma = str_replace(df1$Asthma,"Yes",'1')
df1$Asthma = str_replace(df1$Asthma,"No",'0')
df1
df1<-df1[order(df1$Exposure),]
attach(df1)
A<-as.numeric(as.character(df1$Asthma))
E<-df1$Exposure
#asthma$Asthma <- as.numeric(as.factor(asthma$Asthma))
# par(mfrow=c(1,1))
# fitl <- glm(A~E,family="binomial")
# plot(A~E,col='grey',xlab = 'Exposure',ylab="Asthma")
# lines(E,predict(fitl,data.frame(E = E),interval = 'confidence',type="response"),lwd = 0,col="black")

fitl$deviance
library(locfit)
n <- length(A)
k <- seq(10,n,by=10)    #h = k/n 
cv <- mcr <- gcv <- rep(0,length(k))
for (i in 1:length(k)){
  fit <- locfit(A~lp(E,nn=k[i]/n,deg=1),family="binomial",ev=dat(cv=TRUE))
  pihat <- fitted(fit)
  yhat <- (pihat > .5)*1               # using a threshold of .5 to predict y_hat
  mcr[i] <- mean(abs(A-yhat))      # average miss classification error
  cv[i] <- (-2*(sum(log(pihat[A==1 ]))+sum(log(1-pihat[A==0 ])))/n)   #deviance
  #  gcv[i] <- gcv(fit)["gcv"]
  gcv[i]<- (-2*n*fit$dp["lk"]/(n-fit$dp["df2"])^2)
}
par(mfrow=c(1,2))
#plot(k,mcr,type="l",xlab = 'index',ylab="Misclassification rate", ylim=c(.0,.2))
plot(k,cv,type="l",xlab = 'index',ylab="CV using deviance")

par(mfrow=c(1,1))
plot(k,cv,type="l",xlab = 'index',ylab="CV using deviance",main="Cv vs GCV",ylim=c(0.0,1.5))
lines(k,gcv,type="l",xlab = 'index',ylab="GCV", col='blue')
h_cv<-k[which.min(cv)]
which.min(cv)
h_gcv<-k[which.min(gcv)]
which.min(gcv)
#b
fit <-locfit(A~lp(E,nn=h_cv/n,deg = 1),family="binomial",ev=dat(cv=TRUE))
pihat <- fitted(fit)
plot(E,log(pihat/(1-pihat)),type = 'l')

#c
plot(E,fitted(fit),type="l")
plot(fit,band='local',get.data = T,main='with degree=1')
