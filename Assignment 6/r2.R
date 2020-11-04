  ##Assignment6-Part 2: density estimation
  
  1.  Blackboard/Assignments contains a data set from the National Health and Nutrition Examination Survey (NHANES) that lists the triglyceride levels of 3,026 adult women.

```{r} 
#Load the dataset and view it:
nhanes <- read.delim("~/Documents/classes/Non-parametric/DataSets/nhanes.txt")
View(nhanes)
```

(a) Obtain a histogram estimate for the distribution of triglyceride levels in adult women and plot it in two ways. One using Freedman and Diaconis (1981) method for the bin size h, and another with Sturges method. For FD, compute h and m and put the values within the title of the histogram. For Sturges compute h and put it in the title of the histogram.
#F-D
n=length(nhanes$TRG)
m=ceiling(log(n,2))+1
r=IQR(nhanes$TRG)
h=2*r/n^(1/3)
hist(nhanes$TRG,breaks="FD",main=paste("F-D h= ",h,"m= ",m ),probability = TRUE)

#sturges

m<-ceiling(log(length(nhanes$TRG),2)+1)
hist(nhanes$TRG,main=paste("Sturges m= ",m),probability = TRUE)




(b) Obtain a kernel density estimate for the distribution of triglyceride levels in adult women and plot it in two ways. One the normal rule and the other usin band width selected by cross validation (ucv). Make sure the graphs have the value of the bandwidth.
```{r}
```
triglyceride <-nhanes$TRG
library(locfit)
n<-length(triglyceride)
### Select optimal bandwidth of kernel estimators by normal reference rule/Cross-validation/plug-in
sigma.hat <-min(sd(triglyceride), IQR(triglyceride)/1.34)
h.normal <-1.06*sigma.hat/n^(0.2)
h.cv <- bw.ucv(triglyceride)

f1<-density(triglyceride,bw=h.normal) # normal reference
f2<-density(triglyceride,bw=h.cv) # cross-validation

f11<-density(triglyceride,bw="nrd")
f22<-density(triglyceride,bw="ucv")
library(MASS)
plot(f1,ylim=c(0,0.8),main="h normal reference rule");rug(triglyceride)
lines(f11,col="pink")
plot(f2,ylim=c(0,0.8),main="h by ucv"); rug(triglyceride)
lines(f22,col="pink")


(c) Obtain a parametric density estimate assuming that triglyceride levels follow a normal distribution and overlay this density estimate with your estimate from (a). Repeat with a Gamma distribution. This means that you will have to overlay a curve that has the pdf as the y-values. You will have to go back to your probability notes to remember facts about the Gamma, that is, how the parameters relate to the mean and variance.

x = nhanes$TRG
xseq = seq(-3,3,length=100)
Fhat = ecdf(x)
plot(Fhat)
lines(xseq,pnorm(xseq),col="blue",lwd=2)
d<-density(x,bw=.3)
d
lines(d,col='red')
Comment on your findings. 
Conclussion: 
  