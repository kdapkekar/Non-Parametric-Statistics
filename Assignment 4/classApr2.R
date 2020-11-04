x<-CMB$ell[CMB$ell<400]
y<-CMB$Cl[CMB$ell<400]

plot(x,y)

### regressogram


###bin smoother (boxcar) 


### kernel (a kernel other than boxcar), 
h=floor(.20*398)
mKxy<-function(u,K){
  out<- sum(K((x-u)/h)*y)/sum(K((x-u)/h))
  return(out)
}
K<-function(u){3/4*(1-u^2)*(abs(u)<1)}


xx<-seq(2,399,by=1)
yy<-apply(as.matrix(xx),1,function(x) mKxy(x,K))

plot(x,y,col='grey')
lines(xx,yy,col='blue')


###local linear  regression, loess deg=1


###local quadratic polynomial regression. loess deg =2


## In each case, use cross-validation to choose the amount of smoothing