set.seed(2425)
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
library('ubiquity')
library(svMisc)

# kernel
k=function(x) (3*(1-x^2)/4)*(x<=1&x>=-1)

# local polynomial fit func
locpoly=function(x,y,h,l,ker,der=0){
  n = length(x)
  f=matrix(NA,n,der+1)
  W=rep(0,n)
  for (i in 1:length(x)){
    X=rep(1,n)
    XX=as.vector(outer(x,x[i],"-"))/h
    for (j in 1:l) X=cbind(X,XX^j/factorial(j))
    fit.lm=lm(y~X-1,weights=ker(XX))
    
    f[i,]=(fit.lm$coef)[1:(der+1)]
    H=influence(fit.lm, do.coef=FALSE)$hat
    index=which(labels(H)==i)
    W[i]=H[index]
  }
  return(list(f=f,W=W))
}

# GCV func
GCV=function(x,y,l,ker){
  gcv=function(x,y,h,l,ker){
    fit.lp=locpoly(x,y,h,l,ker)
    return(sum((y-fit.lp$f[,1])^2)/(1-mean(fit.lp$W))^2)
  }
  return(optimize(gcv,interval=c(0.001,1),x=x,y=y,l=l,ker=ker)$minimum)
}

AIC=function(x,y,l,ker){
  gcv=function(x,y,h,l,ker){
    fit.lp=locpoly(x,y,h,l,ker)
    return(log(sum((y-fit.lp$f[,1])^2)) + (2*mean(fit.lp$W)))
  }
  return(optimize(gcv,interval=c(0.001,1),x=x,y=y,l=l,ker=ker)$minimum)
}




n = 300
x = linspace(0, 1, 300)
f = function(x){3*x^3 * sin(4*pi*x) * tan(x*pi/4)}
y = f(x) + 0.3*rnorm(n)
plot(x,y,pch=18,cex=0.8)




n = 300
x = linspace(0, 1, 300)
f = function(x){3*x^3 * sin(4*pi*x) * tan(x*pi/4)}
y = f(x) + 0.3*rnorm(n)
plot(x,y,pch=18,cex=0.8)
h.gcv = AIC(x,y,l=1,ker=k) 
fit.lp = locpoly(x, y, h=h.gcv, l = 1, ker=k)
lines(x,fit.lp$f[,1],col=3,lwd=3)
h.gcv



n = 300
x = linspace(0, 1, n)
f = function(x){3*x^3 * sin(4*pi*x) * tan(x*pi/4)}
y = f(x) + 0.3*rnorm(n)
clrs = c("black","red", "green", "yellow")
plot(x, f(x), type='n', ann=F)
lines(linspace(0, 1, 1000), f(linspace(0, 1, 1000)))
h_list = c()
for (ll in 1:3) {
  h.aic = AIC(x,y,l=ll,ker=k) 
  fit.lp = locpoly(x, y, h=h.aic, l=ll, ker=k)
  lines(x,fit.lp$f[,1],lwd=1, col=clrs[ll+1])
  print(c(ll, h.aic))
  h_list = c(h_list, h.aic)
}
legend(0, -0.5, legend=c("function", "l=1", "l=2", "l=3"),  
       fill = clrs)





n = 300
x = linspace(0, 1, n)
df = function(x){3/4 * x^2 * (pi*x*1/cos((pi*x)/4)^2*sin(4*pi*x) + 4*(4*pi*x*cos(4*pi*x) + 3*sin(4*pi*x))*tan((pi*x)/4))}
y = f(x) + 0.3*rnorm(n)
clrs = c("black","red", "green", "yellow")
plot(x, df(x), type='n', ann=F)
lines(linspace(0, 1, 1000),df(linspace(0, 1, 1000)))
for (ll in 1:3) {
  h.aic = h_list[ll]#AIC(x,y,l=ll,ker=k)
  fit.lp = locpoly(x, y, h=h.aic, l=ll, ker=k, der=1)
  lines(x,fit.lp$f[,2],lwd=1, col=clrs[ll+1])
  print(c(ll, h.aic))
}
legend(0, 30, legend=c("function", "l=1", "l=2", "l=3"),  
       fill = clrs)



n = 300
n_samples = 200
x = linspace(0, 1, n)
f = function(x){3*x^3 * sin(4*pi*x) * tan(x*pi/4)}
y = f(x) + 0.3*rnorm(n)
clrs = c("red", "green", "yellow")

mse_1 = replicate(n, 0)
mse_2 = replicate(n, 0)
mse_3 = replicate(n, 0)
for (i in 1:n_samples) {
  break # remove to compute
  y = f(x) + 0.3*rnorm(n)
  
  ll = 1
  h.aic = AIC(x,y,l=ll,ker=k)
  fit.lp = locpoly(x, y, h=h.aic, l=ll, ker=k)
  mse_1 = mse_1 + (fit.lp$f[,1] - f(x))**2
  
  ll = 2
  h.aic = AIC(x,y,l=ll,ker=k)
  fit.lp = locpoly(x, y, h=h.aic, l=ll, ker=k)
  mse_2 = mse_2 + (fit.lp$f[,1] - f(x))**2
  
  ll = 3
  h.aic = AIC(x,y,l=ll,ker=k)
  fit.lp = locpoly(x, y, h=h.aic, l=ll, ker=k)
  mse_3 = mse_3 + (fit.lp$f[,1] - f(x))**2
  
  progress(i, n_samples)
}
mse_1 = mse_1 / n_samples
mse_2 = mse_2 / n_samples
mse_3 = mse_3 / n_samples



# did that so computation is reduced
load("mse_1.RData")
load("mse_2.RData")
load("mse_3.RData")