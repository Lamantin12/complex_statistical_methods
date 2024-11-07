# Problem 2
# 1. Simulate a dataset of 500 observations from the Gamma distribution 
# with the shape pa- rameter 10 and the rate parameter 1. 
# Set seed to 2425 to ensure comparability.
set.seed(2425)
alpha <- 10
betta <- 1
gamma_data <- rgamma(500, shape = alpha, rate = betta)
# (a) Obtain a parametric estimator for the density of these data, 
# employing moment estimators for the parameters. (1 point)

# Estimators
# alpha := Mean(X, I)^2/Variance(X, I), beta := Variance(X, I)/Mean(X, I)

sample_mean <- mean(gamma_data)
sample_variance <- var(gamma_data)
alpha_hat <- (sample_mean^2) / sample_variance
beta_hat <- sample_mean / sample_variance

hist(gamma_data, freq=FALSE, ylim=c(0, 0.2), 
     main = "Histogram with parametric estimation",)
curve(dgamma(x, shape = alpha_hat, rate = beta_hat), add = TRUE, col = 2, lwd = 2)
legend("topright", legend = "parametric estimation",
       col = 2, lwd = 2)

# (b) Obtain a kernel density estimator with Epanechnikov kernel and a 
# bandwidth chosen by cross-validation. Ensure that the cross-validation 
# criterion has a global minimum by plotting it on a suitable range of 
# bandwidths. (2 points)

# common functions
#kernel density estimator
kde <- function(x,h,ker) apply(ker(outer(x,x,"-")/h),2,mean)/h
#cross-validation
CV<-function(h,X,kern)
{
  n=length(X)
  K=kern(outer(X,X,"-")/h)
  diag(K)=rep(0,n)
  integrand<-function(x) (mean(kern((X-rep(x,n))/h))/h)^2
  CV=integrate(Vectorize(integrand),lower=-Inf,upper=Inf,subdivisions=2000, rel.tol=1e-3)$value-2*sum(K)/(n*(n-1)*h)
}


# Kernel
kernel1 <- function(x) ifelse(abs(x) <= 1, 0.75 * (1 - x^2), 0)
plot(kernel1, -2, 2, main = "Epanechnikov kernel function")
grid=100
X=sort(gamma_data)
h1.grid=seq(0.01,6,length=grid)
cv=rep(NA,grid)
cv=Vectorize(CV,vectorize.args=c("h"))(h1.grid,X,kernel1)
#find minimum 
h1.cv=optimize(CV,interval=c(1,10),X=X,kern=kernel1)$minimum
print(h1.cv)
#plot kde with h.cv and h.Jh
plot(X,kde(X,h=h1.cv,ker=kernel1),lwd=2,type="l",ylim=c(0,0.2),xlab="x",ylab="KDE")
rug(X)
lines(X,dgamma(X, alpha, betta),lwd=2,col=2)
legend("topleft",col=c(1,2),lty=1,c("CV","true"),lwd=2)

print(cat("bandwidth", h1.cv))

?cat

#(c) Obtain a kernel density estimator as in (b), replacing Epanechnikov kernel by a kernel
#of order 3 (1 point)

# Kernel
kernel2 <- function(x) ifelse(abs(x) <= 1, 45/32*(1 - 10/3*x^2 + 7/3*x^4), 0)
plot(kernel2, -2, 2, main = "3-rd order kernel function")
grid=100
X=sort(gamma_data)
h2.grid=seq(0.01,6,length=grid)
cv=rep(NA,grid)
cv=Vectorize(CV,vectorize.args=c("h"))(h2.grid,X,kernel2)
#find minimum 
h2.cv=optimize(CV,interval=c(1,10),X=X,kern=kernel2)$minimum
print(h2.cv)
#plot kde with h.cv and h.Jh
plot(X,kde(X,h=h2.cv,ker=kernel2),lwd=2,type="l",ylim=c(0,0.2),xlab="x",ylab="KDE")
rug(X)
lines(X,dgamma(X, alpha, betta),lwd=2,col=2)
legend("topleft",col=c(1,2),lty=1,c("CV","true"),lwd=2)


#Plot estimators obtained in (a) – (c) together with the true density on one plot, 
#putting a legend. Comment on the results. Compare also the bandwidths obtained in (b) and (c): 
#which one is larger and why (give theoretical justification)? (2 points)

plot(X,kde(X,h=h1.cv,ker=kernel1),lwd=2,type="l",ylim=c(0,0.2),xlab="x",ylab="KDE", col=1)
lines(X,kde(X,h=h2.cv,ker=kernel2),lwd=2, col=2)
rug(X)
lines(X,dgamma(X, alpha, betta),lwd=2,col=3)
legend("topright",col=c(1, 2, 3),c("epanechnikov", "3rd order", "true"),lwd=2)
h1.cv
h2.cv



set.seed(2425)
n_sim <- 300
n_samples <- 500
alpha <- 10
betta <- 1

mse_parametric <- numeric(length(n_samples))
mse_epanechnikov <- numeric(length(n_samples))
mse_order3 <- numeric(length(n_samples))

for (i in 1:n_sim) {

  sample_data <- sort(rgamma(n_samples, alpha, betta))
  x_vals = seq(0, max(sample_data), length.out = n_samples)

  sample_mean <- mean(sample_data)
  sample_var <- var(sample_data)
  alpha_hat <- sample_mean^2 / sample_var
  beta_hat <- sample_mean / sample_var
  parametric_density <- dgamma(sample_data, shape = alpha_hat, rate = beta_hat)
  
  kde_epanechnikov <- kde(sample_data,h=h1.cv,ker=kernel1)
  
  kde_order3 <- kde(sample_data,h=h2.cv,ker=kernel2)
  
  true_density <- dgamma(sample_data, shape = alpha, rate = betta)
  
  mse_parametric <- mse_parametric + (parametric_density - true_density)^2
  mse_epanechnikov <- mse_epanechnikov + (kde_epanechnikov - true_density)^2
  mse_order3 <- mse_order3 + (kde_order3 - true_density)^2
}

mse_parametric <- mse_parametric / n_sim
mse_epanechnikov <- mse_epanechnikov / n_sim
mse_order3 <- mse_order3 / n_sim

plot(x_vals, mse_parametric, type = "l", col = "blue", lwd = 2, ylim=c(0, 1e-4),
     xlab = "x", ylab = "Mean Squared Error (MSE)", main = "MSE of Density Estimators")
lines(x_vals, mse_epanechnikov, col = "red", lwd = 2)
lines(x_vals, mse_order3, col = "green", lwd = 2)
legend("topright", legend = c("Parametric", "Epanechnikov KDE", "Order-3 KDE"),
       col = c("blue", "red", "green"), lwd = 2)




# Problem 3
#Read the dataset “Kenya DHS” into R and consider the variable breastfeeding, 
#which gives the duration of breastfeeding in months. 
#First, redefine this variable to give the duration in years and plot its histogram. 
#Argue why and at which boundary a correction necessary. 
#Next, estimate the density of this variable employing a kernel density estimator
#with Epanechnikov kernel and an appropriate boundary correction suggested 
#by Gasser, T. and Mu ̈ller, H.G. (1979), as given in Lecture 4.
#Set the bandwidth to h = 0.8. Compare this boundary corrected estimator 
#with a usual kernel density estimator, that uses the same kernel and the
#same bandwidth, putting both on the histogram. Comment on the results. (6 points)

?density

setwd("/Users/khodosevichleo/Desktop/HauptUni/1sem/CSM/Excercises/")
data=read.table("KenyaDHS.txt",header=TRUE)
attach(data)
head(data)

data$breastfeeding <- data$breastfeeding / 12
hist(data$breastfeeding)
# We need to use boundary correction at point 2, as there will be a huge drop in density function
X <- sort(data$breastfeeding)
# common functions
#kernel density estimator
kde <- function(x,h,ker) apply(ker(outer(x,x,"-")/h),2,mean)/h
kernel1 <- function(x) ifelse(abs(x) <= 1, 0.75 * (1 - x^2), 0)
#cross-validation
h = 1
hist(X, freq=FALSE)
lines(X, kde(X, h, kernel1))
#lines(density(X,kernel="epanechnikov"),lwd=2,type="l", xlab="x",ylab="KDE")
legend("topright",col=c(1),lty=1,c("kde"),lwd=2)



ker <- kernel1
moments <- function(alpha,degree)
{
  integrand <- function(x) {x^degree*ker(x)}
  integrate(integrand,-1,alpha)
}
x <- seq(-1,1,length=300)
bKernel <- function(alpha,x)
{
  (moments(alpha,2)$value-moments(alpha,1)$value*x)*ker(x)*(x<=alpha&x>=-1)/(moments(alpha,0)$value*moments(alpha,2)$value-(moments(alpha,1)$value)^2)
}

x <- de <- sort(data$breastfeeding)
h=2

for(i in 1:length(x))
{
  if (x[i]>h)
    de[i] <- sum(ker((x[i]-x)/h))/(h*n)
  else
  {
    alpha <- x[i]/h
    de[i] <- sum(bKernel(alpha,(x[i]-x)/h))/(h*n)
  }
}

hist(x, freq=FALSE, ylim=c(0, 0.5))
lines(x, de, lwd=2, xlab="x",ylab="KDE")
legend("topright",col=c(1),lty=1,c("kde"),lwd=2)
