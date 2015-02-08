lbw <- read.table("G://lbw_dat.txt", header=TRUE)
attach(lbw)
lbw[1:2,]
#low age lwt race smoke ptl ht ui ftv bwt
#1 0 19 182 2 0 0 0 1 0 2523
#2 0 33 155 3 0 0 0 0 3 2551


#-----R implementation of logistic regression : gradient descent ------
sigmoid <- function(z) 1/(1 + exp(-1*z))

X <- cbind(age,lwt, smoke, ht, ui)
X.orig <- X
X <- scale(X.orig) # scaling improves convergence

my.logistic<-function(par, X,y, alpha, plot=FALSE)
{
n <- ncol(X)
m <- nrow(X)
ll<- rep(NA, m)
theta_all <- matrix(NA, n, m)

X<-cbind(1,X)
#theta <- c(1.39, -0.034, -0.01, 0.64, 1.89, 0.88) #glm estimates as starting values
theta_all<-theta
for (i in 1:m) 
{ 
dim(X)
length(theta)
hx <- sigmoid(X %*% theta) # matrix product
theta <- theta + alpha * (y - hx)[i] * X[i, ]
logl <- sum( y * log(hx) + (1 - y) * log(1 - hx) ) #direct multiplication

ll[i] <- logl

theta_all = cbind(theta_all, theta)
}

if(plot) {
	par(mfrow=c(4,2))
	plot(na.omit(ll))
	lines(ll[1:i])

	for (j in 1:6)
	{
	plot(theta_all[j, 1:i])
	lines(theta_all[j, 1:i])
	} 
}

return(list(par=theta, loglik=logl))
}

theta <-rep(0,6) # intercept and 5 regerssors
delta <- 1
while (delta > 0.0001) {
ans <- my.logistic(theta, X,low, alpha=0.0005,plot=TRUE)
theta.new <- ans$par
delta <- max(abs(theta - theta.new))
theta <- theta.new
}
ans  
# you can multiply coefficients by std. dev to get back original coeffs  

==============

