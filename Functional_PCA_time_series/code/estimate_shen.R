######################################
####estimation from shen's method#####
######################################


library(Matrix)
library(MASS)


error.bound=10^(-6)
lowrank <-function(data.hist,w,Omega,maxiter){
          s = svd(data.hist)
          D = diag(s$d)
          U = s$u
          V = s$v
          beta0=(U%*%D)[,1]
          f.old=V[,1]
          p=dim(data.hist)[2]
 
          for (i in 1:maxiter){
          beta=data.hist%*%f.old
          f.new=solve(diag(p)+w*Omega)%*%t(data.hist)%*%beta
          f.new=f.new/norm(f.new,"F")
          #cat("i is",i ,"\n")
          error=f.new-f.old
          if (norm(error,"F")< error.bound) break else f.old=f.new
       }
          
      
          fhat=f.new                   
                   
          betahat=data.hist%*%fhat/as.vector((t(fhat)%*%(diag(p)+w*Omega)%*%fhat))
          
          #data.hist=data.hist-betahat%*%t(fhat)
          

          return(list(fhat=fhat,betahat=betahat))  

}


x.fore.one <- function(y,x,u,first=N+1, nroll=30, nhis=N, nh=1,w){
        beta.fore = matrix(0,nrow=nroll,ncol=dim(x)[2])
        x.fore = matrix(0,nrow=nroll,ncol=dim(y)[2])
        
        M=dim(y)[2]
        N=(dim(y)[1])/2
        APE=rep(0,nroll)
        RMSE=rep(0,nroll)
        
        
        #i=1
        for (i in 1:nroll){
        betahat1=rep(0,N)
        betahat2=rep(0,N)
        fhat1=rep(0,M)
        fhat2=rep(0,M)
        data.hist = y[(first-1-nhis+i):(first+i-2),]
        
        fhat1 = lowrank(data.hist,w,Omega,100)$fhat
        betahat1=lowrank(data.hist,w,Omega,100)$betahat 
       
        residual1=data.hist-betahat1%*%t(fhat1)
        fhat2=lowrank(residual1,w,Omega,100)$fhat
        betahat2=lowrank(residual1,w,Omega,100)$betahat

        residual2=data.hist-betahat1%*%t(fhat1)-betahat2%*%t(fhat2)
        fhat3=lowrank(residual2,w,Omega,100)$fhat
        betahat3=lowrank(residual2,w,Omega,100)$betahat

        residual3=data.hist-betahat1%*%t(fhat1)-betahat2%*%t(fhat2)-betahat3%*%t(fhat3)
        fhat4=lowrank(residual3,w,Omega,100)$fhat
        betahat4=lowrank(residual3,w,Omega,100)$betahat
        
        beta1.fit = ar(betahat1, order.max=1, method="ols", demean=T, intercept=F)
        beta2.fit = ar(betahat2, order.max=1, method="ols", demean=T, intercept=F)
        beta3.fit = ar(betahat3, order.max=1, method="ols", demean=T, intercept=F)
        beta4.fit = ar(betahat4, order.max=1, method="ols", demean=T, intercept=F)
        ar1.coef = as.vector(beta1.fit$ar)
        ar2.coef = as.vector(beta2.fit$ar)
        beta1.pred = predict(beta1.fit,n.ahead=nh)$pred[1:nh]
        beta2.pred = predict(beta2.fit,n.ahead=nh)$pred[1:nh]
        beta3.pred = predict(beta3.fit,n.ahead=nh)$pred[1:nh]
        beta4.pred = predict(beta4.fit,n.ahead=nh)$pred[1:nh]


        x.fore[i,]=beta1.pred*fhat1+beta2.pred*fhat2+beta3.pred*fhat3+beta4.pred*fhat4
        plot(u,x[nhis+i,],type="l",ylab="x",main=paste(as.character(i),"step ahead prediction"))
        points(u,x.fore[i,],col=2)
        APE[i]=100/M*sum(abs(x[(nhis+i),]-x.fore[i,])/abs(x[(nhis+i),]))
        RMSE[i]=sqrt(1/M*sum(((x[(nhis+i),]-x.fore[i,]))^2))
        #cat("i is", i ,"\n")
        #cat("fhat2 is ",fhat2,"\n")
        }
          return(list(APE=APE,RMSE=RMSE,x.fore=x.fore))         
        }
