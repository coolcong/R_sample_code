#################################################################
#########         estimation.R  for our method       ############
#################################################################




library(MASS)
library(splines)

prog_B_est=function(u,y){
        N=length(y[,1])       
        M=length(y[1,])
        Ns2=ceiling(N^(1/(2*(degree+1)))*log(log(N)))
        #Ns2=8
        knots=quantile(u,seq(0,1,length=(Ns2+2)))

        basis=bs(u,knots=knots[-c(1,(Ns2+2))],degree=degree,
            intercept=TRUE,Boundary.knots=knots[c(1,(Ns2+2))])
          B=mat.or.vec(M^2,(Ns2+degree+1)^2)
        basis=bs(u,knots=knots[-c(1,(Ns2+2))],degree=degree,
                intercept=TRUE,Boundary.knots=knots[c(1,(Ns2+2))])
        #B matrix
        B=mat.or.vec(M^2,(Ns2+degree+1)^2)

        for (i in 1:M){
            for (j in 1:M){
                B[(i-1)*M+j,]=kronecker(basis[i,],basis[j,])
                }
        }
        B.prog=B%*%solve(t(B)%*%B)%*%t(B)
        invB.B=solve(t(B)%*%B)%*%t(B)
         # make the grid equal space and finer
        u_length=length(u)
        min_dist=u[2]-u[1]
        for (i in 1: (u_length-1)){
            l=u[i+1]-u[i]
         if (l<min_dist) min_dist = l          
        }
       
        u.new=seq(u[1],u[M], by = min_dist)
        M1=length(u.new)
        #############################################################
        #interpolate the Ghat to make the new Ghat###################
        #############################################################
        basis.new=bs(u.new,knots=knots[-c(1,(Ns2+2))],degree=degree,
            intercept=TRUE,Boundary.knots=knots[c(1,(Ns2+2))])
      B.new=mat.or.vec(M1^2,(Ns2+degree+1)^2)
        for (i in 1:M1){
            for (j in 1:M1){
                B.new[(i-1)*M1+j,]=kronecker(basis.new[i,],basis.new[j,])
                }
        }
        list(B.prog=B.prog,invB.B=invB.B,B.new=B.new,M1=M1,min_dist=min_dist) 
}


mu_estGCV=function(u,y){
    N=length(y[,1])
    M=length(y[1,])
    mean_y=apply(y,2,mean)
    
    ############################
    #   GCV knot selection     #
    ############################
    ns1=seq(1:10)
    GCV=rep(0,length(ns1))
    for(kn in 1:length(ns1)){
      Ns1<-ns1[kn]
      knots=quantile(u,seq(0,1,length=(Ns1+2)))
      basis<-bs(u,knots=knots[-c(1,(Ns1+2))],degree=degree,intercept=TRUE,Boundary.knots=knots[c(1,(Ns1+2))]) 
      bbb=solve(t(basis)%*%(basis))%*%t(basis) 
      H=basis%*%bbb
      muhat=H%*%mean_y
      GCV[kn]=mean((mean_y-muhat)^2)/(1- sum(diag(H))/M)^2
    }
    Ns1=ns1[which.min(GCV)]
    ############################
    #     Use selected knots   #
    ############################
    knots=quantile(u,seq(0,1,length=(Ns1+2)))
    basis<-bs(u,knots=knots[-c(1,(Ns1+2))],degree=degree,intercept=TRUE,Boundary.knots=knots[c(1,(Ns1+2))]) 
    bbb=solve(t(basis)%*%(basis))%*%t(basis) 
    muhat=basis%*%bbb%*%mean_y
    #plot(u,muhat,lty=3)
    #points(u,mu,col=2)
      list(muhat=muhat)
}

phi_est=function(y,B.prog,invB.B,B.new,muhat,M1,min_dist){
        N=length(y[,1])
        M=length(y[1,])
        
        #C_j'j matrix 
        C=mat.or.vec(M,M)
        for (j1 in 1:M){
            for (j2 in 1:M){
                C[j1,j2]=(1/N)*t(y[,j1]-muhat[j1])%*%(y[,j2]-muhat[j2])
            }
        }

        CC=as.vector(C)
     
        gammaGhat=invB.B%*%CC  
        Ghat1=B.prog%*%CC
        Ghat1=as.vector(Ghat1)
        Ghat=matrix(Ghat1,M,M,byrow=TRUE)
        
         # make the grid equal space and finer
        #u_length=length(u)
        #min_dist=u[2]-u[1]
        #for (i in 1: (u_length-1)){
        #    l=u[i+1]-u[i]
        # if (l<min_dist) min_dist = l          
        #}
       
        #u.new=seq(u[1],u[M], by = min_dist)
        #M1=length(u.new)
        #############################################################
        #########original G   #######################################
        #############################################################
        #Q=matrix(0,M,M)
        #Q[,1:2]=phi
        #eigvalue=matrix(0,M,M)
        #eigvalue[1,1]=lamda[1]
        #eigvalue[2,2]=lamda[2]
        #G=Q%*%eigvalue%*%t(Q)
        #############################################################
        #interpolate the Ghat to make the new Ghat###################
        #############################################################
        #knots=quantile(u,seq(0,1,length=(Ns2+2)))
        #basis.new=bs(u.new,knots=knots[-c(1,(Ns2+2))],degree=degree,
        #    intercept=TRUE,Boundary.knots=knots[c(1,(Ns2+2))])
        #B.new=mat.or.vec(M1^2,(Ns2+degree+1)^2)
        #for (i in 1:M1){
        #    for (j in 1:M1){
        #        B.new[(i-1)*M1+j,]=kronecker(basis.new[i,],basis.new[j,])
        #        }
        #}
        Ghat2=B.new%*%gammaGhat
        
        Ghat.new=matrix(Ghat2,M1,M1,byrow=TRUE)
       
        #sum((G-Ghat)^2)
         
       #persp(u,u,G,theta = 200, phi =20, expand = .5, col = "green",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "M", ylab = "M", zlab = "G")

       #persp(u,u,Ghat,theta = 200, phi =20, expand = .5, col = "yellow",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "M", ylab = "M", zlab = "Ghat")

       #persp(u.new,u.new,Ghat.new,theta = 200, phi = 20, expand = .5, col = "lightblue",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "M1", ylab = "M1", zlab = "Ghat.new")
                        
        #eigen system of Ghat.new
        eigen_G=eigen(Ghat.new/M1,symmetric=T)
        evectors=as.matrix(eigen_G$vectors)
        evalues=as.vector(eigen_G$values)

        #choose number of eigenvalues
        #kapa=sum(cumsum(evalues/sum(evalues))<0.95)+1
        #print(kapa)    
        kapa=2


         lamdahat=evalues[1:kapa]
        phihat.temp=evectors[,1:kapa]*sqrt(M1)
        
          if(kapa==1) phihat.temp=matrix(phihat.temp,M,1)
          
          phihat=matrix(0,M,kapa)
            index=(u-u[1])/min_dist+1
          
          
     
          phihat=phihat.temp[index,]
          
          
          
        #plot(u,phi[,1],ylim=c(-2.5,1.5))
        #points(u,phihat[,1],col=2)
        list(lamdahat=lamdahat,phihat=phihat,Ghat=Ghat,Ghat.new=Ghat.new)

}
#y=data.hist
beta_est=function(y,muhat,phihat,lamdahat){
        N=length(y[,1])
        M=length(y[1,])
        kapa=length(lamdahat)
        betahat=matrix(0,N,kapa)
        temp= matrix(muhat,nrow=M,ncol=N)
        temp=t(temp)
        u1=c(0,u[1:(M-1)])
        diff=u-u1
        aa=phihat*diff
         
          if(kapa==1){
            
              
                 betahat[,j]= (y-temp)%*%aa
                
           }
       
      
      
      if(kapa>1){
          for(j in 1:kapa)
        
              {betahat[,j]=(y-temp)%*%aa[,j]}
          
          }
          
        list(betahat=betahat)
        #plot(betahat[,2],type="b")
        #points(beta2,type="b",col=2)
}

x_predict=function(h = 1, nroll = 30, nhis = N, first = N+1, y, x, u){
        M=dim(y)[2]
        x.pred=mat.or.vec(nroll,M)

        APE=rep(0,nroll)
        RMSE=rep(0,nroll)
        est1=prog_B_est(u,y)
        B.prog=est1$B.prog
        invB.B=est1$invB.B
        B.new=est1$B.new
        M1=est1$M1
        min_dist=est1$min_dist
        
        #i=2
        for (i in 1:nroll){
            data.hist=y[(first-1-nhis+i):(first+i-2),]
            mu.est=mu_estGCV(u,data.hist)
            muhat=mu.est$muhat
            
            phi.est=phi_est(data.hist,B.prog,invB.B,B.new,muhat,M1,min_dist)
            lamdahat=phi.est$lamdahat
            phihat=phi.est$phihat
            beta.est=beta_est(data.hist,muhat,phihat,lamdahat)
            betahat=beta.est$betahat

            kapa=ncol(betahat)
            ksi=rep(0,M)
            for(k in 1:kapa){
              beta.fit=ar(betahat[,k],order.max=1,method="ols",demean=T,intercept=F)
              beta.coef=as.vector(beta.fit$ar)
              if(length(beta.coef)<2) beta.coef=c(beta.coef,0)
              beta.pred=predict(beta.fit,n.ahead=h)$pred[1:h]                
              ksi=ksi+as.vector(beta.pred*phihat[,k])
            }
            x.pred[i,]=as.vector(muhat)+ksi
    
            plot(u,x.pred[i,],type="l",ylab="x",main=paste(as.character(i),"step ahead prediction"))
            points(u,x[(nhis+i),],col=2)
                      
            APE[i]=100/M*sum(abs(x[(nhis+i),]-x.pred[i,])/abs(x[(nhis+i),]))
            RMSE[i]=sqrt(1/M*sum(((x[(nhis+i),]-x.pred[i,])^2)))
            
        }
                 
        list(APE=APE,RMSE=RMSE,x.pred=x.pred)
}
