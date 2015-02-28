###################################################
#########         data_gen.R           ############
###################################################

#M=30
#N=100
#sig=0.0
data.generator1=function(N,M,sig){
        lamda=c(0.4,0.1)
       
        temp=rnorm(2*N*M)
        
        epsilon=array(temp,c(2*N,M))
        beta1=arima.sim(list(ar=c(.75,.2)),sd=0.1,n=2*N,n.start=1000)
        beta1=sqrt(lamda[1])*(beta1-mean(beta1))/sqrt(var(beta1)) #standardize

        beta2=arima.sim(list(ar=.8,ma=.3),sd=0.1,n=2*N,n.start=1000)
        beta2=sqrt(lamda[2])*(beta2-mean(beta2))/sqrt(var(beta2)) #standardize
        
        u=(1:M)/M
        y=mat.or.vec(2*N,M)
        x=mat.or.vec(2*N,M)
        phi=matrix(0,M,2)
        phi[,1]=-sqrt(2)*cos(pi*(u-0.5))
        phi[,2]=sqrt(2)*sin(pi*(u-0.5))
        mu=sin(2*pi*(u-0.5))+u*10+10
        
        for (i in 1:(2*N)){
            x[i,]=mu+beta1[i]*phi[,1]+beta2[i]*phi[,2]
            #x[i,]=beta1[i]*phi[,1]+beta2[i]*phi[,2]

        }
        y=x+sig*epsilon
        y1 = mat.or.vec(N,M)
        y2 = mat.or.vec(N,M)
        y1=y[1:N,]
        y2=y[(N+1):(2*N),]
        
       #persp(t,u,x,theta =120, phi = 20, expand = .5, col = "lightblue",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "t", ylab = "u", zlab = "y")


        #psi=phi%*%diag(sqrt(lamda))
        #G=psi%*%t(psi)
        #eigen(G/M,symmetric=TRUE)$value
        #
        #sign1=2*(sign(eigen(G/M)$vector[,1])==-1)-1
        #eigen(G/M)$vector[,1]*sqrt(M)*sqrt(0.4)*sign1

        list(u=u,y1=y1,y2=y2,y=y,x=x)                    #u is index on the one curve
                                                         #x is trajectory 
                                                         #y = x+noise
                                                         #y1 is first half of the sample
                                                         #y2 is second half of the sample

}

#unequal space data generater

data.generator4=function(N,M,sig){
        lamda=c(0.4,0.1)
       
        temp=rnorm(2*N*M)
        
        epsilon=array(temp,c(2*N,M))
        beta1=arima.sim(list(ar=c(.75,.2)),sd=0.1,n=2*N,n.start=1000)
        beta1=sqrt(lamda[1])*(beta1-mean(beta1))/sqrt(var(beta1)) #standardize

        beta2=arima.sim(list(ar=.8,ma=.3),sd=0.1,n=2*N,n.start=1000)
        beta2=sqrt(lamda[2])*(beta2-mean(beta2))/sqrt(var(beta2)) #standardize
        a=seq(3,63,by=3)
        b=seq(69,93,by=6)
        c=seq(105,141,by=12)
        u=c(a,b,c)/141
        M=length(u)
        y=mat.or.vec(2*N,M)
        x=mat.or.vec(2*N,M)
        phi=matrix(0,M,2)
        phi[,1]=-sqrt(2)*cos(pi*(u-0.5))
        phi[,2]=sqrt(2)*sin(pi*(u-0.5))
        mu=sin(2*pi*(u-0.5))+u*10+10
        
        for (i in 1:(2*N)){
            x[i,]=mu+beta1[i]*phi[,1]+beta2[i]*phi[,2]
            #x[i,]=beta1[i]*phi[,1]+beta2[i]*phi[,2]

        }
        y=x+sig*epsilon
        y1 = mat.or.vec(N,M)
        y2 = mat.or.vec(N,M)
        y1=y[1:N,]
        y2=y[(N+1):(2*N),]
        
       #persp(t,u,x,theta =120, phi = 20, expand = .5, col = "lightblue",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "t", ylab = "u", zlab = "y")


        #psi=phi%*%diag(sqrt(lamda))
        #G=psi%*%t(psi)
        #eigen(G/M,symmetric=TRUE)$value
        #
        #sign1=2*(sign(eigen(G/M)$vector[,1])==-1)-1
        #eigen(G/M)$vector[,1]*sqrt(M)*sqrt(0.4)*sign1

        list(u=u,y1=y1,y2=y2,y=y,x=x)                    #u is index on the one curve
                                                         #x is trajectory 
                                                         #y = x+noise
                                                         #y1 is first half of the sample
                                                         #y2 is second half of the sample

}

data.generator5=function(N,M,sig){
        lamda=c(1,0.25)
       
        temp=rnorm(2*N*M)
        
        epsilon=array(temp,c(2*N,M))
        beta1=arima.sim(list(ar=c(.75,.2)),sd=0.1,n=2*N,n.start=1000)
        beta1=sqrt(lamda[1])*(beta1-mean(beta1))/sqrt(var(beta1)) #standardize

        beta2=arima.sim(list(ar=.8,ma=.3),sd=0.1,n=2*N,n.start=1000)
        beta2=sqrt(lamda[2])*(beta2-mean(beta2))/sqrt(var(beta2)) #standardize
        a=seq(3,63,by=3)
        b=seq(69,93,by=6)
        c=seq(105,141,by=12)
        u=c(a,b,c)/141
        M=length(u)
        y=mat.or.vec(2*N,M)
        x=mat.or.vec(2*N,M)
        phi=matrix(0,M,2)
        phi[,1]=-sqrt(2)*cos(pi*(u-0.5))
        phi[,2]=sqrt(2)*sin(pi*(u-0.5))
        mu=sin(2*pi*(u-0.5))+u*10+10
        
        for (i in 1:(2*N)){
            x[i,]=mu+beta1[i]*phi[,1]+beta2[i]*phi[,2]
            #x[i,]=beta1[i]*phi[,1]+beta2[i]*phi[,2]

        }
        y=x+sig*epsilon
        y1 = mat.or.vec(N,M)
        y2 = mat.or.vec(N,M)
        y1=y[1:N,]
        y2=y[(N+1):(2*N),]
        
       #persp(t,u,x,theta =120, phi = 20, expand = .5, col = "lightblue",
       #ltheta = 160, shade = 0.75, ticktype = "detailed",
       #xlab = "t", ylab = "u", zlab = "y")


        #psi=phi%*%diag(sqrt(lamda))
        #G=psi%*%t(psi)
        #eigen(G/M,symmetric=TRUE)$value
        #
        #sign1=2*(sign(eigen(G/M)$vector[,1])==-1)-1
        #eigen(G/M)$vector[,1]*sqrt(M)*sqrt(0.4)*sign1

        list(u=u,y1=y1,y2=y2,y=y,x=x)                    #u is index on the one curve
                                                         #x is trajectory 
                                                         #y = x+noise
                                                         #y1 is first half of the sample
                                                         #y2 is second half of the sample

}

data.generator2=function(N,M,sig){
        lamda=c(0.02,0.02)
       
        temp=rnorm(2*N*M)
        
        epsilon=array(temp,c(2*N,M))
        beta1=arima.sim(list(ar=c(.75,.2)),sd=0.1,n=2*N,n.start=1000)
        beta1=sqrt(lamda[1])*(beta1-mean(beta1))/sqrt(var(beta1)) #standardize

        beta2=arima.sim(list(ar=.8,ma=.3),sd=0.1,n=2*N,n.start=1000)
        beta2=sqrt(lamda[2])*(beta2-mean(beta2))/sqrt(var(beta2)) #standardize
        
        #beta1=beta2
  
        u=(1:M)/M
        y=mat.or.vec(2*N,M)
        x=mat.or.vec(2*N,M)
        mu=mat.or.vec(2*N,M)
        phi=matrix(0,M,2)
        phi[,1]=-sqrt(2)*cos(pi*(u-0.5))
        phi[,2]=sqrt(2)*sin(pi*(u-0.5))
        #phi[,2]=phi[,1]
        for (i in 1:(2*N)){

           for (j in 1:M){
              
              mu[i,j]=(j/M-1/2)^2*i+5

           }
         }
              

        for (i in 1:(2*N)){
            x[i,]=mu[i,]+beta1[i]*phi[,1]+beta2[i]*phi[,2]
            
        }
        y=x+sig*epsilon
        y1 = mat.or.vec(N,M)
        y2 = mat.or.vec(N,M)
        y1=y[1:N,]
        y2=y[(N+1):(2*N),]

        #psi=phi%*%diag(sqrt(lamda))
        #G=psi%*%t(psi)
        #eigen(G/M,symmetric=TRUE)$value
        #
        #sign1=2*(sign(eigen(G/M)$vector[,1])==-1)-1
        #eigen(G/M)$vector[,1]*sqrt(M)*sqrt(0.4)*sign1

        list(u=u,y1=y1,y2=y2,mu=mu,y=y,x=x)                  #t is index on the one curve
                                                         #x is trajectory 
                                                         #y = x+noise
                                                         #y1 is first half of the sample
                                                         #y2 is second half of the sample

}

data.generator3=function(N,M,sig){
        lamda=c(1,.25)
       
        temp=rnorm(2*N*M)
        
        epsilon=array(temp,c(2*N,M))
        beta1=arima.sim(list(ar=c(.75,.2)),sd=0.1,n=2*N,n.start=1000)
        beta1=sqrt(lamda[1])*(beta1-mean(beta1))/sqrt(var(beta1)) #standardize

        beta2=arima.sim(list(ar=.8,ma=.3),sd=0.1,n=2*N,n.start=1000)
        beta2=sqrt(lamda[2])*(beta2-mean(beta2))/sqrt(var(beta2)) #standardize
        
        u=(1:M)/M
        y=mat.or.vec(2*N,M)
        x=mat.or.vec(2*N,M)
        mu=mat.or.vec(2*N,M)
        phi=matrix(0,M,2)
        phi[,1]=-sqrt(2)*cos(pi*(u-0.5))
        phi[,2]=sqrt(2)*sin(pi*(u-0.5))
        for (i in 1:(2*N)){

           for (j in 1:M){
              mu[i,j]=10*(1-exp(-1.5*j/M))/(j/M)+5*(sin(i*(1/60)*pi))+10

             }
         }
       
        

        for (i in 1:(2*N)){
            x[i,]=mu[i,]+beta1[i]*phi[,1]+beta2[i]*phi[,2]

        }
        y=x+sig*epsilon
        y1 = mat.or.vec(N,M)
        y2 = mat.or.vec(N,M)
        y1=y[1:N,]
        y2=y[(N+1):(2*N),]

       #psi=phi%*%diag(sqrt(lamda))
       #G=psi%*%t(psi)
       #eigen(G/M,symmetric=TRUE)$value
       #
       #sign1=2*(sign(eigen(G/M)$vector[,1])==-1)-1
       #eigen(G/M)$vector[,1]*sqrt(M)*sqrt(0.4)*sign1

        list(u=u,y1=y1,y2=y2,mu=mu,y=y,x=x)              #t is index on the one curve
                                                         #x is trajectory 
                                                         #y = x+noise
                                                         #y1 is first half of the sample
                                                         #y2 is second half of the sample

}
                        
