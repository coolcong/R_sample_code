source("data_gen.R")
source("estimate_shen.R")

rp=100
nh=1
nroll=30
M=30
w=0.01
error.bound=10^(-6)


###################
#####Q matrix######
###################
l=rep(0,M-1)
a=seq(3,63,by=3)
b=seq(69,93,by=6)
c=seq(105,141,by=12)
t=c(a,b,c)/141
for (j in 1:M-1){
  l[j]=t[j+1]-t[j]
}

Q = mat.or.vec(M,M-2)
###very weird notation of k
for (j in 1:M){
    for (k in 2:M-1){
       if (abs(j-(k-1))>2) Q[j,k-1]=0
       
            else {
             Q[k-1,k-1]=(l[k-1])^(-1)
             Q[k,k-1]=-1/(l[k-1])-1/(l[k]) 
             Q[k+1,k-1]=(l[k])^(-1)
            }    
    
    }

}
###################
#####R matrix######
###################
       

R = mat.or.vec(M-2,M-2)

for (k in 2:M-1){
  
     for ( j in 2:M-1){

    if (abs(j-k)>2) R[j-1,k-1]=0
         else{
               R[j-1,j-1]=1/3*(l[j-1]+l[j])
           if(j< M-1){
                          R[j-1,j]=1/6*l[j]
                          R[j,j-1]=1/6*l[j]
                         
                          }
             }

    }

}

###################
#Omega matrix######
###################

Omega = Q%*%solve(R)%*%t(Q)


###################
####main prog######
###################

simulation = function(N,M,sig,nh,q){
                set.seed(0)
                mean.ape=rep(0,rp)
                mean.rmse=rep(0,rp)
                for (k in 1:rp){

                      if (q == 1) {data.gen = data.generator1(N,M,sig)}
                      if (q == 2) {data.gen = data.generator2(N,M,sig)}
                      if (q == 3) {data.gen = data.generator3(N,M,sig)}
                      if (q == 4) {data.gen = data.generator4(N,M,sig)}
                      if (q == 5) {data.gen = data.generator5(N,M,sig)}
                      
                      
                      u=data.gen$u
                      x=data.gen$x
                      y1=data.gen$y1
                      y2=data.gen$y2
                      y=data.gen$y
                      #y = apply(y,2,function(z){z-mean(z)})+10  #demean and level up the curve
                      #x = apply(x,2,function(z){z-mean(z)})+10


                      measure=x.fore.one(y,x,u,N+1,nroll,N,1,w)
                      APE=measure$APE
                      RMSE=measure$RMSE
                      mean.ape[k]=mean(APE)
                      mean.rmse[k]=mean(RMSE)

}

return(list(mean.ape=mean.ape, mean.rmse=mean.rmse))
#beta=t(rnorm(length(data[,1])))
}

ptm <- proc.time()
sim1=simulation(50,30,0.5,1,5)
sim1$mean.ape
sim1$mean.rmse
summary(sim1$mean.ape)
summary(sim1$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim2=simulation(50,30,1.0,1,5)
sim2$mean.ape
sim2$mean.rmse
summary(sim2$mean.ape)
summary(sim2$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim3=simulation(50,30,1.5,1,5)
sim3$mean.ape
sim3$mean.rmse
summary(sim3$mean.ape)
summary(sim3$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim4=simulation(100,30,0.5,1,5)
sim4$mean.ape
sim4$mean.rmse
summary(sim4$mean.ape)
summary(sim4$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim5=simulation(100,30,1.0,1,5)
sim5$mean.ape
sim5$mean.rmse
summary(sim5$mean.ape)
summary(sim5$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim6=simulation(100,30,1.5,1,5)
sim6$mean.ape
sim6$mean.rmse
summary(sim6$mean.ape)
summary(sim6$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim10=simulation(30,30,0.5,1,5)
sim10$mean.ape
sim10$mean.rmse
summary(sim10$mean.ape)
summary(sim10$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim11=simulation(30,30,1.0,1,5)
sim11$mean.ape
sim11$mean.rmse
summary(sim11$mean.ape)
summary(sim11$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim12=simulation(30,30,1.5,1,5)
sim12$mean.ape
sim12$mean.rmse
summary(sim12$mean.ape)
summary(sim12$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim7=simulation(200,30,0.5,1,5)
sim7$mean.ape
sim7$mean.rmse
summary(sim7$mean.ape)
summary(sim7$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim8=simulation(200,30,1.0,1,5)
sim8$mean.ape
sim8$mean.rmse
summary(sim8$mean.ape)
summary(sim8$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim9=simulation(200,30,1.5,1,5)
sim9$mean.ape
sim9$mean.rmse
summary(sim9$mean.ape)
summary(sim9$mean.rmse)
proc.time() - ptm