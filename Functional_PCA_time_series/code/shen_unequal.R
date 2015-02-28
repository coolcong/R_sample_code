
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
                    
                      
                      
                      u=data.gen$u
                      x=data.gen$x
                      y1=data.gen$y1
                      y2=data.gen$y2
                      y=data.gen$y
                      #y = apply(y,2,function(z){z-mean(z)})+10  #demean and level up the curve
                      #x = apply(x,2,function(z){z-mean(z)})+10


                      measure=x.fore.one(y,x,N+1,nroll,N,1,w)
                      APE=measure$APE
                      RMSE=measure$RMSE
                      mean.ape[k]=mean(APE)
                      mean.rmse[k]=mean(RMSE)

}

return(list(mean.ape=mean.ape, mean.rmse=mean.rmse))
#beta=t(rnorm(length(data[,1])))
}