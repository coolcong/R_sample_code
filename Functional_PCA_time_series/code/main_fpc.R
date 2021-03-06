########################################################
##############main prog for our method##################
########################################################


library(MASS)
library(splines) 

source("data_gen.r")
#source("estimates_fpc.r")
source("estimate1.r")
#Ns1=6 #knots for mean
degree=3 #cubic spline
rp=100   #replication
h=1      #h-step ahead forecast
nroll=30 #size of test set
#q=4      #model q
#N=100
#M=30
#sig=.5
simulation=function(N,M,sig,h,q){
           
	set.seed(0)
	mean.ape=rep(0,rp)
	mean.rmse=rep(0,rp)
	for(k in 1:rp){ 
            
            #generate sample of size N*2M(2N:# of curves;M:# of time points on one curve)
            if (q == 1) {data.gen = data.generator1(N,M,sig)}
            if (q == 2) {data.gen = data.generator2(N,M,sig)}
            if (q == 3) {data.gen = data.generator3(N,M,sig)}
            if (q == 4) {data.gen = data.generator4(N,M,sig)}


            u=data.gen$u
            x=data.gen$x
            y1=data.gen$y1
            y2=data.gen$y2
            y=data.gen$y
             
            all.pred=x_predict(h, nroll, N, N+1, y, x, u)
            x.pred=all.pred$x.pred
            APE=all.pred$APE
            RMSE=all.pred$RMSE
            
            mean.ape[k]=mean(APE)
            mean.rmse[k]=mean(RMSE)
                       
	}

	list(mean.ape=mean.ape,mean.rmse=mean.rmse)
}

ptm <- proc.time()
sim1=simulation(50,30,0.5,1,4)
sim1$mean.ape
sim1$mean.rmse
summary(sim1$mean.ape)
summary(sim1$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim2=simulation(50,30,1.0,1,4)
sim2$mean.ape
sim2$mean.rmse
summary(sim2$mean.ape)
summary(sim2$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim3=simulation(50,30,1.5,1,4)
sim3$mean.ape
sim3$mean.rmse
summary(sim3$mean.ape)
summary(sim3$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim4=simulation(100,30,0.5,1,4)
sim4$mean.ape
sim4$mean.rmse
summary(sim4$mean.ape)
summary(sim4$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim5=simulation(100,30,1.0,1,4)
sim5$mean.ape
sim5$mean.rmse
summary(sim5$mean.ape)
summary(sim5$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim6=simulation(100,30,1.5,1,4)
sim6$mean.ape
sim6$mean.rmse
summary(sim6$mean.ape)
summary(sim6$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim10=simulation(30,30,0.5,1,4)
sim10$mean.ape
sim10$mean.rmse
summary(sim10$mean.ape)
summary(sim10$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim11=simulation(30,30,1.0,1,4)
sim11$mean.ape
sim11$mean.rmse
summary(sim11$mean.ape)
summary(sim11$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim12=simulation(30,30,1.5,1,4)
sim12$mean.ape
sim12$mean.rmse
summary(sim12$mean.ape)
summary(sim12$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim7=simulation(200,30,0.5,1,4)
sim7$mean.ape
sim7$mean.rmse
summary(sim7$mean.ape)
summary(sim7$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim8=simulation(200,30,1.0,1,4)
sim8$mean.ape
sim8$mean.rmse
summary(sim8$mean.ape)
summary(sim8$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim9=simulation(200,30,1.5,1,4)
sim9$mean.ape
sim9$mean.rmse
summary(sim9$mean.ape)
summary(sim9$mean.rmse)
proc.time() - ptm


