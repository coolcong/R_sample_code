
#################################################################
####################main for random walk#########################
#################################################################

source("data_gen.R")
source("estimate_ranwalk.R")

###################
####main prog######
###################
rp=100
nh=1
nroll=30
#N=50
M=50
#q=1
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


                      measure=x.fore.one(y,x,u,N+1,nroll,N,1)
                      APE=measure$APE
                      RMSE=measure$RMSE
                      mean.ape[k]=mean(APE)
                      mean.rmse[k]=mean(RMSE)

}

return(list(mean.ape=mean.ape, mean.rmse=mean.rmse))

}
#######################data mode 1############################################################
ptm <- proc.time()
sim1=simulation(50,50,0.5,1,1)
sim1$mean.ape
sim1$mean.rmse
summary(sim1$mean.ape)
summary(sim1$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim2=simulation(100,50,0.5,1,1)
sim2$mean.ape
sim2$mean.rmse
summary(sim2$mean.ape)
summary(sim2$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim3=simulation(200,50,0.5,1,1)
sim3$mean.ape
sim3$mean.rmse
summary(sim3$mean.ape)
summary(sim3$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim4=simulation(50,50,1.0,1,1)
sim4$mean.ape
sim4$mean.rmse
summary(sim4$mean.ape)
summary(sim4$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim5=simulation(100,50,1.0,1,1)
sim5$mean.ape
sim5$mean.rmse
summary(sim5$mean.ape)
summary(sim5$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim6=simulation(200,50,1.0,1,1)
sim6$mean.ape
sim6$mean.rmse
summary(sim6$mean.ape)
summary(sim6$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim7=simulation(50,50,1.5,1,1)
sim7$mean.ape
sim7$mean.rmse
summary(sim7$mean.ape)
summary(sim7$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim8=simulation(100,50,1.5,1,1)
sim8$mean.ape
sim8$mean.rmse
summary(sim8$mean.ape)
summary(sim8$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim9=simulation(200,50,1.5,1,1)
sim9$mean.ape
sim9$mean.rmse
summary(sim9$mean.ape)
summary(sim9$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim10=simulation(30,50,0.5,1,1)
sim10$mean.ape
sim10$mean.rmse
summary(sim10$mean.ape)
summary(sim10$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim11=simulation(30,50,1.0,1,1)
sim11$mean.ape
sim11$mean.rmse
summary(sim11$mean.ape)
summary(sim11$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim12=simulation(30,50,1.5,1,1)
sim12$mean.ape
sim12$mean.rmse
summary(sim12$mean.ape)
summary(sim12$mean.rmse)
proc.time() - ptm

#############################data mode 2#################################################
ptm <- proc.time()
sim13=simulation(50,50,0.5,1,2)
sim13$mean.ape
sim13$mean.rmse
summary(sim13$mean.ape)
summary(sim13$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim14=simulation(100,50,0.5,1,2)
sim14$mean.ape                
sim14$mean.rmse               
summary(sim14$mean.ape)       
summary(sim14$mean.rmse)      
proc.time() - ptm            
                             
                             
ptm <- proc.time()           
sim15=simulation(200,50,0.5,1,2)
sim15$mean.ape
sim15$mean.rmse
summary(sim15$mean.ape)
summary(sim15$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim16=simulation(50,50,1.0,1,2)
sim16$mean.ape
sim16$mean.rmse
summary(sim16$mean.ape)
summary(sim16$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim17=simulation(100,50,1.0,1,2)
sim17$mean.ape
sim17$mean.rmse
summary(sim17$mean.ape)
summary(sim17$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim18=simulation(200,50,1.0,1,2)
sim18$mean.ape
sim18$mean.rmse
summary(sim18$mean.ape)
summary(sim18$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim19=simulation(50,50,1.5,1,2)
sim19$mean.ape
sim19$mean.rmse
summary(sim19$mean.ape)
summary(sim19$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim20=simulation(100,50,1.5,1,2)
sim20$mean.ape
sim20$mean.rmse
summary(sim20$mean.ape)
summary(sim20$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim21=simulation(200,50,1.5,1,2)
sim21$mean.ape
sim21$mean.rmse
summary(sim21$mean.ape)
summary(sim21$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim22=simulation(30,50,0.5,1,2)
sim22$mean.ape
sim22$mean.rmse
summary(sim22$mean.ape)
summary(sim22$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim23=simulation(30,50,1.0,1,2)
sim23$mean.ape
sim23$mean.rmse
summary(sim23$mean.ape)
summary(sim23$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim24=simulation(30,50,1.5,1,2)
sim24$mean.ape
sim24$mean.rmse
summary(sim24$mean.ape)
summary(sim24$mean.rmse)
proc.time() - ptm

#########################data mode 3###################################
ptm <- proc.time()
sim25=simulation(50,50,0.5,1,3)
sim25$mean.ape
sim25$mean.rmse
summary(sim25$mean.ape)
summary(sim25$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim26=simulation(100,50,0.5,1,3)
sim26$mean.ape                
sim26$mean.rmse               
summary(sim26$mean.ape)       
summary(sim26$mean.rmse)      
proc.time() - ptm            
                             
                             
ptm <- proc.time()           
sim27=simulation(200,50,0.5,1,3)
sim27$mean.ape
sim27$mean.rmse
summary(sim27$mean.ape)
summary(sim27$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim28=simulation(50,50,1.0,1,3)
sim28$mean.ape
sim28$mean.rmse
summary(sim28$mean.ape)
summary(sim28$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim29=simulation(100,50,1.0,1,3)
sim29$mean.ape
sim29$mean.rmse
summary(sim29$mean.ape)
summary(sim29$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim30=simulation(200,50,1.0,1,3)
sim30$mean.ape
sim30$mean.rmse
summary(sim30$mean.ape)
summary(sim30$mean.rmse)
proc.time() - ptm

ptm <- proc.time()
sim31=simulation(50,50,1.5,1,3)
sim31$mean.ape
sim31$mean.rmse
summary(sim31$mean.ape)
summary(sim31$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim32=simulation(100,50,1.5,1,3)
sim32$mean.ape
sim32$mean.rmse
summary(sim32$mean.ape)
summary(sim32$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim33=simulation(200,50,1.5,1,3)
sim33$mean.ape
sim33$mean.rmse
summary(sim33$mean.ape)
summary(sim33$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim34=simulation(30,50,0.5,1,3)
sim34$mean.ape
sim34$mean.rmse
summary(sim34$mean.ape)
summary(sim34$mean.rmse)
proc.time() - ptm



ptm <- proc.time()
sim35=simulation(30,50,1.0,1,3)
sim35$mean.ape
sim35$mean.rmse
summary(sim35$mean.ape)
summary(sim35$mean.rmse)
proc.time() - ptm


ptm <- proc.time()
sim36=simulation(30,50,1.5,1,3)
sim36$mean.ape
sim36$mean.rmse
summary(sim36$mean.ape)
summary(sim36$mean.rmse)
proc.time() - ptm
