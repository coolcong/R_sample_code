#library("Deducer")
result_fpc=read.table("result_fpc.txt",header=T)
result_fpc_k=read.table("result_fpc_k.txt",header=T)
result_rw=read.table("result_rw.txt",header=T)
result_sfm=read.table("result_shen.txt",header=T)
allresult=data.frame(rbind(result_fpc,result_fpc_k,result_sfm,result_rw))
#sort(allresult,by=size)
data_30=subset(allresult,size==30)
data_50=subset(allresult,size==50)
data_100=subset(allresult,size==100)
data_200=subset(allresult,size==200)

data_30_0.5_FPCAR=subset(data_30,error==0.5&method=="FPCAR")
data_30_1.0_FPCAR=subset(data_30,error==1.0&method=="FPCAR")
data_30_1.5_FPCAR=subset(data_30,error==1.5&method=="FPCAR")




data_30_0.5_FPCAR1=subset(data_30,error==0.5&method=="FPCAR*")
data_30_1.0_FPCAR1=subset(data_30,error==1.0&method=="FPCAR*")
data_30_1.5_FPCAR1=subset(data_30,error==1.5&method=="FPCAR*")



data_30_0.5_SFM=subset(data_30,error==0.5&method=="SFM")
data_30_1.0_SFM=subset(data_30,error==1.0&method=="SFM")
data_30_1.5_SFM=subset(data_30,error==1.5&method=="SFM")

data_30_0.5_RW=subset(data_30,error==0.5&method=="RW")
data_30_1.0_RW=subset(data_30,error==1.0&method=="RW")
data_30_1.5_RW=subset(data_30,error==1.5&method=="RW")

summary(data_30_0.5_FPCAR1$ape)
summary(data_30_0.5_SFM$ape)
summary(data_30_0.5_RW$ape)
summary(data_30_0.5_FPCAR$ape)

summary(data_30_1.0_FPCAR1$ape)
summary(data_30_1.0_SFM$ape)
summary(data_30_1.0_RW$ape)
summary(data_30_1.0_FPCAR$ape)


summary(data_30_1.5_FPCAR1$ape)
summary(data_30_1.5_SFM$ape)
summary(data_30_1.5_RW$ape)
summary(data_30_1.5_FPCAR$ape)

summary(data_30_0.5_FPCAR1$rmse)
summary(data_30_0.5_SFM$rmse)
summary(data_30_0.5_RW$rmse)
summary(data_30_0.5_FPCAR$rmse)

summary(data_30_1.0_FPCAR1$rmse)
summary(data_30_1.0_SFM$rmse)
summary(data_30_1.0_RW$rmse)
summary(data_30_1.0_FPCAR$rmse)


summary(data_30_1.5_FPCAR1$rmse)
summary(data_30_1.5_SFM$rmse)
summary(data_30_1.5_RW$rmse)
summary(data_30_1.5_FPCAR$rmse)





data_50_0.5_FPCAR=subset(data_50,error==0.5&method=="FPCAR")
data_50_1.0_FPCAR=subset(data_50,error==1.0&method=="FPCAR")
data_50_1.5_FPCAR=subset(data_50,error==1.5&method=="FPCAR")

data_50_0.5_FPCAR1=subset(data_50,error==0.5&method=="FPCAR*")
data_50_1.0_FPCAR1=subset(data_50,error==1.0&method=="FPCAR*")
data_50_1.5_FPCAR1=subset(data_50,error==1.5&method=="FPCAR*")

data_50_0.5_SFM=subset(data_50,error==0.5&method=="SFM")
data_50_1.0_SFM=subset(data_50,error==1.0&method=="SFM")
data_50_1.5_SFM=subset(data_50,error==1.5&method=="SFM")

data_50_0.5_RW=subset(data_50,error==0.5&method=="RW")
data_50_1.0_RW=subset(data_50,error==1.0&method=="RW")
data_50_1.5_RW=subset(data_50,error==1.5&method=="RW")

c(summary(data_50_0.5_FPCAR1$ape)[2:5],summary(data_50_0.5_SFM$ape)[2:5],
summary(data_50_0.5_RW$ape)[2:5])
summary(data_50_0.5_FPCAR$ape)

c(summary(data_50_1.0_FPCAR1$ape)[2:5],summary(data_50_1.0_SFM$ape)[2:5],
summary(data_50_1.0_RW$ape)[2:5])
summary(data_50_1.0_FPCAR$ape)

c(summary(data_50_1.5_FPCAR1$ape)[2:5],summary(data_50_1.5_SFM$ape)[2:5],
summary(data_50_1.5_RW$ape)[2:5])
summary(data_50_1.5_FPCAR$ape)


c(summary(data_50_0.5_FPCAR1$rmse)[2:5],summary(data_50_0.5_SFM$rmse)[2:5],
summary(data_50_0.5_RW$rmse)[2:5])
summary(data_50_0.5_FPCAR$rmse)

c(summary(data_50_1.0_FPCAR1$rmse)[2:5],summary(data_50_1.0_SFM$rmse)[2:5],
summary(data_50_1.0_RW$rmse)[2:5])
summary(data_50_1.0_FPCAR$rmse)

c(summary(data_50_1.5_FPCAR1$rmse)[2:5],summary(data_50_1.5_SFM$rmse)[2:5],
summary(data_50_1.5_RW$rmse)[2:5])
summary(data_50_1.5_FPCAR$rmse)

data_100_0.5_FPCAR=subset(data_100,error==0.5&method=="FPCAR")
data_100_1.0_FPCAR=subset(data_100,error==1.0&method=="FPCAR")
data_100_1.5_FPCAR=subset(data_100,error==1.5&method=="FPCAR")

data_100_0.5_FPCAR1=subset(data_100,error==0.5&method=="FPCAR*")
data_100_1.0_FPCAR1=subset(data_100,error==1.0&method=="FPCAR*")
data_100_1.5_FPCAR1=subset(data_100,error==1.5&method=="FPCAR*")

data_100_0.5_SFM=subset(data_100,error==0.5&method=="SFM")
data_100_1.0_SFM=subset(data_100,error==1.0&method=="SFM")
data_100_1.5_SFM=subset(data_100,error==1.5&method=="SFM")

data_100_0.5_RW=subset(data_100,error==0.5&method=="RW")
data_100_1.0_RW=subset(data_100,error==1.0&method=="RW")
data_100_1.5_RW=subset(data_100,error==1.5&method=="RW")

c(summary(data_100_0.5_FPCAR1$ape)[2:5],summary(data_100_0.5_SFM$ape)[2:5],
summary(data_100_0.5_RW$ape)[2:5])
summary(data_100_0.5_FPCAR$ape)

c(summary(data_100_1.0_FPCAR1$ape)[2:5],summary(data_100_1.0_SFM$ape)[2:5],
summary(data_100_1.0_RW$ape)[2:5])
summary(data_100_1.0_FPCAR$ape)

c(summary(data_100_1.5_FPCAR1$ape)[2:5],summary(data_100_1.5_SFM$ape)[2:5],
summary(data_100_1.5_RW$ape)[2:5])
summary(data_100_1.5_FPCAR$ape)


c(summary(data_100_0.5_FPCAR1$rmse)[2:5],summary(data_100_0.5_SFM$rmse)[2:5],
summary(data_100_0.5_RW$rmse)[2:5])
summary(data_100_0.5_FPCAR$rmse)

c(summary(data_100_1.0_FPCAR1$rmse)[2:5],summary(data_100_1.0_SFM$rmse)[2:5],
summary(data_100_1.0_RW$rmse)[2:5])
summary(data_100_1.0_FPCAR$rmse)

c(summary(data_100_1.5_FPCAR1$rmse)[2:5],summary(data_100_1.5_SFM$rmse)[2:5],
summary(data_100_1.5_RW$rmse)[2:5])
summary(data_100_1.5_FPCAR$rmse)


par( mfrow= c(3, 3))
 
 boxplot(ape~method,data=data_30,subset=error==.5,main="Size 30, Error 0.5",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_30,subset=error==1.0,main="Size 30, Error 1.0",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_30,subset=error==1.5,main="Size 30, Error 1.5",col = "light grey",ylim=c(1,11.5))
 
 boxplot(ape~method,data=data_50,subset=error==.5,main="Size 50, Error 0.5",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_50,subset=error==1.0,main="Size 50, Error 1.0",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_50,subset=error==1.5,main="Size 50, Error 1.5",col = "light grey",ylim=c(1,11.5))
 
 
 boxplot(ape~method,data=data_100,subset=error==.5,main="Size 100, Error 0.5",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_100,subset=error==1.0,main="Size 100, Error 1.0",col = "light grey",ylim=c(1,11.5))
 boxplot(ape~method,data=data_100,subset=error==1.5,main="Size 100, Error 1.5",col = "light grey",ylim=c(1,11.5))
 
 par( mfrow= c(3, 3))
 
 boxplot(rmse~method,data=data_30,subset=error==.5,main="Size 30, Error 0.5",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_30,subset=error==1.0,main="Size 30, Error 1.0",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_30,subset=error==1.5,main="Size 30, Error 1.5",col = "light grey",ylim=c(.15,1.7))
 
 boxplot(rmse~method,data=data_50,subset=error==.5,main="Size 50, Error 0.5",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_50,subset=error==1.0,main="Size 50, Error 1.0",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_50,subset=error==1.5,main="Size 50, Error 1.5",col = "light grey",ylim=c(.15,1.7))
 
 boxplot(rmse~method,data=data_100,subset=error==.5,main="Size 100, Error 0.5",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_100,subset=error==1.0,main="Size 100, Error 1.0",col = "light grey",ylim=c(.15,1.7))
 boxplot(rmse~method,data=data_100,subset=error==1.5,main="Size 100, Error 1.5",col = "light grey",ylim=c(.15,1.7))
