########sim1###########
ape = sim1$mean.ape
rmse=sim1$mean.rmse
size=rep(50,100)
error=rep(.5,100)
method=rep("FPCAR",100)
data1=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim2###########
ape = sim2$mean.ape
rmse=sim2$mean.rmse
size=rep(50,100)
error=rep(1.0,100)
method=rep("FPCAR",100)
data2=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim3###########
ape = sim3$mean.ape
rmse=sim3$mean.rmse
size=rep(50,100)
error=rep(1.5,100)
method=rep("FPCAR",100)
data3=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim4###########
ape = sim4$mean.ape
rmse=sim4$mean.rmse
size=rep(100,100)
error=rep(.5,100)
method=rep("FPCAR",100)
data4=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim5###########
ape = sim5$mean.ape
rmse=sim5$mean.rmse
size=rep(100,100)
error=rep(1.0,100)
method=rep("FPCAR",100)
data5=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim6###########
ape = sim6$mean.ape
rmse=sim6$mean.rmse
size=rep(100,100)
error=rep(1.5,100)
method=rep("FPCAR",100)
data6=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim7###########
ape = sim7$mean.ape
rmse=sim7$mean.rmse
size=rep(200,100)
error=rep(.5,100)
method=rep("FPCAR",100)
data7=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim8###########
ape = sim8$mean.ape
rmse=sim8$mean.rmse
size=rep(200,100)
error=rep(1.0,100)
method=rep("FPCAR",100)
data8=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim9###########
ape = sim9$mean.ape
rmse=sim9$mean.rmse
size=rep(200,100)
error=rep(1.5,100)
method=rep("FPCAR",100)
data9=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim10###########
ape = sim10$mean.ape
rmse=sim10$mean.rmse
size=rep(30,100)
error=rep(.5,100)
method=rep("FPCAR",100)
data10=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim11###########
ape = sim11$mean.ape
rmse=sim11$mean.rmse
size=rep(30,100)
error=rep(1,100)
method=rep("FPCAR",100)
data11=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

########sim12###########
ape = sim12$mean.ape
rmse=sim12$mean.rmse
size=rep(30,100)
error=rep(1.5,100)
method=rep("FPCAR",100)
data12=data.frame(ape=ape,rmse=rmse,size=size,error=error,method=method)

######## merge data ######
result_fpc=data.frame(rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,data11,data12))
write.table(result_fpc,file="result_fpc.txt") 
