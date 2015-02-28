#################################################################
#########      estimate for random walk    ######################
#################################################################


x.fore.one<-function(y,x,u,first=N+1,nroll,nhis=N,nh=1){
    x.fore = matrix(0,nrow=nroll,ncol=dim(y)[2])
    M=dim(y)[2]
    N=dim(y)[1]/2
    APE=rep(0,nroll)
    RMSE=rep(0,nroll)
    #i=1
    for (i in 1:nroll){
  
    data.hist = y[(first-1-nhis+i):(first+i-2),]
    x.fore[i,]=data.hist[N,]
    APE[i]=100/M*sum(abs(x[(nhis+i),]-x.fore[i,])/abs(x[(nhis+i),]))
    RMSE[i]=sqrt(1/M*sum(((x[(nhis+i),]-x.fore[i,]))^2))
    plot(u,x.fore[i,],type="l",ylab="x",main=paste(as.character(i),"step ahead prediction"))
    points(u,x[nhis+i,],col=2)

   }


    return(list(APE=APE,RMSE=RMSE,x.fore=x.fore))


}