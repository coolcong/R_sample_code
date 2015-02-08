path <- 'report/'
#day <- format(Sys.time(),"%m%d-%H%M%S")
#raw_header <- colnames(read.csv("data/IMF_raw7.csv",header = T, check.names = F,sep="|"))
#IMF_raw <- read.csv("data/IMF_raw7.csv",sep="|")
#IMF_rows <- NROW(IMF_raw)


day <- format(Sys.time(),"%m%d")
IMF_raw <- read.csv("Y:/Marketing_Stat_Team/Models/MAB/Phase2Test/Checking/0RawData/Generated/360Checkings_IMF_111014.csv",header = T)
raw_header= colnames(IMF_raw)
IMF_rows <- NROW(IMF_raw)


IMF_indx <- data.frame(placement=IMF_raw$Placement.Name, rotation=IMF_raw$Rotation.Type, active=IMF_raw$Ad.is.Active)

IMF_indx$concept <- rep(NA,IMF_rows)
IMF_indx$concept[  grep( "Lookatyougo", IMF_raw$Creative.Name ) ] <- "Lookatyougo"
IMF_indx$concept[  grep( "GetAhead", IMF_raw$Creative.Name ) ] <- "GetAhead"
IMF_indx$concept[  grep( "invite", IMF_raw$Creative.Name ) ] <- "Invite"

IMF_indx$size <- rep(NA,IMF_rows)
IMF_indx$size[  grep( "160x600", IMF_raw$Creative.Name ) ] <- "160x600"
IMF_indx$size[  grep( "300x250", IMF_raw$Creative.Name ) ] <- "300x250"
IMF_indx$size[  grep( "728x90", IMF_raw$Creative.Name ) ] <- "728x90"

IMF_indx$tc <- rep(NA,IMF_rows)
IMF_indx$tc[  grep( "Test", IMF_raw$Creative.Name ) ] <- "test"
IMF_indx$tc[  grep( "Cntrl", IMF_raw$Creative.Name ) ] <- "control"

IMF_indx <- data.frame(lapply(IMF_indx, as.character), stringsAsFactors=FALSE)

w <- numeric()
for (i in 1:NROW(outrotationweights.df)){
  concept <- as.character(outrotationweights.df[i,"Concept"])
  plcmt <- as.character(outrotationweights.df[i,"Placement"])
  tc <- as.character(outrotationweights.df[i,"TestControl"])
  size <- as.character(outrotationweights.df[i,"Size"])
  wt <- outrotationweights.df[i,"RotationWeight"]
  
  indx <- (IMF_indx$concept == concept & IMF_indx$placement == plcmt & IMF_indx$size == size & IMF_indx$rotation == "Weighted" & IMF_indx$active=='Yes' & IMF_indx$tc == tc)
  w <- c(w,which(indx))
  IMF_raw[which(indx),"Rotation.Value"] <- wt
  #IMF_raw[which(indx),"Error.Message"] <- "mab"
  IMF_raw[which(indx),"MAB.Used"] <- "Yes"
}

length(sort(unique(w)))



probi = numeric()
w <- numeric()
for (i in 1:NROW(outrotationweights.df)){
  concept <- as.character(outrotationweights.df[i,"Concept"])
  plcmt <- as.character(outrotationweights.df[i,"Placement"])
  tc <- as.character(outrotationweights.df[i,"TestControl"])
  size <- as.character(outrotationweights.df[i,"Size"])
  wt <- outrotationweights.df[i,"RotationWeight"]
  
  indx <- (IMF_indx$concept == concept & IMF_indx$placement == plcmt & IMF_indx$size == size & IMF_indx$rotation == "Weighted" & IMF_indx$active=='Yes' & IMF_indx$tc == tc)
   if (length(which(indx))==0) {
     print(cbind(concept, plcmt,size,tc))
     probi = c(probi,i)
   } 
  w <- c(w,which(indx))
  IMF_raw[which(indx),"Rotation.Value"] <- wt
  IMF_raw[which(indx),"MAB.Used"] <- "Yes"
}



write.csv(outrotationweights.df, file=paste(path,"outrotationweights.wcontrol_",day,".csv",sep="") )

write.table(IMF_raw, file=paste(path,"IMF_",day,".csv",sep="") ,row.names=F, na="",col.names=colnames(IMF_raw),sep=',')
