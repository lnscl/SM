# water requirements table

# files
folder<-"" #add full path of folder that contains input files (Cd)

ALF<-do.call(rbind, lapply(2001:2014, function(y) {
f<-paste0("alfalfa_IS_CW_", y, ".SCH")
tf<-read.table(paste0(folder, f), skip=1)
dys<-format(as.Date(paste0(y, "-", tf$V1-51), format="%Y-%j"), "%m-%d")
alfi<-data.frame(y, dates=as.Date(paste(y, dys, sep="-")), wr=tf$V2)
return(alfi)}))

PCH<-do.call(rbind, lapply(2001:2014, function(y) {
f<-paste0("peach_IS_CW_", y, ".SCH")
tf<-read.table(paste0(folder, f), skip=1)
dys<-format(as.Date(paste0(y, "-", tf$V1-92), format="%Y-%j"), "%m-%d")
pchi<-data.frame(y, dates=as.Date(paste(y, dys, sep="-")), wr=tf$V2)
return(pchi)}))

# leap years
ALF$dates[ALF$y%in%c(2004,2008,2012)]<-ALF$dates[ALF$y%in%c(2004,2008,2012)]+1
PCH$dates[PCH$y%in%c(2004,2008,2012)]<-PCH$dates[PCH$y%in%c(2004,2008,2012)]+1

# LCM
LCMf<-"LCM_WaterFluxes.txt"
tf<-read.table(paste0(folder, LCMf), header=T)
tf<-tf[,c("Year", "Month", "Day", "Irr")]
first.day<-which(tf$Day==13&tf$Month==4&tf$Year==2001)
last.day<-which(tf$Day==27&tf$Month==9&tf$Year==2014)
tf2<-tf[first.day:last.day,]
tf3<-tf2[tf2$Month%in%3:9,]
tf3<-tf3[-which(tf3$Day%in%28:30&tf3$Month==9),]
tf3.dates<-as.Date(paste(tf3$Year, tf3$Month, tf3$Day, sep="-"))
LCM<-tf3[tf3.dates%in%ALF$dates,]# only decision days

for (i in 2001:2014){
tf4<-tf3[tf3$Year==i,]
wr<-rollapply(tf4$Irr, width=14, FUN=sum, by=14)
LCM[LCM$Year==i,"Irr"][1:length(wr)]<-wr
}

# SCM
SCMf<-"SCM_WaterFluxes.txt"
tf<-read.table(paste0(folder, SCMf), header=T)
tf<-tf[,c("Year", "Month", "Day", "Irr")]
first.day<-which(tf$Day==24&tf$Month==5&tf$Year==2001)
last.day<-which(tf$Day==27&tf$Month==9&tf$Year==2014)
tf2<-tf[first.day:last.day,] # from 13th April 2001 to 27th Septembe 2014
tf3<-tf2[tf2$Month%in%3:9,]
tf3<-tf3[-which(tf3$Day%in%28:30&tf3$Month==9),]
tf3.dates<-as.Date(paste(tf3$Year, tf3$Month, tf3$Day, sep="-"))
SCM<-tf3[tf3.dates%in%ALF$dates,]# only decision days

for (i in 2001:2014){
tf4<-tf3[tf3$Year==i,]
wr<-rollapply(tf4$Irr, width=14, FUN=sum, by=14)
SCM[SCM$Year==i,"Irr"][1:length(wr)]<-wr
}


# combine
wr.all<-data.frame(ALF[,-1], PCH=rep(0, nrow(ALF)), LCM=rep(0, nrow(ALF)), SCM=rep(0, nrow(ALF)))
colnames(wr.all)<-c("dates", "ALF", "PCH", "LCM", "SCM")

pch.pos<-match(PCH$dates, ALF$dates)
wr.all$PCH[pch.pos]<-PCH$wr

lcm.dates<-as.Date(paste(LCM$Year, LCM$Month, LCM$Day, sep="-"))
lcm.pos<-match(lcm.dates, ALF$dates)
wr.all$LCM[lcm.pos]<-LCM$Irr

scm.dates<-as.Date(paste(SCM$Year, SCM$Month, SCM$Day, sep="-"))
scm.pos<-match(scm.dates, ALF$dates)
wr.all$SCM[scm.pos]<-SCM$Irr

write.table(wr.all, file.path(path,"table_wr.txt"))