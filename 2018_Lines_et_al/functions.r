
availability<-function(reservoir_inflow, percentiles_file, MD.dt=NA, write.file=F){

if(all(is.na(MD.dt))){
hyear<-start_year:end_year
MD.dt<-do.call("c", lapply(hyear, function(x) {
SIS.dt<-paste(x, SIS.dy, sep="-")
EIS.dt<-paste(x, EIS.dy, sep="-")
MD.dt.y<-seq.Date(as.Date(SIS.dt, format="%Y-%m-%d"), as.Date(EIS.dt, format="%Y-%m-%d"), 14)
return(MD.dt.y)}))
}

# AI (Cummulative available water on MD.dt)
inflow <- read.table(reservoir_inflow, header=T)
inflow.dt <- as.Date(paste(inflow[,1], inflow[,2], inflow[,3], sep="-"), format="%Y-%m-%d")
hyear <- start_year:end_year

inflow.MD <- match(MD.dt, inflow.dt)
AIh <- inflow[inflow.MD, "accu_vol"]
AIh.IS <- data.frame(MD.dt, day = format(MD.dt, "%m-%d"), AIh)

# AIne (cummulative available water at the end of the season, for percentile on MD.dt) 
per <- read.table(percentiles_file, header=T) # Matrix: rows - percentile number from 0 to 100, cols - date
colnames(per) <- format(seq(as.Date("2000-oct-01", "%Y-%b-%d"), as.Date("2001-sep-30", "%Y-%b-%d"),1), "%m-%d")

pAIn <- unlist(lapply(AIh.IS[,"AIh"], function(x) {
if(!is.na(x)){
i <- which(AIh.IS[,"AIh"]==x)
per.dy <- per[,as.character(AIh.IS[i,"day"])]
val <- which(abs(per.dy-AIh.IS[i,"AIh"])==min(abs(per.dy-AIh.IS[i,"AIh"])))
return(c(per.dy[val], which(per.dy==per.dy[val]), per[(val+1),365]))}}))

pAIn <- matrix(pAIn, ncol=3, byrow=T)
colnames(pAIn) <- c("pAIh", "per", "AIne") 
# pAIh = closest percentile to AIh. 

AIne <- data.frame(AIh.IS, pAIn)

if(write.file==T){
write.table(AIne, file.path(path, "An.txt"), col.names=T, row.names=F, append=F)}

return(AIne)
}

availabilityF <- function(reservoirInflow, percentilesFile, MDdt = NA, write.file = F){
  
  if(all(is.na(MDdt))){
    hyear <- startYear:endYear
    MDdt <- do.call("c", lapply(hyear, function(x) {
      SISdt <- paste(x, SISdy, sep = "-")
      EISdt <- paste(x, EISdy, sep = "-")
      MDdty <- seq.Date(as.Date(SISdt, format = "%Y-%m-%d"), as.Date(EISdt, format = "%Y-%m-%d"), 14)
      return(MDdty)}))
  }
  
  # (1) observed values - accumulated inflow, AI (Cummulative available water on MDdt)
  inflow <- read.table(reservoirInflow, header=T)
  inflowdt <- as.Date(paste(inflow[,1], inflow[,2], inflow[,3], sep = "-"), format = "%Y-%m-%d")
  hyear <- startYear:endYear
  inflowMD <- match(MDdt, inflowdt)
  
  # 
  EISdts <- as.Date(paste(hyear, EISdy, sep = "-"))
  pos <- match(substr(MDdt, 1, 4), substr(EISdts, 1, 4))  
  
  # (2) forecast values
  forecastFile<-file.path(dirname(folder), "test_forecast.txt") # test data
  forecast<-read.table(forecastFile)
  inflowF <- cbind(year=inflow$year, month=inflow$month, day=inflow$day, hyear=inflow$hyear, accu_vol=inflow$accu_vol, forecast)
  
  inflowFMD <- inflowF[inflowMD, ]
  forecastMD <- data.frame(MDdt, day = format(MDdt, "%m-%d"), forecast[inflowMD, ])
  
  # (3) Percentiles for the forecast values and associated values at the end of the season
  per <- read.table(percentilesFile, header = T) # Matrix: rows - percentile number from 0 to 100, cols - date
  colnames(per) <- format(seq(as.Date("2000-oct-01", "%Y-%b-%d"), as.Date("2001-sep-30", "%Y-%b-%d"), 1), "%m-%d")
  perIS <- per[, as.character(unique(forecastMD$day))]
  ndyIS <- dim(perIS)[2]
  
  perF <- unlist(lapply(3:ncol(forecastMD), function(n) {
    fcol <- forecastMD[, n]
    lapply(1:length(fcol), function(i) {
      dypos <- (i / ndyIS - floor(i / ndyIS)) * ndyIS
      dypos[dypos == 0] <- ndyIS
      pcol <- perIS[, dypos]
      val <- which(abs(pcol - fcol[i]) == min(abs(pcol - fcol[i])))
      return(c(colnames(forecastMD)[n], pcol[val], val, perIS[val, ncol(perIS)]))
    })
  }))
  
  
  nrows <- nrow(forecastMD) * (ncol(forecastMD) - 2)
  perF <- matrix(perF, nrow = nrows, ncol = 4, byrow = T)
  colnames(perF) <- c("forecast", "perAIhF", "per", "AIne")  
  # pAIh = closest percentile to AIh.
  
  AIne <- data.frame(forecastMD, perF)
  
  if(write.file==T){
    write.table(AIne, file.path(path, "An.txt"), col.names=T, row.names=F, append=F)}
  
  return(AIne)
}

crop.demand<-function(cs){

# cs: Proportion variable crops
# area: Area that is irrigated only from Barasona = upper zone area (Area zona alta)
# p.alf & p.woo: Proportion of crops in upper zone area

# Input data (mm)
wrt<-read.table(wrt.file, header=T)
crops<-c(crops, "FAL1", "FAL2")

# crops proportions
p.var<-1-p.alf-p.woo
SCM.prop<-cs[,"SCM"]*p.var
LCM.prop<-cs[,"LCM"]*p.var
ALF.prop<-rep(p.alf, nrow(cs))
PCH.prop<-rep(p.woo, nrow(cs))

cols1<-which(!colnames(cs)%in%crops)
hyear<-ifelse(length(cols1)>0, hy<-cs[,cols1], hy<-rownames(cs))
prop.crops<-data.frame(hyear=hy, ALF=ALF.prop, PCH=PCH.prop, LCM=as.numeric(LCM.prop), SCM=as.numeric(SCM.prop))
prop.crops[is.na(prop.crops)]<-0

# total demand [in cubic hectometer]
cols2<-which(colnames(wrt)%in%crops)
cols3<-which(colnames(prop.crops)%in%crops)
if(all(colnames(wrt)[cols2]==colnames(prop.crops)[cols3])){
prop.crops2<-do.call(rbind, lapply(1:nrow(wrt), function(x) prop.crops[prop.crops$hyear==substr(wrt$dates[x],1,4),]))
}
first.row<-which(substr(wrt$dates,1,4)==prop.crops2[1,1])[1]
demanda.total.crop<-wrt[first.row:nrow(wrt),cols2]*prop.crops2[,cols3]*area*10^-5
demanda.total<-data.frame(dates=as.Date(wrt$dates[first.row:nrow(wrt)]), Dtot=rowSums(demanda.total.crop)/eff.riego, Dvc=rowSums(demanda.total.crop[,c("LCM", "SCM")])/eff.riego)

return(demanda.total)}

curtailment.decision<-function(A.D){
# perfect.info
d<-A.D$MD.dt
hyear<-substr(d,1,4)
weeks<-as.numeric(round((as.Date(paste(hyear, EIS.dy, sep="-"))-as.Date(d))/7))+2
curtsi<-data.frame(dates=d, An.eff=A.D$An.eff, Dn=A.D$Dn, weeks, dif=rep(NA, length(weeks)), red=rep(NA, length(weeks)), Dn.mod=rep(NA, length(weeks)), D.vc=A.D$Dvc, Dvc.red=rep(NA, length(weeks)))

first<-match(unique(hyear),hyear)
curtsi[first,"dif"]<-curtsi[first,"Dn"]-curtsi[first,"An.eff"]
curtsi[first,"red"]<-ifelse(curtsi[first,"dif"]>1, curtsi[first,"dif"]/curtsi[first,"weeks"]*2, 0)
curtsi[first,"Dvc.red"]<-ifelse(curtsi[first,"D.vc"]>=curtsi[first,"red"], curtsi[first,"red"], curtsi[first,"D.vc"])
curtsi[first,"Dn.mod"]<-curtsi[first,"Dn"]-curtsi[first,"Dvc.red"]

for(y in first[1]:(first[2]-2)){
filas<-first+y
curtsi[filas,"dif"]<-curtsi[filas-1,"Dn.mod"]-curtsi[filas,"An.eff"]
curtsi[filas,"red"]<-ifelse(curtsi[filas,"dif"]>1, curtsi[filas,"dif"]/curtsi[filas,"weeks"]*2, 0)
curtsi[filas,"Dvc.red"]<-ifelse(curtsi[filas,"D.vc"]>=curtsi[filas,"red"], curtsi[filas,"red"], curtsi[filas,"D.vc"])
curtsi[filas,"Dn.mod"]<-curtsi[filas-1,"Dn.mod"]-curtsi[filas,"Dvc.red"]
}
curtsi[,5:7]<-round(curtsi[,5:7],2)
curtsi$pvc.red<-curtsi$Dvc.red*100/curtsi$D.vc
curtsi$pvc.red[is.na(curtsi$pvc.red)]<-0

return(curtsi)}

curtailment.decision2<-function(A.D, curts){
# not perfect info

d<-A.D$dates
hyear<-substr(d,1,4)
weeks<-as.numeric(round((as.Date(paste(hyear, EIS.dy, sep="-"))-as.Date(d))/7))+2
curtsi<-data.frame(dates=d, An.eff=A.D$AIne.eff, Dn=A.D$Dn, weeks, dif=rep(NA, length(weeks)), red=rep(NA, length(weeks)), Dn.mod=rep(NA, length(weeks)), D.vc=A.D$D_vc, Dvc.red=rep(NA, length(weeks)), accu.Dvc.red=rep(NA, length(weeks)))
first<-match(unique(hyear),hyear)

if(length(curts)==0){
curtsi[first,"dif"]<-curtsi[first,"Dn"]-curtsi[first,"An.eff"]
curtsi[first,"red"]<-ifelse(curtsi[first,"dif"]>1, curtsi[first,"dif"]/curtsi[first,"weeks"]*2, 0)
curtsi[first,"Dvc.red"]<-ifelse(curtsi[first,"D.vc"]>=curtsi[first,"red"], curtsi[first,"red"], curtsi[first,"D.vc"])
curtsi[first,"accu.Dvc.red"]<-curtsi[first,"Dvc.red"]
curtsi[first,"Dn.mod"]<-curtsi[first,"Dn"]-curtsi[first,"Dvc.red"]
}else{
cu<-curts[[length(curts)]]
years.cu<-substr(cu$dates,1,4)
last.cu<-length(years.cu)-match(unique(years.cu),rev(years.cu))+1
curtsi[first,c("dif", "red", "Dn.mod", "accu.Dvc.red")]<-cu[last.cu,c("dif", "red", "Dn.mod", "accu.Dvc.red")]
curtsi[first,"Dvc.red"]<-ifelse(curtsi[first,"D.vc"]>=curtsi[first,"red"], curtsi[first,"red"], curtsi[first,"D.vc"])
}

for(y in first[1]:(first[2]-2)){
filas<-first+y
curtsi[filas,"dif"]<-curtsi[filas-1,"Dn.mod"]-curtsi[filas,"An.eff"]
curtsi[filas,"red"]<-ifelse(curtsi[filas,"dif"]>1, curtsi[filas,"dif"]/curtsi[filas,"weeks"]*2, 0)
curtsi[filas,"Dvc.red"]<-ifelse(curtsi[filas,"D.vc"]>=curtsi[filas,"red"], curtsi[filas,"red"], curtsi[filas,"D.vc"])
curtsi[filas, "accu.Dvc.red"]<-curtsi[filas,"Dvc.red"]+curtsi[filas-1,"accu.Dvc.red"]
curtsi[filas,"Dn.mod"]<-curtsi[filas,"Dn"]-curtsi[filas,"accu.Dvc.red"]
}
curtsi[,5:7]<-round(curtsi[,5:7],2)
curtsi$pvc.red<-curtsi$Dvc.red*100/curtsi$D.vc
curtsi$pvc.red[is.na(curtsi$pvc.red)]<-0

return(curtsi)}

calculate.yield<-function(cs, reduction){
# csp = crop surface proportions

ct<-colnames(cs)[-c(ncol(cs), ncol(cs)-1)]
if(sum(ct=="hyear")>0){ct<-ct[-which(ct=="hyear")]}
wr.tables<-lapply(ct, function(x) {read.table(file.path(infolder, "Cd", paste0(x, "_WR_AC.txt")), header=T)}) #tonne/ha

yield<-matrix(data=NA, nrow=nrow(cs), ncol=length(ct)+1)
yield[,1]<-start_year:end_year
first.year<-unlist(lapply(1:length(ct), function(x) substr(wr.tables[[x]][1,5],7,10)))
first.year[first.year<start_year]<-start_year

for(i in seq(along=ct)){
first.yield<-which(yield[,1]==first.year[i])
first.wr<-which(substr(wr.tables[[i]][,5],7,10)==first.year[i])
last.wr<-nrow(wr.tables[[i]])
yield[first.yield:nrow(yield),i+1]<-wr.tables[[i]]$Yield[first.wr:last.wr]
}
colnames(yield)<-c("years", ct)

total.yield<-yield[,-1]*cs[,match(colnames(yield)[-1],colnames(cs))]*area/3
total.yield[,c("SCM", "LCM")]<-total.yield[,c("SCM","LCM")]*(1-reduction/100)
total.yield[reduction>100,c("SCM", "LCM")]<-0
total.yield<-data.frame(hyear=yield[,1], total.yield, pvc.red=reduction)

return(total.yield)}

