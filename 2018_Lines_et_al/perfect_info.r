
perfect_info<-function(input.file){
source(file.path(path, "model_parameters.r"))
source(input.file)
source(file.path(dirname(path), "functions.r"))

# farmers
farmers<-read.table(farmers.file)
first.row<-which(rownames(farmers)==start_year)
farmers<-farmers[first.row:nrow(farmers),]

# Managers' decision dates
hyear<-start_year:end_year
MD.dt<-do.call("c", lapply(hyear, function(x) {
SIS.dt<-paste(x, SIS.dy, sep="-")
EIS.dt<-paste(x, EIS.dy, sep="-")
MD.dt.y<-seq.Date(as.Date(SIS.dt, format="%Y-%m-%d"), as.Date(EIS.dt, format="%Y-%m-%d"), 14)
return(MD.dt.y)}))

## Accumulated inflow
inflow<-read.table(reservoir_inflow, header=T)
inflow.dt<-as.Date(paste(inflow$year, sprintf("%02d", inflow$month), sprintf("%02d", inflow$day), sep="-"))
EIS.dts<-as.Date(paste(hyear, EIS.dy, sep="-"))
pos<-match(substr(MD.dt,1,4), substr(EIS.dts,1,4))

# Availability at the end of the season based on percentiles
AIne<-availability(reservoir_inflow, percentiles_file, write=T)
A<-data.frame(MD.dt=AIne$MD.dt, An=AIne$AIne, An.eff=AIne$AIne*allo.factor)

# matrix of possible farmers' decisions
# all situations starting with 1 have the same outcome -> only 1,1,1,1 kept.
# 0,1,1,1, and 0,1,0,1 have the same outcome since decision 3 only matters if both 1 and 2 are 0. Only the first kept.
mpos<-expand.grid(c(0,1), c(0,1), c(0,1), c(0,1))
a<-which(mpos[,1]==1&rowSums(mpos==0)>0)
r0101<-c(0,1,0,1)
r0100<-c(0,1,0,0)
colnames(mpos)<-NULL
b<-which(apply(mpos, 1, identical, r0101))
e<-which(apply(mpos, 1, identical, r0100))
mpos<-mpos[-c(a,b,e),]
rownames(mpos)<-1:nrow(mpos)
mpos<-as.matrix(mpos)

# crop surfaces for each option
pycrops<-lapply(1:nrow(mpos), function(i) {

pcrops<-lapply(1:nrow(farmers), function(y){

T1<-farmers[y,"T1"]
T2<-farmers[y,"T2"]
R1<-farmers[y,"R1"]
R2<-farmers[y,"R2"]
R3<-farmers[y,"R3"]

# decision
crops<-matrix(0, ncol=7, nrow=1)
colnames(crops)<-c("SCB", "SCM", "LCM", "SCBl", "LCB", "FAL1", "FAL2")

if(mpos[i,1]==1){
crops[1, "SCB"]<-T1
crops[1, "SCM"]<-T1
crops[1, "LCM"]<-T2
} else {
crops[1, "SCB"]<-T1
crops[1, "LCB"]<-T2*R1}

if(mpos[i,1]==0){
	ifelse(mpos[i,2]==1,
	crops[1, "LCM"]<-T2*(1-R1),
	crops[1, "SCBl"]<-T2*R2)}

if(mpos[i,1]==0&mpos[i,2]==0){
	ifelse(mpos[i,3]==1,
	crops[1, "LCM"]<-T2*R3,
	crops[1, "FAL1"]<-T2*R3)}

if(mpos[i,1]==0){
	ifelse(mpos[i,4]==1,
		crops[1, "SCM"]<-T1,
		crops[1, "FAL2"]<-T1)}

return(crops)
})
pcrops<-do.call(rbind, pcrops)
rownames(pcrops)<-rownames(farmers)
return(pcrops)
})

# crop demand for each option
pcd<-lapply(1:length(pycrops), function(x){
cs<-data.frame(hyear=rownames(pycrops[[x]]), pycrops[[x]])
crop.demand(cs)})

# curtailment decision for each option
D<-data.frame(MD.dt, pcd)
D<-D[,-grep("dates", colnames(D))]
Dtot.cols<-grep("Dtot", colnames(D))
Dvc.cols<-grep("Dvc", colnames(D))
Dn<-aggregate(D[,Dtot.cols], by=list(substr(D$MD.dt,1,4)), sum)
Dn.vc<-aggregate(D[,Dvc.cols], by=list(substr(D$MD.dt,1,4)), sum)
colnames(Dn)[colnames(Dn)=="Group.1"]<-"hyear"
colnames(Dn.vc)[colnames(Dn.vc)=="Group.1"]<-"hyear"

curts<-lapply(2:ncol(Dn), function(x){
A.D<-data.frame(A, Dn=Dn[pos,x], Dvc=D[,Dvc.cols[x-1]])
curtsi<-curtailment.decision(A.D)})

df.curts<-data.frame(MD.dt, curts)
surf.red<-df.curts[,grep("pvc.red", colnames(df.curts))]
surf.red<-data.frame(hyear=as.numeric(substr(MD.dt,1,4)), surf.red)
surf.red.max<-aggregate(surf.red[,-1], by=list(surf.red$hyear), max)

# yield for each option
yield<-lapply(1:length(pycrops), function(x) calculate.yield(pycrops[[x]], reduction=surf.red.max[,x+1]))
write.table(yield, file.path(path, TX, "yield.txt"))

# Value
val<-lapply(1:length(yield), function(x) {
gain.barley<-rowSums(yield[[x]][,c("SCB", "SCBl", "LCB")]*val.barley)
gain.maize<-rowSums(yield[[x]][,c("LCM", "SCM")]*val.maize)
cost.barley<-rowSums(pycrops[[x]][,c("SCB", "SCBl", "LCB")]*cos.barley*area/3)
cost.maize<-rowSums(pycrops[[x]][,c("LCM", "SCM")]*cos.maize*area/3)
val<-gain.barley+gain.maize-cost.barley-cost.maize
return(val)
})
val<-do.call(cbind, val)
write.table(val, file.path(path, TX, "val_all.txt"))

best.option<-unlist(apply(val, 1, which.max))
val2<-val[!is.na(match(rownames(val), names(best.option))),]
val.bo<-unlist(lapply(1:nrow(val2), function(x) val2[x,best.option[x]]))
write.table(data.frame(best.option, val=val.bo), file.path(path, TX, "best_option.txt"))
write.table(val.bo, file.path(path, TX, "val.txt"))

return(val.bo)
}
