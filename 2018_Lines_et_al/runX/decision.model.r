
decision.model<-function(input.file, tv.nov, tv.feb, tv.apr, tv.may){
source(file.path(path, "model_parameters.r"))
source(input.file)
source(file.path(dirname(path), "functions.r"))
name<-paste(tv.nov, tv.feb, tv.apr, tv.may, sep="_")

### INPUT TABLE ###

# Farmer parameters and availability thresholds
farmers<-read.table(farmers.file)
first.row<-which(rownames(farmers)==start_year)
farmers<-farmers[first.row:nrow(farmers),]
add_cols<-matrix(ncol=4, nrow=nrow(farmers))
mod.p<-cbind(farmers, add_cols)


th<-define.thresholds(mod.p, variation, var.vals, av_snow=additional_information, tv.nov, tv.feb, tv.apr, tv.may)
mod.p[!is.na(match(rownames(mod.p), rownames(th))),6:9]<-th
colnames(mod.p)[6:9]<-colnames(th)
rows.with.NA<-unique(which(is.na(mod.p), arr.ind=T)[,1])
if(length(rows.with.NA)>0){mod.p<-mod.p[-rows.with.NA,]}

### output lists ###

# farmer output: CSa
# manager output: Cse and curts (curtailments)
CSa<-list()
CSe<-list()
curts<-list()
Vt<-list()
FD.Va<-list()
AD<-list()

### FARMER DECISION 1 (before SIS) ####

FD1.dy<-FD.dy[as.Date(FD.dy, format="%m-%d")<as.Date(SIS.dy, format="%m-%d")|as.Date(FD.dy, format="%m-%d")>as.Date(EIS.dy, format="%m-%d")]
Vo1<-read.table(Vo.file, header=T)
first.row<-which(Vo1$hyears==start_year)
Vo1<-Vo1[first.row:nrow(Vo1),]
CSai<-define.cs(mod.p, FD.dy, FD1.dy, av.source=Vo1, farmer.decision=T)
rownames(CSai)<-rownames(mod.p)
CSa[[1]]<-CSai

### IRRIGATION SEASON ###

# initiate table
Vt0<-read.table(file.path(dirname(path), "input_files", "Vt0.txt"))
Vt0$dates<-as.Date(Vt0$dates)
# Vt0<-start.Vt()
FD.Va[[1]]<-Vo1[-ncol(Vo1)]

# sequential filling of D and V
hyear<-start_year:end_year

MD.dt<-do.call("c", lapply(hyear, function(x) {
SIS.dt<-paste(x, SIS.dy, sep="-")
EIS.dt<-paste(x, EIS.dy, sep="-")
MD.dt.y<-seq.Date(as.Date(SIS.dt, format="%Y-%m-%d"), as.Date(EIS.dt, format="%Y-%m-%d"), 14)
return(MD.dt.y)}))
MD.dy<-unique(substr(MD.dt,6,10))

MD.blocks<-c(SIS.dy, setdiff(FD.dy, FD1.dy), EIS.dy)
MD.blocks.m<-MD.dy[unlist(lapply(MD.blocks, function(x) which.min(abs(MD.dt[1:length(MD.dy)]-as.Date(paste(hyear[1], x, sep="-"))))))]
n<-length(MD.blocks)

for (i in 1:(n-1)){
FD.dyi<-MD.blocks[c(-1, -(0:i), -n)]

if(length(FD.dyi)>0){
bn<-MD.blocks.m[which(MD.blocks==FD.dyi[1])] # last MD of the block
bi<-MD.blocks.m[which(MD.blocks==FD.dyi[1])-1] # first date of the block
bdt.all<-do.call(c, lapply(hyear, function(x) seq(as.Date(paste(x, bi, sep="-")),as.Date(paste(x, bn, sep="-")),1)))
bdy<-MD.dy[which(MD.dy==bi):which(MD.dy==bn)]

Vti<-Vt0[Vt0$dates%in%bdt.all,]


# FARMER DECISION 2 (Part 1): table AI (from Vti), D (calculated from CSa), AD and V [sum of V(i-1)+AI(i)-AI(i-1)-D(i)] for the block.

Vti<-block.demand(cs=CSa[[i]], vol.table=Vti, bdt.all)


# MANAGER DECISION 1 (before last FD)

# Parameters for Dn: Csn (requires AId, Dd and Vd)
# AId table (derived from AIi, pAi -> values from An file)
AIsd<-table.AIsd(MD.blocks, FD.dyi, An.file, percentiles_file, MD.dt)

# Dsn
Dsn<-rep(NA, length(MD.dt))

## Ds:i
pos1<-match(Vti$dates,MD.dt)
Dsn[pos1]<-Vti$D

## Di:n
Davg<-read.table(Davg.file, header=T)
pos2<-which(is.na(Dsn))
pos3<-match(substr(MD.dt[pos2],6,10), Davg$dy)
Dsn[pos2]<-Davg[pos3,"davg"]

Dsn<-data.frame(year=substr(MD.dt, 1,4), Dsn)
ADsn<-aggregate(Dsn$Dsn, by=list(Dsn$year), FUN=sum)

y<-match(substr(Vti$dates,1,4), ADsn$Group.1)
Vti$Dn<-ADsn$x[y]

# AIne
AIne.all<-availability(reservoir_inflow, percentiles_file, MD.dt, write.file=F)
Vti$AIne<-AIne.all[AIne.all$MD.dt%in%Vti$dates, "AIne"]
Vti$AIne.eff<-AIne.all[AIne.all$MD.dt%in%Vti$dates, "AIne"]*allo.factor

# curtailments
curts[[i]]<-curtailment.decision2(Vti, curts)
Vti<-data.frame(Vti, curts[[i]][,4:7])

# NEXT FARMER DECISION (FARMER DECISION 2 - Part 2)
FD.dt.m<-MD.blocks.m[MD.blocks==FD.dyi[1]]
FD.data<-Vti[which(substr(Vti$dates,6,10)==FD.dt.m),]

## threshold update considering curtailments (if there is curtailment threshold=999)
m<-month.abb[as.numeric(substr(FD.dt.m,1,2))]
mcol<-grep(substr(m,2,3), colnames(mod.p))
mod.p2<-mod.p
mod.p2[FD.data$red>0,mcol]<-999 

## crop surfaces
Vi<-Vti[which(substr(Vti$dates,6,20)==FD.dt.m),"V"]
Vfd<-data.frame(FD.Va[[i]], Vi)
colnames(Vfd)[ncol(Vfd)]<-paste0("a.", m)
CSai<-define.cs(mod.p2, FD.dy, FD.dyi=FD.dyi[1], av.source=Vfd, farmer.decision=T)
rownames(CSai)<-rownames(mod.p2)
CSa[[i+1]]<-CSa[[i]]+CSai
FD.Va[[i+1]]<-Vfd
Vt[[i]]<-Vti

write.table(CSa, file.path(path, TX, paste0("CSa", name, ".txt")))
write.table(FD.Va[[i+1]], file.path(path, TX, "FD_V.txt"))

} else {

# MANAGER DECISION 2 (after last FD)

bn<-MD.blocks.m[which(MD.blocks==MD.blocks[n])] # last MD of the block
bi<-MD.blocks.m[which(MD.blocks==MD.blocks[n-1])] # first date of the block
bdt.all<-do.call(c, lapply(hyear, function(x) seq(as.Date(paste(x, bi, sep="-")),as.Date(paste(x, bn, sep="-")),1)))
bdy<-MD.dy[which(MD.dy==bi):which(MD.dy==bn)]

Vti<-Vt0[Vt0$dates%in%bdt.all,]
Vti<-block.demand(cs=CSa[[i]], vol.table=Vti, bdt.all)

# AIn
AIne.all<-availability(reservoir_inflow, percentiles_file, MD.dt, write.file=F)
Vti$AIne<-AIne.all[AIne.all$MD.dt%in%Vti$dates, "AIne"]
Vti$AIne.eff<-AIne.all[AIne.all$MD.dt%in%Vti$dates, "AIne"]*allo.factor

# Dn
# ADna (s:i) - already in table Vti
# ADne (i:n) 
Davg<-read.table(Davg.file, header=T)
Vti$Davgi<-unlist(lapply(as.character(substr(Vti$dates,6,10)), function(x) Davg[which(as.character(Davg$dy)==x),"davg"]))
Vti$AinDavg<-c(rev(cumsum(rev(Vti$Davgi[2:length(bdy)]))),0)
# Dn
Vti$Dn<-Vti$AD+Vti$AinDavg
Vt[[i]]<-Vti

# curtailments
curts[[i]]<-curtailment.decision2(Vti, curts)
}}

curts2<-do.call(rbind, curts)
curts2<-curts2[order(curts2$dates),]
curts2<-curts2[!duplicated(curts2[,1]),]
write.table(curts2, file.path(path, TX, paste0("curts_", name, ".txt")), col.names=T, row.names=F)

Vt2<-rbind(Vt[[1]][,1:10], Vt[[2]][,1:10], Vt[[3]][,1:10])
Vt2<-Vt2[order(Vt2$dates),]
Vt2<-Vt2[!duplicated(Vt2[,1]),]
write.table(Vt2, file.path(path, TX, paste0("Vt_", name, ".txt")), col.names=T, row.names=F)

# reduction
surf.red<-data.frame(hyear=as.numeric(substr(MD.dt,1,4)), curts2$pvc.red)
surf.red.max<-aggregate(surf.red[,-1], by=list(surf.red$hyear), max)

# yield
yield<-calculate.yield(CSa[[i]], surf.red.max$x)
write.table(yield, file.path(path, TX, paste0("yield_", name, ".txt")))

# value
gain.barley<-rowSums(yield[,c("SCB", "SCBl", "LCB")]*val.barley)
gain.maize<-rowSums(yield[,c("LCM", "SCM")]*val.maize)
cost.barley<-rowSums(CSa[[i]][,c("SCB", "SCBl", "LCB")]*cos.barley*area/3)
cost.maize<-rowSums(CSa[[i]][,c("LCM", "SCM")]*cos.maize*area/3)
val<-gain.barley+gain.maize-cost.barley-cost.maize
write.table(val, file.path(path, TX, paste0("val_", name, ".txt")))
return(val)
}
