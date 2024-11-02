path<-"" #add folder (full path) that contains the run details 
outfolder <- "" #add path of folder to save the plot
meses<-c("Nov", "Feb", "Apr", "May")
hyear<-2001:2014

library(reshape2)
library(ggplot2)

# best.option
bo0<-read.table(paste0(path, "T1//best_option.txt"))
## matrix options
mpos<-matrix(c(0,0,0,0,0,0,1,0,0,1,2,0,0,0,0,1,0,0,1,1,0,1,2,1,1,2,2,2), nrow=7, ncol=4, byrow=T)
## bo and mpos
bo<-bo0[,"best.option"]
m<-mpos[bo,]
colnames(m)<-paste0(meses)
m2<-melt(m)
m2<-data.frame(hyear, m2)

# Reservoir level (2 obs, 2 modelled)
Vm<-read.table(paste0(path, "T3\\FD_V.txt"))
Vm<-Vm[,-1]
colnames(Vm)<-meses
Vm2<-melt(Vm)
Vm2<-data.frame(hyear, Vm2, type="reservoir level [hm3]", op=m2[,4])

# snow coverage
snw<-read.table(paste0(dirname(path), "\\input_files\\Av_snow.txt"), header=T)
snw<-snw[-nrow(snw),-1]
colnames(snw)<-meses
snw2<-melt(snw)
snw2<-data.frame(hyear, snw2, type="snow coverage [%]", op=m2[,4])

# combine
dat<-rbind(Vm2, snw2)


cols<-c("firebrick1", "deepskyblue", "goldenrod1")
par(mfrow=c(3,4), cex.lab=1.5, cex.main=1.7, cex.axis=1.5)


thrs<-expand.grid(variable = c("Nov", "Feb", "Apr", "May"), type = c("reservoir level [hm3]", "reservoir level [hm3]", "snow coverage [%]"))
thrs<-data.frame(thrs, th=c(62, 62, 62, 82, 82, 82, 82, NA, 25, 35, 15, NA), thresholds=c(rep("individual", 4), rep("combined", 4), rep("individual", 4)))
units<-c("reservoir"="reservoir level [hm3]", "snow"="snow coverage [%]")

# plot
p<-ggplot(dat, aes(as.factor(hyear), value))+
geom_point(aes(colour=as.factor(op)))+
scale_colour_manual("optimal course", values=c("firebrick1", "deepskyblue", "goldenrod1"), labels=c("poor availability", "good availability", "indifferent"))+
facet_grid(type~variable)+
geom_hline(data=thrs, aes(yintercept=th, linetype=thresholds))+
scale_linetype_discrete(breaks=c("individual", "combined"))+
labs(y="", x="year")+
scale_x_discrete(breaks=seq(2001, 2013, 2), labels=c("01", "", "05", "", "09", "", "13"))

png(file.path(outfolder, "optimal_thresholds.png")), width=900, height=900, res=150)
print(p)
dev.off()
