# Davg

main.path<-"" #add working folder full path

Vt2<-rbind(Vt[[1]][,c("dates", "D", "D_vc")], Vt[[2]][,c("dates", "D", "D_vc")], Vt[[3]][,c("dates", "D", "D_vc")])
Vt2<-Vt2[order(Vt2$dates),]
Vt2<-Vt2[!duplicated(Vt2[,1]),]

a<-Vt2
a[,"dates"]<-substr(a[,"dates"],6,10)
Davg<-aggregate(a[,"D"], by=list(a[,"dates"]), FUN=mean)
Davg_vc<-aggregate(a[,"D_vc"], by=list(a[,"dates"]), FUN=mean)


Davg.df<-data.frame(Davg, Davg_vc$x)
colnames(Davg.df)<-c("dy", "davg", "davg_vc")
write.table(Davg.df, paste0(main.path, "input_files\\Davg_00-14.txt"))