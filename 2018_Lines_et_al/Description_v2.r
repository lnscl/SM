# Modifications (v10):
# - merged the code for optimised thresholds and 10 sets of thresholds. 
# - Single REV output table

# snow thresholds: 25/35/15/- ; if below the snow threshold -> new threshold for reservoir is higher

############


run.model<-function(folder){
# folder=name of the folder that contains the model.options.r and TX.r files. e.g. "run1"
assign("path", file.path("D:", "Blocks", "P2", "Tests", "180201_P2_v10d", folder), envir = .GlobalEnv)
input.files<-file.path(path, paste0("T", 1:4, ".r"))
outfolders<-file.path(path, paste0("T", 1:4))
lapply(outfolders, dir.create)

th10<-seq(35, 80, 5)
thm<-matrix(rep(th10,10), nrow=10, ncol=4)
colnames(thm)<-c("tv.nov", "tv.feb", "tv.apr", "tv.may")

run.T3y4<-function(x){
tv.nov<-x[1]
tv.feb<-x[2]
tv.apr<-x[3]
tv.may<-x[4]

source(file.path(dirname(path), "decision.model.r"))
decision.model(input.files[3], tv.nov, tv.feb, tv.apr, tv.may)
decision.model(input.files[4], tv.nov, tv.feb, tv.apr, tv.may)
}

apply(thm, 1, run.T3y4)

run.T3y4(opt.thresholds)

source(file.path(dirname(path), "perfect_info.r"))
perfect_info(input.files[1])

source(file.path(dirname(path), "avg_info.r"))
avg_info(input.files[2])

# REV
outpath<-file.path(path, "REV")
dir.create(outpath)
output.list<-list.files(outfolders[3], pattern="val")
val.opt<-paste0("val_", paste(opt.thresholds, collapse="_"), ".txt")
output.list<-output.list[-match(val.opt, output.list)]
val.pi<-read.table(file.path(outfolders[1], "val.txt"), header=T)
val.av<-read.table(list.files(outfolders[2], pattern="val", full=T), header=T)
REV<-matrix(NA, ncol=7, nrow=length(output.list)+1)
REV2<-do.call(rbind, lapply(c(output.list, val.opt), function(x) {
i<-match(x, c(output.list, val.opt))
file.names<-file.path(outfolders[3:4], x)
val.table<-lapply(seq(file.names), function(y) read.table(file.names[y], header=T))
val.table<-data.frame(val.pi, val.av, val.table)
colnames(val.table)<-c("perfect", "average", "res", "res+snow")
write.table(val.table, file.path(outpath, paste0("val.table", substr(x, 5,6), ".txt")))

val<-colSums(val.table)
REV.r<-(val["res"]-val["average"])/(val["perfect"]-val["average"])
REV.rs<-(val["res+snow"]-val["average"])/(val["perfect"]-val["average"])
REV[i,]<-c(as.integer(substr(x, 5,6)), val, REV=REV.r, REV=REV.rs)
}))
write.table(REV2, file.path(outpath, "REV.txt") )
}