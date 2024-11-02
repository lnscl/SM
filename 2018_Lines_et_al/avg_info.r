
avg_info<-function(input.file){
source(input.file)
val<-read.table(file.path(path, "T1", "val_all.txt"))

op<-which(colSums(val)==max(colSums(val)))
val.op<-val[,op]
if(class(val.op)!="numeric"){val.op<-val.op[,1]}

write.table(val.op, file.path(path, TX, paste0("val_", op[1], ".txt")))
return(val.op)
}
