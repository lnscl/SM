
#################
# MODEL OPTIONS #
#################

additional_information=F
calculate.an=F
new.farmers=F

# thresholds
red=F
red.val=NA

###############
# INPUT FILES #
###############

TX="T2"

# availability
reservoir_inflow<-file.path(infolder, "9013_9047_accuvol.txt")
percentiles_file<-file.path(infolder, "percentiles_9013_9047_accuvol.txt")
An.file<-file.path(infolder, "An.txt")
Vo.file<-file.path(infolder, "Vo.avg.txt")
Vo.file.all<-file.path(infolder, "Av_Vo_all.txt")

# demand
Davg.file<-file.path(infolder, "Davg_00-14.txt")
wrt.file<-file.path(infolder, "table_wr.txt")



 