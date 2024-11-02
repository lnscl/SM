

#################
# MODEL OPTIONS #
#################

additional_information=F
calculate.an=T
new.farmers=F
perfect.info=F


###############
# INPUT FILES #
###############

TX="T3"

# availability
reservoir_inflow<-file.path(infolder, "9013_9047_accuvol.txt")
percentiles_file<-file.path(infolder, "percentiles_9013_9047_accuvol.txt")
reservoir_vol<-file.path(infolder, "9848_volume.csv")
An.file<-file.path(infolder, "An.txt")
Vo.file<-file.path(infolder, "Av_Vo.txt")
Vo.file.all<-file.path(infolder, "Av_Vo_all.txt")

# demand
Davg.file<-file.path(infolder, "Davg_00-14.txt")
wrt.file<-file.path(infolder, "table_wr.txt")





 