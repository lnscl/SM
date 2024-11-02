
#################
# MODEL OPTIONS #
#################

additional_information=T
calculate.an=T
new.farmers=F
perfect.info=F

# thresholds
ths.snow<-c(th.Nov=25, th.Feb=35, th.Apr=15, th.May=20)
variation=T
var.vals=c(20,20,20,0)

###############
# INPUT FILES #
###############

TX="T4"

# availability
reservoir_inflow<-file.path(infolder, "9013_9047_accuvol.txt")
percentiles_file<-file.path(infolder, "percentiles_9013_9047_accuvol.txt")
reservoir_vol<-file.path(infolder, "9848_volume.csv")
snow_file<-file.path("D:", "Data", "MOD10A2", "snow_pixels_count.txt")
av.snow.file<-file.path(infolder, "Av_snow.txt")
An.file<-file.path(infolder, "An.txt")
Vo.file<-file.path(infolder, "Av_Vo.txt")
Vo.file.all<-file.path(infolder, "Av_Vo_all.txt")

# demand
Davg.file<-file.path(infolder, "Davg_00-14.txt")
wrt.file<-file.path(infolder, "table_wr.txt")






 