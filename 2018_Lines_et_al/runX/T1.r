
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

TX="T1"

# availability
reservoir_inflow<-file.path(infolder, "9013_9047_accuvol.txt")
percentiles_file=file.path(infolder, "percentiles_9013_9047_accuvol.txt")
reservoir_vol<-file.path(infolder, "9848_volume.csv")

# demand
wrt.file<-file.path(infolder, "table_wr.txt")


 