####################
# INPUT PARAMETERS #
####################

# folder and path
infolder<-file.path(dirname(path), "input_files")

# farmer decision dates
FD.dy=c("11-15", "02-01", "04-15", "05-25")
crops<-c("SCB", "SCBl", "LCB", "LCM", "SCM", "ALF", "PCH")
opt.thresholds<-c(tv.nov=62, tv.feb=62, tv.apr=85, tv.may=82)

# irrigation season start and end dates 
SIS.dy<-"03-01" # start irrigation season (day)
EIS.dy<-"09-30" # end irrigation season (day)

# Period modelled (latest possible date 30 sep 2014)
# years considered: 2000-2014 (years for which there is information about curtailments). Some function parameters are defined only for this period.
start_year<-2001
end_year<-2014

eff.riego<-0.8
allo.factor<-0.55

# area: are issigated from Barasona only = upper zone area (Area zona Alta)
# p.alf & p.woo: Portion of crops in the upper zone area
area=51747 #ha
p.alf=0.33
p.woo=0.33

val.barley=159 #euro/1000kg
val.maize=171.3
cos.barley<-496 #euro/ha
cos.maize<-1807 #euro/ha

# farmers
farmers.file<-file.path(infolder, "farmers_mid.txt")