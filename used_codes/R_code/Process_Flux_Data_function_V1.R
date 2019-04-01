##############################################################################
##Process raw OzFlux data
##Converting from nc to csv time series datasets
##Functions
##Last modified: July-04-2016
##############################################################################
#######################------REQUIRED PACKAGES------##########################
require(ncdf4)
require(quantreg)
require(zoo)
require(gam)

##############################################################################
##########################------FUNCTIONS------###############################
##############################################################################

##############################################################################
##PLOT 
makeTplot <- function(sitename){
  
  tmet <- nc_open(paste0(sitename,"OzFlux2.0_met.nc"))
  tair <- ncvar_get(tmet,varid="Tair") - 273.15
  rain <- ncvar_get(tmet,varid="Rainf")*3600
  SW <- ncvar_get(tmet,varid="SWdown")
  
  tflx <- nc_open(paste0(sitename,"OzFlux2.0_flux.nc"))
  tgpp <- ncvar_get(tflx,varid="GPP")
  tnee <- ncvar_get(tflx,varid="NEE")
  
  all <- data.frame(tair,rain,SW,tgpp,tnee)
  
  with(subset(all,SW>500),plot(tair,tgpp,main=sitename,xlim=c(-10,40),
                               xlab="Air Temperature (deg C)", ylab="GPP (umol m-2 s-1)"))
  with(subset(all,SW>0),plot(SW,tair,main=sitename,
                             xlab="shortwave", ylab="GPP (umol m-2 s-1)"))
  #  with(subset(all,SW>0),points(tair,-tnee,main=sitename,col="red"))
  
  all$tair2 <- all$tair^2
  fit <- rq(tgpp~tair+tair2,tau=0.95,data=all)
  summary(fit)
}