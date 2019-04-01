setwd("~/Documents/PostDoc/OzFlux")

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

par(mfrow=c(2,1))
makeTplot(sitename="Tumbarumba")
makeTplot(sitename="WombatStateForest")
makeTplot(sitename="AdelaideRiver")
makeTplot(sitename="Calperum")
makeTplot(sitename="CapeTribulation")
makeTplot(sitename="CowBay")
makeTplot(sitename="CumberlandPlains")
makeTplot(sitename="DalyPasture")
makeTplot(sitename="DalyUncleared")
makeTplot(sitename="DryRiver")
makeTplot(sitename="Emerald")
makeTplot(sitename="Gingin")
makeTplot(sitename="GreatWesternWoodlands")
makeTplot(sitename="HowardSprings")
makeTplot(sitename="Otway")
makeTplot(sitename="RiggsCreek")
makeTplot(sitename="Whroo")
makeTplot(sitename="Yanco")


sitename <- "Tumbarumba"
tmet <- nc_open(paste0(sitename,"OzFlux2.0_met.nc"))
SW <- ncvar_get(tmet,varid="SWdown")
tair <- ncvar_get(tmet,varid="Tair") - 273.15
tflx <- nc_open(paste0(sitename,"OzFlux2.0_flux.nc"))
gpp <- ncvar_get(tflx,varid="GPP")
all <- data.frame(tair,SW,gpp)
day <- subset(all,SW > 500)
day$tair2 <- day$tair^2
fit <- rq(gpp~tair,tau=0.5,data=day[1:500,])
summary(lm(gpp~tair+tair2,data=day[1:500,]))
