##############################################################################
##Process raw OzFlux data and GDY output (daily)
##
##Scripts
##Last modified: July-08-2016
##############################################################################
require(ncdf)
require(quantreg)
require(zoo)
require(gam)
require(Metrics)
require(mgcv)
require(sm)
##############################################################################
##All OzFlux sites that I have data of
sitename = "AdelaideRiver"
sitename = "Calperum"
sitename = "CapeTribulation"
sitename = "CowBay"
sitename = "CumberlandPlains"
sitename = "DalyPasture"
sitename = "DalyUncleared"
sitename = "DryRiver"
sitename = "Emerald"
sitename = "Gingin"
sitename = "GreatWesternWoodlands"
sitename = "HowardSprings"
sitename = "Otway"
sitename = "RedDirtMelonFarm"
sitename = "RiggsCreek"
sitename = "Samford"
sitename = "SturtPlains"
sitename = "Tumbarumba"
sitename = "Whroo"
sitename = "WombatStateForest"
sitename = "Yanco"

##############################################################################
####To define which group of SiteName to use
####If G = 1, then run forested sites that started midnight exactly
####If G = 2, then run sites that started 30min/1h after midnight
G <- 1
####If G = 1, then run sites that started midnight exactly
####If G = 2, then run sites that started 30min/1h after midnight
ifelse(G == 1,
       SiteName <- c("Calperum","CapeTribulation","CowBay","CumberlandPlains",
                     "DalyPasture","DalyUncleared","DryRiver","Emerald",
                     "Gingin","Samford","SturtPlains","Whroo",
                     "WombatStateForest","Yanco"),
       SiteName <- c("GreatWesternWoodlands","HowardSprings",
                     "RiggsCreek"))

##############################################################################

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  ##############################################################################
  
  ##Read Ozflux data 
  tmet <- open.ncdf(paste0("~/Documents/PostDoc/OzFlux/",sitename,"OzFlux2.0_met.nc"))
  #print(tmet)
  
  lat <- get.var.ncdf(tmet,"latitude")
  lon <- get.var.ncdf(tmet,"longitude")
  #SW <- get.var.ncdf(tmet,varid="SWdown")
  
  t <- get.var.ncdf(tmet, "time")
  tunits <- att.get.ncdf(tmet, "time","units")
  nt <- dim(t)
  sec.var <- as.numeric(t[2]-t[1])
  
  tair <- get.var.ncdf(tmet,"Tair") - 273.15
  ifelse(sec.var == 1800, 
         rain <- get.var.ncdf(tmet,"Rainf")*1800,
         rain <- get.var.ncdf(tmet,"Rainf")*3600)
  
  #qair <- get.var.ncdf(tmet, varid="Qair")
  #wind <- get.var.ncdf(tmet, varid="Wind")
  #psurf <- get.var.ncdf(tmet, varid="PSurf")
  #LW <- get.var.ncdf(tmet, varid="LWdown")
  #co2 <- get.var.ncdf(tmet, varid="CO2air")
  #swqc <- get.var.ncdf(tmet, varid="SWdown_qc")
  #tairqc <- get.var.ncdf(tmet, varid="Tair_qc")
  #ranfqc <- get.var.ncdf(tmet, varid="Rainf_qc")
  #qairqc <- get.var.ncdf(tmet, varid="Qair_qc")
  #windqc <- get.var.ncdf(tmet, varid="Wind_qc")
  #psurfqc <- get.var.ncdf(tmet, varid="PSurf_qc")
  #co2qc <- get.var.ncdf(tmet, varid="CO2air_qc")
  #lwqc <- get.var.ncdf(tmet, varid="LWdown_qc")
  #refh <- get.var.ncdf(tmet, varid="reference_height")
  #utc <- get.var.ncdf(tmet, varid="utc_offset")
  #avep <- get.var.ncdf(tmet, varid="averagePrecip")
  #avet <- get.var.ncdf(tmet, varid="averageTemp")

  
  #myDF <- data.frame(lat, lon, SW, tair, rain, qair, wind, psurf, LW, co2, swqc, tairqc, ranfqc, qairqc, windqc, psurfqc, co2qc,
  #                   lwqc, refh,utc, avep, avet)
  
  close.ncdf(tmet)
  
  tflx <- open.ncdf(paste0("~/Documents/PostDoc/OzFlux/",sitename,"OzFlux2.0_flux.nc"))
  
  #print(tflx)
  
  qle <- get.var.ncdf(tflx, varid="Qle")
  tgpp <- get.var.ncdf(tflx,varid="GPP")
  tnee <- get.var.ncdf(tflx,varid="NEE")
  
  ozDF <- data.frame(lat,lon,tair,rain,tgpp,tnee, qle)
  
  close.ncdf(tflx)
  
  t.start <- substr(tunits$value,15,33)
  
  ifelse(sec.var == 1800, t.end <- nt/48, t.end <- nt/24)
  head(t)
  
  year.start <- as.numeric(substr(t.start, 1, 4))
  year.end <- round(t.end/365 - 1 + year.start)
  
  ##check for G name
  ifelse(G == 1, 
         ifelse(sec.var == 1800,
                t_series <- seq(from = as.POSIXct(t.start),
                                to = as.POSIXct(paste(year.end,"-12-31 23:30", sep="")),
                                by = paste(sec.var, " sec", sep="")),
                t_series <- seq(from = as.POSIXct(t.start),
                                to = as.POSIXct(paste(year.end,"-12-31 23:00", sep="")),
                                by = paste(sec.var, " sec", sep=""))),
         ifelse(sec.var == 1800,
                t_series <- seq(from = as.POSIXct(t.start),
                                to = as.POSIXct(paste(year.end+1,"-01-01 00:00", sep="")),
                                by = paste(sec.var, " sec", sep="")),
                t_series <- seq(from = as.POSIXct(t.start),
                                to = as.POSIXct(paste(year.end+1,"-01-01 00:00", sep="")),
                                by = paste(sec.var, " sec", sep=""))))
  
  ozDF <- cbind(t_series, ozDF)
  
  #ozDF$Trans <- ozDF$qle / ((2.501E6 - 2.365E3 * (ozDF$tair + 273.15)) * 18E-3) * 1800
    

  Sum1 <- aggregate(ozDF['tgpp'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    sum)
  
  Sum2 <- aggregate(ozDF['tnee'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    sum)
  
  Sum3 <- aggregate(ozDF['rain'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    sum)
  
  mean1 <- aggregate(ozDF['tair'],
                     list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                     mean)
  
  min1 <- aggregate(ozDF['tair'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    min)
  
  max1 <- aggregate(ozDF['tair'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    max)
  
  Sum4 <- aggregate(ozDF['qle'],
                    list(day = cut(as.POSIXct(ozDF$t_series),"day")),
                    sum)
  
  ozDF_new <- cbind(mean1, min1$tair, max1$tair, Sum3$rain, Sum1$tgpp, Sum2$tnee, Sum4$qle)
  
  colnames(ozDF_new) <- c("Date","Tair_mean","Tair_min","Tair_max","Rain",
                          "GPP","NEE","Transpiration")
  
  ozDF_new$GPP <- ozDF_new$GPP* 0.000001*12*sec.var    ##if hourly, change 1800 to 3600
  ozDF_new$NEE <- ozDF_new$NEE* 0.000001*12*sec.var    ##if hourly, change 1800 to 3600
  ozDF_new$Transpiration <- ozDF_new$Transpiration* (1.0/(2.45*1E6))*sec.var     ##if hourly, change 1800 to 3600
  
  
  ifelse(G == 1,
         ozDF <- ozDF,
         ozDF <- ozDF[-nrow(ozDF),])
  
  ##Ozflux DF ready to use
  
  ##############################################################################
  
  ##read gday simulated outputs
  gdayDF <- read.table(paste(getwd(), "/outputs/", sitename, "_simulation.csv", sep=""),
                       sep=",",skip=4)
  
  colnames(gdayDF) <- c("Year","DOY","Total_swc","NEP","GPP","NPP","C_exudation","C_VOC_Flux",
                        "R_eco","R_a","R_leaf_maint","R_wood_maint","R_fineroot_maint","R_growth",
                        "R_h","R_soil","ET","Transpiration","Soil_evap","Canopy_evap","Runoff",
                        "Drainage","Latent","Sensible","C_leaf","C_wood","C_coarse_root","C_fine_root",
                        "C_storage_TNC","C_fine_litter_total","C_fine_litter_above","C_fine_litter_below",
                        "C_course_litter","C_soil","C_leaf_growth","C_wood_growth","C_coarse_root_growth",
                        "C_fine_root_growth","C_reprod_growth","C_leaf_litterfall","C_coarse_root_litter",
                        "C_Fine_root_litter","C_wood_litter","LAI_projected","Leaf_C_over_area","N_Conc_leaf",
                        "N_leaf","N_wood","N_coarse_root","N_fine_root","N_storage","N_litter_above","N_litter_below",
                        "N_dead_wood","N_soil_total","N_mineral","N_organic","N_fix","N_leaf_litter","N_wood_litter",
                        "N_coarse_root_litter","N_fine_root_litter","N_biomass_uptake","N_gross_mineralization",
                        "N_net_mineralization","V_Volatilization","N_leaching","N_leaf_growth","N_wood_growth",
                        "N_corase_root_growth","N_fine_root_growth","Aborbed_PAR","Ave_daytime_canopy_conductance",
                        "Ave_daytime_aerodynamic_conductance","Ave_daytime_leaf_boundary_conductance","Soil_moist_stress",
                        "Foliage_retranslocation","Wood_retranslocation","Coarse_root_retranslocation",
                        "Fine_root_retranslocation","C_flux_from_litter_slow_to_active_soil_pool",
                        "C_flux_from_litter_active_to_slow_pool","C_flux_from_active_slow_to_passive_pool",
                        "C_flux_from_active_to_slow_pool","C_flux_from_active_to_passive_pool",
                        "C_flux_from_slow_to_active_pool","C_flux_from_slow_to_passitve_pool",
                        "C_flux_from_passive_to_active_pool","C_active_SOM_pool","C_slow_SOM_pool",
                        "C_passive_SOM_pool","CO2_efflux_from_surf_structural_litter",
                        "CO2_efflux_from_soil_structural_litter","CO2_efflux_from_surf_metabolic_litter",
                        "CO2_efflux_from_soil_metabolic_litter","CO2_efflux_from_fast_SOM",
                        "CO2_efflux_from_slow_SOM","CO2_efflux_from_passive_SOM","Temp_scalar_on_C_efflux_from_SOM",
                        "Root_exudation_C","Root_exudation_N","CO2_released_from_exudation","Total_C_flux_from_active",
                        "Residence_time_slow_pool","REXC_C_use_efficiency","Total_C_slow","Total_N_slow",
                        "Total_C_active","Total_N_active")
  
  gdayDF$Day <- ozDF_new$Date
  gdayDF$Day <- round(as.POSIXct(gdayDF$Day), unit = "day")
  
  
  ##############################################################################
  ##Plot time series comparison of climate, GPP, NEP and Transpiration data
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_time_series.pdf",sep=""))
  
  par(mfrow=c(2,2),
      mar=c(5.1,5.1,4.1,4.1))
  
  
  ##Plot temperature and rain data
  y.lab1 <- expression("Rain ( mm " * d^-1 * ")")
  y.lab2 <- expression("Tair (" * degree * "C" * ")")
  
  plot(gdayDF$Day,ozDF_new$Rain, type="l", xlab = "Date", ylab = y.lab1,
       col = "blue")
  par(new = T)
  plot(gdayDF$Day, ozDF_new$Tair_mean, xlab = NA, ylab = NA,yaxt="n",xaxt="n",
       col = "red")
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T)
  legend("topright", c("Rain", "Tair"), col = c("blue", "red"), lty= c(1,0), pch = c(NA, 16))
  
  temp.DF <- cbind(gdayDF$GPP, ozDF_new$GPP)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),0)+1
  
  ##Plot ozflux vs. gday outputs - GPP
  
  y.lab <- expression("GPP (g " * m^-2 * d^-1 * ")")
  
  plot(gdayDF$Day, gdayDF$GPP, xlab = "Date", ylab = y.lab, 
       ylim = c(v.low,v.high), col ="lightgreen")
  
  points(gdayDF$Day, ozDF_new$GPP, col="grey")
  
  gpp_mv <- rollmean(gdayDF$GPP, 30, fill = list(NA, NULL,NA))
  gdayDF$gpp_mv <- gpp_mv
  lines(gdayDF$Day, gdayDF$gpp_mv, lwd = 2, col="darkgreen")
  
  gpp_mv <- rollmean(ozDF_new$GPP, 30, fill = list(NA, NULL,NA))
  ozDF_new$gpp_mv <- gpp_mv
  lines(gdayDF$Day, ozDF_new$gpp_mv, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), lwd = c(2,2), lty = c(1,1))
  
  temp.DF <- cbind(gdayDF$Transpiration, ozDF_new$Transpiration)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),0)+1
  
  ##Plot ozflux vs. gday outputs - Transpiration
  y.lab <- expression("LE (kg " * m^-2 * d^-1 * ")")
  
  plot(gdayDF$Day, gdayDF$Transpiration, xlab = "Date", ylab = y.lab, 
       ylim = c(v.low,v.high), col ="lightblue")
  
  points(gdayDF$Day, ozDF_new$Transpiration, col="grey")
  
  tran_mv <- rollmean(gdayDF$Transpiration, 30, fill = list(NA, NULL,NA))
  gdayDF$tran_mv <- tran_mv
  lines(gdayDF$Day, gdayDF$tran_mv, lwd = 2, col="blue")
  
  tran_mv <- rollmean(ozDF_new$Transpiration, 30, fill = list(NA, NULL,NA))
  ozDF_new$tran_mv <- tran_mv
  lines(gdayDF$Day, ozDF_new$tran_mv, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "blue"), lwd = c(2,2), lty = c(1,1))
  
  temp.DF <- cbind(gdayDF$NEP, -ozDF_new$NEE)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),0)+1
  
  ##Plot ozflux vs. gday outputs - NEP
  y.lab <- expression("NEP (g " * m^-2 * d^-1 * ")")
  
  plot(gdayDF$Day, gdayDF$NEP, xlab = "Date", ylab = y.lab, 
       ylim = c(v.low,v.high), col ="lightgreen")
  
  points(gdayDF$Day, -ozDF_new$NEE, col="grey")
  
  nep_mv <- rollmean(gdayDF$NEP, 30, fill = list(NA, NULL,NA))
  gdayDF$nep_mv <- nep_mv
  lines(gdayDF$Day, gdayDF$nep_mv, lwd = 2,col = "green")
  
  nep_mv <- rollmean(-ozDF_new$NEE, 30, fill = list(NA, NULL,NA))
  ozDF_new$nep_mv <- nep_mv
  lines(gdayDF$Day, ozDF_new$nep_mv, lwd = 2, col="black")
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "green"), lwd = c(2,2), lty = c(1,1))
  
  dev.off()
  
  
  ##############################################################################
  ##Make a new file and save into each output_analyses folder
  
  newDF <- ozDF_new[,1:8]
  newDF <- cbind(newDF, gdayDF$Year, gdayDF$DOY, gdayDF$NEP, gdayDF$GPP, gdayDF$Transpiration, gdayDF$Day)
  colnames(newDF) <- c("date","Tair_mean","Tair_min","Tair_max","Rain","ozGPP","ozNEE","ozTrans",
                       "Year","DOY","gdayNEP","gdayGPP","gdayTrans","Date")
  newDF$date <- NULL
  newDF$Month <- format(newDF$Date, "%m")
  newDF <- as.data.frame(newDF, stringsAsFactors=F)
  newDF$Date <- as.Date(newDF$Date)
  newDF$ozNEP <- -newDF$ozNEE
  
  write.table(newDF, paste(getwd(), "/output_analyses/gday_ozflux_result.csv",sep=""),
              sep=",",row.names=F,col.names=T)
  
  
  ##############################################################################
  ##Plot temp vs. gpp site-model comparison
  pdf(paste(getwd(), "/output_analyses/",sitename,"_tempVSgpp.pdf",sep=""))
  
  par(mar = c(5.1,5.1,4.1,4.1))
  
  temp.DF <- cbind(gdayDF$GPP, ozDF_new$GPP)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),0)+1
  
  x.lab <- expression("Tair (" * degree * "C" * ")")
  y.lab <- expression("GPP (g " * m^-2 * d^-1 * ")")
  
  
  plot(ozGPP ~ Tair_mean, data = newDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.2), pch = 16)
  points(newDF$Tair_mean, newDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.2), pch = 16)
  
  
  #m1 <- gamm(ozGPP ~ s(Tair_mean, bs = "cc", k = 12), 
  #           data = newDF)
  #summary(m1$gam)
  #points(newDF$Tair_mean, fitted(m1$gam), col = "black", lwd = 2)
  
  #m2 <- gamm(gdayGPP ~ s(Tair_mean, bs = "cc", k = 12), 
  #           data = newDF)
  #summary(m2$gam)
  #points(newDF$Tair_mean, fitted(m2$gam), col = "green", lwd = 2)
  
  
  ##Using nls function
  p1 = 1
  p2 = 2
  fit <- nls(ozGPP ~ p1*Tair_mean^2 + p2*Tair_mean, start=list(p1=p1,p2=p2), data = newDF, trace=T)
  new <- data.frame(Tair_mean = seq(min(newDF$Tair_mean),max(newDF$Tair_mean), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair_mean)^2 + conf[p2,"2.5%"] * new$Tair_mean
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair_mean
  polygon(c(rev(new$Tair_mean), new$Tair_mean), c(rev(new$c975), new$c25), col = adjustcolor('grey80',alpha=0.3), border = NA)
  lines(new$Tair_mean,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair_mean, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair_mean, new$c975, col = "black", lty = 2, lwd = 2)
  
  ##Using nls function
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair_mean^2 + p2*Tair_mean, start=list(p1=p1,p2=p2), data = newDF, trace=T)
  new <- data.frame(Tair_mean = seq(min(newDF$Tair_mean),max(newDF$Tair_mean), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair_mean)^2 + conf[p2,"2.5%"] * new$Tair_mean
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair_mean
  polygon(c(rev(new$Tair_mean), new$Tair_mean), c(rev(new$c975), new$c25), col = adjustcolor('lightgreen',alpha=0.3), border = NA)
  lines(new$Tair_mean,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair_mean, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair_mean, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "green"), pch = 16)
  
  dev.off()
  
  ##############################################################################
  ##PLot model performance against GPP, LE and GPP (r2, RMSE, bias)
  
  ##ceiling of each fluxes
  gpp.c <- round(max(newDF$ozGPP, newDF$gdayGPP),0)
  nep.c <- round(max(newDF$ozNEP, newDF$gdayNEP),0)
  le.c  <- round(max(newDF$ozTrans, newDF$gdayTrans),0)
  
  ##floor of each fluxes
  gpp.f <- round(min(newDF$ozGPP, newDF$gdayGPP),0)
  nep.f <- round(min(newDF$ozNEP, newDF$gdayNEP),0)
  le.f  <- round(min(newDF$ozTrans, newDF$gdayTrans),0)
  
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_model_performance.pdf",sep=""))
  
  par(mfrow=c(3,2),
      mar=c(5.1,5.1,4.1,4.1))
  
  
  #########################     GPP          ############################
  
  x.lab <- expression("Obs. GPP (g " * m^-2 * d^-1 * ")")
  y.lab <- expression("Sim. GPP (g " * m^-2 * d^-1 * ")")
  
  plot(newDF$ozGPP, newDF$gdayGPP, xlab = x.lab,
       ylab = y.lab, xlim = c(0, gpp.c),
       ylim = c(0, gpp.c), col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline( a= 0, b = 1, col="red")
  
  reg <- lm(newDF$gdayGPP~newDF$ozGPP)
  adj.r.sq <- summary(reg)$adj.r.squared
  intercept <- round(coef(reg)["(Intercept)"],2)
  sl <- round(coef(reg)["newDF$ozGPP"],2)
  rmsev <- rmse(newDF$ozGPP, newDF$gdayGPP)
  
  
  leg <- vector("expression",4)
  leg[1] = substitute(expression(italic(SL) == a), 
                      list(a = format(sl, dig = 2)))[2]
  leg[2] = substitute(expression(italic(I/C) == a), 
                      list(a = format(intercept, dig = 2)))[2]
  leg[3] = substitute(expression(italic("adj. R")^2 == a),
                      list(a = format(adj.r.sq, dig = 3)))[2]
  leg[4] = substitute(expression(italic(MRSE) == a),
                      list(a = format(rmsev, dig = 3)))[2]
  
  abline(reg, lwd = 2, lty = 2)
  legend("topleft", c("1:1 line", "fit line"),
         col=c("red","black"), lwd = c(2,2), lty = c(1, 2))
  legend("bottomright", legend = leg, bty = "n")
  
  
  ##Calculate and plot  residual
  reg.res <- resid(reg)
  
  plot(newDF$ozGPP, reg.res, ylab = "Residuals",
       xlab = x.lab, col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline(0,0, col="red")
  
  #########################     LE          ############################
  
  x.lab <- expression("Obs. LE (kg " * m^-2 * d^-1 * ")")
  y.lab <- expression("Sim. LE (kg " * m^-2 * d^-1 * ")")
  
  plot(newDF$ozTrans, newDF$gdayTrans, xlab = x.lab,
       ylab = y.lab, xlim = c(le.f, le.c),
       ylim = c(le.f, le.c), col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline( a= 0, b = 1, col="red")
  
  reg <- lm(newDF$gdayTrans~newDF$ozTrans)
  adj.r.sq <- summary(reg)$adj.r.squared
  intercept <- round(coef(reg)["(Intercept)"],2)
  sl <- round(coef(reg)["newDF$ozTrans"],2)
  rmsev <- rmse(newDF$ozTrans, newDF$gdayTrans)
  
  
  leg <- vector("expression",4)
  leg[1] = substitute(expression(italic(SL) == a), 
                      list(a = format(sl, dig = 2)))[2]
  leg[2] = substitute(expression(italic(I/C) == a), 
                      list(a = format(intercept, dig = 2)))[2]
  leg[3] = substitute(expression(italic("adj. R")^2 == a),
                      list(a = format(adj.r.sq, dig = 3)))[2]
  leg[4] = substitute(expression(italic(MRSE) == a),
                      list(a = format(rmsev, dig = 3)))[2]
  
  abline(reg, lwd = 2, lty = 2)
  legend("topleft", c("1:1 line", "fit line"),
         col=c("red","black"), lwd = c(2,2), lty = c(1, 2))
  legend("bottomright", legend = leg, bty = "n")
  
  
  ##Calculate and plot  residual
  reg.res <- resid(reg)
  
  plot(newDF$ozTrans, reg.res, ylab = "Residuals",
       xlab = x.lab, col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline(0,0, col="red")
  
  #########################     NEP          ############################
  
  x.lab <- expression("Obs. NEP (g " * m^-2 * d^-1 * ")")
  y.lab <- expression("Sim. NEP (g " * m^-2 * d^-1 * ")")
  
  plot(newDF$ozNEP, newDF$gdayNEP, xlab = x.lab,
       ylab = y.lab, xlim = c(nep.f, nep.c),
       ylim = c(nep.f, nep.c), col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline( a= 0, b = 1, col="red")
  
  reg <- lm(newDF$gdayNEP~newDF$ozNEP)
  adj.r.sq <- summary(reg)$adj.r.squared
  intercept <- round(coef(reg)["(Intercept)"],2)
  sl <- round(coef(reg)["newDF$ozTrans"],2)
  rmsev <- rmse(newDF$ozNEP, newDF$gdayNEP)
  
  
  leg <- vector("expression",4)
  leg[1] = substitute(expression(italic(SL) == a), 
                      list(a = format(sl, dig = 2)))[2]
  leg[2] = substitute(expression(italic(I/C) == a), 
                      list(a = format(intercept, dig = 2)))[2]
  leg[3] = substitute(expression(italic("adj. R")^2 == a),
                      list(a = format(adj.r.sq, dig = 3)))[2]
  leg[4] = substitute(expression(italic(MRSE) == a),
                      list(a = format(rmsev, dig = 3)))[2]
  
  abline(reg, lwd = 2, lty = 2)
  legend("topleft", c("1:1 line", "fit line"),
         col=c("red","black"), lwd = c(2,2), lty = c(1, 2))
  legend("bottomright", legend = leg, bty = "n")
  
  
  ##Calculate and plot  residual
  reg.res <- resid(reg)
  
  plot(newDF$ozNEP, reg.res, ylab = "Residuals",
       xlab = x.lab, col = adjustcolor("grey", alpha = 0.1), pch = 16)
  abline(0,0, col="red")
  
  
  dev.off()
  
  
}

##############################################################################
##Processed MODIS LAI data and saved into respective folders

##############################################################################
##LAI comparison between MODIS LAI and GDAY LAI_projected

corDF <- read.table("~/Documents/PostDoc/OzFlux/Site_coordinates.csv",sep=",",
                    header=T)

mod.path <- "~/Documents/PostDoc/GDAY/MODIS_LAI/extracted"

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  ##read gday simulated outputs
  gdayDF <- read.table(paste(getwd(), "/outputs/", sitename, "_simulation.csv", sep=""),
                       sep=",",skip=4)
  
  colnames(gdayDF) <- c("Year","DOY","Total_swc","NEP","GPP","NPP","C_exudation","C_VOC_Flux",
                        "R_eco","R_a","R_leaf_maint","R_wood_maint","R_fineroot_maint","R_growth",
                        "R_h","R_soil","ET","Transpiration","Soil_evap","Canopy_evap","Runoff",
                        "Drainage","Latent","Sensible","C_leaf","C_wood","C_coarse_root","C_fine_root",
                        "C_storage_TNC","C_fine_litter_total","C_fine_litter_above","C_fine_litter_below",
                        "C_course_litter","C_soil","C_leaf_growth","C_wood_growth","C_coarse_root_growth",
                        "C_fine_root_growth","C_reprod_growth","C_leaf_litterfall","C_coarse_root_litter",
                        "C_Fine_root_litter","C_wood_litter","LAI_projected","Leaf_C_over_area","N_Conc_leaf",
                        "N_leaf","N_wood","N_coarse_root","N_fine_root","N_storage","N_litter_above","N_litter_below",
                        "N_dead_wood","N_soil_total","N_mineral","N_organic","N_fix","N_leaf_litter","N_wood_litter",
                        "N_coarse_root_litter","N_fine_root_litter","N_biomass_uptake","N_gross_mineralization",
                        "N_net_mineralization","V_Volatilization","N_leaching","N_leaf_growth","N_wood_growth",
                        "N_corase_root_growth","N_fine_root_growth","Aborbed_PAR","Ave_daytime_canopy_conductance",
                        "Ave_daytime_aerodynamic_conductance","Ave_daytime_leaf_boundary_conductance","Soil_moist_stress",
                        "Foliage_retranslocation","Wood_retranslocation","Coarse_root_retranslocation",
                        "Fine_root_retranslocation","C_flux_from_litter_slow_to_active_soil_pool",
                        "C_flux_from_litter_active_to_slow_pool","C_flux_from_active_slow_to_passive_pool",
                        "C_flux_from_active_to_slow_pool","C_flux_from_active_to_passive_pool",
                        "C_flux_from_slow_to_active_pool","C_flux_from_slow_to_passitve_pool",
                        "C_flux_from_passive_to_active_pool","C_active_SOM_pool","C_slow_SOM_pool",
                        "C_passive_SOM_pool","CO2_efflux_from_surf_structural_litter",
                        "CO2_efflux_from_soil_structural_litter","CO2_efflux_from_surf_metabolic_litter",
                        "CO2_efflux_from_soil_metabolic_litter","CO2_efflux_from_fast_SOM",
                        "CO2_efflux_from_slow_SOM","CO2_efflux_from_passive_SOM","Temp_scalar_on_C_efflux_from_SOM",
                        "Root_exudation_C","Root_exudation_N","CO2_released_from_exudation","Total_C_flux_from_active",
                        "Residence_time_slow_pool","REXC_C_use_efficiency","Total_C_slow","Total_N_slow",
                        "Total_C_active","Total_N_active")
  
  
  gdayDF$Date <- strptime(paste(gdayDF$Year, gdayDF$DOY), format = "%Y %j")
  gdayDF$Date <- as.Date(gdayDF$Date)
  
  modDF <- read.table(paste(mod.path, "/", sitename, "_LAI.csv", sep=""),
                      header=T,sep=",")
  
  modDF$newdate <- as.Date("1800-01-01") + modDF$Date
  modDF$newlai <- modDF$LAI*2
  lai_mv <- rollmean(modDF$newlai, 10, fill = list(NA, NULL,NA))
  modDF$lai_mv <- lai_mv
  
  y.max <- round(max(modDF$newlai, modDF$LAI, gdayDF$LAI_projected, na.rm=T),0)
  y.min <- round(min(modDF$newlai, modDF$LAI, gdayDF$LAI_projected, na.rm=T),0)
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_LAI_comparison.pdf",sep=""))
  
  par(mar = c(5.1,5.1,4.1,4.1))
  
  plot(gdayDF$Date, gdayDF$LAI_projected, col = "green", 
       ylim = c(y.min, y.max), xlab = "Date", ylab = "LAI",
       lwd = 4, type = "l")
  
  points(modDF$newdate, modDF$newlai, col = "grey")
  lines(modDF$newdate, modDF$lai_mv, lwd = 4, col="black")
  
  points(modDF$newdate, modDF$LAI, col = "red")
  
  legend("topright", c("MODIS single", "MODIS double","Gday"), 
         col = c("red","black", "green"), pch = c(16,16,16))
  
  dev.off()
  
}  

##############################################################################
##Isolate impact-based drought and investigate model vs. observational difference

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  myDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_result.csv",sep=""),
                   sep=",",header=T)

  ##read gday simulated outputs
  gdayDF <- read.table(paste(getwd(), "/outputs/", sitename, "_simulation.csv", sep=""),
                       sep=",",skip=4)
  
  colnames(gdayDF) <- c("Year","DOY","Total_swc","NEP","GPP","NPP","C_exudation","C_VOC_Flux",
                        "R_eco","R_a","R_leaf_maint","R_wood_maint","R_fineroot_maint","R_growth",
                        "R_h","R_soil","ET","Transpiration","Soil_evap","Canopy_evap","Runoff",
                        "Drainage","Latent","Sensible","C_leaf","C_wood","C_coarse_root","C_fine_root",
                        "C_storage_TNC","C_fine_litter_total","C_fine_litter_above","C_fine_litter_below",
                        "C_coarse_litter","C_soil","C_leaf_growth","C_wood_growth","C_coarse_root_growth",
                        "C_fine_root_growth","C_reprod_growth","C_leaf_litterfall","C_coarse_root_litter",
                        "C_fine_root_litter","C_wood_litter","LAI_projected","Leaf_C_over_area","N_Conc_leaf",
                        "N_leaf","N_wood","N_coarse_root","N_fine_root","N_storage","N_litter_above","N_litter_below",
                        "N_dead_wood","N_soil_total","N_mineral","N_organic","N_fix","N_leaf_litter","N_wood_litter",
                        "N_coarse_root_litter","N_fine_root_litter","N_biomass_uptake","N_gross_mineralization",
                        "N_net_mineralization","V_Volatilization","N_leaching","N_leaf_growth","N_wood_growth",
                        "N_corase_root_growth","N_fine_root_growth","Aborbed_PAR","Ave_daytime_canopy_conductance",
                        "Ave_daytime_aerodynamic_conductance","Ave_daytime_leaf_boundary_conductance","Soil_moist_stress",
                        "Foliage_retranslocation","Wood_retranslocation","Coarse_root_retranslocation",
                        "Fine_root_retranslocation","C_flux_from_litter_slow_to_active_soil_pool",
                        "C_flux_from_litter_active_to_slow_pool","C_flux_from_active_slow_to_passive_pool",
                        "C_flux_from_active_to_slow_pool","C_flux_from_active_to_passive_pool",
                        "C_flux_from_slow_to_active_pool","C_flux_from_slow_to_passitve_pool",
                        "C_flux_from_passive_to_active_pool","C_active_SOM_pool","C_slow_SOM_pool",
                        "C_passive_SOM_pool","CO2_efflux_from_surf_structural_litter",
                        "CO2_efflux_from_soil_structural_litter","CO2_efflux_from_surf_metabolic_litter",
                        "CO2_efflux_from_soil_metabolic_litter","CO2_efflux_from_fast_SOM",
                        "CO2_efflux_from_slow_SOM","CO2_efflux_from_passive_SOM","Temp_scalar_on_C_efflux_from_SOM",
                        "Root_exudation_C","Root_exudation_N","CO2_released_from_exudation","Total_C_flux_from_active",
                        "Residence_time_slow_pool","REXC_C_use_efficiency","Total_C_slow","Total_N_slow",
                        "Total_C_active","Total_N_active")
  
  gdayDF$date <- as.Date(strptime(paste(gdayDF$Year, gdayDF$DOY), format = "%Y %j"))
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_gday_output_summary.pdf",sep=""))
  
  #########################     Page1          ############################
  
  
  par(mfrow=c(2,2),
      mar=c(5.1,5.1,4.1,4.1))
  
  ## Total swc vs. time
  
  plot(Total_swc~date, data = gdayDF, xlab = "Date",
       ylab = "total soil water content (mm)")
  
  
  #########################     GPP          ############################
  
  v.low <- round(min(myDF$ozGPP, myDF$gdayGPP),0)
  v.high <- round(max(myDF$ozGPP, myDF$gdayGPP),0)
  
  x.lab <- expression("Rainfall (mm " * d^-1 * ")")
  y.lab <- expression("GPP (g " * m^-2 * d^-1 * ")")
  
  plot(ozGPP~Rain, data = myDF, xlab = x.lab, ylab = y.lab,
       ylim = c(v.low, v.high), pch = 16)
  points(gdayGPP~Rain, data = myDF, col = "green",
         pch = 16)

  legend("topright", c("OzFlux","Gday"), 
         col = c("black", "green"), pch = c(16,16))
  
  #########################     Trans          ############################
  
  v.low <- round(min(myDF$ozTrans, myDF$gdayTrans),0)
  v.high <- round(max(myDF$ozTrans, myDF$gdayTrans),0)
  
  x.lab <- expression("Rainfall (mm " * d^-1 * ")")
  y.lab <- expression("Transpiration (kg " * m^-2 * d^-1 * ")")
  
  plot(ozTrans~Rain, data = myDF, xlab = x.lab, ylab = y.lab,
       ylim = c(v.low, v.high), pch = 16)
  points(gdayTrans~Rain, data = myDF, col = "green",
         pch = 16)
  
  legend("topright", c("OzFlux","Gday"), 
         col = c("black", "green"), pch = c(16,16))
  
  #########################     NEP          ############################
  
  v.low <- round(min(myDF$ozNEP, myDF$gdayNEP),0)
  v.high <- round(max(myDF$ozNEP, myDF$gdayNEP),0)
  
  x.lab <- expression("Rainfall (mm " * d^-1 * ")")
  y.lab <- expression("NEP (g " * m^-2 * d^-1 * ")")
  
  plot(ozNEP~Rain, data = myDF, xlab = x.lab, ylab = y.lab,
       ylim = c(v.low, v.high), pch = 16)
  points(gdayNEP~Rain, data = myDF, col = "green",
         pch = 16)
  
  legend("topright", c("OzFlux","Gday"), 
         col = c("black", "green"), pch = c(16,16))
  
  dev.off()
  
  
  
  
  
  
  
  
  
  
  plot(ozGPP~as.Date(myDF$Date), data = myDF)
  short.date <- strftime(myDF$Date, "%Y/%m")
  gpp.monthly <- aggregate(myDF$ozGPP ~ short.date, FUN = sum)
  colnames(gpp.monthly) <- c("date","ozGPP")
  gppDF <- ts(gpp.monthly$ozGPP, frequency = 12, start = min(myDF$Year))
  
  plot(as.xts(gppDF), major.format = "%Y/%m", xlab = "Date",
       ylab = y.lab)
  
}
