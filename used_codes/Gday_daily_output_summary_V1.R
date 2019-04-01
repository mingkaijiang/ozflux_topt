##############################################################################
##Process raw Gday results and compute figures
##Include ozflux meteorological data
##
##Scripts
##Last modified: July-15-2016
##############################################################################
require(ncdf)
require(quantreg)
require(zoo)
require(gam)
require(Metrics)
require(mgcv)
require(lme4)
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
  
  ##Read Ozflux meteorological data 
  tmet <- open.ncdf(paste0("~/Documents/PostDoc/OzFlux/",sitename,"OzFlux2.0_met.nc"))
  
  t <- get.var.ncdf(tmet, "time")
  tunits <- att.get.ncdf(tmet, "time","units")
  nt <- dim(t)
  sec.var <- as.numeric(t[2]-t[1])
  
  tair <- get.var.ncdf(tmet,"Tair") - 273.15
  ifelse(sec.var == 1800, 
         rain <- get.var.ncdf(tmet,"Rainf")*1800,
         rain <- get.var.ncdf(tmet,"Rainf")*3600)
  
  SW <- get.var.ncdf(tmet,varid="SWdown")
  qair <- get.var.ncdf(tmet, varid="Qair")
  wind <- get.var.ncdf(tmet, varid="Wind")
  psurf <- get.var.ncdf(tmet, varid="PSurf")
  LW <- get.var.ncdf(tmet, varid="LWdown")
  co2 <- get.var.ncdf(tmet, varid="CO2air")
  
  
  metDF <- data.frame(tair, rain, SW, qair, wind, psurf, LW, co2)
  
  close.ncdf(tmet)
  
  t.start <- substr(tunits$value,15,33)
  
  ifelse(sec.var == 1800, t.end <- nt/48, t.end <- nt/24)

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
  
  metDF <- cbind(t_series,metDF)
  
  Sum1 <- aggregate(metDF['SW'],
                    list(day = cut(as.POSIXct(metDF$t_series),"day")),
                    sum)
  
  Sum2 <- aggregate(metDF['LW'],
                    list(day = cut(as.POSIXct(metDF$t_series),"day")),
                    sum)
  
  Sum3 <- aggregate(metDF['rain'],
                    list(day = cut(as.POSIXct(metDF$t_series),"day")),
                    sum)
  
  mean1 <- aggregate(metDF['tair'],
                     list(day = cut(as.POSIXct(metDF$t_series),"day")),
                     mean)
  
  mean2 <- aggregate(metDF['wind'],
                     list(day = cut(as.POSIXct(metDF$t_series),"day")),
                     mean)
  
  mean3 <- aggregate(metDF['psurf'],
                     list(day = cut(as.POSIXct(metDF$t_series),"day")),
                     mean)
  
  mean4 <- aggregate(metDF['qair'],
                     list(day = cut(as.POSIXct(metDF$t_series),"day")),
                     mean)
  
  min1 <- aggregate(metDF['tair'],
                    list(day = cut(as.POSIXct(metDF$t_series),"day")),
                    min)
  
  max1 <- aggregate(metDF['tair'],
                    list(day = cut(as.POSIXct(metDF$t_series),"day")),
                    max)
  
  metDF_new <- cbind(Sum1, Sum2$LW, mean1$tair, min1$tair, max1$tair, Sum3$rain, 
                     mean2$wind, mean3$psurf, mean4$qair)
  
  colnames(metDF_new) <- c("Date","SW","LW","Tair_mean","Tair_min","Tair_max","Rain",
                          "wind","psurf","qair")
  
  metDF_new$Date <- as.Date(metDF_new$Date)
  
  ifelse(G == 1,
         metDF <- metDF_new,
         metDF <- metDF_new[-nrow(metDF_new),])
  
  ##Ozflux meteorological DF ready to use
  
  ##############################################################################
  ##Read in ozflux data data, and gdayDF daily simulation results

  ozDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_result.csv",sep=""),
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
                        "N_coarse_root_growth","N_fine_root_growth","Aborbed_PAR","Ave_daytime_canopy_conductance",
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
  
  ##############################################################################
  ##Start plotting
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_gday_output_summary.pdf",sep=""))
  
  par(mar=c(5.1,5.1,4.1,5.1),
      oma = c(0,0,0,2))
  
  ## Total swc ~ time, rain ~ time, same figure
  
  y.lab1 <- expression("Total soil water content ( mm " * d^-1 * ")")
  y.lab2 <- expression("Rain ( mm " * d^-1 * ")")
  
  plot(Total_swc~date, data = gdayDF, xlab = "Date",
       ylab = y.lab1, col = "blue", ylim = c(0, 300),
       type = "l", lwd = 2)
  
  par(new = T)
  
  barplot(metDF$Rain, xlab = NA, ylab = NA,
          yaxt = "n", xaxt = "n")
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T, line = -2)
  
  legend("topright", c("Total soil water content", "Rain"),
         col = c("blue", "black"), lty= c(1,1), lwd = 2)
  
  ## GPP, NPP and NEP ~ Total_swc
  x.lab <- y.lab1
  y.lab <- expression("C flux (g " * m^-2 * d^-1 * ")")
  y.min <- as.numeric(round(min(gdayDF$GPP, gdayDF$NEP, gdayDF$NPP),0))
  y.max <- as.numeric(round(max(gdayDF$GPP, gdayDF$NEP, gdayDF$NPP),0))
  
  plot(GPP~Total_swc, data = gdayDF, xlab = x.lab,
       ylab = y.lab, col = "darkgreen", pch = 16,
       ylim = c(y.min, y.max))
  points(NEP~Total_swc, data = gdayDF, col = "brown",
         pch = 16)
  points(NPP~Total_swc, data = gdayDF, col = "green",
         pch = 16)
  
  #mod <- nls(GPP~Total_swc, data = gdayDF)
  #newx <- gdayDF$Total_swc
  #preds <- predict(mod, interval = "confidence")
  #polygon(c(rev(newx), newx), c(rev(preds[,3]), preds[,2]),
  #        col = "grey80", border = NA)
  #abline(mod)
  #lines(newx, preds[,3], lty = "dashed", col = "red")
  #lines(newx, preds[,2], lty = "dashed", col = "red")
  
  legend("topleft", c("GPP", "NPP", "NEP"),
         col = c("darkgreen", "green", "brown"), pch = 16)
  
  
  ## ET, Trans ~ Total_swc
  x.lab <- expression("Total soil water content ( mm " * d^-1 * ")")
  y.lab <- expression(H[2] * "O flux (kg " * m^-2 * d^-1 * ")")
  
  plot(ET~Total_swc, data = gdayDF, xlab = x.lab,
       ylab = y.lab, col = "darkblue", pch = 16)
  points(Transpiration~Total_swc, data = gdayDF, col = "lightblue",
         pch = 16)
  
  legend("topleft", c("ET", "Transpiration"),
         col = c("darkblue", "lightblue"), pch = 16)
  
  ## GPP, NPP and NEP ~ time
  y.lab <- expression("C flux (g " * m^-2 * d^-1 * ")")
  
  y.min <- as.numeric(round(min(gdayDF$GPP, gdayDF$NEP, gdayDF$NPP,
                                gdayDF$R_eco, gdayDF$R_a, gdayDF$R_h),0))
  y.max <- as.numeric(round(max(gdayDF$GPP, gdayDF$NEP, gdayDF$NPP,
                                gdayDF$R_eco, gdayDF$R_a, gdayDF$R_h),0))
  
  plot(GPP~date, data = gdayDF, xlab = "Date",
       ylab = y.lab, col = "darkgreen", pch = 16,
       ylim = c(y.min, y.max))
  points(NEP~date, data = gdayDF, col = "brown",
         pch = 16)
  points(NPP~date, data = gdayDF, col = "green",
         pch = 16)
  
  legend("topleft", c("GPP", "NPP", "NEP"),
         col = c("darkgreen", "green", "brown"), pch = 16)
  
  points(R_eco~date, data = gdayDF, xlab = "Date",
       ylab = y.lab, col = "blue", pch = 16,
       ylim = c(y.min, y.max))
  points(R_h~date, data = gdayDF, col = "grey",
         pch = 16)
  points(R_a~date, data = gdayDF, col = "black",
         pch = 16, cex = 0.5)
  
  legend("topright", c("R_eco", "R_a", "R_h"),
         col = c("blue", "black", "grey"), pch = 16)

  ## Canopy evap, soil evap ~ time
  
  ## Canopy evap, soil evap ~ prec
  tmp <- as.data.frame(cbind(gdayDF$Canopy_evap, gdayDF$Soil_evap, metDF$Rain,
               metDF$Tair_mean))
  colnames(tmp) <- c("Canopy_evap", "Soil_evap", "Rain", "Tair_mean")
  
  x.lab <- expression("Tair (" * C * degree * ")")
  y.lab <- expression("Evaporation (kg " * m^-2 * d^-1 * ")")
  
  plot(Canopy_evap~Rain, data=tmp,
       xlab = x.lab, ylab = y.lab, col = "green", pch = 16)
  points(Soil_evap~Rain, data=tmp, col = "brown", pch = 16)
  
  legend("topright", c("Canopy", "Soil"),
         col = c("green", "brown"), pch = 16)
  
  ## C_leaf, wood, coarse_root, fine_root ~ date
  y.lab1 <- expression("C leaf/root stock (g " * m^-2 * d^-1 * ")")
  y.lab2 <- expression("C wood stock (g " * m^-2 * d^-1 * ")")
  y.min <- as.numeric(round(min(gdayDF$C_leaf, 
                                gdayDF$C_coarse_root, gdayDF$C_fine_root),0))
  y.max <- as.numeric(round(max(gdayDF$C_leaf, 
                                gdayDF$C_coarse_root, gdayDF$C_fine_root),0))
  
  plot(C_leaf~date, data=gdayDF,
       xlab = "Date", ylab = y.lab1, col = "green", pch = 16,
       ylim = c(y.min, y.max))
  points(C_coarse_root~date, data=gdayDF, col = "orange", pch = 16)
  points(C_fine_root~date, data=gdayDF, col = "pink", pch = 16)
  
  par(new = T)
  plot(C_wood~date, data=gdayDF, col = "brown", pch = 16,
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T, line = -2)
  
  legend("topleft", c("Leaf", "Wood","Coarse root", "Fine root"),
         col = c("green","brown","orange","pink"), pch = 16)
  
  ##CUE = 1 - (Ra/GPP)
  gdayDF$CUE <- round(1-(gdayDF$R_a / gdayDF$GPP),1)
  
  plot(CUE~date, data = gdayDF, xlab = "Date",
       ylab = "CUE")
  
  ##WUE = GPP/Transpiration
  y.lab <- expression("WUE (g of C/kg of " * H[2] * "O)")
  gdayDF$WUE <- round(gdayDF$GPP/gdayDF$Transpiration, 2)
  plot(WUE~date, data = gdayDF, xlab = "Date",
       ylab = y.lab, col = "orange", pch = 16)
  
  ##C_fine_litter_total, %above/total ~ time
  y.lab1 <- expression("C fine litter stock (g " * m^-2 * ")")
  
  plot(C_fine_litter_total~date, data=gdayDF,
       xlab = "Date", ylab = y.lab1, col = "green", pch = 16)
  
  par(new = T)
  plot((gdayDF$C_fine_litter_above/gdayDF$C_fine_litter_total * 100)~date, 
       data=gdayDF, col = "brown", type="l",lwd = 2,
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, "Aboveground contribution (%)", outer = T, line = -2)
  legend("topleft", c("C fine litter stock", "Aboveground contribution"),
         col = c("green","brown"), lwd = 2)
  
  ##C_soil, active, slow, passive pools ~ time
  tmpDF <- as.data.frame(cbind(gdayDF$date, gdayDF$C_soil, gdayDF$C_active_SOM_pool,
               gdayDF$C_slow_SOM_pool, gdayDF$C_passive_SOM_pool),
               stringsAsFactors=F)
  colnames(tmpDF) <- c("date","C_soil","C_active","C_slow","C_passive")
  tmpDF$C_activeP <- round(tmpDF$C_active/tmpDF$C_soil,2) * 100
  tmpDF$C_slowP <- round(tmpDF$C_slow/tmpDF$C_soil,2) * 100
  tmpDF$C_passiveP <- round(tmpDF$C_passive/tmpDF$C_soil,2) * 100
  
  y.min <- as.numeric(round(min(tmpDF$C_activeP, 
                                tmpDF$C_slowP, tmpDF$C_passiveP),0))
  y.max <- as.numeric(round(max(tmpDF$C_activeP, 
                                tmpDF$C_slowP, tmpDF$C_passiveP),0))
  
  y.lab1 <- expression("Proportion contribution (%)")
  y.lab2 <- expression("C soil stock (g " * m^-2 * ")")
  
  plot(tmpDF$C_activeP~gdayDF$date, ylim = c(y.min, y.max),
       xlab = "Date", ylab = y.lab1, type = "l", lwd = 2,
       col = "lightgreen")
  lines(tmpDF$C_slowP~gdayDF$date, lwd = 2, col = "green")
  lines(tmpDF$C_passiveP~gdayDF$date, lwd = 2, col = "darkgreen")
  
  par(new = T)
  plot(C_soil~date, 
       data=gdayDF, col = "brown", type="l",lwd = 2,
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T, line = -2)
  
  legend("left", c("Active %","Slow %","Passive %","Soil C"), 
         col = c("lightgreen","green","darkgreen", "brown"),
         lwd = 2)
  
  ##LAI, Leaf_C/LAI ~ time
  gdayDF$Leaf_C_over_area <- round(gdayDF$Leaf_C_over_area, 2)
  y.lab <- expression("Leaf C/LAI (g C " * m^-2 * ")")
  
  plot(Leaf_C_over_area~date, data = gdayDF, xlab = "Date",
       ylab = y.lab, type = "l", lwd = 2,
       col = "orange")
  
  par(new = T)
  plot(LAI_projected~date, 
       data=gdayDF, col = "green", type="l",lwd = 2,
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, "LAI", outer = T, line = -2)
  
  legend("topright", c("LAI","Leaf C/LAI"), 
         col = c("green","orange"),
         lwd = 2)
  
  ##LAI ~ GPP
  x.lab <- expression("GPP (g C " * m^-2 * d^-1 * ")")
  
  plot(LAI_projected~GPP, data = gdayDF, pch = 16,
       col = "orange", xlab = x.lab, ylab = "LAI")
  
  ##C_leaf_growth, C_wood_growth, C_fine_root_growth ~ GPP
  x.lab <- expression("GPP (g C " * m^-2 * d^-1 * ")")
  y.lab <- expression("C growth (g C " * m^-2 * d^-1 * ")")
  
  y.min <- as.numeric(round(min(gdayDF$C_leaf_growth, 
                                gdayDF$C_wood_growth, gdayDF$C_coarse_root_growth,
                                gdayDF$C_fine_root_growth),2))
  y.max <- as.numeric(round(max(gdayDF$C_leaf_growth, 
                                gdayDF$C_wood_growth, gdayDF$C_coarse_root_growth,
                                gdayDF$C_fine_root_growth),2))
  
  plot(C_leaf_growth~GPP, data = gdayDF, pch = 16, 
       col = "green", xlab = x.lab, ylab = y.lab,
       ylim = c(y.min, y.max))
  points(C_wood_growth~GPP, data = gdayDF, pch = 16, col = "darkgreen")
  points(C_coarse_root_growth~GPP, data = gdayDF, pch = 16, col = "brown")
  points(C_fine_root_growth~GPP, data = gdayDF, pch = 16, col = "orange")
  
  legend("topleft", c("Leaf","Wood","Coarse root",
                      "Fine root"), 
         col = c("green","darkgreen","brown","orange"),
         pch = 16)
  
  ##C_soil & total_swc ~ soil_moist_stress
  y.lab1 <- expression("C soil stock (g " * m^-2 * ")")
  y.lab2 <- expression("Total soil water content ( mm " * d^-1 * ")")
  plot(C_soil ~ Soil_moist_stress, data = gdayDF, type = "b",
       pch = 16, col = "green", ylab = y.lab1,
       xlab = "Soil moisture stress (%)")
  
  par(new=T)

  plot(Total_swc~Soil_moist_stress, data = gdayDF,
       col = "blue",
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T, line = -2)
  
  legend("top", c("C soil stock", "Total soil water content"),
         col = c("green", "blue"), pch = 16)
  
  ##GPP / Ave_daytime_canopy_conductance ~ Aborbed PAR
  y.lab1 <- expression("GPP (g C " * m^-2 * d^-1 * ")")
  y.lab2 <- expression("Average daytime canopy conductance (mol " * H[2] * O * m^-2 *
                         s^-1 * ")")
  x.lab <- expression("Absorbed PAR ( MJ " * m^-2 * d^-1 * ")")
  
  plot(GPP~Aborbed_PAR, data = gdayDF, pch = 16,
       col = "green", xlab = x.lab, ylab = y.lab1)
  
  par(new=T)
  plot(Ave_daytime_canopy_conductance~Aborbed_PAR, data = gdayDF,
       col = "blue",
       xaxt = "n", yaxt = "n", xlab = NA, ylab = NA)
  axis(side = 4)
  mtext(side = 4, y.lab2, outer = T, line = -2)

  legend("topleft", c("GPP", "Ave_daytime_canopy_conductance"),
         col = c("green", "blue"), pch = 16)
  
  ##GPP ~ daytime canopy conductance
  y.lab <- expression("GPP (g C " * m^-2 * d^-1 * ")")
  x.lab <- expression("Average daytime canopy conductance (mol " * H[2] * O * m^-2 *
                         s^-1 * ")")
  plot(GPP~Ave_daytime_canopy_conductance, data = gdayDF,
       xlab = x.lab, ylab = y.lab, col = "orange")
  
  ##from litter to active and slow pools ~ date
  litter_to_active <- round(gdayDF$C_flux_from_litter_slow_to_active_soil_pool - 
                              gdayDF$C_flux_from_slow_to_active_pool, 2)
  litter_to_slow <- round(gdayDF$C_flux_from_litter_active_to_slow_pool -
                            gdayDF$C_flux_from_active_to_slow_pool, 2)
  
  y.min <- as.numeric(round(min(litter_to_active, litter_to_slow),2))
  y.max <- as.numeric(round(max(litter_to_active, litter_to_slow),2))
  
  y.lab <- expression("C fluxes (g C " * m^-2 * d^-1 * ")")
  
  plot(litter_to_active ~ gdayDF$date,
       type = "l", lwd = 2, col = "orange", ylim = c(y.min, y.max),
       xlab = "Date", ylab = y.lab)
  points(litter_to_slow ~ gdayDF$date,
       type = "l", lwd = 2, col = "brown", ylim = c(y.min, y.max))
  
  legend("topright", c("litter to active", "litter to slow"),
         col = c("orange", "brown"), lwd = 2)
  
  
  ##from active to slow, passive pools ~ date
  y.min <- as.numeric(round(min(gdayDF$C_flux_from_active_to_slow_pool,
                                gdayDF$C_flux_from_active_to_passive_pool),2))
  y.max <- as.numeric(round(max(gdayDF$C_flux_from_active_to_slow_pool,
                                gdayDF$C_flux_from_active_to_passive_pool),2))
  
  y.lab <- expression("C fluxes (g C " * m^-2 * d^-1 * ")")
  
  plot(C_flux_from_active_to_slow_pool~date, data = gdayDF,
       type = "l", lwd = 2, col = "orange",
       xlab = "Date", ylab = y.lab, ylim = c(y.min, y.max))
  points(C_flux_from_active_to_passive_pool~date, data = gdayDF,
         type = "l", lwd = 2, col = "brown",
         ylim = c(y.min, y.max))
  
  legend("topright", c("active to slow", "active to passive"),
         col = c("orange", "brown"), lwd = 2)
  
  ##from slow to active, passive ~ date
  y.min <- as.numeric(round(min(gdayDF$C_flux_from_slow_to_active_pool,
                                gdayDF$C_flux_from_slow_to_passive_pool),0))
  y.max <- as.numeric(round(max(gdayDF$C_flux_from_slow_to_active_pool,
                                gdayDF$C_flux_from_slow_to_passive_pool),2))
  
  y.lab <- expression("C fluxes (g C " * m^-2 * d^-1 * ")")
  
  plot(C_flux_from_slow_to_active_pool~date, data = gdayDF,
       type = "l", lwd = 2, col = "orange", 
       xlab = "Date", ylab = y.lab, ylim = c(y.min, y.max))
  lines(C_flux_from_slow_to_passitve_pool~date, data = gdayDF,
        lwd = 2, col = "brown", ylim = c(y.min, y.max))
  
  legend("topright", c("slow to active", "slow to passive"),
         col = c("orange", "brown"), lwd = 2)
  
  ##from passive to active ~ date
  y.lab <- expression("C fluxes (g C " * m^-2 * d^-1 * ")")
  
  plot(C_flux_from_passive_to_active_pool~date, data = gdayDF,
       type = "l", lwd = 2, col = "orange", 
       xlab = "Date", ylab = y.lab)
  
  legend("topright", "passive to active",
         col = c("orange"), lwd = 2)
  
  
  dev.off()
  
}


