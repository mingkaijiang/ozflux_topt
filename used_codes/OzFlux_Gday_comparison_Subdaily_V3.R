##############################################################################
##Process raw OzFlux data and GDY output (subdaily)
##
##Scripts
##Version 3
##Difference to V2: use neural network regression rather than vector support machines to find best fit
##
##Last modified: July-14-2016
##############################################################################
##Known problem/check back issues:
##1. Time consuming to run svm function  >> changed to lsqnonlin function
##2. Write functions to make it easier to interpret and more structural
##3. Topt problematic at both ends, need a better way of finding Topt
##4. For SW > 1000 in dry site, gday simulation has two possible GPP vs. temp relationships, 
##   need to investigate further, e.g. Calperum
##5. SW and prec data only plotted for Calperum, not for other sites
##6. sites plotted only included forested sites

##############################################################################

##############################################################################
##All OzFlux sites that I have data of
sitename = "AdelaideRiver"                ##Savanna
sitename = "Calperum"                     ##Mallee
sitename = "CapeTribulation"              ##Rainforest
sitename = "CowBay"                       ##Tropical rainforest
sitename = "CumberlandPlains"             ##Temperate dry sclerophyll
sitename = "DalyPasture"                  ##Tropical pasture
sitename = "DalyUncleared"                ##Woodland savanna
sitename = "DryRiver"                     ##Open forest savanna
sitename = "Emerald"                      ##Forested tussock grasslands and croplands
sitename = "Gingin"                       ##Banksia woodland
sitename = "GreatWesternWoodlands"        ##Woodlands
sitename = "HowardSprings"                ##Open woodland savanna
sitename = "Otway"                        ##Dairy pasture
sitename = "RedDirtMelonFarm"             ##Farmland
sitename = "RiggsCreek"                   ##Dryland agriculture
sitename = "Samford"                      ##Improved pasture
sitename = "SturtPlains"                  ##Open grasslands
sitename = "Tumbarumba"                   ##Cool temperate wet sclerophyll
sitename = "Whroo"                        ##Woodlands
sitename = "WombatStateForest"            ##Cool temperate dry sclerophyll
sitename = "Yanco"                        ##
##############################################################################
####To define which group of SiteName to use
####If G = 1, then run forested sites that started midnight exactly
####If G = 2, then run sites that started 30min/1h after midnight
G <- 1
####If G = 1, then run sites that started midnight exactly
####If G = 2, then run sites that started 30min/1h after midnight
#ifelse(G == 1,
#       SiteName <- c("Calperum","CapeTribulation","CowBay","CumberlandPlains",
#                     "DalyPasture","DalyUncleared","DryRiver","Emerald",
#                     "Gingin","Samford","SturtPlains","Whroo",
#                     "WombatStateForest","Yanco"),
#       SiteName <- c("GreatWesternWoodlands","HowardSprings",
#                     "RiggsCreek"))
##############################################################################

ifelse(G == 1,
       SiteName <- c("Calperum","CapeTribulation","CowBay","CumberlandPlains",
                     "DryRiver","Gingin","Whroo","WombatStateForest"),
       SiteName <- c("GreatWesternWoodlands","HowardSprings"))

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
  SW <- get.var.ncdf(tmet,varid="SWdown")

  t <- get.var.ncdf(tmet, "time")
  tunits <- att.get.ncdf(tmet, "time","units")
  nt <- dim(t)
  sec.var <- as.numeric(t[2]-t[1])
  
  tair <- get.var.ncdf(tmet,"Tair") - 273.15
  ifelse(sec.var == 1800, 
         rain <- get.var.ncdf(tmet,"Rainf")*1800,
         rain <- get.var.ncdf(tmet,"Rainf")*3600)
  
  close.ncdf(tmet)
  
  tflx <- open.ncdf(paste0("~/Documents/PostDoc/OzFlux/",sitename,"OzFlux2.0_flux.nc"))
  
  #print(tflx)
  
  qle <- get.var.ncdf(tflx, varid="Qle")
  tgpp <- get.var.ncdf(tflx,varid="GPP")
  tnee <- get.var.ncdf(tflx,varid="NEE")
  
  ozDF <- data.frame(lat,lon,tair,rain,SW, tgpp,tnee, qle)
  
  close.ncdf(tflx)
  
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
  
  
  ozDF <- cbind(t_series, ozDF)
  
  colnames(ozDF) <- c("Date","lat","lon","Tair","Rain","SW",
                      "GPP","NEE","LE")
  
  ozDF$GPP <- ozDF$GPP* 0.000001*12*sec.var    
  ozDF$NEE <- ozDF$NEE* 0.000001*12*sec.var    
  ozDF$Trans <- ozDF$LE* (1.0/(2.45*1E6))*sec.var     
  ozDF$NEP <- -ozDF$NEE
  
  ##Delete last row for sites that started 30min/1h after midnight
  ifelse(G == 1,
         ozDF <- ozDF,
         ozDF <- ozDF[-nrow(ozDF),])                                   

  ##Ozflux DF ready to use
  
  ##############################################################################
  
  ##read gday simulated outputs
  gdayDF <- read.table(paste(getwd(), "/outputs/", sitename, "_simulation_30min.csv", sep=""),
                       sep=",",header=T)
  
  ##an_canopy: canopy net photosynthesis (umol m-2 s-1)
  ##rd_canopy: canopy respiration in the light (unol m-2 s-1)
  ##gsc_canopy: canopy stomatal conductance to CO2 (umol m-2 s-1)
  ##apar_canopy: canopy abs. photosyn. active rad. (umol m-2 s-1)
  ##trans_canopy: canopy transpiration (mm 30 min-1)
  ##tleaf: leaf temperature (deg C)
  
  gdayDF$an_canopy <- gdayDF$an_canopy* 0.000001*12*sec.var  ## Convert from umol m-2 s-1 to g C m-2 subdaily unit-1    
  gdayDF$rd_canopy <- gdayDF$rd_canopy* 0.000001*12*sec.var  ## Convert from umol m-2 s-1 to g C m-2 subdaily unit-1
  gdayDF$gsc_canopy <- gdayDF$gsc_canopy* 0.000001*12*sec.var  ## Convert from umol m-2 s-1 to g C m-2 subdaily unit-1
  gdayDF$apar_canopy <- gdayDF$apar_canopy* 0.000001*12*sec.var  ## Convert from umol m-2 s-1 to umol m-2 subdaily unit-1
  
  newDF <- cbind(gdayDF, ozDF)
  
  newDF$GDAYtrans <- newDF$trans_canopy * sec.var * 0.001 * 18.02   ##Convert from mol/s to mm/30min

  ##Following lines check the Gday transpiration unit conversion with daily results
  #plot(newDF$Trans, newDF$GDAYtrans, xlab = "Ozflux", ylab = "GDAY")
  #abline(a=0, b=1, col="red")
  #plot(newDF$Date, newDF$Trans, col = "grey")
  #points(newDF$Date, newDF$GDAYtrans, col = "lightblue")
  
  newDF$trans_canopy <- NULL
  colnames(newDF) <- c("Year", "DOY", "HOD", "gdayGPP", "gdayRA", "gdaySC", 
                       "gdayPAR","gdayTleaf", "Date", "Lat","Lon", "Tair", "Rain",
                       "SW", "ozGPP", "ozNEE", "ozLE", "ozTrans", "ozNEP", "gdayTrans")
  
  newDF$gdayPAR_hlfhr <- newDF$gdayPAR * sec.var
  newDF$gdayNEP <- newDF$gdayGPP - newDF$gdayRA
  #summary(newDF$gdayPAR)
  
  newDF <- as.data.frame(newDF, stringsAsFactors=F)
  
  newDF$newozGPP <- newDF$ozGPP
  newDF$newozGPP <- ifelse(newDF$newozGPP < 0, 0, newDF$newozGPP)  
  
  newDF <- as.data.frame(newDF, stringsAsFactors=F)
  
  write.table(newDF, paste(getwd(), "/output_analyses/gday_ozflux_processed.csv",sep=""),
              row.names=F,col.names=T,sep=",")

}

##end process raw data loop
    
##############################################################################
for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  newDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_processed.csv",sep=""),
                      sep=",",header=T)
  
  ##Plot temp vs. gpp site-model comparison
  pdf(paste(getwd(), "/output_analyses/",sitename,"_topt_subdaily.pdf",sep=""))
  
  par(mfrow=c(3,2),
      mar = c(5.1,5.1,4.1,4.1))
  
  temp.DF <- cbind(newDF$newozGPP, newDF$gdayGPP)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),2)
  
  x.lab <- expression("Tair (" * degree * "C" * ")")
  ifelse(sec.var == 1800,
  y.lab <- expression("GPP (g " * m^-2 * "30" * min^-1 * ")"),
  y.lab <- expression("GPP (g " * m^-2 * hour^-1 * ")"))
  
  plot(newozGPP ~ Tair, data = newDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha=0.2), pch = 16)
  points(newDF$Tair, newDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.2), pch = 16)
  
  ##Subset data to include only high PAR points
  #newDF2 <- subset(newDF, SW > 500)
  newDF2 <- subset(newDF, gdayPAR > 10)
  
  plot(newozGPP ~ Tair, data = newDF2, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.1), pch = 16)
  points(newDF2$Tair, newDF2$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.1), pch = 16)
  
  ####ozflux data manipulation
  ##Using support vector regression to fine best fit model
  #m1 <- svm(newDF2$newozGPP ~ newDF2$Tair)
  #predictedY1 <- predict(m1, data = newDF2)
  #tune1 <- tune(svm, newozGPP ~ Tair, data = newDF2, 
  #              ranges = list(epsilon = seq(0,1,0.2), cost = 2^(2:4)))
  #tuneM1 <- tune1$best.model
  #tuneM1Y1 <- predict(tuneM1, newDF2)
  #rmse_tuned1 <- rmse(newDF2$newozGPP, tuneM1Y1)
  #points(newDF2$Tair, tuneM1Y1, col = "black", pch = 4)
  #tmpDF <- as.data.frame(cbind(newDF2$Tair, tuneM1Y1))
  #topt1 <- round(tmpDF[which(tmpDF$tuneM1Y1 == max(tmpDF$tuneM1Y1)),1],1)
  
  ##Using neural network regression
  #fit <- nnet(newozGPP~Tair, data=newDF2, size=4, decay=0.0001, maxit=500)
  #summary(fit)

  ##Using nls function
  p1 = 1
  p2 = 2
  fit <- nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newDF2, trace=T)
  new <- data.frame(Tair = seq(min(newDF2$Tair),max(newDF2$Tair), len = 200))
  #sum(resid(fit)^2)
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'grey80', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "black", lty = 2, lwd = 2)
  
  predictions <- predict(fit, data = newDF2)
  #points(newDF2$Tair, predictions, col = "black", pch = 16)
  tmpDF <- as.data.frame(cbind(newDF2$Tair, predictions))
  colnames(tmpDF) <- c("Tair", "predictions")
  topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
  
  ######gday data manipulation
  #m2 <- svm(newDF2$gdayGPP ~ newDF2$Tair)
  #tune2 <- tune(svm, gdayGPP ~ Tair, data = newDF2, 
  #              ranges = list(epsilon = seq(0,1,0.2), cost = 2^(2:4)))
  #tuneM2 <- tune2$best.model
  #tuneM2Y2 <- predict(tuneM2, newDF2)
  #rmse_tuned2 <- rmse(newDF2$gdayGPP, tuneM2Y2)
  #points(newDF2$Tair, tuneM2Y2, col = "darkgreen", pch = 4)
  #tmpDF <- as.data.frame(cbind(newDF2$Tair, tuneM2Y2))
  #topt2 <- round(tmpDF[which(tmpDF$tuneM2Y2 == max(tmpDF$tuneM2Y2)),1],1)
  
  ##Using neural network regression
  #fit <- nnet(gdayGPP~Tair, data=newDF2, size=4, decay=0.0001, maxit=500)
  #summary(fit)
  #predictions <- predict(fit, data = newDF2)
  
  #points(newDF2$Tair, predictions, col = "green", pch = 16)
  #tmpDF <- as.data.frame(cbind(newDF2$Tair, predictions))
  #colnames(tmpDF) <- c("Tair", "predictions")
  #topt2 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
  
  ##Plot Topt for both model vs. obs. 
  #abline(v=topt1, col = "black", lwd = 1.5)
  #abline(v=topt2, col = "green", lwd = 1.5)
  
  ##Using nls function
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newDF2, trace=T)
  new <- data.frame(Tair = seq(min(newDF2$Tair),max(newDF2$Tair), len = 200))
  #sum(resid(fit)^2)
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'lightgreen', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  predictions <- predict(fit, data = newDF2)
  #points(newDF2$Tair, predictions, col = "black", pch = 16)
  tmpDF <- as.data.frame(cbind(newDF2$Tair, predictions))
  colnames(tmpDF) <- c("Tair", "predictions")
  topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), pch = c(16,16))
  
  ##bootstrap topt
  d <- as.data.frame(cbind(newDF2$Tair, newDF2$newozGPP))
  colnames(d) <- c("Tair", "newozGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 500),]
    fit = nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
    
  } 
  
  topt.result <- do.call(rbind, lapply(1:500, topt))
  
  x.lab <- expression("Topt (" * degree * "C" * ")")
  hist(topt.result, xlab = x.lab, main = NA, col = "grey")
  
  ##bootstrap topt
  d <- as.data.frame(cbind(newDF2$Tair, newDF2$gdayGPP))
  colnames(d) <- c("Tair", "gdayGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 500),]
    fit = nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
    
  } 
  
  topt.result <- do.call(rbind, lapply(1:500, topt))
  
  x.lab <- expression("Topt (" * degree * "C" * ")")
  hist(topt.result, xlab = x.lab, main = NA, col = "green")
  
  ##Plot histogram 
  newDF2$Tair_box <- as.numeric(cut(newDF2$Tair, 5))
  
  lab1 <- paste(round(min(newDF2[newDF2$Tair_box == 1, "Tair"]),1), "-",
                round(max(newDF2[newDF2$Tair_box == 1, "Tair"]),1))
  lab2 <- paste(round(min(newDF2[newDF2$Tair_box == 2, "Tair"]),1), "-",
                round(max(newDF2[newDF2$Tair_box == 2, "Tair"]),1))
  lab3 <- paste(round(min(newDF2[newDF2$Tair_box == 3, "Tair"]),1), "-",
                round(max(newDF2[newDF2$Tair_box == 3, "Tair"]),1))
  lab4 <- paste(round(min(newDF2[newDF2$Tair_box == 4, "Tair"]),1), "-",
                round(max(newDF2[newDF2$Tair_box == 4, "Tair"]),1))
  lab5 <- paste(round(min(newDF2[newDF2$Tair_box == 5, "Tair"]),1), "-",
                round(max(newDF2[newDF2$Tair_box == 5, "Tair"]),1))
  
  boxplot(newozGPP ~ Tair_box, data = newDF2, notch = T,
          names=c(lab1,lab2,lab3,lab4,lab5), col = "grey",
          xlab = x.lab, ylab = y.lab)
  
  boxplot(gdayGPP ~ Tair_box, data = newDF2, notch = T,
          names=c(lab1,lab2,lab3,lab4,lab5), col = "green",
          xlab = x.lab, ylab = y.lab)
  
  dev.off()

}
 
##############################################################################
##########Dry vs. wet period comparison of Topt

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  newDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_processed.csv",sep=""),
                      sep=",",header=T)
  
  ####################################################
  ##Check dry vs. wet relationship at hourly timestep
  ##wet definition: hourly prec > 0 mm
  #newDF2 <- subset(newDF, SW > 500)
  newDF2 <- subset(newDF, gdayPAR > 10)
  dryDF <- subset(newDF2, Rain == 0)
  wetDF <- subset(newDF2, Rain > 0)
  
  temp.DF <- cbind(newDF2$newozGPP, newDF2$gdayGPP)
  v.low <- round(min(temp.DF),0); v.high <- round(max(temp.DF),2)
  
  x.lab <- expression("Tair (" * degree * "C" * ")")
  ifelse(sec.var == 1800,
         y.lab <- expression("GPP (g " * m^-2 * "30" * min^-1 * ")"),
         y.lab <- expression("GPP (g " * m^-2 * hour^-1 * ")"))
  
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_topt_wetdry_hourly.pdf",sep=""))
  
  par(mfrow=c(2,2),
      mar=c(5.1,5.1,4.1,4.1))
  
  ############################## dry periods ################################
  plot(newozGPP ~ Tair, data = dryDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.1), pch = 16, 
       main = "Dry periods")
  points(dryDF$Tair, dryDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.1), pch = 16)
  
  ####ozflux data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = dryDF, trace=T)
  new <- data.frame(Tair = seq(min(dryDF$Tair),max(dryDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'grey80', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "black", lty = 2, lwd = 2)
  
  ######gday data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = dryDF, trace=T)
  new <- data.frame(Tair = seq(min(dryDF$Tair),max(dryDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'lightgreen', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), pch = c(16,16))
  
  
  ############################## wet periods ################################
  plot(newozGPP ~ Tair, data = wetDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 1),
       pch = 16, main = "Wet periods")
  points(wetDF$Tair, wetDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 1), pch = 16)
  
  ####ozflux data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = wetDF, trace=T)
  new <- data.frame(Tair = seq(min(wetDF$Tair),max(wetDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = adjustcolor('grey80', alpha = 0.2), 
          border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "black", lty = 2, lwd = 2)
    
  ######gday data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = wetDF, trace=T)
  new <- data.frame(Tair = seq(min(wetDF$Tair),max(wetDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = adjustcolor('lightgreen', alpha = 0.2),
          border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), pch = c(16,16))
  
  ############################## dry periods ################################
  d <- as.data.frame(cbind(dryDF$Tair, dryDF$newozGPP))
  colnames(d) <- c("Tair", "newozGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result1 <- do.call(rbind, lapply(1:50, topt))
  topt.result1 <- as.data.frame(topt.result1[,1])
  topt.result1$group <- 1
  colnames(topt.result1) <- c("topt","group")
  
  ##bootstrap topt
  d <- as.data.frame(cbind(dryDF$Tair, dryDF$gdayGPP))
  colnames(d) <- c("Tair", "gdayGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result2 <- do.call(rbind, lapply(1:50, topt))
  topt.result2 <- as.data.frame(topt.result2[,1])
  topt.result2$group <- 2
  colnames(topt.result2) <- c("topt","group")
  
  topt.result <- rbind(topt.result1, topt.result2)
  x.lab <- expression("Topt (" * degree * "C" * ")")
  sm.density.compare(topt.result$topt, xlab = x.lab, 
                     group = topt.result$group, col = c("black", "green"),
                     lty = c(2,1), lwd = 2)
  legend("topright", c("OzFlux", "Gday"), lwd = 2, lty = c(2,1),
         col=c("black","green"))
  
  
  ############################## wet periods ################################
  d <- as.data.frame(cbind(wetDF$Tair, wetDF$newozGPP))
  colnames(d) <- c("Tair", "newozGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result1 <- do.call(rbind, lapply(1:50, topt))
  topt.result1 <- as.data.frame(topt.result1[,1])
  topt.result1$group <- 1
  colnames(topt.result1) <- c("topt","group")
  
  ##bootstrap topt
  d <- as.data.frame(cbind(wetDF$Tair, wetDF$gdayGPP))
  colnames(d) <- c("Tair", "gdayGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result2 <- do.call(rbind, lapply(1:50, topt))
  topt.result2 <- as.data.frame(topt.result2[,1])
  topt.result2$group <- 2
  colnames(topt.result2) <- c("topt","group")
  
  topt.result <- rbind(topt.result1, topt.result2)
  x.lab <- expression("Topt (" * degree * "C" * ")")
  sm.density.compare(topt.result$topt, xlab = x.lab, 
                     group = topt.result$group, col = c("black", "green"),
                     lty = c(2,1), lwd = 2)
  legend("topright", c("OzFlux", "Gday"), lwd = 2, lty = c(2,1),
         col=c("black","green"))
  
  dev.off()
  
}  
  
####################################################
##Check dry vs. wet relationship at monthly timestep meteorological data

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  newDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_processed.csv",sep=""),
                      sep=",",header=T)
  
  newDF$month <- as.numeric(format(as.Date(newDF$Date), "%m"))
  newDF$month <- month(as.Date(newDF$Date))
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_topt_wetdry_monthly_rainfall_sw.pdf",sep=""))
  
  par(mfrow=c(2,1),
      mar = c(5.1,5.1,4.1,4.1))
  
  ##wet month definition: monthly prec > 75th percentile
  short.date <- strftime(newDF$Date, "%Y/%m")
  prec.monthly <- aggregate(newDF$Rain ~ short.date, FUN = sum)
  prec.monthly <- as.data.frame(prec.monthly)
  colnames(prec.monthly) <- c("Date","Prec")
  precDF <- ts(prec.monthly$Prec, frequency = 12, start = min(newDF$Year))
  
  plot(as.xts(precDF), major.format = "%Y/%m", xlab = "Date",
       ylab = "Rainfall (mm/mon)", main = "Monthly rainfall")
  
  wet_def <- quantile(precDF, 0.75)
  abline(h = wet_def, col = "red", lwd = 1.5, lty = 2)
  legend("topright", legend = "75th percentile", col = "red", lty = 2,
         lwd = 2)
  
  sw.monthly <- aggregate(newDF$SW ~ short.date, FUN = sum)
  sw.monthly <- as.data.frame(sw.monthly)
  colnames(sw.monthly) <- c("Date","sw")
  sw.monthly$sw <- sw.monthly$sw/30
  swDF <- ts(sw.monthly$sw, frequency = 12, start = min(newDF$Year))
  
  #plot(as.xts(swDF), major.format = "%Y/%m", xlab = "Date",
  #     ylab = "SW (W m-2)", main = "Monthly mean daily sum SW")
  
  plot(newDF$SW ~ newDF$HOD, col = newDF$month, xlab = "HOD",
       ylab = expression("SW (W " * m^-2 * ")"), main = "Shortwave radiation")
  legend("topright", c("Jan","Feb","Mar","Apr","May","Jun",
                       "Jul","Aug","Sep","Oct","Nov","Dec"),
         col = unique(newDF$month), pch = 19, cex = 0.5)
  
  dev.off()
  
  ####################################################
  ##Check dry vs. wet relationship at monthly timestep
  
  ##Wet period data extraction
  wet_date <- subset(prec.monthly, Prec >= wet_def)
  tmp <- strsplit(wet_date$Date, "/")
  wet_date <- as.data.frame(do.call(rbind, tmp),stringsAsFactors=F)
  colnames(wet_date) <- c("Year","Month")
  wet_date$Year <- as.numeric(wet_date$Year)
  wet_date$Month <- as.numeric(wet_date$Month)
  
  #newDF$month <- round(as.numeric(format(newDF$Date, "%m")),0)
  
  wet_period <- subset(newDF, Year == wet_date[1,1] & month == wet_date[1,2])
  d.length <- length(wet_period$Year)
  
  for (i in 2:length(wet_date$Year))
  {
    tmpDF <- subset(newDF, Year == wet_date[i,1] & month == wet_date[i,2])
    wet_period <- rbind(wet_period, tmpDF)
  }
  
  
  write.table(wet_period, paste(getwd(), "/output_analyses/wet_month.csv", sep=""),
              row.names=F, col.names=T,sep=",")
  
  ##Dry period data extraction
  dry_date <- subset(prec.monthly, Prec < wet_def)
  tmp <- strsplit(dry_date$Date, "/")
  dry_date <- as.data.frame(do.call(rbind, tmp),stringsAsFactors=F)
  colnames(dry_date) <- c("Year","Month")
  dry_date$Year <- as.numeric(dry_date$Year)
  dry_date$Month <- as.numeric(dry_date$Month)
  
  dry_period <- subset(newDF, Year == dry_date[1,1] & month == dry_date[1,2])
  d.length <- length(dry_period$Year)
  
  for (i in 2:length(dry_date$Year))
  {
    tmpDF <- subset(newDF, Year == dry_date[i,1] & month == dry_date[i,2])
    dry_period <- rbind(dry_period, tmpDF)
  }
  
  write.table(wet_period, paste(getwd(), "/output_analyses/dry_month.csv", sep=""),
              row.names=F, col.names=T,sep=",")
  
  ##Define high light condition  
  #dryDF <- subset(dry_period, SW > 500)
  #wetDF <- subset(wet_period, SW > 500)
  dryDF <- subset(dry_period, gdayPAR > 10)
  wetDF <- subset(wet_period, gdayPAR > 10)
  
  
  ##################### Plot monthly comparison #############################
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_topt_wetdry_monthly.pdf",sep=""))
  
  par(mfrow=c(2,2),
      mar=c(5.1,5.1,4.1,4.1))
  
  ############################## dry periods ################################
  plot(newozGPP ~ Tair, data = dryDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.1), pch = 16, main = "Dry periods")
  points(dryDF$Tair, dryDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.1), pch = 16)
  
  ####ozflux data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = dryDF, trace=T)
  new <- data.frame(Tair = seq(min(dryDF$Tair),max(dryDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'grey80', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "black", lty = 2, lwd = 2)
  
  ######gday data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = dryDF, trace=T)
  new <- data.frame(Tair = seq(min(dryDF$Tair),max(dryDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'lightgreen', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), pch = c(16,16))
  
  ############################## wet periods ################################
  plot(newozGPP ~ Tair, data = wetDF, type = "p", ylab = y.lab, xlab = x.lab,
       ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.2), pch = 16, main = "Wet periods")
  points(wetDF$Tair, wetDF$gdayGPP, col = adjustcolor("lightgreen", alpha = 0.2), pch = 16)
  
  ####ozflux data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = wetDF, trace=T)
  new <- data.frame(Tair = seq(min(wetDF$Tair),max(wetDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'grey80', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "black",lwd = 2)
  lines(new$Tair, new$c25, col = "black", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "black", lty = 2, lwd = 2)
  
  ######gday data manipulation
  p1 = 1
  p2 = 2
  fit <- nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = wetDF, trace=T)
  new <- data.frame(Tair = seq(min(wetDF$Tair),max(wetDF$Tair), len = 200))
  conf <- confint(fit)
  new$c25 <- conf[p1,"2.5%"] * (new$Tair)^2 + conf[p2,"2.5%"] * new$Tair
  new$c975 <- conf[p1,"97.5%"] * (new$Tair)^2 + conf[p2,"97.5%"] * new$Tair
  polygon(c(rev(new$Tair), new$Tair), c(rev(new$c975), new$c25), col = 'lightgreen', border = NA)
  lines(new$Tair,predict(fit, newdata=new), col = "darkgreen",lwd = 2)
  lines(new$Tair, new$c25, col = "darkgreen", lty = 2, lwd = 2)
  lines(new$Tair, new$c975, col = "darkgreen", lty = 2, lwd = 2)
  
  legend("topright", c("OzFlux", "Gday"), 
         col = c("black", "darkgreen"), pch = c(16,16))
  
  
  ############################## dry periods ################################
  d <- as.data.frame(cbind(dryDF$Tair, dryDF$newozGPP))
  colnames(d) <- c("Tair", "newozGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result1 <- do.call(rbind, lapply(1:50, topt))
  topt.result1 <- as.data.frame(topt.result1[,1])
  topt.result1$group <- 1
  colnames(topt.result1) <- c("topt","group")
  
  ##bootstrap topt
  d <- as.data.frame(cbind(dryDF$Tair, dryDF$gdayGPP))
  colnames(d) <- c("Tair", "gdayGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result2 <- do.call(rbind, lapply(1:50, topt))
  topt.result2 <- as.data.frame(topt.result2[,1])
  topt.result2$group <- 2
  colnames(topt.result2) <- c("topt","group")
  
  topt.result <- rbind(topt.result1, topt.result2)
  x.lab <- expression("Topt (" * degree * "C" * ")")
  sm.density.compare(topt.result$topt, xlab = x.lab, 
                     group = topt.result$group, col = c("black", "green"),
                     lty = c(2,1), lwd = 2)
  legend("topright", c("OzFlux", "Gday"), lwd = 2, lty = c(2,1),
         col=c("black","green"))
  
  
  ############################## wet periods ################################
  d <- as.data.frame(cbind(wetDF$Tair, wetDF$newozGPP))
  colnames(d) <- c("Tair", "newozGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(newozGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result1 <- do.call(rbind, lapply(1:50, topt))
  topt.result1 <- as.data.frame(topt.result1[,1])
  topt.result1$group <- 1
  colnames(topt.result1) <- c("topt","group")
  
  ##bootstrap topt
  d <- as.data.frame(cbind(wetDF$Tair, wetDF$gdayGPP))
  colnames(d) <- c("Tair", "gdayGPP")
  
  topt <- function(formula, data) 
  {
    newd <- d[sample(nrow(d), 50),]
    fit = nls(gdayGPP ~ p1*Tair^2 + p2*Tair, start=list(p1=p1,p2=p2), data = newd)
    predictions <- predict(fit, data = newd)
    tmpDF <- as.data.frame(cbind(newd$Tair, predictions))
    colnames(tmpDF) <- c("Tair", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    return(topt1)
  } 
  
  topt.result2 <- do.call(rbind, lapply(1:50, topt))
  topt.result2 <- as.data.frame(topt.result2[,1])
  topt.result2$group <- 2
  colnames(topt.result2) <- c("topt","group")
  
  topt.result <- rbind(topt.result1, topt.result2)
  x.lab <- expression("Topt (" * degree * "C" * ")")
  sm.density.compare(topt.result$topt, xlab = x.lab, 
                     group = topt.result$group, col = c("black", "green"),
                     lty = c(2,1), lwd = 2)
  legend("topright", c("OzFlux", "Gday"), lwd = 2, lty = c(2,1),
         col=c("black","green"))
  
  dev.off()  
  
} 

##############################################################################
##Start of a new loop to ease life

ifelse(G == 1,
       SiteName <- c("Calperum","CapeTribulation","CowBay","CumberlandPlains",
                     "DryRiver","Gingin","Whroo","WombatStateForest"),
       SiteName <- c("GreatWesternWoodlands","HowardSprings"))

for (sitename in SiteName)
{
  setwd(paste("~/Documents/PostDoc/GDAY/ozflux/", sitename, sep=""))
  
  ##############################################################################
  ##Plot PAR vs. GPP relationship at subdaily timestep
  
  newDF <- read.table(paste(getwd(), "/output_analyses/gday_ozflux_processed.csv",sep=""),
                      sep=",",header=T)
  
  tmp.DF <- as.data.frame(cbind(newDF$newozGPP, newDF$gdayGPP),stringsAsFactors=F)
  
  v.low <- round(min(tmp.DF),0); v.high <- round(max(tmp.DF),2)
  
  x.lab <- expression("PAR (umol " * m^-2 * s^-1 * ")")
  ifelse(sec.var == 1800,
         y.lab <- expression("GPP (g " * m^-2 * "30" * min^-1 * ")"),
         y.lab <- expression("GPP (g " * m^-2 * hour^-1 * ")"))
  
  pdf(paste(getwd(), "/output_analyses/",sitename,"_PAR_GPP_relation.pdf",sep=""))
  
  par(mar=c(5.1,5.1,4.1,4.1))
  
  plot(newozGPP ~ gdayPAR, data = newDF, xlab = x.lab, 
       ylab = y.lab, col = "grey")
  points(gdayGPP ~ gdayPAR, data = newDF, col = "lightgreen")
  
  test <- as.data.frame(cbind(newDF$gdayPAR, newDF$newozGPP,newDF$gdayGPP),
                        stringsAsFactors=F)
  colnames(test) <- c("PAR","newozGPP","gdayGPP")
  parDF <- test[order(test$PAR),]
  
  ##Moving average plot
  #gpp_mv <- rollmean(parDF$newozGPP, 100, fill = list(NA, NULL,NA))
  #parDF$ozgpp_mv <- gpp_mv
  #gpp_mv <- rollmean(parDF$gdayGPP, 100, fill = list(NA, NULL,NA))
  #parDF$gdaygpp_mv <- gpp_mv
  #lines(parDF$PAR, parDF$ozgpp_mv, lwd = 2, col="black")
  #lines(parDF$PAR, parDF$gdaygpp_mv, lwd = 2, col="darkgreen")
  
  ##Check quantile regression with linear fit
  #parDF$PAR2 <- parDF$PAR^2
  #fit <- rq(newozGPP~PAR,tau=0.5,data=parDF)
  #abline(lm(newozGPP~PAR+PAR2,data=parDF), col = "red")
  #abline(fit, col = "blue")
  
  ##vector plot 
  parDF$PAR_box <- as.numeric(cut(parDF$PAR, 10))
  
  ##newozGPP
  for (i in 1:10)
  {
    DF <- paste("DF", i, sep="")
    
    DF <- subset(parDF, PAR_box == i)
    x.min <- as.numeric(min(DF$PAR))
    x.max <- as.numeric(max(DF$PAR))
    
    fit <- lm(newozGPP~PAR, data = DF)
    y.min <- summary(fit)$coefficient[1] + x.min * summary(fit)$coefficient[2]
    y.max <- summary(fit)$coefficient[1] + x.max * summary(fit)$coefficient[2]
    
    arrows(x.min, y.min, x.max, y.max, col = "black",
           lwd = 3)
  }
  
  ##gdayGPP
  for (i in 1:10)
  {
    DF <- paste("DF", i, sep="")
    
    DF <- subset(parDF, PAR_box == i)
    x.min <- as.numeric(min(DF$PAR))
    x.max <- as.numeric(max(DF$PAR))
    
    fit <- lm(gdayGPP~PAR, data = DF)
    y.min <- summary(fit)$coefficient[1] + x.min * summary(fit)$coefficient[2]
    y.max <- summary(fit)$coefficient[1] + x.max * summary(fit)$coefficient[2]
    
    arrows(x.min, y.min, x.max, y.max, col = "green",
           lwd = 3)
  }
  
  legend("topright", c("OzFlux","GDAY"), col=c("black","green"),
         lwd = 1.5)
  
  dev.off()
  
}



