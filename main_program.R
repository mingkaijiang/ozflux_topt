#### Process Ozflux data to investigate optimum Tair
#### Author: Mingkai Jiang
#### Contact: m.jiang@westernsydney.edu.au

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

################################# Master script ################################# 

#### clear wk space
rm(list=ls(all=TRUE))

############## source
source("prepare.R")

################################## subdaily data ################################# 
#### Process sub-daily data
###use neural network regression rather than vector support machines to find best fit


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
#### read all gpp data from nc and save them as csv
for (sitename in SiteName) {
    ##############################################################################
    ##Read Ozflux data 
    tmet <- nc_open(paste0("OzFlux/",sitename,"OzFlux2.0_met.nc"))
    #print(tmet)
    
    lat <- ncvar_get(tmet,"latitude")
    lon <- ncvar_get(tmet,"longitude")
    SW <- ncvar_get(tmet,varid="SWdown")
    
    t <- ncvar_get(tmet, "time")
    tunits <- ncatt_get(tmet, "time","units")
    nt <- dim(t)
    sec.var <- as.numeric(t[2]-t[1])
    
    tair <- ncvar_get(tmet,"Tair") - 273.15
    rain <- c()
    ifelse(sec.var == 1800, 
           rain <- ncvar_get(tmet,"Rainf")*1800,
           rain <- ncvar_get(tmet,"Rainf")*3600)
    
    nc_close(tmet)
    
    tflx <- nc_open(paste0("OzFlux/",sitename,"OzFlux2.0_flux.nc"))
    
    #print(tflx)
    
    qle <- ncvar_get(tflx, varid="Qle")
    tgpp <- ncvar_get(tflx,varid="GPP")
    tnee <- ncvar_get(tflx,varid="NEE")
    
    ozDF <- data.frame(lat,lon,tair,rain,SW, tgpp,tnee, qle)
    
    nc_close(tflx)
    
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
    newDF <- ozDF
    
    ## add date information
    newDF$Year <- year(newDF$Date)
    newDF$DOY <- yday(newDF$Date)
    newDF$HOD <- hour(newDF$Date)
    
    colnames(newDF) <- c("Date", "Lat","Lon", "Tair", "Rain",
                         "SW", "ozGPP", "ozNEE", "ozLE", "ozTrans", "ozNEP",
                         "Year", "DOY", "HOD")
    
    newDF <- as.data.frame(newDF, stringsAsFactors=F)
    
    newDF$newozGPP <- newDF$ozGPP
    newDF$newozGPP <- ifelse(newDF$newozGPP < 0, 0, newDF$newozGPP)  
    
    write.csv(newDF, paste0("output/processed_data/", sitename, "_processed.csv"))
    
    
    
    
    
    
    
    ### Make plots 
    ##Plot temp vs. gpp site-model comparison
    pdf(paste0("output/plots/",sitename,"_topt_subdaily.pdf"), width=6, height=10)
    
    par(mfrow=c(3,1),
        mar = c(5.1,5.1,4.1,4.1))
    
    x.lab <- expression("Tair (" * degree * "C" * ")")
    ifelse(sec.var == 1800,
           y.lab <- expression("GPP (g " * m^-2 * "30" * min^-1 * ")"),
           y.lab <- expression("GPP (g " * m^-2 * hour^-1 * ")"))
    
    #plot(newozGPP ~ Tair, data = newDF, type = "p", ylab = y.lab, xlab = x.lab,
    #     col = adjustcolor("grey", alpha=0.2), pch = 16)
    
    ##Subset data to include only high PAR points
    newDF2 <- subset(newDF, SW > 500)
    
    plot(newozGPP ~ Tair, data = newDF2, type = "p", ylab = y.lab, xlab = x.lab,
         col = adjustcolor("grey", alpha = 0.1), pch = 16, main="GPP under high light")
    
    ####ozflux data manipulation
    ##Using nls function
    p1 = 1
    p2 = 2
    lo <- smooth.spline(newDF2$Tair, newDF2$newozGPP, spar=0.05)
    lines(predict(lo), col='red', lwd=2)
    qplot(newDF2$Tair,newDF2$newozGPP, geom='smooth', span =0.5)
    
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
    
    legend("topright", c("OzFlux"), 
           col = c("black"), pch = c(16,16))
    
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
    hist(topt.result, xlab = x.lab, main = "Bootstrapped Topt", col = "grey")
    
    
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
            xlab = x.lab, ylab = y.lab, main="Topt by bin")
    
    dev.off()
    
}   





####################################################
##Check dry vs. wet relationship at monthly timestep meteorological data

for (sitename in SiteName)
{

    newDF <- read.csv(paste0("output/processed_data/", sitename, "_processed.csv"))
    
    newDF$month <- as.numeric(format(as.Date(newDF$Date), "%m"))
    newDF$month <- month(as.Date(newDF$Date))
    
    pdf(paste0("output/plots/",sitename,"_topt_wetdry_monthly_rainfall.pdf"))
    
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
    
    
    ##Define high light condition  
    dryDF <- subset(dry_period, SW > 500)
    wetDF <- subset(wet_period, SW > 500)

    
    ##################### Plot monthly comparison #############################
    
    pdf(paste(getwd(), "/output_analyses/",sitename,"_topt_wetdry_monthly.pdf",sep=""))
    
    par(mfrow=c(2,2),
        mar=c(5.1,5.1,4.1,4.1))
    
    ############################## dry periods ################################
    plot(newozGPP ~ Tair, data = dryDF, type = "p", ylab = y.lab, xlab = x.lab,
         ylim = c(v.low,v.high), col = adjustcolor("grey", alpha = 0.1), pch = 16, main = "Dry periods")

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
    
 
    legend("topright", c("OzFlux"), 
           col = c("black"), pch = c(16,16))
    
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
    

    legend("topright", c("OzFlux"), 
           col = c("black"), pch = c(16,16))
    
    
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
    
    topt.result2 <- do.call(rbind, lapply(1:50, topt))
    topt.result2 <- as.data.frame(topt.result1[,2])
    topt.result2$group <- 2
    colnames(topt.result2) <- c("topt","group")
    
    #### plot
    topt.result <- rbind(topt.result1, topt.result2)
    x.lab <- expression("Topt (" * degree * "C" * ")")
    sm.density.compare(topt.result$topt, xlab = x.lab, 
                       group = topt.result$group, col = c("red2", "blue3"),
                       lty = c(2,1), lwd = 2)
    legend("topright", c("dry", "wet"), lwd = 2, lty = c(2,1),
           col=c("red2","blue3"))
    
    dev.off()  
    
} 

