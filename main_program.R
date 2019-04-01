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

    colnames(newDF) <- c("DateTime", "Lat","Lon", "Tair", "Rain",
                         "SW", "ozGPP", "ozNEE", "ozLE", "ozTrans", "ozNEP",
                         "Year", "DOY", "HOD")
    
    newDF$Date <- as.Date(as.character(gsub( "\\s.*", "", newDF$DateTime)))
    
    newDF$newozGPP <- newDF$ozGPP
    newDF$newozGPP <- ifelse(newDF$newozGPP < 0, 0, newDF$newozGPP)  
    
    ### calcualte daily total GPP
    dDF <- summaryBy(newozGPP+ozNEE+ozNEP+ozTrans+Rain~Date+Year+DOY+Lat+Lon, data=newDF, FUN=sum,
                     na.rm=T, keep.names=T)
    tDF <- summaryBy(Tair+SW~Date, data=newDF, FUN=mean,
                     na.rm=T, keep.names=T)
    dDF$Tair <- tDF$Tair
    dDF$SW <- tDF$SW
    
    mDF <- summaryBy(Tair~Date, data=newDF, FUN=max,
                     na.rm=T, keep.names=T)
    dDF$Tmax <- mDF$Tair
    
    
    dDF <- dDF[,c("Lat", "Lon", "Date", "Year", "DOY", "Tair", "Tmax", "SW", "Rain", "newozGPP", "ozNEE", "ozNEP", "ozTrans")]
    colnames(dDF) <- c("Lat", "Lon", "Date", "Year", "DOY", "Tair", "Tmax", "SW", "Rain", "GPP", "NEE", "NEP", "Trans")
    
    write.csv(dDF, paste0("output/processed_data/", sitename, "_processed.csv"))
    
}   





####################################################
## make basic plots
set.seeds(1234)

for (sitename in SiteName) {
    ### read in data
    newDF <- read.csv(paste0("output/processed_data/", sitename, "_processed.csv"))
    
    ### only fit data to every 1 degree temperature range
    ### and the 90th percentile within this bin
    v.max <- round(max(newDF$Tmax),0)-1
    v.min <- round(min(newDF$Tmax),0)
    
    ## artifically create a range of 1 degree temperature to hold the data
    aDF <- data.frame(c(v.min:v.max), NA, NA, NA)
    colnames(aDF) <- c("temp", "fifth", "nintyfifth", "samplesize")
    
    ### store gpp values 90th percentile
    for (i in aDF$temp) {
        vmax <- aDF$temp[aDF$temp==i] +1
        vmin <- aDF$temp[aDF$temp==i]
        test <- subset(newDF, Tmax >= vmin & Tmax < vmax)
        qt <- quantile(test$GPP, probs = c(0.05, 0.95))
        
        aDF$fifth[aDF$temp==i] <- qt[1]
        aDF$nintyfifth[aDF$temp==i] <- qt[2]
        aDF$samplesize[aDF$temp==i] <- length(test$Tmax)
    }
    
    outDF2 <- c()
    
    for (i in aDF$temp) {
        vmax <- aDF$temp[aDF$temp==i] +1
        vmin <- aDF$temp[aDF$temp==i]
        gmin <- aDF$fifth[aDF$temp==i]
        gmax <- aDF$nintyfifth[aDF$temp==i]
        
        outDF <- subset(newDF, Tmax >= vmin & Tmax < vmax & GPP >= gmin & GPP < gmax)
        outDF2 <- rbind(outDF2, outDF)
    }
    
    ### assign tmax bin
    outDF2$Tmax_bin <- round(outDF2$Tmax,0)
    newDF <- outDF2
    rm(outDF2)

    
    ### Make plots 
    x.lab <- expression("Tair (" * degree * "C" * ")")
    y.lab <- expression("GPP (g " * m^-2 * d^-1 * ")")

    
    ##Plot temp vs. gpp site-model comparison
    pdf(paste0("output/plots/",sitename,"_topt_subdaily.pdf"), width=6, height=10)
    par(mfrow=c(2,1),
        mar = c(5.1,5.1,4.1,4.1))
    
    #plot1 <- ggplot(newDF) + 
    #    geom_point(aes(Tmax_bin,GPP), color=adjustcolor("lightblue", alpha = 0.3))+
    #    geom_smooth(aes(Tmax_bin, GPP), method = gam, se=T, formula = y ~ splines::bs(x, 3), span=0.8) +
    #    xlab(x.lab)+
    #    ylab(y.lab)
    #
    #plot(plot1)
    
    ## Using nls function
    p1 = 1
    p2 = 2
    fit <- nls(GPP ~ p1*Tmax_bin^2 + p2*Tmax_bin, start=list(p1=p1,p2=p2), data = newDF, trace=T)
    new <- data.frame(Tmax_bin = seq(min(newDF$Tmax_bin),max(newDF$Tmax_bin), len = 200))
    #sum(resid(fit)^2)
    conf <- confint(fit)
    new$c25 <- conf[p1,"2.5%"] * (new$Tmax_bin)^2 + conf[p2,"2.5%"] * new$Tmax_bin
    new$c975 <- conf[p1,"97.5%"] * (new$Tmax_bin)^2 + conf[p2,"97.5%"] * new$Tmax_bin
    
    plot(GPP ~ Tmax_bin, data = newDF, type = "p", ylab = y.lab, xlab = x.lab,
         col = adjustcolor("grey", alpha = 0.3), pch = 16)
    polygon(c(rev(new$Tmax_bin), new$Tmax_bin), c(rev(new$c975), new$c25), col = 'grey80', border = NA)
    lines(new$Tmax,predict(fit, newdata=new), col = "black",lwd = 2)
    lines(new$Tmax, new$c25, col = "black", lty = 2, lwd = 2)
    lines(new$Tmax, new$c975, col = "black", lty = 2, lwd = 2)
    
    predictions <- predict(fit, data = newDF)
    # points(newDF$Tmax, predictions, col = "black", pch = 16)
    tmpDF <- as.data.frame(cbind(newDF$Tmax_bin, predictions))
    colnames(tmpDF) <- c("Tmax_bin", "predictions")
    topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
    
    legend("topright", c("OzFlux"), 
           col = c("black"), pch = c(16,16))
    
    ##bootstrap topt
    d <- as.data.frame(cbind(newDF$Tmax_bin, newDF$GPP))
    colnames(d) <- c("Tmax_bin", "GPP")
    
    topt <- function(formula, data) {
        newd <- d[sample(nrow(d), 500),]
        fit = nls(GPP ~ p1*Tmax_bin^2 + p2*Tmax_bin, start=list(p1=p1,p2=p2), data = newd)
        predictions <- predict(fit, data = newd)
        tmpDF <- as.data.frame(cbind(newd$Tmax_bin, predictions))
        colnames(tmpDF) <- c("Tmax_bin", "predictions")
        topt1 <- round(tmpDF[which(tmpDF$predictions == max(tmpDF$predictions)),1],1)
        return(topt1)
        
    } 
    
    topt.result <- do.call(rbind, lapply(1:500, topt))
    
    x.lab <- expression("Topt (" * degree * "C" * ")")
    hist(topt.result, xlab = x.lab, main = "Bootstrapped Topt", col = "grey")
    
    
    #topt.result.plot <- as.data.frame(topt.result)
    
    #plot2 <- ggplot(data=topt.result.plot, aes(topt.result.plot$V1)) + 
    #    geom_histogram(binwidth=1.0, fill="grey", color="black")+
    #    xlab(x.lab)
    
    #plot(plot2)
    
    ###Plot histogram 
    #newDF$Tmax_box <- as.numeric(cut(newDF$Tmax, 5))
    #
    #lab1 <- paste(round(min(newDF[newDF$Tmax_box == 1, "Tmax"]),1), "-",
    #              round(max(newDF[newDF$Tmax_box == 1, "Tmax"]),1))
    #lab2 <- paste(round(min(newDF[newDF$Tmax_box == 2, "Tmax"]),1), "-",
    #              round(max(newDF[newDF$Tmax_box == 2, "Tmax"]),1))
    #lab3 <- paste(round(min(newDF[newDF$Tmax_box == 3, "Tmax"]),1), "-",
    #              round(max(newDF[newDF$Tmax_box == 3, "Tmax"]),1))
    #lab4 <- paste(round(min(newDF[newDF$Tmax_box == 4, "Tmax"]),1), "-",
    #              round(max(newDF[newDF$Tmax_box == 4, "Tmax"]),1))
    #lab5 <- paste(round(min(newDF[newDF$Tmax_box == 5, "Tmax"]),1), "-",
    #              round(max(newDF[newDF$Tmax_box == 5, "Tmax"]),1))
    #
    #boxplot(GPP ~ Tmax_box, data = newDF, notch = T,
    #        names=c(lab1,lab2,lab3,lab4,lab5), col = "grey",
    #        xlab = x.lab, ylab = y.lab, main="Topt by bin")
    
    #plot3 <- ggplot(newDF2, aes(Tair_box, newozGPP, group=Tair_box))+
    #    geom_boxplot(fill="grey")+
    #    scale_x_discrete(name=x.lab, limits=c(1,2,3,4,5),
    #                     labels=c(lab1, lab2, lab3, lab4, lab4))+
    #    ylab(y.lab)
    
    
    #grid.arrange(plot1, plot2, plot3, 
    #             ncol = 1, nrow = 3)
    
    dev.off()
    
}


#### To do list:
### 1. fiure out what function to use to fit the temperature response function
### 2. hourly data subtraction
### 3. consistent plot with x-axis - possibly with ggplot
### 4. go through B's list




