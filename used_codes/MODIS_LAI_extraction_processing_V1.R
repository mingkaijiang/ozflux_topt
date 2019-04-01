##############################################################################
##Process MODIS LAI data 
##extract MODIS LAI for ozflux sites
##Scripts
##Last modified: July-12-2016
##############################################################################
##Data source:
##http://www.auscover.org.au/thredds/catalog/auscover/lpdaac-csiro/c5/v2-nc4/aust/MOD15A2.005/catalog.html
##Data coverage:
##MOD15A2.005
##1 km resolution, 8 days
##2002-01-01 -- 2014-12-31

##############################################################################
##Known problem/check back issues:
##1. 

##############################################################################
require(ncdf)
require(quantreg)
require(zoo)
require(gam)
require(Metrics)
require(mgcv)
require(e1071)
require(xts)
require(ncdf4)
require(spatstat)
require(fields)
require(raster)
require(dismo)
require(rgdal)
##############################################################################
##All OzFlux sites that I have data for
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
##############################################################################

SiteName <- c("AdelaideRiver","Calperum","CapeTribulation","CowBay","CumberlandPlains",
              "DalyPasture","DalyUncleared","DryRiver","Emerald",
              "Gingin","GreatWesternWoodlands","HowardSprings","Otway",
              "RedDirtMelonFarm","RiggsCreek","Samford","SturtPlains",
              "Tumbarumba", "Whroo","WombatStateForest","Yanco")

##############################################################################
##Extract ozflux lon lat information and save into a csv file
outDF <- as.data.frame(matrix(nrow=21,ncol=4))
colnames(outDF) <- c("SiteName","ID","Lon","Lat")

for (i in 1:21)
{
  outDF$SiteName[i] <- SiteName[i]
  outDF$ID[i] <- i
  outDF <- as.data.frame(outDF, stringsAsFactors=F)
  
  tmet <- open.ncdf(paste0("~/Documents/PostDoc/OzFlux/",outDF$SiteName[i],"OzFlux2.0_met.nc"))
  
  lat <- get.var.ncdf(tmet,"latitude")
  lon <- get.var.ncdf(tmet,"longitude")
  outDF[i, "Lon"] <- lon
  outDF[i, "Lat"] <- lat
}

write.table(outDF, "~/Documents/PostDoc/OzFlux/Site_coordinates.csv",
            sep=",",col.names=T,row.names=F)

##############################################################################
##Process nc data and extract data

corDF <- read.table("~/Documents/PostDoc/OzFlux/Site_coordinates.csv",sep=",",
                    header=T)

newDF <- corDF

coordinates(corDF) <- c("Lon","Lat")


outDF <- as.data.frame(matrix(nrow=1, ncol=3))
colnames(outDF) <- c("ID","Date","LAI")
outDF01 <- outDF; outDF02 <- outDF; outDF03 <- outDF;
outDF04 <- outDF; outDF05 <- outDF; outDF06 <- outDF;
outDF07 <- outDF; outDF08 <- outDF; outDF09 <- outDF;
outDF10 <- outDF; outDF11 <- outDF; outDF12 <- outDF;
outDF13 <- outDF; outDF14 <- outDF; outDF15 <- outDF;
outDF16 <- outDF; outDF17 <- outDF; outDF18 <- outDF;
outDF19 <- outDF; outDF20 <- outDF; outDF21 <- outDF;

outDF01[1,"ID"] <- 1; outDF02[1,"ID"] <- 2; outDF03[1,"ID"] <- 3;
outDF04[1,"ID"] <- 4; outDF05[1,"ID"] <- 5; outDF06[1,"ID"] <- 6;
outDF07[1,"ID"] <- 7; outDF08[1,"ID"] <- 8; outDF09[1,"ID"] <- 9;
outDF10[1,"ID"] <- 10; outDF11[1,"ID"] <- 11; outDF12[1,"ID"] <- 12;
outDF13[1,"ID"] <- 13; outDF14[1,"ID"] <- 14; outDF15[1,"ID"] <- 15;
outDF16[1,"ID"] <- 16; outDF17[1,"ID"] <- 17; outDF18[1,"ID"] <- 18;
outDF19[1,"ID"] <- 19; outDF20[1,"ID"] <- 20; outDF21[1,"ID"] <- 21;

setwd("~/Documents/PostDoc/GDAY/MODIS_LAI/")

sourceDir <- paste(getwd(), "/raw", sep="")
destDir <- paste(getwd(), "/extracted",sep="")
DatFiles <- list.files(path = sourceDir, pattern = "\\.nc")

for (i in 1:length(DatFiles)) 
  {
  inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)

  modDF <- nc_open(inName)
  
  #lat <- ncvar_get(modDF,"latitude")
  #lon <- ncvar_get(modDF,"longitude")
  t <- ncvar_get(modDF, "time")
  tunits <- ncatt_get(modDF, "time","units")
  date <- t
  
  nc_close(modDF)
  
  outDF01[i,"Date"] <- date; outDF02[i,"Date"] <- date; outDF03[i,"Date"] <- date; 
  outDF04[i,"Date"] <- date; outDF05[i,"Date"] <- date; outDF06[i,"Date"] <- date; 
  outDF07[i,"Date"] <- date; outDF08[i,"Date"] <- date; outDF09[i,"Date"] <- date; 
  outDF10[i,"Date"] <- date; outDF11[i,"Date"] <- date; outDF12[i,"Date"] <- date; 
  outDF13[i,"Date"] <- date; outDF14[i,"Date"] <- date; outDF15[i,"Date"] <- date; 
  outDF16[i,"Date"] <- date; outDF17[i,"Date"] <- date; outDF18[i,"Date"] <- date; 
  outDF19[i,"Date"] <- date; outDF20[i,"Date"] <- date; outDF21[i,"Date"] <- date; 
  
  d <- raster(inName, varname="lai")
  #plot(d)
  #myDF <- rasterToPoints(d)
  #myDF <- as.data.frame(myDF, stringsAsFactors=F)
  #colnames(myDF) <- c("x","y","LAI")
  #myDF <- subset(myDF, LAI <= 10 & LAI >= 0)  
  
  newDF$lai <- extract(d, corDF)
  
  outDF01[i,"LAI"] <- newDF$lai[1]; outDF02[i,"LAI"] <- newDF$lai[2]; outDF03[i,"LAI"] <- newDF$lai[3]; 
  outDF04[i,"LAI"] <- newDF$lai[4]; outDF05[i,"LAI"] <- newDF$lai[5]; outDF06[i,"LAI"] <- newDF$lai[6]; 
  outDF07[i,"LAI"] <- newDF$lai[7]; outDF08[i,"LAI"] <- newDF$lai[8]; outDF09[i,"LAI"] <- newDF$lai[9]; 
  outDF10[i,"LAI"] <- newDF$lai[10]; outDF11[i,"LAI"] <- newDF$lai[11]; outDF12[i,"LAI"] <- newDF$lai[12]; 
  outDF13[i,"LAI"] <- newDF$lai[13]; outDF14[i,"LAI"] <- newDF$lai[14]; outDF15[i,"LAI"] <- newDF$lai[15]; 
  outDF16[i,"LAI"] <- newDF$lai[16]; outDF17[i,"LAI"] <- newDF$lai[17]; outDF18[i,"LAI"] <- newDF$lai[18]; 
  outDF19[i,"LAI"] <- newDF$lai[19]; outDF20[i,"LAI"] <- newDF$lai[20]; outDF21[i,"LAI"] <- newDF$lai[21]; 
  
  }

write.table(outDF01, paste(destDir, "/", newDF$SiteName[1], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF02, paste(destDir, "/", newDF$SiteName[2], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF03, paste(destDir, "/", newDF$SiteName[3], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF04, paste(destDir, "/", newDF$SiteName[4], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF05, paste(destDir, "/", newDF$SiteName[5], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF06, paste(destDir, "/", newDF$SiteName[6], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF07, paste(destDir, "/", newDF$SiteName[7], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF08, paste(destDir, "/", newDF$SiteName[8], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF09, paste(destDir, "/", newDF$SiteName[9], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF10, paste(destDir, "/", newDF$SiteName[10], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF11, paste(destDir, "/", newDF$SiteName[11], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF12, paste(destDir, "/", newDF$SiteName[12], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF13, paste(destDir, "/", newDF$SiteName[13], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF14, paste(destDir, "/", newDF$SiteName[14], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF15, paste(destDir, "/", newDF$SiteName[15], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF16, paste(destDir, "/", newDF$SiteName[16], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF17, paste(destDir, "/", newDF$SiteName[17], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF18, paste(destDir, "/", newDF$SiteName[18], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF19, paste(destDir, "/", newDF$SiteName[19], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF20, paste(destDir, "/", newDF$SiteName[20], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF21, paste(destDir, "/", newDF$SiteName[21], "_LAI.csv", sep=""),
            row.names=F, col.names=T, sep=",")

##############################################################################
##read individual MODIS LAI files, compare to gday output





  