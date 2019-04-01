##Grab online precipitation data and save onto my own laptop
##Script V 1
##Created by Mingkai Jiang
##Last edit: 07/21/2016
##data source: http://rs-data1-mel.csiro.au/thredds/catalog/bawap/rain/catalog.html
##############################################################################
#require(ncdf4)
#require(data.table)
require(utils)
require(sp)
require(ncdf4)
require(raster)
##############################################################################
##Download daily data
setwd("~/Documents/PostDoc/GDAY/precip/raw/daily/")

##Basic url link
url.pre <- "http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/rain/"
url.end <- ".nc"
url.resolution <- "day"
url.month.array <- c("01","02","03","04","05","06",
                     "07","08","09","10","11","12")

month31 <- c("01","03","05","07","08","10","12")
month30 <- c("04","06","09","11")


##Function to check if leap year
is.leapyear <- function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

##Function to download data
dl <- function(doy){
  nc.name1 <- paste("bom-rain_", url.resolution, "-", url.year, url.month, "01-", 
                    url.year, url.month, sep="")
  nc.name2 <- paste(nc.name1, doy, url.end, sep="")
  url.path <- paste(url.pre, url.resolution, "/", url.year, "/", nc.name2, sep="")
  download.file(url.path, paste(getwd(), nc.name2, sep="/"))  
}


##Function to download feb data
dlfeb <- function(doy){
  nc.name1 <- paste("bom-rain_", url.resolution, "-", url.year, "0201-", 
                    url.year, "02", sep="")
  nc.name2 <- paste(nc.name1, doy, url.end, sep="")
  url.path <- paste(url.pre, url.resolution, "/", url.year, "/", nc.name2, sep="")
  download.file(url.path, paste(getwd(), nc.name2, sep="/")) 
}

#############################
##Loop through year
##download daily data
url.resolution <- "day"

for (url.year in 1900:2016)
{
  leap.check <- ifelse(is.leapyear(url.year) == T, 1, 2)
  
    ##download months with 31 days
    for (url.month in month31)
    {
      dl(31)
    }
    
    ##download months with 30 days
    for (url.month in month30)
    {
      dl(30)
    }
    
    ##download data in leap year month
    ifelse(leap.check == 1, dlfeb(29), dlfeb(28))
}

##############################################################################
##Download monthly data
setwd("~/Documents/PostDoc/GDAY/precip/raw/monthly/")

##Basic url link
url.pre <- "http://rs-data1-mel.csiro.au/thredds/fileServer/bawap/rain/"
url.end <- ".nc"
url.resolution <- "month"

##Loop through year
##download monthly data

for (url.year in 1900:2015)
{
    nc.name1 <- paste("bom-rain_", url.resolution, "-", url.year, "0101-", 
                      url.year, "1231", url.end, sep="")
    url.path <- paste(url.pre, url.resolution, "/", nc.name1, sep="")
    download.file(url.path, paste(getwd(), nc.name1, sep="/"))  

}

##############################################################################
##Process nc data and extract monthly prec data
setwd("~/Documents/PostDoc/GDAY/precip/raw/")

sourceDir <- paste(getwd(), "/monthly", sep="")
destDir <- paste(getwd(), "/extracted",sep="")
DatFiles <- list.files(path = sourceDir, pattern = "\\.nc")

corDF <- read.table("~/Documents/PostDoc/OzFlux/Site_coordinates.csv",sep=",",
                    header=T)

newDF <- corDF

coordinates(corDF) <- c("Lon","Lat")

outDF <- as.data.frame(matrix(nrow=length(DatFiles)*12, ncol=5))
colnames(outDF) <- c("ID","Date","Year","Month","prec")

outDF[,"Date"] <- seq(as.Date("1900/1/1"), by = "month", length.out = length(DatFiles)*12)
outDF[,"Year"] <- as.numeric(format(outDF$Date, "%Y"))
outDF[,"Month"] <- as.numeric(format(outDF$Date, "%m"))
outDF <- as.data.frame(outDF, stringsAsFactors=F)

outDF01 <- outDF; outDF02 <- outDF; outDF03 <- outDF;
outDF04 <- outDF; outDF05 <- outDF; outDF06 <- outDF;
outDF07 <- outDF; outDF08 <- outDF; outDF09 <- outDF;
outDF10 <- outDF; outDF11 <- outDF; outDF12 <- outDF;
outDF13 <- outDF; outDF14 <- outDF; outDF15 <- outDF;
outDF16 <- outDF; outDF17 <- outDF; outDF18 <- outDF;
outDF19 <- outDF; outDF20 <- outDF; outDF21 <- outDF;

outDF01[,"ID"] <- 1; outDF02[,"ID"] <- 2; outDF03[,"ID"] <- 3;
outDF04[,"ID"] <- 4; outDF05[,"ID"] <- 5; outDF06[,"ID"] <- 6;
outDF07[,"ID"] <- 7; outDF08[,"ID"] <- 8; outDF09[,"ID"] <- 9;
outDF10[,"ID"] <- 10; outDF11[,"ID"] <- 11; outDF12[,"ID"] <- 12;
outDF13[,"ID"] <- 13; outDF14[,"ID"] <- 14; outDF15[,"ID"] <- 15;
outDF16[,"ID"] <- 16; outDF17[,"ID"] <- 17; outDF18[,"ID"] <- 18;
outDF19[,"ID"] <- 19; outDF20[,"ID"] <- 20; outDF21[,"ID"] <- 21;

for (i in 1:length(DatFiles)) 
{
  inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
  
  modDF <- nc_open(inName)
  
  t <- ncvar_get(modDF, "time")
  tunits <- ncatt_get(modDF, "time","units")
  date <- t
  
  #nc_close(modDF)
  
  d <- brick(inName, varname = "rain_month")
  #nlayers(d)
  #plot(d, 2)
  
  newDF$prec <- extract(d, corDF)
  
  y.index <- 1899+i
  
  t01 <- t(newDF[1,]); outDF01[outDF01$Year == y.index, "prec"] <- as.numeric(t01[5:16,1])
  t02 <- t(newDF[2,]); outDF02[outDF02$Year == y.index, "prec"] <- as.numeric(t02[5:16,1])
  t03 <- t(newDF[3,]); outDF03[outDF03$Year == y.index, "prec"] <- as.numeric(t03[5:16,1])
  t04 <- t(newDF[4,]); outDF04[outDF04$Year == y.index, "prec"] <- as.numeric(t04[5:16,1])
  t05 <- t(newDF[5,]); outDF05[outDF05$Year == y.index, "prec"] <- as.numeric(t05[5:16,1])
  t06 <- t(newDF[6,]); outDF06[outDF06$Year == y.index, "prec"] <- as.numeric(t06[5:16,1])
  t07 <- t(newDF[7,]); outDF07[outDF07$Year == y.index, "prec"] <- as.numeric(t07[5:16,1])
  t08 <- t(newDF[8,]); outDF08[outDF08$Year == y.index, "prec"] <- as.numeric(t08[5:16,1])
  t09 <- t(newDF[9,]); outDF09[outDF09$Year == y.index, "prec"] <- as.numeric(t09[5:16,1])
  t10 <- t(newDF[10,]); outDF10[outDF10$Year == y.index, "prec"] <- as.numeric(t10[5:16,1])
  t11 <- t(newDF[11,]); outDF11[outDF11$Year == y.index, "prec"] <- as.numeric(t11[5:16,1])
  t12 <- t(newDF[12,]); outDF12[outDF12$Year == y.index, "prec"] <- as.numeric(t12[5:16,1])
  t13 <- t(newDF[13,]); outDF13[outDF13$Year == y.index, "prec"] <- as.numeric(t13[5:16,1])
  t14 <- t(newDF[14,]); outDF14[outDF14$Year == y.index, "prec"] <- as.numeric(t14[5:16,1])
  t15 <- t(newDF[15,]); outDF15[outDF15$Year == y.index, "prec"] <- as.numeric(t15[5:16,1])
  t16 <- t(newDF[16,]); outDF16[outDF16$Year == y.index, "prec"] <- as.numeric(t16[5:16,1])
  t17 <- t(newDF[17,]); outDF17[outDF17$Year == y.index, "prec"] <- as.numeric(t17[5:16,1])
  t18 <- t(newDF[18,]); outDF18[outDF18$Year == y.index, "prec"] <- as.numeric(t18[5:16,1])
  t19 <- t(newDF[19,]); outDF19[outDF19$Year == y.index, "prec"] <- as.numeric(t19[5:16,1])
  t20 <- t(newDF[20,]); outDF20[outDF20$Year == y.index, "prec"] <- as.numeric(t20[5:16,1])
  t21 <- t(newDF[21,]); outDF21[outDF21$Year == y.index, "prec"] <- as.numeric(t21[5:16,1])
  
}

write.table(outDF01, paste(destDir, "/", newDF$SiteName[1], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF02, paste(destDir, "/", newDF$SiteName[2], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF03, paste(destDir, "/", newDF$SiteName[3], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF04, paste(destDir, "/", newDF$SiteName[4], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF05, paste(destDir, "/", newDF$SiteName[5], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF06, paste(destDir, "/", newDF$SiteName[6], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF07, paste(destDir, "/", newDF$SiteName[7], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF08, paste(destDir, "/", newDF$SiteName[8], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF09, paste(destDir, "/", newDF$SiteName[9], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF10, paste(destDir, "/", newDF$SiteName[10], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF11, paste(destDir, "/", newDF$SiteName[11], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF12, paste(destDir, "/", newDF$SiteName[12], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF13, paste(destDir, "/", newDF$SiteName[13], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF14, paste(destDir, "/", newDF$SiteName[14], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF15, paste(destDir, "/", newDF$SiteName[15], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF16, paste(destDir, "/", newDF$SiteName[16], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF17, paste(destDir, "/", newDF$SiteName[17], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF18, paste(destDir, "/", newDF$SiteName[18], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF19, paste(destDir, "/", newDF$SiteName[19], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF20, paste(destDir, "/", newDF$SiteName[20], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")
write.table(outDF21, paste(destDir, "/", newDF$SiteName[21], "_month_prec.csv", sep=""),
            row.names=F, col.names=T, sep=",")

##############################################################################
##Process extracted monthly prec data to obtain drought information
setwd("~/Documents/PostDoc/GDAY/precip/raw/")

corDF <- read.table("~/Documents/PostDoc/OzFlux/Site_coordinates.csv",sep=",",
                    header=T)


for (i in 1:21)
{
  myDF <- read.table(paste(getwd(), "/extracted/",corDF$SiteName[i], 
                           "_month_prec.csv", sep = ""),
                     header=T, sep = ",")
  
  myDF$Date <- as.Date(myDF$Date)
  
  myDF$drought <- 0
  
  jan10 <- quantile(myDF[myDF$Month == 1, "prec"], 0.1)
  feb10 <- quantile(myDF[myDF$Month == 2, "prec"], 0.1)
  mar10 <- quantile(myDF[myDF$Month == 3, "prec"], 0.1)
  apr10 <- quantile(myDF[myDF$Month == 4, "prec"], 0.1)
  may10 <- quantile(myDF[myDF$Month == 5, "prec"], 0.1)
  jun10 <- quantile(myDF[myDF$Month == 6, "prec"], 0.1)
  jul10 <- quantile(myDF[myDF$Month == 7, "prec"], 0.1)
  aug10 <- quantile(myDF[myDF$Month == 8, "prec"], 0.1)
  sep10 <- quantile(myDF[myDF$Month == 9, "prec"], 0.1)
  oct10 <- quantile(myDF[myDF$Month == 10, "prec"], 0.1)
  nov10 <- quantile(myDF[myDF$Month == 11, "prec"], 0.1)
  dec10 <- quantile(myDF[myDF$Month == 12, "prec"], 0.1)
  
  pcDF <- rbind(jan10, feb10, mar10, apr10, may10, jun10, 
                jul10, aug10, sep10, oct10, nov10, dec10)
  pcDF <- as.data.frame(pcDF, stringsAsFactors=F)
  pcDF$month <- c(1:12)
  colnames(pcDF) <- c("10th_value","month")
  
  write.table(pcDF, paste(getwd(), "/drought_index/", corDF$SiteName[i], 
                    "_monthly_10th_percentile_value.csv", sep=""), col.names=T,
              row.names=F, sep=",")
  
  for (m in 1:12)
  {
    myDF[myDF$Month == m & myDF$prec <= pcDF[pcDF$month == m, "10th_value"], "drought"] <- 1
  }
  
  jan10 <- subset(myDF, Month == 1 & prec <= pcDF[pcDF$month == 1, "10th_value"])
  feb10 <- subset(myDF, Month == 2 & prec <= pcDF[pcDF$month == 2, "10th_value"])
  mar10 <- subset(myDF, Month == 3 & prec <= pcDF[pcDF$month == 3, "10th_value"])
  apr10 <- subset(myDF, Month == 4 & prec <= pcDF[pcDF$month == 4, "10th_value"])
  may10 <- subset(myDF, Month == 5 & prec <= pcDF[pcDF$month == 5, "10th_value"])
  jun10 <- subset(myDF, Month == 6 & prec <= pcDF[pcDF$month == 6, "10th_value"])
  jul10 <- subset(myDF, Month == 7 & prec <= pcDF[pcDF$month == 7, "10th_value"])
  aug10 <- subset(myDF, Month == 8 & prec <= pcDF[pcDF$month == 8, "10th_value"])
  sep10 <- subset(myDF, Month == 9 & prec <= pcDF[pcDF$month == 9, "10th_value"])
  oct10 <- subset(myDF, Month == 10 & prec <= pcDF[pcDF$month == 10, "10th_value"])
  nov10 <- subset(myDF, Month == 11 & prec <= pcDF[pcDF$month == 11, "10th_value"])
  dec10 <- subset(myDF, Month == 12 & prec <= pcDF[pcDF$month == 12, "10th_value"])
  
  pcDF <- rbind(jan10, feb10, mar10, apr10, may10, jun10, 
                jul10, aug10, sep10, oct10, nov10, dec10)
  pcDF <- as.data.frame(pcDF, stringsAsFactors=F)
  pcDF$Date <- as.Date(pcDF$Date)
  
  write.table(pcDF, paste(getwd(), "/drought_index/", corDF$SiteName[i], 
                          "_monthly_10th_percentile_month.csv", sep=""), col.names=T,
              row.names=F, sep=",")
  
  pdf(paste(getwd(), "/plot/",corDF$SiteName[i], 
            "10th_time_series.pdf", sep = ""))
  
  plot(prec~Date, data = myDF, xlab = "Date", ylab = "monthly rainfall (mm)")
  points(prec~Date, data = pcDF, pch = 16, col = "red")
  legend("topleft", c("monthly rainfall", "monthly 10th percentile rainfall"),
         col = c("black","red"), pch= c(1, 16))
  
  dev.off()
  
  write.table(myDF, paste(getwd(), "/drought_index/", corDF$SiteName[i], 
                          "_monthly_prec_drought_indicator.csv", sep=""), col.names=T,
              row.names=F, sep=",")
  
}

##############################################################################
##Process extracted monthly prec data to obtain drought information
setwd("~/Documents/PostDoc/GDAY/precip/raw/")