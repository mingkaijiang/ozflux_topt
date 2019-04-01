##Grab online MODIS GPP data and save onto my own laptop
##Script V 1
##Created by Mingkai Jiang
##Last edit: 07/26/2016

##############################################################################
#require(data.table)
require(utils)
require(sp)
require(ncdf4)
require(raster)
require(maps)
##############################################################################

##data source: http://www.auscover.org.au/thredds/catalog/auscover/modis-gpp/catalog.html
##MODIS GPP resolution: 250 m, monthly
##MODIS coverage: 200101 - 201212
##############################################################################
##Download monthly data
setwd("/Volumes/JIANG/data/MODIS_GPP/raw/")

url.path <- "http://www.auscover.org.au/thredds/fileServer/auscover/modis-gpp/"
pre.name <- "Hume_GPP_250m_v5_monthly_"

month <- c("01","02", "03", "04", "05", "06",
                      "07", "08", "09", "10", "11", "12")

##Loop through year and month
##download monthly data
for (y in 2001:2012)
{
  for (m in 1:length(month))
  {
    nc.name <- paste(url.path, pre.name, y, month[m], ".nc", sep="")
    download.file(nc.name, paste(getwd(), "/", pre.name, y, month[m], ".nc", sep=""))
  }
}

##############################################################################
##Process nc data and extract monthly gpp data
setwd("~/Documents/PostDoc/GDAY/MODIS_GPP/")

sourceDir <- "/Volumes/JIANG/data/MODIS_GPP/raw/"
destDir <- paste(getwd(), "/extracted",sep="")
DatFiles <- list.files(path = sourceDir, pattern = "\\.nc")

i <- 1
inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)

##aggregate data to coarser resolution
d <- raster(inName, varname = "total")
r <- aggregate(d, fun = mean, fact = 400, na.rm=T)

test <- rasterToPoints(d)
test <- as.data.frame(test)
colnames(test) <- c("x","y","GPP")

outDF <- rasterToPoints(r)
outDF<- as.data.frame(outDF, stringsAsFactors=F)
colnames(outDF) <- c("x","y","GPP")

for (i in 2:length(DatFiles))
{
  inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
  
  ##aggregate data to coarser resolution
  d <- raster(inName, varname = "total")
  r <- aggregate(d, fun = mean, fact = 400, na.rm=T)
  out <- rasterToPoints(d)
  out <- as.data.frame(out, stringsAsFactors=F)
  colnames(out) <- c("x","y","GPP")
  
  outDF[,(i+2)] <- out$GPP
}

write.table(outDF, paste(destDir, "/MODIS_GPP_200101-200512.csv", sep=""),
            col.names=T, row.names=F, sep=",")


##############################################################################
##Process modis nc data and save onto individual CMIP extent and resolution

setwd("~/Documents/PostDoc/GDAY/MODIS_GPP/")

sourceDir <- "/Volumes/JIANG/data/MODIS_GPP/raw/"
destDir <- paste(getwd(), "/extracted",sep="")
DatFiles <- list.files(path = sourceDir, pattern = "\\.nc")

i <- 1
inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)

##aggregate data to coarser resolution
d <- raster(inName, varname = "total")
r <- aggregate(d, fun = mean, fact = 400, na.rm=T)

test <- rasterToPoints(d)
test <- as.data.frame(test)
colnames(test) <- c("x","y","GPP")

outDF <- rasterToPoints(r)
outDF<- as.data.frame(outDF, stringsAsFactors=F)
colnames(outDF) <- c("x","y","GPP")

for (i in 2:length(DatFiles))
{
  inName <- file.path(sourceDir, DatFiles[i], fsep = .Platform$file.sep)
  
  ##aggregate data to coarser resolution
  d <- raster(inName, varname = "total")
  r <- aggregate(d, fun = mean, fact = 400, na.rm=T)
  out <- rasterToPoints(d)
  out <- as.data.frame(out, stringsAsFactors=F)
  colnames(out) <- c("x","y","GPP")
  
  outDF[,(i+2)] <- out$GPP
}

write.table(outDF, paste(destDir, "/MODIS_GPP_200101-200512.csv", sep=""),
            col.names=T, row.names=F, sep=",")



