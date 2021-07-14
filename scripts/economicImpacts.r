### Determine the economic loss for specific lakes with data

## Load packages and functions
library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)


## load data

economic <- read.csv("data//economicData.csv")


### Create a raster to estimate the difference for each RCP

allModels <- read.csv("data//rasterFilepaths.csv", stringsAsFactors=F) ## load in all raster filepaths
allModels <- allModels %>% filter(!(lakemodel == "lake" & GCM == "gfdl-esm2m" & RCP == "rcp85")) ## drop anomalous model


outDurationChange <- function(iceOnPaths, IceoffPaths, rcp) {
  
  ## Get current ice duration
  iceOn <- grep(iceOnPaths, pattern=rcp, value=T)
  iceOn <- stack(iceOn, bands=70:99)
  avgOn <- mean(iceOn, na.rm=T)
  iceOff <- grep(IceoffPaths, pattern=rcp, value=T)
  iceOff <- stack(iceOff, bands=70:99)
  avgOff <- mean(iceOff, na.rm=T)
  currentDuration <- avgOn-avgOff
  
  ## Get future ice duration
  iceOn <- grep(iceOnPaths, pattern=rcp, value=T)
  iceOn <- stack(iceOn, bands=170:99)
  avgOn <- mean(iceOn, na.rm=T)
  iceOff <- grep(IceoffPaths, pattern=rcp, value=T)
  iceOff <- stack(iceOff, bands=170:99)
  avgOff <- mean(iceOff, na.rm=T)
  futureDuration <- avgOn-avgOff
  
  perChange <- (futureDuration-currentDuration)/currentDuration
  
  writeRaster(perChange, paste0("data//durationChangeRCP//DurationDiff",rcp,".tif"), overwrite=T)
}


outDurationChange(allModels[allModels$IceTiming=="icestart","filepath"], allModels[allModels$IceTiming=="iceend","filepath"], rcp="rcp26") 
outDurationChange(allModels[allModels$IceTiming=="icestart","filepath"], allModels[allModels$IceTiming=="iceend","filepath"], rcp="rcp60") 
outDurationChange(allModels[allModels$IceTiming=="icestart","filepath"], allModels[allModels$IceTiming=="iceend","filepath"], rcp="rcp85") 


## pull values for the specific lakes

lakeSpecific <- economic[!is.na(economic$lat),]
lakeGPS <- lakeSpecific
coordinates(lakeGPS) <- ~lon+lat
proj4string(lakeGPS) <- "+proj=longlat +datum=WGS84"


diff26 <- raster("data//durationChangeRCP//DurationDiffrcp26.tif")
diff60 <- raster("data//durationChangeRCP//DurationDiffrcp60.tif")
diff85 <- raster("data//durationChangeRCP//DurationDiffrcp85.tif")

lakeBuffer26 <-  raster::extract(diff26, lakeGPS, buffer=100000)
lakeBuffer60 <-  raster::extract(diff60, lakeGPS, buffer=100000)
lakeBuffer85 <-  raster::extract(diff85, lakeGPS, buffer=100000)

lakeSpecific[,"rcp26"] <-  sapply(lakeBuffer26, mean, na.rm=T)
lakeSpecific[,"rcp60"] <-  sapply(lakeBuffer60, mean, na.rm=T)
lakeSpecific[,"rcp85"] <-  sapply(lakeBuffer85, mean, na.rm=T)


write.csv(lakeSpecific, "data//economicSpecificLake.csv", row.names = F)

### Get the polygons for the larger regions 

USApoly <- getData("GADM", country="USA", level=0)
Statepoly <- getData("GADM", country="USA", level=1)
MinnPoly <- Statepoly[Statepoly$NAME_1=="Minnesota",]
Canpoly <- getData("GADM", country="CAN", level=1)
OntPoly <- Canpoly[Canpoly$NAME_1=="Ontario",]
NovaPoly <- Canpoly[Canpoly$NAME_1=="Nova Scotia",]
SwedenPoly <- getData("GADM", country="SWE", level=0)
ChinaPoly <- getData("GADM", country="CHN", level=1)
HeilPoly <- ChinaPoly[ChinaPoly$NAME_1=="Heilongjiang",]

## Simplify the provincial polygons
OntPoly <- OntPoly[,c("GID_0","NAME_0")]
OntPoly$NAME_0 <- "Ontario"
NovaPoly <- NovaPoly[,c("GID_0","NAME_0")]
NovaPoly$NAME_0 <- "Nova Scotia"
HeilPoly <- HeilPoly[,c("GID_0","NAME_0")]
HeilPoly$NAME_0 <- "Heilongjiang"
MinnPoly <- MinnPoly[,c("GID_0","NAME_0")]
MinnPoly$NAME_0 <- "Minnesota"

allPoly <- list(USApoly, OntPoly, NovaPoly, HeilPoly, SwedenPoly,MinnPoly)
combinedPoly <- do.call(rbind, allPoly)

## Extract all values within each polygon
polyRCP26 <-  raster::extract(diff26, combinedPoly)
polyRCP60 <-  raster::extract(diff60, combinedPoly)
polyRCP85 <-  raster::extract(diff85, combinedPoly)

## Average across each polygon
meanNA <- function(x) { mean(x, na.rm=T)}
avgPoly26 <- unlist(lapply(polyRCP26, meanNA ))
avgPoly60 <- unlist(lapply(polyRCP60, meanNA ))
avgPoly85 <- unlist(lapply(polyRCP85, meanNA ))

regionPatterns <- data.frame(Region=c("USA","Ontario","Nova Scotia","Heilongjiang", "Sweden","Minnesota"),
           RCP26 = avgPoly26,
           RCP60 = avgPoly60,
           RCP85 = avgPoly85)




#### Plot the patterns of economic loss

economicData <- read.csv("data//economicLoss.csv")

economicData$Category2 <- factor(economicData$Category2, levels=c("Manufacturing","Sports & Entertainment","Ice Fishing"))

ggplot(economicData, aes(x=Category2, y= Economic.Value..in.USD./1000000)) + geom_bar(stat="identity") + 
  theme_classic() + xlab("") + ylab("Economic Value in Millions of Dollars (USD)")

economicData %>% group_by(Category2) %>% summarize(Millions=sum(Economic.Value..in.USD.)/1000000) %>% data.frame()