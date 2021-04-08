#### Ice duration

## Load packages and functions
library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)

source("functions.r")

## List files
allfiles <- list.files("data//isimip_ice", full.names = T)
iceOnFiles <- allfiles[grep("icestart", allfiles)]
iceEndFiles <- allfiles[grep("iceend", allfiles)]

## get model names
allmodels <- iceOnFiles %>% gsub(".*_ice/", "", .) %>% gsub("_historical.*", "", .)
GCMmodels <- gsub(".*_", "", allmodels)
lakemodels <- gsub("_.*", "", allmodels)
RCPmodels <- iceOnFiles %>% gsub(".*historical_", "", .) %>% gsub("_icestart.*", "", .)

## Convert models to text
modelsOn <- data.frame(lakemodel = lakemodels, GCM = GCMmodels, RCP = RCPmodels, filepath=iceOnFiles, stringsAsFactors = F)

## get model names
allmodels <- iceEndFiles %>% gsub(".*_ice/", "", .) %>% gsub("_historical.*", "", .)
GCMmodels <- gsub(".*_", "", allmodels)
lakemodels <- gsub("_.*", "", allmodels)
RCPmodels <- iceEndFiles %>% gsub(".*historical_", "", .) %>% gsub("_iceend.*", "", .) 

## Convert models to text
modelsOff <- data.frame(lakemodel = lakemodels, GCM = GCMmodels, RCP = RCPmodels, filepath=iceEndFiles, stringsAsFactors = F)


## Pull out ice duration for each pair raster


durationDiff <- function(model, GCM, RCP){

## Select models
modelTemp <- paste0("(",model,")(?:.+)(",GCM,")(?:.+)(",RCP,")")

## Current duration
iceOn <- stack(grep(modelTemp, modelsOn$filepath, value=T), bands=70:99)
iceOff <- stack(grep(modelTemp, modelsOff$filepath, value=T), bands=70:99)
durationCurrent <- mean(iceOff-iceOn)

## future duration
iceOn <- stack(grep(modelTemp, modelsOn$filepath, value=T), bands=170:199)
iceOff <- stack(grep(modelTemp, modelsOff$filepath, value=T), bands=170:199)
durationFuture <- mean(iceOff-iceOn)

## Determine difference
diffFuture <- (durationFuture-durationCurrent)
return(diffFuture)
}

durationDiff("albm","gfdl","rcp26")

lapply(1:46, function(i) {
  rasterTemp <- durationDiff(modelsOff[i,"lakemodel"], modelsOff[i, "GCM"], modelsOff[i,"RCP"])
  writeRaster(rasterTemp, paste0("data//iceDiffDuration//",modelsOff[i,"lakemodel"],"_",modelsOff[i, "GCM"],"_",modelsOff[i,"RCP"],".tif"))
})




### Duration of ice loss over time

allTimeseries <- lapply(1:46, function(i) {
  
model <- modelsOff[i,"lakemodel"]
GCM <-modelsOff[i, "GCM"]
RCP <- modelsOff[i,"RCP"]
  
modelTemp <- paste0("(",model,")(?:.+)(",GCM,")(?:.+)(",RCP,")")


## Current duration
iceOn <- stack(grep(modelTemp, modelsOn$filepath, value=T))
iceOff <- stack(grep(modelTemp, modelsOff$filepath, value=T))
durationAll <- iceOff-iceOn



yearOut <- lapply(1:199, function(j){
   mean(values(durationAll[[j]]),na.rm=T)
})
durationTimeseries <- data.frame(model= model, GCM=  GCM, RCP=RCP, Year= 1900+1:199, duration= unlist(yearOut))
durationTimeseries
})

condensedSeries <- do.call(rbind, allTimeseries)

write.csv(condensedSeries, "data//durationTimeseries.csv", row.names=FALSE)


### Plot change in time series

timeseriesChange <- read.csv("data//durationTimeseries.csv")


avgTimeseries <- timeseriesChange %>% filter(!(GCM == "gfdl-esm2m" & model=="lake")) %>% ## drop anomalous model
    group_by(Year, RCP) %>% summarize(meanDuration = mean(duration))

currentTimeseries <- avgTimeseries %>% filter(Year <2007) %>% group_by(Year) %>% summarize(meanDuration = mean(meanDuration))



ggplot() + theme_classic() + xlim(1901,2099) + ylim(190,250) + 
  geom_line(data = currentTimeseries, aes(x=Year, y= meanDuration), size=1.3) + 
  geom_line(data = avgTimeseries %>% filter(Year >2005), aes(x=Year, y= meanDuration, color=RCP ), size=1.3) +
  ylab("Mean Lake Ice duration (days)") + 
  scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00"))