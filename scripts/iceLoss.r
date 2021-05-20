#### Ice duration

## Load packages and functions
library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)

source("functions.r")

## Lake Characteristics
lakearea <- raster("data//isimip_lake_info2.nc", varname="tot_area")
lakearea <- crop(lakearea, extent(-180, 180, 30, 90))

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


## Pull out ice cover for each pair raster


durationDiff <- function(model, GCM, RCP){
  
  ## Select models
  modelTemp <- paste0("(",model,")(?:.+)(",GCM,")(?:.+)(",RCP,")")
  
  ## calculate duration across timeseries
  iceOn <- stack(grep(modelTemp, modelsOn$filepath, value=T))
  iceOff <- stack(grep(modelTemp, modelsOff$filepath, value=T))
  durationAll <- iceOff-iceOn
  
  ## reclassify Ice present/absence
  icePresence <- reclassify(durationAll, rcl= data.frame(from = c(-999,0), to=c(0,999), becomes=c(0,1)))
  iceArea <- lakearea * icePresence
 
  ## Get yearly means
  dfIce <- data.frame(Model= model, GCMs = GCM, RCPs = RCP, Year = 1:199, GlobalIceArea = cellStats(iceArea, sum))

  return(dfIce)
}

test <- durationDiff("albm","gfdl","rcp26")



dfIceAll <- lapply(1:46, function(i) {
  durationDiff(modelsOff[i,"lakemodel"], modelsOff[i, "GCM"], modelsOff[i,"RCP"])
  
})

allCombined <- do.call(rbind, dfIceAll)

write.csv(allCombined, "out//iceLossPatterns.csv", row.names=FALSE)

iceLossPatterns <- read.csv("out//iceLossPatterns.csv")


meanIceLoss <- iceLossPatterns %>% mutate(Year = Year+1900) %>%  filter(!(Model == "lake" & GCMs == "gfdl-esm2m" & RCPs == "rcp85")) %>% 
  group_by(RCPs, Year) %>% summarize(iceArea = mean(GlobalIceArea)) %>% data.frame()


## averaging from the norm
iceAreaAverage <- mean(meanIceLoss[meanIceLoss$Year %in% 1970:1999, "iceArea"] )

currentIceloss <- meanIceLoss %>% filter(Year <2007) %>% group_by(Year) %>% 
  summarize(iceAreamean = mean(iceArea)) %>% mutate(perChange = 1-(iceAreaAverage-iceAreamean) ) %>% 
  data.frame()
futureIceloss <- meanIceLoss %>% filter(Year >2006) %>%  mutate(perChange = 1-(iceAreaAverage-iceArea) ) 

ggplot() + geom_line(data=currentIceloss, aes(x=Year, y=perChange), size=1.2) +
  geom_line(data=futureIceloss, aes(x=Year, y=perChange, color=RCPs), size=1.2) +
  theme_Publication() +  scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  ylab("Change in lake ice area (km2)") + 
  geom_hline(yintercept=1, lty=2) +
  annotate(geom="text", x=2050, y= 10000, label="1.5 Million km2 of Ice (1970-1999)")






