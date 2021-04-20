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

currentTimeseries <- avgTimeseries %>% filter(Year <2007) %>% group_by(Year) %>% summarize(meanDuration = mean(meanDuration)) %>% data.frame()

## Calculate mean to determine anomaly
meanSeries <- mean(currentTimeseries[currentTimeseries$Year %in% c(1970:1999), "meanDuration"])

ggplot() + theme_classic() + xlim(1901,2099) + 
  geom_line(data = currentTimeseries, aes(x=Year, y= meanDuration-meanSeries), size=1.3) + 
  geom_line(data = avgTimeseries %>% filter(Year >2005), aes(x=Year, y= meanDuration-meanSeries, color=RCP ), size=1.3) +
  ylab("Mean Lake Ice duration (days)") + 
  scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  geom_hline(yintercept=0, lty=2)


modelsOndropGFDL <- modelsOn[grep("(lake)(?:.+)(gfdl)(?:.+)(rcp85)", modelsOn$filepath, invert=T),]

modelsOffdropGFDL <- modelsOff[grep("(lake)(?:.+)(gfdl)(?:.+)(rcp85)", modelsOff$filepath, invert=T),]

## load current duration to take difference
currentDiff <- raster( "out//CurrentDurationAll.tif")

## specify cluster
CL <- makeCluster(3, type="PSOCK")
clusterExport(cl=CL, list("currentDiff", "modelsOndropGFDL", "modelsOffdropGFDL", "modelsOn"),
              envir=environment())
registerDoParallel(cores = CL)

foreach(i = 1:3,  cl=CL, .packages=c("raster","ncdf4")) %dopar%  {
  

  RCPs <- unique(modelsOn$RCP)
  

  ## future duration
  # iceOn <- stack(modelsOn$filepath, bands=70:99) ## current ice on 
  # iceOff <- stack(modelsOff$filepath, bands=70:99) ## current ice off
  iceOn <- stack(grep(RCPs[i], modelsOndropGFDL$filepath, value=T), bands=170:199)
  iceOff <- stack(grep(RCPs[i], modelsOffdropGFDL$filepath, value=T), bands=170:199)
  durationAll <- iceOff-iceOn
  meanDuration <- mean(durationAll)
  
  ## Diff raster
  futureDiffDuration <- currentDiff - meanDuration
  
  writeRaster(futureDiffDuration, paste0("out//Future",RCPs[i],".tif") )


}

## function to make map
WorldMap <- function(x, plotlabel){

## make plot
## Convert the dataframe to plot properly
modelDiffDF <- as(x, "SpatialPixelsDataFrame")
modelDiffDF <- as.data.frame(modelDiffDF)
colnames(modelDiffDF) <- c("value", "x", "y")


# ## Current
# world <- map_data("world")
# ggplot() +  geom_map(data=world, map = world, aes(long, lat, map_id = region), fill = "darkgray") +theme_Publication() +
#   geom_tile(data= modelDiffDF , aes(x=x, y=y, fill=value)) +
#   scale_fill_gradient2(  low = "#00008B", mid = "#87CEFA", high="#FFFFFF", midpoint = 250) +
#   xlab("Longitude") + ylab("Latitude")+
#   theme(legend.position = c(0.1, 0.3),legend.direction = "vertical", legend.title = element_blank(),legend.key.height = unit(0.5,"cm")) +
#   xlim(-180,180) + ylim(-90,90) + annotate(geom="text", -170, 90, label = plotlabel, size=6)
# 
## Future difference
world <- map_data("world")
ggplot() +  geom_map(data=world, map = world, aes(long, lat, map_id = region), fill = "darkgray") +theme_Publication() +
  geom_tile(data= modelDiffDF , aes(x=x, y=y, fill=value)) +
  scale_fill_gradient2(  low = "yellow", mid = "orange", high="red", midpoint = 35) +
  xlab("Longitude") + ylab("Latitude")+
  theme(legend.position = c(0.1, 0.3),legend.direction = "vertical", legend.title = element_blank(),legend.key.height = unit(0.5,"cm")) +
  xlim(-180,180) + ylim(-90,90) + annotate(geom="text", -170, 90, label = plotlabel, size=6)
}


listRasters <- list.files("out//", full.names = T)

c0 <- raster(listRasters[1])
f1 <- raster(listRasters[2])
f2 <- raster(listRasters[3])
f3 <- raster(listRasters[4])

# 
# p1 <- WorldMap(c0, "Current")
p2 <- WorldMap(f1, "RCP 2.6")
p3 <- WorldMap(f2, "RCP 6.0")
p4 <- WorldMap(f3, "RCP 8.5")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

##  Extract the total area from the raster
test <- nc_open("data//isimip_lake_info2.nc")
lakearea = ncvar_get(test, "tot_area")


str(lakearea)