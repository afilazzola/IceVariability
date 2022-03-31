#### Ice duration

## Load packages and functions
library(terra)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)

source("functions.r")
se <- function(x) { sd(x, na.rm = T)/ sqrt(length(x[!is.na(x)]))}

## Lake Characteristics
lakearea <- rast("data//isimip_lake_info2.nc")[[3]]
lakearea <- crop(lakearea, y = ext(-180, 180, 30, 90))


## List files
allfiles <- list.files("data//isimip_ice", full.names = T)
iceOnFiles <- allfiles[grep("icestart", allfiles)]
iceEndFiles <- allfiles[grep("iceend", allfiles)]

## function to find lake area
getIceArea <- function(rasterDuration){
  rasterDuration[!is.na(rasterDuration)] <- 1
  rasterArea <- rasterDuration * lakearea
}

## Function to pull DOY and area for ice on and ice off
getYearlyDuration <- function(rasterFilepath){
  rasterLoad <- rast(rasterFilepath)
  rasterArea <- getIceArea(rasterLoad)
  rasterValues <- sapply(1:199, function(i){
    meanRasterVal <- mean(values(rasterLoad[[i]]), na.rm = T)
  })
  rasterAreas <- sapply(1:199, function(i){
    meanRasterArea <- sum(values(rasterArea[[i]]), na.rm = T)
  })
  dfOut <- data.frame(rasterFile = rasterFilepath, 
                      year = 1901:2099,
                      DOY = rasterValues,
                      Area = rasterAreas)
  return(dfOut)
}

## Extract data for all rasters
IceOnList <- lapply(iceOnFiles, function(i){
  getYearlyDuration(i)
})
IceOnDF <- do.call(rbind, IceOnList)
IceOffList <- lapply(iceEndFiles, function(i){
  getYearlyDuration(i)
})
IceOffDF <- do.call(rbind, IceOffList)
allIceData <- rbind(IceOnDF, IceOffDF)


allmodels <- allIceData$rasterFile %>% gsub(".*_ice/", "", .) %>% gsub("_historical.*", "", .)
allIceData[,"icephenology"] <- ifelse(grepl("icestart", allIceData$rasterFile), "iceOn", "iceOff")
allIceData[,"GCMmodels"] <- gsub(".*_", "", allmodels)
allIceData[,"lakemodels"] <- gsub("_.*", "", allmodels)
allIceData[,"RCPmodels"] <- allIceData$rasterFile %>% gsub("_icestart.*|_iceend.*", "", .) %>% gsub(".*historical_", "", .)
# write.csv(allIceData, "data//AllIceModels.csv", row.names = F)
 


##### Load all data
allIceData <- read.csv("data//AllIceModels.csv")


###### Duration


## Get duration and historical baseline
durationData <- allIceData %>% 
  dplyr::select(-rasterFile, -Area) %>% 
  spread(icephenology, DOY) %>% 
  filter(!is.na(iceOff)) %>% 
  mutate(duration = 365 - iceOff + iceOn ) %>% 
  mutate(timeline = ifelse(year < 2006, "historical","future"))

## Add baseline data
baseline <- durationData %>% 
  filter(year %in% 1970:1999) %>% 
  group_by(GCMmodels, lakemodels, RCPmodels) %>% 
  summarize(baselineMean = mean(duration))
durationDataBaseline <- durationData %>% 
  left_join(baseline) %>% 
  mutate(iceChange = baselineMean - duration)
baselineDuration <- round(mean(unique(baseline$baselineMean)),0)

avgPoints <- durationDataBaseline %>% 
  group_by(year, RCPmodels, timeline) %>% 
  summarize(avgDurationChange = mean(iceChange), errorDuration = se(iceChange))

rollingDuration <- durationDataBaseline %>% 
  group_by(GCMmodels, lakemodels, RCPmodels) %>% 
  mutate(rollingDuration = zoo::rollapply(iceChange, 31, mean, align='center', fill=NA)) %>% 
  filter(!is.na(rollingDuration))

summarizedRollingDuration <- rollingDuration %>% 
  group_by(year, RCPmodels, timeline) %>% 
  summarize(avgDuration = mean(rollingDuration), errorDuration = se(rollingDuration))

durationPlot <- ggplot(summarizedRollingDuration %>%  filter(timeline == "future"), 
       aes(x = year, y= avgDuration, fill = RCPmodels, color = RCPmodels)) + 
  geom_line() +
  geom_ribbon(aes(ymin = avgDuration - errorDuration, ymax = avgDuration + errorDuration), alpha = 0.3, color = NA) +
  geom_line(data  = summarizedRollingDuration %>%  filter(timeline == "historical"),
              aes(x = year, y= avgDuration), fill = "Grey50", color = "black", size = 1.5) +
  xlim(1930, 2085) +
  geom_point(data = avgPoints %>%  filter(timeline == "future"), 
             aes(x = year, y= avgDurationChange, fill = RCPmodels, color = RCPmodels), alpha=0.7) +
  geom_point(data = avgPoints %>%  filter(timeline == "historical"), 
             aes(x = year, y= avgDurationChange), fill = "Grey50", color = "black", alpha=0.7) +
  theme_classic() + 
  scale_fill_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  scale_color_manual(values=c("#F0E442","#E69F00",  "#D55E00")) + 
  theme(text = element_text(size = 20), legend.position = c(0.2, 0.2)) +
  ylab("Change in ice duration (days)")  + xlab("") +
  geom_hline(yintercept = 0, lty = 2, size = 1.5, color = "Grey50") +
  annotate(geom = "text", x = 2050, y = 3, label = paste0("Ice duration ", baselineDuration, " days"), size = 6)
durationPlot


###### Ice area

## Get duration and historical baseline
areaData <- allIceData %>% 
  dplyr::select(-rasterFile, -DOY) %>% 
  spread(icephenology, Area) %>% 
  filter(!is.na(iceOff)) %>% 
  mutate(area = (iceOff + iceOn)/2) %>% 
  mutate(timeline = ifelse(year < 2006, "historical","future")) %>% 
  filter(area >100000) ## Anomalous values in one GCM-Lake combination


## Add baseline data
baseline <- areaData %>% 
  filter(year %in% 1970:1999) %>% 
  group_by(GCMmodels, lakemodels, RCPmodels) %>% 
  summarize(baselineMean = mean(area, na.rm =T))
areaDataBaseline <- areaData %>% 
  left_join(baseline) %>% 
  mutate(iceChange =  area - baselineMean)
baselineArea <- round(mean(unique(baseline$baselineMean)),0)

avgPoints <- areaDataBaseline %>% 
  group_by(year, RCPmodels, timeline) %>% 
  summarize(avgAreaChange = mean(iceChange), errorDuration = se(iceChange))

rollingArea <- areaDataBaseline %>% 
  group_by(GCMmodels, lakemodels, RCPmodels) %>% 
  mutate(rollingArea = zoo::rollapply(iceChange, 31, mean, align='center', fill=NA)) %>% 
  filter(!is.na(rollingArea))

summarizedRollingArea <- rollingArea %>% 
  group_by(year, RCPmodels, timeline) %>% 
  summarize(avgArea = mean(rollingArea), errorArea = se(rollingArea))

areaPlot <- ggplot(summarizedRollingArea %>%  filter(timeline == "future"), 
       aes(x = year, y= avgArea, fill = RCPmodels, color = RCPmodels)) + 
  geom_line() +
  geom_ribbon(aes(ymin = avgArea - errorArea, ymax = avgArea + errorArea), alpha = 0.3, color = NA) +
  geom_line(data  = summarizedRollingArea %>%  filter(timeline == "historical"),
            aes(x = year, y= avgArea), fill = "Grey50", color = "black", size = 1.5) +
  xlim(1930, 2085) +
  geom_point(data = avgPoints %>%  filter(timeline == "future"), 
             aes(x = year, y= avgAreaChange, fill = RCPmodels, color = RCPmodels), alpha=0.7) +
  geom_point(data = avgPoints %>%  filter(timeline == "historical"), 
             aes(x = year, y= avgAreaChange), fill = "Grey50", color = "black", alpha=0.7) +
  theme_classic() + 
  scale_fill_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  scale_color_manual(values=c("#F0E442","#E69F00",  "#D55E00")) + 
  theme(text = element_text(size = 20), legend.position = c(0.2, 0.2)) +
  ylab("Area of ice loss (km2)")  + xlab("") + 
  geom_hline(yintercept = 0, lty = 2, size = 1.5, color = "Grey50") +
  annotate(geom = "text", x = 2050, y = 10000, label = paste0("Ice area ", baselineArea, " km2"), size = 6)
areaPlot

gridExtra::grid.arrange(durationPlot, areaPlot, ncol = 2 )


######### Economic change
source("scripts/economicDataLocations.r") ## load functions to get ice data for economic regions

economic <- read.csv("data//economicData.csv", stringsAsFactors = F)

## Pull out ice duration for each pair raster
meanNA <- function(x) { apply(x, 2, mean, na.rm=T)}


#### Get mean and error change in ice duration
cl = makeCluster(6)
clusterExport(cl=cl, list("durationDiff","allModels","meanNA","se"),
              envir=environment())
registerDoParallel(cl)
YearlyDuration <-foreach(i = 1:45, .packages=c("raster","dismo"), .combine = rbind) %dopar% {
  durationDiff(allModels[i,"lakemodel"], allModels[i, "GCM"], allModels[i,"RCP"], meanNA)
}
YearlyDurationError <- lapply(1:45, function(i) {
  durationDiff(allModels[i,"lakemodel"], allModels[i, "GCM"], allModels[i,"RCP"], se)
})
AllDurations <- do.call(rbind, YearlyDuration)
AllDurationErrors <- do.call(rbind, YearlyDurationError)


## Clean up economic data to merge with change in duration
economicReduced <- economic[,c("Regions.Lakes","Economic.Value..in.USD.","Country")]
economicReduced$Economic.Value..in.USD. <- economicReduced$Economic.Value..in.USD. %>% gsub("\\$", "", .) %>% gsub(",", "", .)
economicReduced <- economicReduced %>% mutate(economicValue = as.numeric(Economic.Value..in.USD.),
                                              Regions.Lakes=as.character(Regions.Lakes),
                                              Country = as.character(Country))
economicReduced[economicReduced$Regions.Lakes=="COl lake, Alberta","economicValue"] <- 57194
economicReduced <- economicReduced[!is.na(economicReduced$economicValue),]
economicReduced[,"Region"] <- economicReduced$Regions.Lakes
economicReduced[c(1:3,5),"Region"] <- c("USA","Sweden","Sweden","Minnesota")





