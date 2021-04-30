library(tidyverse)

timeseriesChange <- read.csv("data//durationTimeseries.csv")



currentDuration <-  timeseriesChange %>% filter(Year %in% 1971:1999) %>% group_by(model) %>% summarize(currentDur= mean(duration))
futureDuration <-  timeseriesChange %>% filter(Year %in% 2071:2099) %>% group_by(model, RCP, GCM) %>% summarize(futureDur = mean(duration))

allDuration <- merge(currentDuration, futureDuration)
allDuration[,"changeDuration"] <- allDuration$futureDur - allDuration$currentDur


### Get differences in climate
allClimate <- list.files("data//climateNCDF//", full.names = T)

## get model names
allmodels <- list.files("data//climateNCDF//") %>% gsub("_annual.nc4", "", .) %>% gsub("tas-", "", .)
GCMmodels <- gsub(".*_", "", allmodels)
lakemodels <- gsub("_.*", "", allmodels)
RCPmodels <- iceOnFiles %>% gsub(".*historical_", "", .) %>% gsub("_icestart.*", "", .)

## Convert models to text
models <- data.frame(lakemodel = lakemodels, GCM = GCMmodels, RCP = RCPmodels, filepath=iceOnFiles, stringsAsFactors = F)

