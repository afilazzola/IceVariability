### Difference models
### Load packages

library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)

source("functions.r")


### Read in files
allfiles <- list.files("data//isimip_ice", full.names = T)

### Create models with difference 

iceOnFiles <- allfiles[grep("icestart", allfiles)]
iceEndFiles <- allfiles[grep("iceend", allfiles)]

## get model names
allmodels <- iceOnFiles %>% gsub(".*_ice/", "", .) %>% gsub("_historical.*", "", .)
GCMmodels <- gsub(".*_", "", allmodels)
lakemodels <- gsub("_.*", "", allmodels)
RCPmodels <- iceOnFiles %>% gsub(".*historical_", "", .) %>% gsub("_icestart.*", "", .)

## Convert models to text
models <- data.frame(lakemodel = lakemodels, GCM = GCMmodels, RCP = RCPmodels, filepath=iceOnFiles, stringsAsFactors = F)


### Run through all ice on models

iceStartLat <- foreach(j = 1:nrow(models), .combine=rbind, .packages=c("raster","ncdf4","dplyr","doParallel")) %do% {
  
  ## Select subset of models
  iterModel <- subset(models, filepath == models$filepath[j])
  
  ## Select time periods
  iterCurrent <- stack(iterModel[,"filepath"], bands=70:99)
  iterFuture <- stack(iterModel[,"filepath"], bands=170:199)
  
  ## Calculate means
  meanCurrent <- mean(iterCurrent)
  meanFuture <- mean(iterFuture)
  
  ## Determine difference
  diffFuture <- (meanFuture-meanCurrent)
  
  newName <- basename(iterModel[,"filepath"]) %>% gsub("_1901_2099", "", .)%>% gsub("historical_", "", .)
  writeRaster(diffFuture, paste0("data//diffRasters//diff_",newName))
}


## get model names
allmodels <- iceEndFiles %>% gsub(".*_ice/", "", .) %>% gsub("_historical.*", "", .)
GCMmodels <- gsub(".*_", "", allmodels)
lakemodels <- gsub("_.*", "", allmodels)
RCPmodels <- iceEndFiles %>% gsub(".*historical_", "", .) %>% gsub("_icestart.*", "", .)

## Convert models to text
models <- data.frame(lakemodel = lakemodels, GCM = GCMmodels, RCP = RCPmodels, filepath=iceEndFiles, stringsAsFactors = F)


### Run through all ice end models

iceStartLat <- foreach(j = 1:nrow(models), .combine=rbind, .packages=c("raster","ncdf4","dplyr","doParallel")) %do% {
  
  ## Select subset of models
  iterModel <- subset(models, filepath == models$filepath[j])
  
  ## Select time periods
  iterCurrent <- stack(iterModel[,"filepath"], bands=70:99)
  iterFuture <- stack(iterModel[,"filepath"], bands=170:199)
  
  ## Calculate means
  meanCurrent <- mean(iterCurrent)
  meanFuture <- mean(iterFuture)
  
  ## Determine difference
  diffFuture <- (meanFuture-meanCurrent)
  
  newName <- basename(iterModel[,"filepath"]) %>% gsub("_1901_2099", "", .)%>% gsub("historical_", "", .)
  writeRaster(diffFuture, paste0("data//diffRasters//diff_",newName))
}
