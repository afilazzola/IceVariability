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

## functions
meanNA <- function(x) { apply(x, 2, mean, na.rm=T)}


## model df
allModels <- read.csv("data//rasterFilepaths.csv", stringsAsFactors=F) ## load in all raster filepaths
allModels <- allModels %>% 
  filter(!(lakemodel == "lake" & GCM == "gfdl-esm2m" & RCP == "rcp85")) %>%  ## drop anomalous model
  filter(filepath != "data//isimip_ice/clm45_hadgem2-es_historical_rcp85_icestart_1901_2099.nc") ## drop uneven model
modelsOn <- grep("icestart", allModels$filepath, value=T)
modelsOff <- grep("iceend", allModels$filepath, value=T)

#### Get mean and error change in ice duration
cl = makeCluster(6)
clusterExport(cl=cl, list("durationDiff","allModels","meanNA","se","modelsOn","modelsOff"),
              envir=environment())
registerDoParallel(cl)
YearlyDuration <-foreach(i = 1:45, .packages=c("raster","dismo"), .combine = rbind) %dopar% {
  durationDiff(allModels[i,"lakemodel"], allModels[i, "GCM"], allModels[i,"RCP"], meanNA)
}

# write.csv(YearlyDuration, "data//YearlyDurationAllmodelsSelectSites.csv", row.names = F)

YearlyDuration <- read.csv("data//YearlyDurationAllmodelsSelectSites.csv")

### Get baseline to compare change
baselineData <- YearlyDuration %>% 
  filter(Year %in% 1970:1999) %>% 
  group_by(Region, Model, GCMs, RCPs) %>% 
  summarize(baselineDuration = mean(duration))
YearlyDurationChange <- YearlyDuration %>% 
  filter(Year > 1988) %>% 
  left_join(baselineData) %>% 
  mutate(durationChange = duration/ baselineDuration)


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

### Load yield and change data 
discount <- read.csv("data//Economic//DiscountFactor.csv")
yields <- read.csv("data//Economic//Yields.csv")

## Dataframes for yields and discounts before prediction
historicYield <- expand.grid(Seed = paste0("Seed", 1:100), Year = rep(2005:2021), yield = 1)
historicDiscount <- expand.grid(Seed = paste0("Seed", 1:100), Year = rep(2005:2021), discount = 1)

### Plot yields
names(yields) <- paste0("Seed", 1:100)
yields[,"Year"] <- 2022:2098
yieldsLong <- yields %>%  
  gather(Seed, yield, Seed1:Seed100) %>% 
  rbind(historicYield)
yieldsAverage <- yieldsLong %>% 
  group_by(Year) %>% 
  summarize(avgYield = mean(yield), errorYield = se(yield))

yieldPlot <- ggplot(yieldsAverage, aes(x= Year, y= avgYield)) +
  geom_line() +  theme_classic() +
  geom_ribbon(aes(ymin = avgYield - errorYield, ymax = avgYield + errorYield), alpha= 0.3) +
  theme(text = element_text(size = 20)) +
  ylab("Annual yield")  + xlab("")
yieldPlot

names(discount) <- paste0("Seed", 1:100)
discount[,"Year"] <- 2022:2098
discountLong <- discount %>%  
  gather(Seed, discount, Seed1:Seed100) %>% 
  rbind(historicDiscount)
discountAverage <- discountLong %>% 
  group_by(Year) %>% 
  summarize(avgDiscount = mean(discount), errorDiscount= se(discount))


discountPlot <- ggplot(discountAverage, aes(x= Year, y= avgDiscount)) +
  geom_line() +  theme_classic() +
  geom_ribbon(aes(ymin = avgDiscount - errorDiscount, ymax = avgDiscount + errorDiscount), alpha= 0.3) +
  theme(text = element_text(size = 20)) +
  ylab("Discount rate")  + xlab("")
discountPlot

gridExtra::grid.arrange(yieldPlot, discountPlot, ncol = 2)

## join economic data with lake ice
econs <- economicReduced[,c("Region","economicValue")] %>% filter(Region != "Heilonngjiang Province") ## link no longer available
econsDuration <- YearlyDurationChange %>% 
  left_join(econs) %>% 
  mutate(economicChange = economicValue * durationChange)
baselineValue <- sum(econs$economicValue)

econsRollingChange <- econsDuration  %>% 
  group_by(GCMs, Model, RCPs, Year) %>% 
  summarize(totalValueChange = sum(economicChange, na.rm = T)- baselineValue) %>%  
  mutate(totalValueChangeMillions = totalValueChange / 1000000) %>% 
  mutate(rollingEconomics = zoo::rollapply(totalValueChangeMillions, 31, mean, align='center', fill=NA)) 

### Calculate summary for plot
econsChangeError <- econsRollingChange %>% 
  group_by(Year, RCPs) %>% 
  summarize(avgTotalChange = mean(totalValueChangeMillions, na.rm =T),
            errorTotalChange = se(totalValueChangeMillions),
            avgRollingChange = mean(rollingEconomics, na.rm =T),
            errorRollingChange = se(rollingEconomics))


ggplot(econsChangeError, aes(x = Year, y= avgRollingChange, fill = RCPs, color = RCPs)) + 
  geom_line() +
  geom_ribbon(aes(ymin = avgRollingChange - errorRollingChange, ymax = avgRollingChange + errorRollingChange), alpha = 0.3, color = NA) +
  xlim(2020, 2085) +
  geom_point(aes(x = Year, y= avgTotalChange, fill = RCPs, color = RCPs), alpha=0.7) +
  theme_classic() + 
  scale_fill_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  scale_color_manual(values=c("#F0E442","#E69F00",  "#D55E00")) + 
  theme(text = element_text(size = 20), legend.position = c(0.2, 0.2)) +
  ylab("Value lost of lake ice (millions $USD)")  + xlab("") +
  geom_hline(yintercept = 0, lty = 2, size = 1.5, color = "Grey50") +
  annotate(geom = "text", x = 2070, y = 20, label = paste0("Annual value ", round(baselineValue/1000000,1), " million $USD"), size = 6)
durationPlot
