
##### Look at difference in iceon-iceoff based on two baselines


## Load libraries
library(tidyverse)
library(raster)
library(doParallel)
library(foreach)


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
  
  
  ## load all models
  iceOn <- stack(grep(modelTemp, modelsOn$filepath, value=T))
  iceOff <- stack(grep(modelTemp, modelsOff$filepath, value=T))

  
  ### Calculate  averages across models
  iceOnMean <- cellStats(iceOn, mean)
  iceOffMean <- cellStats(iceOff, mean)

    ## Determine difference for permanent 
  durationPatterns <- data.frame(Model = model, GCMs = GCM, RCPs=RCP,
                                 lakeType = rep(c("Iceon","Iceoff"), each=199), 
                                 Year = c(1901:2099,1901:2099),Value = c(iceOnMean,iceOffMean))
  
  return(durationPatterns)
}

durationDiff("albm","gfdl","rcp26")


## specify cluster
CL <- makeCluster(5, type="PSOCK")
clusterExport(cl=CL, list("durationDiff", "modelsOff", "modelsOn","lakearea"),
              envir=environment())
clusterEvalQ(CL, library(raster))
registerDoParallel(cores = CL)

#### Get seasonal and permanent ice duratino for each lake
YearlyDuration <- parLapply(cl=CL, 1:46, function(i) {
  durationDiff(modelsOff[i,"lakemodel"], modelsOff[i, "GCM"], modelsOff[i,"RCP"])
})

AllDurations <- do.call(rbind, YearlyDuration)

currentConditions <- AllDurations %>% filter(!(GCMs == "gfdl-esm2m" & Model=="lake")) %>% ## drop anomalous model
  filter(Year < 2006) %>% group_by(lakeType, Year) %>% summarize(yearlyValue=mean(Value)) %>%  data.frame()
futureConditions <- AllDurations %>% filter(!(GCMs == "gfdl-esm2m" & Model=="lake")) %>% ## drop anomalous model
  filter(Year > 2006-32) %>% group_by(lakeType, Year, RCPs) %>% summarize(yearlyValue=mean(Value)) %>%  data.frame()


### Historic baseline - 1901:1930

summaryValues <- currentConditions %>% filter(Year %in% 1901:1930) %>% group_by(lakeType) %>% 
  summarize(avgVal = mean(yearlyValue)) %>% data.frame()
avgIceoff <- summaryValues[summaryValues$lakeType=="Iceoff","avgVal"]
avgIceon <- summaryValues[summaryValues$lakeType=="Iceon","avgVal"]



### Calculate running average for dataset
currentConditions <- currentConditions %>% group_by(lakeType) %>% mutate(rollingMean=zoo::rollapply(yearlyValue,31,mean,align='right',fill=NA))
futureConditions <- futureConditions %>% group_by(lakeType, RCPs) %>% mutate(rollingMean=zoo::rollapply(yearlyValue,31,mean,align='right',fill=NA))


## Change in permanent ice cover
plot1 <- ggplot() + geom_point(data=currentConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=yearlyValue-avgIceoff), alpha=0.3) +
  geom_point(data=futureConditions %>% filter(lakeType=="Iceoff" & Year>2006), aes(x=Year, y=yearlyValue-avgIceoff, color=RCPs), alpha=0.3) + scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  theme_classic() + ylab("Days different in ice off") + geom_hline(yintercept=0, lty=2) + 
  annotate(geom="text", x=2070, y= 2, label=paste0("Average ice off date ",round(avgIceoff,0), " DOY")) +
  geom_line(data=futureConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=rollingMean-avgIceoff, color=RCPs), size=1.5) +
  geom_line(data=currentConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=rollingMean-avgIceoff), size=1.5)  +
  xlim(1930,2100)


plot2 <- ggplot() + geom_point(data=currentConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=yearlyValue-avgIceon), alpha=0.3) +
  geom_point(data=futureConditions %>% filter(lakeType=="Iceon" & Year>2006), aes(x=Year, y=yearlyValue-avgIceon, color=RCPs), alpha=0.3) + scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  theme_classic() + ylab("Days different in ice on") + geom_hline(yintercept=0, lty=2) + 
  annotate(geom="text", x=2070, y= 2, label=paste0("Average ice on date ",round(avgIceon,0), " DOY")) +
  geom_line(data=futureConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=rollingMean-avgIceon, color=RCPs), size=1.5) +
  geom_line(data=currentConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=rollingMean-avgIceon), size=1.5)  +
  xlim(1930,2100)




### Contemporary baseline - 1991-2020

summaryValues <- currentConditions %>% filter(Year %in% 1991:2020) %>% group_by(lakeType) %>% 
  summarize(avgVal = mean(yearlyValue)) %>% data.frame()
avgIceoff <- summaryValues[summaryValues$lakeType=="Iceoff","avgVal"]
avgIceon <- summaryValues[summaryValues$lakeType=="Iceon","avgVal"]



### Calculate running average for dataset
currentConditions <- currentConditions %>% group_by(lakeType) %>% mutate(rollingMean=zoo::rollapply(yearlyValue,31,mean,align='right',fill=NA))
futureConditions <- futureConditions %>% group_by(lakeType, RCPs) %>% mutate(rollingMean=zoo::rollapply(yearlyValue,31,mean,align='right',fill=NA))


## Change in permanent ice cover
plot3 <- ggplot() + geom_point(data=currentConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=yearlyValue-avgIceoff), alpha=0.3) +
  geom_point(data=futureConditions %>% filter(lakeType=="Iceoff" & Year>2006), aes(x=Year, y=yearlyValue-avgIceoff, color=RCPs), alpha=0.3) + scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  theme_classic() + ylab("Days different in ice off") + geom_hline(yintercept=0, lty=2) + 
  annotate(geom="text", x=2070, y= 2, label=paste0("Average ice off date ",round(avgIceoff,0), " DOY")) +
  geom_line(data=futureConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=rollingMean-avgIceoff, color=RCPs), size=1.5) +
  geom_line(data=currentConditions %>% filter(lakeType=="Iceoff"), aes(x=Year, y=rollingMean-avgIceoff), size=1.5)  +
  xlim(1930,2100)


plot4 <- ggplot() + geom_point(data=currentConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=yearlyValue-avgIceon), alpha=0.3) +
  geom_point(data=futureConditions %>% filter(lakeType=="Iceon" & Year>2006), aes(x=Year, y=yearlyValue-avgIceon, color=RCPs), alpha=0.3) + scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  theme_classic() + ylab("Days different in ice on") + geom_hline(yintercept=0, lty=2) + 
  annotate(geom="text", x=2070, y= 2, label=paste0("Average ice on date ",round(avgIceon,0), " DOY")) +
  geom_line(data=futureConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=rollingMean-avgIceon, color=RCPs), size=1.5) +
  geom_line(data=currentConditions %>% filter(lakeType=="Iceon"), aes(x=Year, y=rollingMean-avgIceon), size=1.5)  +
  xlim(1930,2100)

gridExtra::grid.arrange(plot1, plot2,plot3,plot4)
