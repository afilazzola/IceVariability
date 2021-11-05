### Determine the economic loss for specific lakes with data

## Load packages and functions
library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)


## load data

economic <- read.csv("data//economicData.csv", stringsAsFactors = F)

## make a filelist of website screenshots
economic <- read.csv("figs//EconomicTable.csv", stringsAsFactors = F)
economic <- economic %>% rownames_to_column(var="ID") %>% filter(Economic.Value..in.USD. != "")
lapply(9:nrow(economic), function(i) {
  tryCatch({
webshot::webshot(economic[i,"Source"], paste0("out//WebsiteSourceID",i,".png"))
}, error = function(e) e) })

### Create a raster to estimate the difference for each RCP

allModels <- read.csv("data//rasterFilepaths.csv", stringsAsFactors=F) ## load in all raster filepaths
allModels <- allModels %>% 
              filter(!(lakemodel == "lake" & GCM == "gfdl-esm2m" & RCP == "rcp85")) %>%  ## drop anomalous model
              filter(filepath != "data//isimip_ice/clm45_hadgem2-es_historical_rcp85_icestart_1901_2099.nc") ## drop uneven model

modelsOn <- grep("icestart", allModels$filepath, value=T)
modelsOff <- grep("iceend", allModels$filepath, value=T)

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
  
  stackDuration <- stack(currentDuration, futureDuration)
  perChange <- overlay(stackDuration, fun =   
                    function(x,y) { 
                      ifelse( !is.na(x) & is.na(y) == 1, -1, ((y/x) -1))
                    } )

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
diff26[diff26>0] <- NA
diff60 <- raster("data//durationChangeRCP//DurationDiffrcp60.tif")
diff60[diff60>0] <- NA
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





################ Repeat extraction of change in duration

## Pull out ice duration for each pair raster
meanNA <- function(x) { apply(x, 2, mean, na.rm=T)}

## Lake Characteristics
durationDiff <- function(model, GCM, RCP){
  
  ## Select models
  modelTemp <- paste0("(",model,")(?:.+)(",GCM,")(?:.+)(",RCP,")")
  
  
  ## Current duration
  iceOn <- stack(grep(modelTemp, modelsOn, value=T))
  iceOff <- stack(grep(modelTemp, modelsOff, value=T))
  duration <- iceOff-iceOn
  
  ## specific lake points
  lakeBuffer <-  raster::extract(duration, lakeGPS, buffer=100000)
  lakeSpecific <-  unlist(lapply(lakeBuffer, meanNA ))
  lakePatterns <- data.frame(Region=rep(lakeGPS$Regions.Lakes,each=199),
                             Year= rep(1901:2099,6),
                             duration = lakeSpecific)
  ## polygons for larger regions
  polyRCP <-  raster::extract(duration, combinedPoly)
  avgPoly <- unlist(lapply(polyRCP, meanNA ))
  regionPatterns <- data.frame(Region=rep(c("USA","Ontario","Nova Scotia","Heilongjiang", "Sweden","Minnesota"),each=199),
                               Year= rep(1901:2099,6),
                               duration = avgPoly)
  
  ## Create output dataframe
  durationPatterns <- rbind(lakePatterns, regionPatterns)
  durationPatterns[,"Model"] <- model
  durationPatterns[,"GCMs"] <- GCM
  durationPatterns[,"RCPs"] <- RCP

  return(durationPatterns)
}

durationDiff("albm","gfdl","rcp26")


#### Get seasonal and permanent ice duratino for each lake
YearlyDuration <- lapply(1:45, function(i) {
  durationDiff(allModels[i,"lakemodel"], allModels[i, "GCM"], allModels[i,"RCP"])
})

AllDurations <- do.call(rbind, YearlyDuration)

# write.csv(AllDurations, "data//durationChangeAllModels.csv", row.names=FALSE)
AllDurations <- read.csv("data//durationChangeAllModels.csv")
AllDurations <- AllDurations %>% filter(Region != "Heilongjiang") ## link no longer available

meanDurations <- AllDurations %>% group_by(Region, Year, RCPs) %>% summarize(meanDur = mean(duration, na.rm=T))

## Calculate historic ice duration for each site
historicDuration <- meanDurations %>% group_by(Region, RCPs) %>% 
                    filter(Year %in% 1970:1999) %>% 
                    summarize(historicDur = mean(meanDur))

## Determine the future change based on historic values
futureDuration <- left_join(meanDurations, historicDuration) %>% 
                    filter(Year >1970) %>% 
                    mutate(changeDuration =  ifelse(meanDur==0 & historicDur>0, -1, (meanDur/historicDur)-1 )) %>% 
                    mutate(Region = as.character(Region))

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

## Select only columns with economics and region
econs <- economicReduced[,c("Region","economicValue")] %>% filter(Region != "Heilonngjiang Province") ## link no longer available
econsDuration <- merge(econs, futureDuration, by="Region") %>% mutate(economicLoss = economicValue*changeDuration)


### Plot pattern over time
yearlyEco <- econsDuration %>% group_by(Year, RCPs) %>% summarize(historicEconomicValue = sum(economicValue)/1000000,
                                                                  meanDurationChange = mean(changeDuration),
                                                                  yearlyEconomicLoss = sum(economicLoss)/1000000)
rollingEco <- yearlyEco %>% group_by(RCPs) %>% mutate(rollingMean=zoo::rollapply(yearlyEconomicLoss,31,mean,align='right',fill=NA))

## Apply future value to estimates
bondValues <- data.frame(Year = 1971:2099, ## Determine bond values over the last 30 years and into the future
                          rate= c(rep(0,50),
                                  c(0.066, 0.15, 0.415, 0.676, 0.676, 1.102, 1.102, 1.376, 1.376, 1.376, 1.523), ## 2022-2031
                                  rep(1.523, 9), rep(1.964, 10), rep(1.959, 10), # 2031-2061
                                  rep(1.959,39)))
yearlyEco <- yearlyEco %>% left_join(bondValues) %>% 
              mutate(futureT = ifelse(Year <2021, 0, Year-2021)) %>% 
              mutate(FV = yearlyEconomicLoss*(1+(rate/100))^futureT)

rollingEco <- rollingEco %>% left_join(bondValues) %>% 
  mutate(futureT = ifelse(Year <2021, 0, Year-2021)) %>% 
  mutate(rollingMeanFV = rollingMean*(1+(rate/100))^futureT)

ggplot(yearlyEco %>% filter(Year >2004), aes(x=Year, y=FV), color=RCPs) + geom_point(alpha=0.3, size=2) + 
  theme_classic() + ylab("Annual Economic Loss (billions $USD)") + geom_hline(yintercept=0, lty=2) +
  scale_colour_manual(values=c("#F0E442","#E69F00",  "#D55E00")) +
  geom_line(data=rollingEco%>% filter(Year >2004), aes(x=Year, y=rollingMeanFV , color=RCPs), size=1.5) +
  annotate(geom="text", x=2070, y= 50, label="$2,036 million USD annual economic revenue") 
#### *** Add COP findings for contextualization of results ***
## https://www.nature.com/articles/nclimate2465
    
## Century total loss
yearlyEco %>% filter(Year >2004) %>% ungroup() %>% group_by(RCPs) %>% summarize(totalLostRevenue=sum(yearlyEconomicLoss)/1000)
yearlyEco %>% filter(Year >2004) %>% ungroup() %>% group_by(RCPs) %>% summarize(totalLostRevenue=mean(yearlyEconomicLoss)/1000)



## Mean costs
meanEco <- econsDuration %>% group_by(Region, RCPs) %>% summarize(meanEconomicValue = mean(economicValue)/1000000,
                                                meanChange = mean(changeDuration),
                                                meanEconomicLoss = mean(economicLoss)/1000000) %>% 
          ungroup() %>% group_by(RCPs) %>% summarize(GlobalEconomicValue = sum(meanEconomicValue),
                                                     meanGlobalChange = mean(meanChange),
                                                     GlobalEconomicLoss = sum(meanEconomicLoss))

## End of century costs
totalEco <- econsDuration %>% group_by(RCPs) %>% summarize(BillionsOfRevenue = sum(economicValue)/1000000000,
                                                           BillionsOfLoss = sum(economicLoss)/1000000000,
                                              percentLossRevenue=BillionsOfLoss/BillonsOfRevenue)

write.csv(merge(meanEco, totalEco), "data//CenturyEconomicCosts.csv", row.names=FALSE)

meanEco <- econsDuration %>% group_by(Region,RCPs) %>% summarize(meanEconomicValue = mean(economicValue)/1000000,
                                                          meanChange = mean(changeDuration),
                                                          meanEconomicLoss = mean(economicLoss)/1000000)

## End of century costs
totalEco <- econsDuration %>% group_by(Region,RCPs) %>% summarize(BillonsOfRevenue = sum(economicValue)/1000000000,
                                                           BillonsOfLoss = sum(economicLoss)/1000000000,
                                                           percentLossRevenue=BillonsOfLoss/BillonsOfRevenue)
write.csv(merge(meanEco, totalEco), "data//CenturyEconomicCostsRegion.csv", row.names=FALSE)
