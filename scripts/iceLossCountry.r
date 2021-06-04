### Ice loss by country

## Load packages and functions
library(raster)
library(ncdf4)
library(tidyverse)
library(ggthemes)
library(doParallel)
library(foreach)

source("functions.r")

### Load country characteristics 
countryData <- read.csv("data//countryStats//SummarizedCountryMetrics.csv", stringsAsFactors = F)

## Load country boundaries
# boundaries <- getData("countries")
# boundaries <- boundaries[,c("ID_0","ISO","NAME_ENGLISH","NAME_ISO","NAME_FAO","NAME_LOCAL")]
# CP <- as(extent(-180, 180, 20,85), "SpatialPolygons")
# proj4string(CP) <- CRS(proj4string(boundaries))
# NHbound <- crop(boundaries, CP, byid=TRUE)
# NHsimple <- rgeos::gSimplify(NHbound, tol=1, topologyPreserve=T)
# NHsimple <- SpatialPolygonsDataFrame(NHsimple, NHbound@data)
# rgdal::writeOGR(NHsimple, "data", layer="NHboundaries.shp", driver="ESRI Shapefile")
NHboundary <- rgdal::readOGR("data//NHboundaries.shp")

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
  
  ## calculate current duration
  currentDuration <-  mean(iceArea[[70:99]])
  futureDuration <- mean(iceArea[[170:199]])
  timeframes <- stack(currentDuration,futureDuration)
  
  ## Extract ice area per country in every year
  dfIce <- lapply(1:2, function(i) {
  countryLakes <- raster::extract(timeframes[[i]], NHboundary)
  countryIce <-  lapply(countryLakes, sum, na.rm=T)
  countryIce <-  do.call(c, countryIce)
  
  dfOut <- data.frame(Model= model, GCMs = GCM, RCPs = RCP, Year=c("Current","Future")[i], country = as.character(NHboundary$NAME_EN), iceArea = countryIce)
  })
  ## Get yearly means
  dfIce <- do.call(rbind, dfIce)
  
  return(dfIce)
}

### Run all lake models in parallel
cl <- makeCluster(4, type="PSOCK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
clusterExport(cl, varlist=list("modelsOn","modelsOff","durationDiff"),
              envir = environment())
registerDoParallel(cl)

dfIceAll <- foreach(j = 1:46, .packages=c("raster","rgdal"), .errorhandling = "remove", .combine=rbind) %dopar%  {
  durationDiff(modelsOff[j,"lakemodel"], modelsOff[j, "GCM"], modelsOff[j,"RCP"])
}


### Summarize across models
iceBest <- dfIceAll  %>%  filter(!(Model == "lake" & GCMs == "gfdl-esm2m" & RCPs == "rcp85")) %>% 
  group_by(RCPs, Year, country) %>% summarize(ice = mean(iceArea)) %>%
  spread(Year, ice) %>% mutate(diffArea = Future - Current) %>% 
  mutate(originalCountry = as.character(country), country= as.character(country)) %>% data.frame()

## look for best match
for(i in 1:nrow(iceBest)){
checkMatch <- grep(iceBest[i,"originalCountry"], countryData$country, value=T, ignore.case=T)
iceBest[i,"country"] <-  ifelse(identical(checkMatch, character(0)), NA, checkMatch)
}

##  Join data and Drop zeros
iceCountry <- merge(iceBest, countryData)
iceCountry <- iceCountry %>% filter(Current > 0)

## Add country area for lake density
countryArea <- data.frame(originalCountry = NHboundary$NAME_EN, countryArea = area(NHboundary))
for(i in 1:nrow(countryArea)){
  checkMatch <- grep(countryArea[i,"originalCountry"], countryData$country, value=T, ignore.case=T)
  countryArea[i,"country"] <-  ifelse(identical(checkMatch, character(0)), NA, checkMatch)
}

## calculate lake area density
iceCountryArea <- merge(iceCountry,countryArea)
iceCountryArea <- iceCountryArea %>% mutate(CurrentDens=Current/countryArea, FutureDens=Future/countryArea, diffDens = FutureDens-CurrentDens)

### Examine relationship with human population
m1 <- lm(abs(diffArea) ~ log10(Population) * RCPs, data=iceCountryArea)
anova(m1)

### Plot
plot1 <- ggplot(iceCountryArea, aes(x=Population , y=abs(diffArea))) + 
  geom_point(size=3, pch=21, aes(fill=RCPs)) + theme_classic() + scale_fill_brewer(palette="YlOrRd") +
  scale_y_log10() + ylab("Area of ice lost (km2)") + xlab("Population of country (log)") +
  geom_smooth(method="lm", se=T, color="black") + scale_x_log10()+
  geom_label( label=iceCountryArea$Country.Code, aes(fill=RCPs))
plot1



### Examine relationship with CO2
m2 <- lm(abs(diffArea) ~ log10(CO2) * RCPs, data=iceCountryArea)
anova(m2)
summary(m2)

### Plot
plot2 <- ggplot(iceCountryArea, aes(x=CO2 , y=abs(diffArea))) + 
  geom_point(size=3, pch=21, aes(fill=RCPs)) + theme_classic() + scale_fill_brewer(palette="YlOrRd") +
  scale_y_log10() + ylab("Area of ice lost (km2)") + xlab("Carbon emissions (metric tons per capita)") +
  geom_smooth(method="lm", se=T, color="black") + scale_x_log10() + 
  geom_label( label=iceCountryArea$Country.Code, aes(fill=RCPs))
plot2


### Examine relationship with GDP
m3 <- lm(abs(diffArea) ~ log10(GDP) * RCPs, data=iceCountryArea)
anova(m3)
summary(m3)

### Plot
plot3 <- ggplot(iceCountryArea, aes(x=GDP , y=abs(diffArea))) + 
  geom_point(size=3, pch=21, aes(fill=RCPs)) + theme_classic() + scale_fill_brewer(palette="YlOrRd") +
  scale_y_log10() + ylab("Area of ice lost (km2)") + xlab("Gross Domestic Product ($USD)") +
  geom_smooth(method="lm", se=T, color="black") + scale_x_log10() +
  geom_label( label=iceCountryArea$Country.Code, aes(fill=RCPs))
plot3



### Examine relationship with GDP
m4 <- lm(abs(diffArea) ~ log10(FreshwaterExtract) * RCPs, data=iceCountryArea)
anova(m4)
summary(m4)

### Plot
plot4 <- ggplot(iceCountryArea, aes(x=FreshwaterExtract , y=abs(diffArea))) + 
  geom_point(size=3, pch=21, aes(fill=RCPs)) + theme_classic() + scale_fill_brewer(palette="YlOrRd") +
  scale_y_log10() + ylab("Area of ice lost (km2)") + xlab("Annual freshwater withdrawal (billions m3 water)") +
  geom_smooth(method="lm", se=T, color="black")+  scale_x_log10() +
  geom_label( label=iceCountryArea$Country.Code, aes(fill=RCPs))
plot4


gridExtra::grid.arrange(plot1, plot2, plot3, plot4, ncol=2)



