### Get spatial polygons and points of economic data
library(raster)
library(dismo)


economic <- read.csv("data//economicData.csv", stringsAsFactors = F)

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



lakeSpecific <- economic[!is.na(economic$lat),]
lakeGPS <- lakeSpecific
coordinates(lakeGPS) <- ~lon+lat
proj4string(lakeGPS) <- "+proj=longlat +datum=WGS84"



### Function to pull lake characteristics

## Lake Characteristics
durationDiff <- function(model, GCM, RCP, fun){
  
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
  avgPoly <- unlist(lapply(polyRCP, fun ))
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
