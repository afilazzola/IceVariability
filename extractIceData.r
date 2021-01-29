library(doParallel)
library(foreach)

### Extract raster data to dataframe
allfiles <- list.files("data//isimip_ice", full.names = T)


### Create cluster
cl <- makeCluster(6, type="PSOCK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
registerDoParallel(cores = cl)


dfOut <- foreach(i = allfiles[1:20], .combine=rbind, .packages=c("raster","ncdf4","doParallel","foreach","dplyr")) %:%
  foreach(j = 1:199, .combine=rbind, .packages=c("raster","ncdf4","doParallel","foreach","dplyr")) %dopar% {
  ## Load one model for example
  tempModel <- brick(i)
  
  names(tempModel) <- paste0("Year",1901:2099) ## label year
  
  ## Extract data
  tempModelDF <- as(tempModel[[j]], "SpatialPixelsDataFrame") %>% data.frame()
  
  ## Extract model name
  tempModelDF["Year"] <- as.numeric(gsub("Year","", names(tempModelDF)[1]))
  modelNames <- gsub(".*_ice/", "",  basename(allfiles[j]))
  tempModelDF[,"GCM"] <- gsub("_historical.*", "", modelNames)
  tempModelDF[,"RCP"]  <- modelNames %>%  gsub(".*historical_", "", .) %>%  gsub("_ice.*", "", .)
  tempModelDF[,"IceMeasure"] <- modelNames %>%  gsub(".*_ice", "", .) %>%  gsub("_1901.*", "", .)
  names(tempModelDF)[1] <- "phenology"
 
  return(tempModelDF)
}

write.csv(tempModelDF, "ExtractedPhenologyData.csv", row.names=FALSE)