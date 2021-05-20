### Global Metrics taking from the world bank


## Libraries
library(tidyverse)


### Load in the different metrics
co2 <- read.csv("data//countryStats//CO2Emissions.csv", stringsAsFactors = F) ##  CO2 emissions (metric tons per capita)
freshwater <- read.csv("data//countryStats//FreshwaterExtract.csv", stringsAsFactors = F)  ## annual freshwater withdrawals in billion cubic meteres
gdp <- read.csv("data//countryStats//GDP.csv", stringsAsFactors = F) ### GDP in $USD
pop <- read.csv("data//countryStats//Population.csv", stringsAsFactors = F) ## total population 

## Select the latest year and variable for a simplified dataset
co2 <- co2 %>% select(country = ï..Country.Name, CO2 = X2016)
freshwater <- freshwater %>% select(country = ï..Country.Name, FreshwaterExtract = X2017)
gdp <- gdp %>% select(country = ï..Country.Name, GDP = X2019)
pop <- pop %>% select(country = ï..Country.Name, Population = X2019)

## All metrics joined
countryMetrics <- left_join(co2, freshwater) %>% left_join(gdp) %>% left_join(pop)
countryMetrics[,"GDPperCapital"] <- countryMetrics$GDP / countryMetrics$Population
countryMetrics[,"CO2perCapital"] <- countryMetrics$CO2 / countryMetrics$Population

write.csv(countryMetrics, "SummarizedCountryMetrics.csv", row.names = F)
