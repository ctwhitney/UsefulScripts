## Water Level processing script
## Modified from Reese Levea Python code

###############################################

library(tidyverse)

## Read in relevant datasets

Site <- "CCBP"

## Stage height data
Stage <- read.csv("Data/CCBP_Water_Level_2021.csv", header = TRUE, stringsAsFactors = FALSE)
Stage$DateTime <- as.POSIXct(Stage$DateTime, format = "%m/%d/%Y %H:%M")
names(Stage)[2:3] <- c("StagePres", "StageTemp")


## Baro pressure data
Baro <- read.csv("Data/Barometer_Marshview_2021.csv", header = TRUE, stringsAsFactors = FALSE)
Baro$DateTime <- as.POSIXct(Baro$DateTime, format = "%m/%d/%Y %H:%M")
names(Baro)[2:3] <- c("BaroPres", "BaroTemp")

## Join dataframes together

Combined <- left_join(Stage, Baro, by = "DateTime")

## Barometric pressure compensation to convert stge height from kPa to m H2O

## Calculate water density
Combined$wat_dens <- 1000 / (1 + (Combined$StageTemp / 100) * 0.2)

## Gravity constant
g <- 9.81 # m/2^2

## Water level (m)
Combined$WaterLevel.m <- (Combined$StagePres - Combined$BaroPres) / (Combined$wat_dens * g) * 1000

## Aggregate to daily averages

## Separate date from DateTime
Combined$Date <- as.Date(Combined$DateTime, format = "%Y-%m-%d")
Year <- format(Combined$Date[1], "%Y")

CombinedDaily <- Combined %>% select("Date", "WaterLevel.m", "StageTemp") %>% group_by(Date) %>% 
  summarize(across(WaterLevel.m:StageTemp, ~mean(., na.rm = TRUE)))

## Export data to file

## 15-minute data
Combined %>% select(DateTime, StageTemp, WaterLevel.m) %>% rename(Temp.C = StageTemp) %>% 
  write.csv(file = paste0("Data/", Site,"_Stage_", Year, "_", Sys.Date(), ".csv"), row.names = FALSE)

## Daily data
CombinedDaily %>% rename(Temp.C = StageTemp) %>% write.csv(file = paste0("Data/", Site, "_Stage_Daily_", Year, "_", Sys.Date(), ".csv"), row.names = FALSE)
  
  
  
  