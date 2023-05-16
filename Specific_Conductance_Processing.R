## Specific Conductance processing script


###############################################

library(tidyverse)

## Read in relevant datasets

Site <- "CC"

## Conductivity data
Cond <- read.csv("Data/Raw/CC_Conductivity_2017.csv", header = TRUE, stringsAsFactors = FALSE)
Cond$DateTime <- as.POSIXct(Cond$DateTime, format = "%m/%d/%Y %H:%M", tz = "EST")

## Calculate specific conductance from conductivity and water temperature
## Specifc Cond (uS/cm) = Cond (uS/cm) / (1 + 0.02 * (Temp (C) - 25))

Cond <- Cond %>% mutate(SpCond.uScm = FullRange.uScm / (1 + 0.02 * (Temp.C - 25)))

## Aggregate to daily averages

## Separate date from DateTime
Cond$Date <- as.Date(format(Cond$DateTime, "%Y-%m-%d"))
Year <- format(Cond$Date[1], "%Y")

CondDaily <- Cond %>% select("Date", "SpCond.uScm", "Temp.C") %>% group_by(Date) %>% 
  summarize(across(SpCond.uScm:Temp.C, ~mean(., na.rm = TRUE)))

## Export data to file

## 15-minute data
Cond %>% select(DateTime, Temp.C, SpCond.uScm)  %>% 
  write.csv(file = paste0("Data/", Site, "_Cond_", Year, "_", Sys.Date(), ".csv"), row.names = FALSE)

## Daily data
CondDaily  %>% write.csv(file = paste0("Data/", Site, "_Cond_Daily_", Year, "_", Sys.Date(), ".csv"), row.names = FALSE)
