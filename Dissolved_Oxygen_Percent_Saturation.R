## Dissolved Oxygen percent saturation processing script
## Assumes that data has been exported from HOBOWare as a .csv file, data have been QA/QC'd, and overlap one another
## Need DO: "DateTime", "DO.mgL", "Temp.C"; Baro: "DateTime", "AbsPres.kPa", "Temp.C"


###############################################

library(tidyverse)

## Read in relevant datasets

Site <- "CCBP"
Station <- "SIDE"

## Read in relevant datasets (DO, baro pressure)

DO <- read.csv("Data/Raw/CCBP_SIDE_DO_2021.csv", header = TRUE, stringsAsFactors = FALSE)
DO$DateTime <- as.POSIXct(DO$DateTime, format = "%m/%d/%Y %H:%M", tz = "EST")
DO <- DO %>% na_if(-888.88) ## Convert '-888.88' to NA

Baro <- read.csv("Data/Raw/Barometer_Marshview_2021.csv", header = TRUE, stringsAsFactors = FALSE)
Baro$DateTime <- as.POSIXct(Baro$DateTime, format = "%m/%d/%Y %H:%M", tz = "EST")
Baro <- Baro %>% na_if(-888.88) ## Convert '-888.88' to NA

## Join DO and baro dataframes
Combined <- left_join(DO, Baro, by = "DateTime", suffix = c("_DO", "_Baro"))

## Borrowed some functions from Bob Hall and Hilary Madinger's MIMS processing script

### Water density of air saturated water given water temperature in degC.  
### Source: Paterson and Morris 1994, Meterologia

watdens<-function(temp){
  
  t<-temp
  
  A <- 7.0132e-5
  B <- 7.926295e-3 
  C <-  -7.575477e-5 
  D<- 7.314701e-7
  E <-  -3.596363e-9
  to<- 3.9818
  
  dens<- (999.97358- (A*(t-to) + B*(t-to)^2 +C*(t-to)^3 + D*(t-to)^4+E*(t-to)^5) ) -4.873e-3 + 1.708e-4*t - 3.108e-6 * t^2
  dens/1000
}

### Oxygen saturation. Alternative formulation from Garcia and Gordon (umol/kg), which is converted to mg/L and corrected for water density.  
### This function gives the same values as from Colt and is the one a MIMSer should use.
### u is the vapor pressure of water
### Ending units mg/L

osat1<- function(temp, bp) {
  u <- 10^(8.10765-(1750.286/(235+temp)))
  ts <- log((298.15-temp) / (273.15 + temp))
  a0 <- 5.80871
  a1 <- 3.20291
  a2 <- 4.17887
  a3 <- 5.1006
  a4 <- -9.86643e-2
  a5 <- 3.88767
  
  #u <- 10^(8.10765-(1750.286/(235+temp)))
  sato<-(exp(a0 + a1*ts + a2*ts^2 + a3*ts^3 + a4*ts^4)+a5*ts^5)*((bp-u)/(760-u))
  watdens(temp)*sato*(31.9988/1000)##converts umol/kg to mg/L
  
}

## Convert baro pressure from kPa to mmHg
Combined$AbsPres.mmHg <- Combined$AbsPres.kPa / 0.133322387415

## Calculate DO saturation as osat1(Temp.C, Baro.mmHg)
Combined$DOSat.mgL <- osat1(Combined$Temp.C_DO, Combined$AbsPres.mmHg)

## Calculate percent DO saturation
Combined$DO.pct <- (Combined$DO.mgL / Combined$DOSat.mgL)*100


## Aggregate to daily averages

Combined$Date <- as.Date(format(Combined$DateTime, "%Y-%m-%d"))
Year <- format(Combined$Date[1], "%Y")

CombinedDaily <- Combined %>% select("Date", "Temp.C_DO", "DO.mgL", "DO.pct") %>% group_by(Date) %>% 
  summarize(across(Temp.C_DO:DO.pct, ~mean(., na.rm = TRUE)))

## Diagnostic plots
ggplot()+
  geom_line(data = Combined, aes(x = DateTime, y = DO.pct), color = "black", alpha = 0.5)+
  geom_line(data = CombinedDaily, aes(x = as.POSIXct(Date), y = DO.pct), color = "red")

## Write data to file

## 15-minute data
Combined %>% select(DateTime, Temp.C_DO, DO.mgL, DO.pct) %>% rename(Temp.C = Temp.C_DO) %>% 
  write.csv(file = paste0("Data/", Site, "_", Station, "_DO_", Year, "_", Sys.Date(), ".csv"), row.names = FALSE)

## Daily data
CombinedDaily %>% rename(Temp.C = Temp.C_DO) %>% 
  write.csv(file = paste0("Data/", Site, "_", Station, "_DO_Daily_", Year, "_", Sys.Date(), ".csv"),
            row.names = FALSE)







