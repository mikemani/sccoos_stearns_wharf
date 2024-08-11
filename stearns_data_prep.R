#automatically update data weekly... somehow?
library(tidyverse)
library(forecast)
library(lubridate)

cal_ts = function(utcTime){
  toTz = "us/pacific"
  utcTime = lubridate::force_tz(utcTime,tzone= 'GMT')
  dt = as.POSIXct(format(utcTime,tz = toTz,origin ='GMT', usetz=TRUE))
  dt = lubridate::force_tz(dt,tzone= toTz)
  return(dt)
}

sw_station <- readr::read_csv("https://erddap.sccoos.org/erddap/tabledap/autoss.csvp?station%2Ctime%2Ctemperature%2Cchlorophyll%2Csalinity&time%3E=2015-01-01T00%3A00%3A00Z&time%3C=2024-04-29T22%3A19%3A11Z", lazy = TRUE) %>%
  dplyr::filter(., stringr::str_detect(station, "stearns")) %>%
  #default units time(UTC), temperature (celcius), chlorophyll (ug/L), salinity (1e-3)
  dplyr::rename(., datetime= "time (UTC)",temp="temperature (celsius)",
         chl= "chlorophyll (ug/L)", sal=  "salinity (1e-3)" )

sw_station <- sw_station %>%
  dplyr::mutate(., datetime=cal_ts(datetime)) %>% # change time zone
  dplyr::mutate(day = lubridate::floor_date(datetime, "day")) %>% # isolate date
  dplyr::group_by(day) %>%
  dplyr::summarise(across(c(temp, chl, sal), mean)) %>% # calculate daily mean
  dplyr::mutate(across(c(temp, chl, sal), forecast::tsclean)) # convert outliers to linearly interpolated replacements

sw <- read_csv("https://erddap.sccoos.org/erddap/tabledap/HABs-StearnsWharf.csvp?Location_Code%2Clatitude%2Clongitude%2Cdepth%2CSampleID%2Ctime%2CTemp%2CAir_Temp%2CSalinity%2CChl_Volume_Filtered%2CChl1%2CChl2%2CAvg_Chloro%2CPhaeo1%2CPhaeo2%2CAvg_Phaeo%2CPhosphate%2CSilicate%2CNitrite%2CNitrite_Nitrate%2CAmmonium%2CNitrate%2CDA_Volume_Filtered%2CpDA%2CtDA%2CdDA%2CVolume_Settled_for_Counting%2CAkashiwo_sanguinea%2CAlexandrium_spp%2CDinophysis_spp%2CLingulodinium_polyedra%2CProrocentrum_spp%2CPseudo_nitzschia_delicatissima_group%2CPseudo_nitzschia_seriata_group%2CCeratium_spp%2CCochlodinium_spp%2CGymnodinium_spp%2COther_Diatoms%2COther_Dinoflagellates%2CTotal_Phytoplankton&time%3E=2015-04-22T00%3A00%3A00Z&time%3C=2024-04-29T15%3A00%3A00Z")

sw <- sw %>%
  dplyr::select( datetime= "time (UTC)", # select and rename columns
         "Chl-a extracted (ug/L)"= "Avg_Chloro (mg/m3)",
         "Phosphate (uM)",
         "Silicate (uM)",
         "Ammonium (uM)",
         "Nitrite_Nitrate (uM)",
         "tDA (ng/mL)",
         "pDA (ng/mL)",
         "dDA (ng/mL)",
         "Akashiwo_sanguinea (cells/L)",
         "Alexandrium_spp (cells/L)",
         "Dinophysis_spp (cells/L)",
         "Lingulodinium_polyedra (cells/L)",
         "Prorocentrum_spp (cells/L)",
         "Pseudo_nitzschia_delicatissima_group (cells/L)",
         "Pseudo_nitzschia_seriata_group (cells/L)",
         "Ceratium_spp (cells/L)",
         "Cochlodinium_spp (cells/L)",
         "Gymnodinium_spp (cells/L)",
         "Other_Diatoms (cells/L)",
         "Other_Dinoflagellates (cells/L)",
         "Total_Phytoplankton (cells/L)") %>%
  dplyr::mutate(., `Ammonium (uM)` =forecast::tsclean(.$`Ammonium (uM)`)) %>% # remove a few of the really crazy ammonium data points
  dplyr::mutate(., datetime=cal_ts(datetime)) %>% # change time zone
  dplyr::mutate(., `Ammonium (uM)` = as.numeric(`Ammonium (uM)`)) %>% #convert timeseries to numeric
  dplyr::mutate(., day = lubridate::floor_date(datetime, "day")) %>% # isolate date
  dplyr::select(., -datetime)

sw <- dplyr::full_join(sw_station, sw, by = "day") %>%
  dplyr::rename(., "Chlorophyll fluorescence (ug/L)"= chl, "Temperature (Celcius)"=temp, "Salinity(1e-3)"=sal)

sw_long <- sw %>%
  pivot_longer(., !day, names_to = "Variable", values_to= "data_vals")

readr::write_csv(sw, "sw_shiny_data.csv")

readr::write_csv(sw_long, "sw_long_shiny_data.csv")
