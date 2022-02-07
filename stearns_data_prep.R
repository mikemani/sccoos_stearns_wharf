#automatically update data weekly... somehow?
library(tidyverse)
library(forecast)

cal_ts = function(utcTime){
  toTz = "us/pacific"
  utcTime = lubridate::force_tz(utcTime,tzone= 'GMT')
  dt = as.POSIXct(format(utcTime,tz = toTz,origin ='GMT', usetz=TRUE))
  dt = lubridate::force_tz(dt,tzone= toTz)
  return(dt)
}

test <- readr::read_csv("https://erddap.sccoos.org/erddap/tabledap/autoss.csvp?station%2Ctime%2Ctemperature%2Cchlorophyll%2Csalinity&time%3E=2015-01-01T00%3A00%3A00Z&time%3C=2022-02-06T22%3A19%3A11Z", lazy = TRUE) %>%
  filter(., stringr::str_detect(station, "stearns")) %>%
  #default units time(UTC), temperature (celcius), chlorophyll (ug/L), salinity (1e-3)
  dplyr::rename(., datetime= "time (UTC)",temp="temperature (celsius)",
         chl= "chlorophyll (ug/L)", sal=  "salinity (1e-3)" )

test1 <- test %>%
  dplyr::mutate(., datetime=cal_ts(datetime))

test1 <- test1 %>%
  dplyr:mutate(day = floor_date(datetime, "day")) %>%
  dplyr:group_by(day) %>%
  dplyr:summarise(across(c(temp, chl, sal), mean)) # calculate daily mean
dplyr:mutate(across(c(temp, chl, sal), forecast::tsclean)) # convert outliers to linearly interpolated replacements

# rename variables

ggplot(test2, aes(day,temp)) +
  geom_line()+
  geom_point() +
  theme_classic()

sw <- read_csv("https://erddap.sccoos.org/erddap/tabledap/HABs-StearnsWharf.csvp?latitude%2Clongitude%2Cdepth%2CSampleID%2CLocation_Code%2Ctime%2CTemp%2CAir_Temp%2CChl1%2CPhosphate%2CSilicate%2CAmmonium%2CNitrate%2CpDA%2CAkashiwo_sanguinea%2CAlexandrium_spp%2CDinophysis_spp%2CLingulodinium_polyedra%2CProrocentrum_spp%2CPseudo_nitzschia_delicatissima_group%2CPseudo_nitzschia_seriata_group%2CCeratium%2CCochlodinium%2CGymnodinium_spp%2COther_Diatoms%2COther_Dinoflagellates%2CTotal_Phytoplankton&time%3E=2015-01-01T00%3A00%3A00Z&time%3C=2022-01-10T15%3A30%3A00Z") %>%
  dplyr::select( datetime= "time (UTC)",
         "Chl-a extracted (ug/L)"= "Chl1 (mg/m3)",
         "Phosphate (uM)",
         "Silicate (uM)",
         "Ammonium (uM)",
         "Nitrate (uM)",
         "pDA (ng/mL)",
         "Akashiwo_sanguinea (cells/L)",
         "Alexandrium_spp (cells/L)",
         "Dinophysis_spp (cells/L)",
         "Lingulodinium_polyedra (cells/L)",
         "Prorocentrum_spp (cells/L)",
         "Pseudo_nitzschia_delicatissima_group (cells/L)",
         "Pseudo_nitzschia_seriata_group (cells/L)",
         "Ceratium (cells/L)",
         "Cochlodinium (cells/L)",
         "Gymnodinium_spp (cells/L)",
         "Other_Diatoms (cells/L)",
         "Other_Dinoflagellates (cells/L)",
         "Total_Phytoplankton (cells/L)") %>%
  dplyr::mutate(., `Ammonium (uM)` =forecast::tsclean(.$`Ammonium (uM)`)) %>%
  dplyr::mutate(., `Ammonium (uM)` = as.numeric(`Ammonium (uM)`)) %>%
  dplyr::mutate(., day = as.Date(datetime))  %>%
  dplyr::select(., -datetime) %>%
  dplyr::left_join(test2, ., by = "day") %>%
  dplyr::rename(., "Chlorophyll fluorescence (ug/L)"= chl, "Temperature (Celcius)"=temp, "Salinity(1e-3)"=sal)

write_csv()


ggplot(sw, aes(datetime,`Ammonium (uM)`)) +
  geom_line()+
  geom_point() +
  theme_classic()
