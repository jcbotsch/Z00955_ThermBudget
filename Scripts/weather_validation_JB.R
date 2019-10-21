#=====Load Packages=====
library(tidyverse)
library(lubridate)


#=====Load Data====
NLDAS <- read_csv("Data/Trout_daily_NlDAS2.csv")

# Station data from LTER Woodruff Airport
stat <- read_csv("Data/ntl_weather_long.csv")



#====Prep Data=====

#find overlapping timeperiod
min(NLDAS$time)
min(stat$sampledate)

max(NLDAS$time)
max(stat$sampledate)

#modify datasets to match
stat_match <- stat%>%
  select(sampledate, tot_precip, avg_air_temp, avg_wind_speed, avg_longwave_rad, avg_shortwave_rad, avg_rel_hum) %>% 
  rename(time = sampledate,
         ShortWave = avg_shortwave_rad,
         LongWave = avg_longwave_rad,
         AirTemp = avg_air_temp,
         RelHum = avg_rel_hum,
         WindSpeed = avg_wind_speed) %>% 
  mutate(dataset = "weather_long Station",
         tot_precip = tot_precip/1000) %>% 
  filter(time<as.Date("2011-12-31"))

NLDAS <- NLDAS %>% 
  rowwise() %>% 
  mutate(tot_precip = sum(Rain, Snow, na.rm=TRUE),
         time = as.Date(time),
         dataset = "NLDAS") %>% 
  filter(time>as.Date("1989-01-01"))
  

#long data
weather_long = bind_rows(NLDAS, stat_match)


#wide data

#add descriptor to column names
stat_match_w <- stat_match
colnames(stat_match_w)[-1] <- paste("station", colnames(stat_match_w)[-1], sep = "_")

NLDAS_w <- NLDAS
colnames(NLDAS_w)[-1] <- paste("NLDAS", colnames(NLDAS_w)[-1], sep = "_")


weather_wide = NLDAS_w %>% 
    inner_join(stat_match_w)



#=====Plot Data====

# Use scatterplot w/ 1:1 line (r^2)

colnames(weather_long_long)

#compare Temp
weather_long %>% 
  ggplot(aes(x=time, y = AirTemp, col = dataset)) +
  geom_line(alpha = 0.8)+
  theme_classic()

weather_wide %>% 
  ggplot(aes(x = NLDAS_AirTemp, y = station_AirTemp)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()

#compare Short Wave
weather_long %>% 
  ggplot(aes(x=time, y = ShortWave, col = dataset)) +
  geom_line(alpha = 1)+
  theme_classic()

weather_wide %>% 
  ggplot(aes(x = NLDAS_ShortWave, y = station_ShortWave)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()

#compare long Wave
weather_long %>% 
  filter(LongWave<1000) %>% 
  ggplot(aes(x=time, y = LongWave, col = dataset)) +
  geom_line(alpha = 0.8)+
  theme_classic()

weather_wide %>% 
  filter(station_LongWave<1000) %>% 
  ggplot(aes(x = NLDAS_LongWave, y = station_LongWave)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()

#compare precipitation
weather_long %>% 
  ggplot(aes(x=time, y = tot_precip, col = dataset)) +
  geom_line(alpha = 0.8)+
  theme_classic()

weather_wide %>% 
  ggplot(aes(x = NLDAS_tot_precip, y = station_tot_precip)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()

#compare wind
weather_long %>% 
  ggplot(aes(x=time, y = WindSpeed, col = dataset)) +
  geom_line(alpha = 1)+
  theme_classic()

weather_wide %>% 
  ggplot(aes(x = NLDAS_WindSpeed, y = station_WindSpeed)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()


#compare humidity
weather_long %>% 
  ggplot(aes(x=time, y = RelHum, col = dataset)) +
  geom_line(alpha = 1)+
  theme_classic()

weather_wide %>% 
  ggplot(aes(x = NLDAS_RelHum, y = station_RelHum)) +
  geom_point(size = 3, alpha = 0.2)+
  geom_abline(slope = 1, col = "firebrick", size = 1)+
  geom_smooth(method = "lm", col = "dodgerblue")+
  theme_classic()

