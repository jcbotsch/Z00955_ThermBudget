#=====Load Packages=====
library(tidyverse)
library(lubridate)


#====Load Data====
#Data downloaded from: https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-physical-limnology-primary-study-lakes-1981-current
#has data approximately every two weeks, perhaps that's too low a resolution, but has nice long time series.
wtemp = read_csv("Data/routine_wtemp14day.csv", skip = 1) %>% 
  rename(temp = wtemp)

#====Preview Data====

#how many sampling locations within each lake?
wtemp %>% 
  select(lakeid, sta) %>% 
  unique()

wtemp %>% 
  select(lakeid, sta) %>% 
  count(sta)

#only 13 obs in sta3, so I'll toss those

wtemp = wtemp %>% filter(sta!=3)


#plot number of observations per year
wtemp %>% 
  group_by(lakeid) %>% 
  mutate(year = year(sampledate)) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n))+
  geom_point(size = 2)+
  facet_wrap(~lakeid)+
  theme_bw()

#plot number of observations at each depth
wtemp %>% 
  group_by(lakeid) %>% 
  mutate(year = year(sampledate)) %>% 
  count(depth) %>% 
  ggplot(aes(x = depth, y = n))+
  geom_point(size = 2)+
  facet_wrap(~lakeid, scales = "free")+
  theme_bw()


#plot number of observations at each depth per year to see if there are any weird patterns
wtemp %>% 
  group_by(lakeid) %>% 
  mutate(year = year(sampledate)) %>% 
  count(depth, year) %>% 
  ggplot(aes(x = depth, y = n, col = year))+
  geom_point(size = 2)+
  facet_wrap(~lakeid, scales = "free")+
  theme_bw()


wtemp %>% 
  group_by(lakeid) %>% 
  select(lakeid, depth) %>% 
  count(depth) %>% 
  filter(n>600) %>% 
  summarise(max.depth = max(depth),
            min.depth = min(depth))

#look at water temp at different layers 
wtemp %>% 
  filter(depth == ifelse(lakeid == "CR", c(0, 19),c(0,32))) %>% 
  mutate(layer = ifelse(depth == 0, "surface", "bottom")) %>% 
  ggplot(aes(x = sampledate, y = temp, col = layer))+
  geom_point()+
  facet_wrap(~lakeid)+
  theme_bw()

#look at watertemp at different layers subset by date
lim = as.Date(c("2011-01-01","2015-01-01"))
wtemp %>% 
  filter(depth == ifelse(lakeid == "CR", c(1, 18),c(1,30))) %>% 
  mutate(layer = ifelse(depth == 1, "surface", "bottom")) %>% 
  ggplot(aes(x = sampledate, y = temp, col = layer))+
  geom_point()+
  facet_wrap(~lakeid)+
  scale_x_date(limits = lim)+
  theme_bw()


#====Higher Resolution Data====
############
# There were issues with the size of these files on github, so I've uploaded the cleaned files. This is the code I used to clean them:

# 
# #I went looking for high frequency sensor data on the lter website and found these.
# #=====Higher Resolution Data Upload====
# # https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-high-frequency-water-temperature-data-dissolved-oxygen-chlorophyl
# ctemp = read_csv("Data/sensor_crystal.csv")
# 
# # https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-high-frequency-meteorological-and-dissolved-oxygen-data-trout-lak
# ttemp = read_csv("Data/sensor_trout_lake_russ_watertemp_hourly.csv", 
#                  skip = 1)
# 
# #===Prep Data====
# #crystal has lots of data, too high frequency, I'm going ot make it hourly,
# ctemp = ctemp %>% 
#   filter(minute(sampledate) == 0) %>% 
#   select(sampledate, depth_calculated, water_temp) %>% 
#   rename(datetime = sampledate, depth = depth_calculated, temp = water_temp) %>% 
#   filter(!is.na(temp))
#   
# #create datetime
# ttemp = ttemp %>% 
#   mutate(hour_update = paste0(substr(hour, 1,2), ":", substr(hour, 3,4), ":00"),
#          datetime = as_datetime(paste(sampledate, hour_update, sep = " "))) %>% 
#   select(datetime, depth, temp = wtemp) %>% 
#   filter(!is.na(temp))

#I don't know if I want to write these to new csvs but if I did here's the code I'd use
# write_csv(ctemp, "Data/CR_sensor.csv")
# write_csv(ttemp, "Data/TR_sensor.csv")

#=====Load Data====
ctemp = read_csv("Data/CR_sensor.csv")

ttemp = read_csv("Data/TR_sensor.csv")

#====Visualizing high res data====


#plot number of observations per year
ttemp %>% 
  mutate(year = year(datetime)) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n))+
  geom_point(size = 2)+
  ggtitle("Trout")+
  theme_bw()

#plot number of observations per year
ctemp %>% 
  mutate(year = year(datetime)) %>% 
  count(year) %>% 
  ggplot(aes(x = year, y = n))+
  geom_point(size = 2)+
  ggtitle("Crystal")+
  theme_bw()

#plot number of observations at each depth
ttemp %>% 
  filter(!is.na(temp)) %>% 
  mutate(year = year(datetime)) %>% 
  count(depth) %>% 
  ggplot(aes(x = depth, y = n))+
  geom_point(size = 2)+
  theme_bw()


#plot number of observations at each depth per year to see if there are any weird patterns
ttemp %>% 
  count(depth) %>% 
  arrange(desc(n)) 
  

ttemp %>% 
  mutate(year = year(datetime)) %>% 
  count(depth, year) %>% 
  ggplot(aes(x = depth, y = n, col = year))+
  geom_point(size = 2)+
  theme_bw()



#plot number of observations at each depth
ctemp %>% 
  count(depth) %>% 
  arrange(desc(n))

ctemp %>% 
  mutate(year = year(datetime)) %>% 
  count(depth) %>% 
  ggplot(aes(x = depth, y = n))+
  geom_point(size = 2)+
  theme_bw()


#plot number of observations at each depth per year to see if there are any weird patterns
ctemp %>% 
  mutate(year = year(datetime)) %>% 
  count(depth, year) %>% 
  ggplot(aes(x = depth, y = n, col = year))+
  geom_point(size = 2)+
  theme_bw()

#look at water temp at different layers 
ttemp %>% 
  filter(year(datetime)>=2011,
         depth %in% c(1,30)) %>% 
  ggplot(aes(x = datetime, y = temp, col = factor(depth)))+
  geom_point(alpha = 0.2)+
  theme_bw()


#look at water temp at different layers 
ctemp %>% 
  filter(depth %in% c(1,18)) %>% 
  ggplot(aes(x = datetime, y = temp, col = factor(depth)))+
  geom_point(alpha = 0.2)+
  theme_bw()


#====Comparing the two datasets=====

wtemp_comp = wtemp %>% 
  mutate(datetime = as_datetime(paste(sampledate, "12:00:00", sep = " "))) %>% 
  select(lakeid, datetime, depth, temp) %>% 
  rename(survey_temp = temp)
  
# write_csv(wtemp_comp %>% 
#             filter(lakeid=="CR") %>% 
#             select(datetime, depth, temp = survey_temp),
#           "Data/CR_routine.csv")

# write_csv(wtemp_comp %>% 
#             filter(lakeid=="TR") %>% 
#             select(datetime, depth, temp = survey_temp),
#           "Data/TR_routine.csv")


wtemp_comp %>% 
  filter(lakeid == "CR") %>% 
  left_join(ctemp %>% 
               mutate(lakeid = "CR")) %>% 
  ggplot(aes(x = survey_temp, y = temp)) +
  geom_point(alpha = 0.2, size = 2) +
  geom_abline(slope = 1, col= "firebrick") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()

wtemp_comp %>% 
  filter(lakeid == "TR") %>% 
  left_join(ttemp %>% 
               mutate(lakeid = "TR")) %>% 
  ggplot(aes(x = survey_temp, y = temp)) +
  geom_point(alpha = 0.2, size = 2) +
  geom_abline(slope = 1, col = "firebrick") +
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw()
