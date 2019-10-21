#====Load Packages=====
library(tidyverse)
library(lubridate)
library(pracma)

#====Load Data=====
#Data from LTER databases
hypso <- read_csv("Data/lake_hypsometry.csv",
                  skip = 1)
morpho <- read_csv("Data/lake_morphometry.csv",
                   skip = 1)

#height above sea level
TR_elev = 492
CR_elev = 502

#====Plot Data=====
morpho %>% 
  ggplot(aes(y = -depth, x = area))+
  facet_wrap(~lakeid, scales = "free")+
  geom_point()+
  theme_classic()


morpho %>% 
  ggplot(aes(y = -depth, x = volume))+
  facet_wrap(~lakeid, scales = "free")+
  geom_point()+
  theme_classic()

hypso_TR <- as.data.frame(apply(morpho, 2, rev)) %>% 
  filter(lakeid == "TR") %>%
  mutate(depth = as.numeric(depth),
         area = as.numeric(area))
hypso_TR$Volume <- abs((cumtrapz(hypso_TR$depth, hypso_TR$area))) 
hypso_TR$VolPerc <- ((((hypso_TR$Volume * 100) / max(hypso_TR$Volume)))) 
hypso_TR$AreaPerc <- (hypso_TR$area * 100) / max(hypso_TR$area)
hypso_TR$Depth <- max(hypso_TR$depth) - hypso_TR$depth
ggplot(hypso_TR) + geom_point(aes(x = AreaPerc, y = Depth, col = "Area")) +
  geom_point(aes(x = VolPerc, y = Depth, col = "Volume")) +
  facet_wrap(~lakeid)+
  xlab('Area/Volume Percentage') +
  scale_y_reverse()+
  theme_classic()


hypso_CR <- as.data.frame(apply(morpho, 2, rev)) %>% 
  filter(lakeid == "CR") %>%
  mutate(depth = as.numeric(depth),
         area = as.numeric(area))
hypso_CR$Volume <- abs((cumtrapz(hypso_CR$depth, hypso_CR$area))) 
hypso_CR$VolPerc <- ((((hypso_CR$Volume * 100) / max(hypso_CR$Volume)))) 
hypso_CR$AreaPerc <- (hypso_CR$area * 100) / max(hypso_CR$area)
hypso_CR$Depth <- max(hypso_CR$depth) - hypso_CR$depth
ggplot(hypso_CR) + geom_point(aes(x = AreaPerc, y = Depth, col = "Area")) +
  geom_point(aes(x = VolPerc, y = Depth, col = "Volume")) +
  facet_wrap(~lakeid)+
  xlab('Area/Volume Percentage') +
  scale_y_reverse()+
  theme_classic()





#====Files for GLM====

TR_morpho = morpho %>% 
  filter(lakeid == "TR") %>% 
  mutate(height = TR_elev- depth)

CR_morpho = morpho %>% 
  filter(lakeid == "CR") %>% 
  mutate(height = CR_elev - depth)



#====Export Data to .nml for GLM3=====
#I didn't want to type out all the numbers for h and a so a labmate helped write this:
#It will probably need a little bit of adjustment (MetaData)

#Trout
TR = list(
  lake_name = "Trout",
  latitude =  46.0461,
  longitude = -89.6751,
  bsn_wid = 3940, #m at widest part in S basin
  bsn_len = 3770, #m in S basin
  H = TR_morpho$height, #m
  A = TR_morpho$area 
)

##add quotes around lake name
TR[["lake_name"]] <- sprintf("\"%s\"", TR[["lake_name"]])

##convert to string in appropriate format
str <- sapply(1:length(TR), function(i) sprintf("%s = %s", names(TR)[i], paste(TR[[i]], collapse = ","))) %>% 
  paste(collapse = "\n") 

##write to nml file
writeLines(str, "TR_out.nml")

#Crystal
CR = list(
  lake_name = "Crystal",
  latitude = 46.0018,
  longitude = -89.6136,
  bsn_len = 570, #m tall
  bsn_wid = 785, #m wide
  H = CR_morpho$height,
  A = CR_morpho$area
)


CR[["lake_name"]] <- sprintf("\"%s\"", CR[["lake_name"]])

str <- sapply(1:length(CR), function(i) sprintf("%s = %s", names(CR)[i], paste(CR[[i]], collapse = ","))) %>% 
  paste(collapse = "\n") 

writeLines(str, "CR_out.nml")
