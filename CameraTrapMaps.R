rm(list = ls())
library(tidyverse)
library(sf)
#devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
library(rdrop2)

hp <- st_read('IND_adm1.shp')
hp <- hp %>% filter(NAME_1 == 'Himachal Pradesh')
hp <- rbind(hp,hp,hp)
sltheoretical <- st_read('HP_3200_5200UTM.shp')

locations <- drop_search('trap_info  csv')
locations <- grep(unlist(lapply(lapply(locations$matches,'[[','metadata'),'[[','path_lower')), pattern = 'secr', value = T)
dat <- do.call(bind_rows,lapply(locations[1:6], drop_read_csv))
dat <- dat %>% dplyr::select(Area,Station,Longitude,Latitude)
colnames(dat) <- c('Region','Station','long','lat')

dat$long <- as.numeric(dat$long)
dat$lat <- as.numeric(dat$lat)

dat1 <- st_as_sf(dat, coords = c('long','lat'))
st_crs(dat1) <- 4326

ggplot() +
  geom_sf(data = hp, fill = 'grey70')+
  geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
  geom_sf(data = dat1, aes(fill = Region), shape = 21, size =2)+
  geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
  north(hp,symbol = 12)+
  scalebar(hp,dd2km = T, dist = 50)+
  blank()+
  guides(fill=guide_legend(override.aes=list(colour=NA)))

library(concaveman)

all_buff <- st_sf(st_sfc())

st_crs(all_buff) <- 4326

for(i in unique(dat$Region)){
  
  dat_reg <- dat %>%  filter(Region == i)
  
  coordinates(dat_reg) <- dat_reg[,c('long','lat')]
  
  dat_reg <- st_as_sf(concaveman(dat_reg,concavity = 1))
  
  dat_reg$id <- i
  
  st_crs(dat_reg) <- 4326
  
  buff <- st_buffer(dat_reg,dist = .03)
  
  all_buff <- rbind(all_buff,buff)
}

png('Output/regions_concave_03degBuff.png',width = 1000,height = 750, res = 100)

ggplot() +
  geom_sf(data = hp, fill = 'grey70')+
  geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
  geom_sf(data = all_buff, aes(fill = id), alpha = 0.9)+
  scale_fill_discrete(name = 'Region')+
  geom_sf(data = dat1, aes(fill = Region), shape = 21, size =2)+
  geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
  north(hp,symbol = 12)+
  scalebar(hp,dd2km = T, dist = 50)+
  blank()+
  guides(fill=guide_legend(override.aes=list(colour=NA)))

dev.off()

library(adehabitatHR)

dat_reg <- dat
coordinates(dat_reg) <- dat_reg[,c('long','lat')]
mcp_dat <- st_as_sf(mcp(dat_reg[,1], percent = 100))
mcp_dat <- st_buffer(mcp_dat, dist = .03)
st_crs(mcp_dat) <- 4326

png('Output/regions_convex_03degBuff.png',width = 1000,height = 750, res = 100)

ggplot() +
  geom_sf(data = hp, fill = 'grey70')+
  geom_sf(data = sltheoretical, fill = 'palegreen3', show.legend = F)+
  geom_sf(data = mcp_dat, aes(fill = id), alpha = 0.9)+
  scale_fill_discrete(name = 'Region')+
  geom_sf(data = dat1, aes(fill = Region), shape = 21, size =2)+
  geom_sf(data = hp, fill = NA, colour = 'black',size = 1)+
  north(hp,symbol = 12)+
  scalebar(hp,dd2km = T, dist = 50)+
  blank()+
  guides(fill=guide_legend(override.aes=list(colour=NA)))

dev.off()