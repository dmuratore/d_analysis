## Plan for this evening:
## Reading in CTD (conductivity-temperature-depth) sensor data
## from the Hawaii Ocean Time Series data repository 
## (approximately 1 sample/month from 1988-now)
## Right now what's available online is just October 1988-2018
## Let's start with loading libraries that we like
library(tidyverse)
library(patchwork)
library(lubridate)
library(zoo)
theme_preset<-theme_bw()+
  theme(text=element_text(size=16),
        legend.position='bottom')


## Start with reading the data
ctd_data<-read.csv('data/hotdogsdata/hots_ctd_data.txt')
## Figuring out what data are in this file
str(ctd_data)
units<-ctd_data[1,]
no_units<-ctd_data[-1,] %>%
  select(-X)
no_units_numeric<-data.frame(apply(no_units,2,as.numeric))

example_cruise<-no_units_numeric %>%
  filter(crn==314)

temp_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=temp))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Temperature [C]')+
  theme_preset

density_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=sigma))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Potential Density [kg/m^3]')+
  theme_preset

salinity_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=sal))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Salinity')+
  theme_preset

triple_profile<-temp_profile+density_profile+salinity_profile

chl_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=fluor))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Estimated Chla [ug/L]')+
  theme_preset

o2_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=oxy))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('O2 [umol/kg]')+
  theme_preset

ctd_with_dates<-no_units_numeric %>%
  mutate(julian_date=as.Date(julian,origin='1988-10-01'),
         month=month(julian_date)) %>%
  mutate(season=ifelse(between(month,6,8),'Summer',
                       ifelse(between(month,9,12),'Winter',
                              ifelse(between(month,1,3),'Winter',
                                     'Spring'))))

## Add in seasonal shading
ggplot(cruises_2000s) +
  geom_tile(aes(x=julian_date,
                y=press,fill=temp))+
  scale_fill_viridis_c()+
  scale_y_reverse()+
  theme_preset+
  ylab('Pressure [dbar]')+
  xlab('Date')

cruises_2000s<-ctd_with_dates %>%
  filter(between(year(julian_date),2000,2002))


## Looking at seasonality in mixed layer
mld_data<-read.csv('hotdogsdata/mld_data.txt')
mld_units<-mld_data[1,]
mld_no_units<-data.frame(apply(mld_data[-1,],2,as.numeric)) %>%
  mutate(julian_date=as.Date(julian,origin='1988-10-01')) %>%
  mutate(month=month(julian_date))

ggplot(mld_no_units)+
  geom_line(aes(x=julian_date,y=mean))+
  geom_errorbar(aes(x=julian_date,ymin=mean-stdDev,ymax=mean+stdDev))+
  theme_preset+
  ylab('Mixed Layer Depth [m]')+
  xlab('Date')
            