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
## Getting the units out
units<-ctd_data[1,]
## Turning character observations back into number
no_units<-ctd_data[-1,] %>%
  select(-X)
no_units_numeric<-data.frame(apply(no_units,2,as.numeric))
for(i in 1:nrow(no_units_numeric)){
  old_row<-no_units_numeric[i,]
  bad_numbers<-which(old_row<0)
  new_row<-old_row
  new_row[bad_numbers]<-NA
  no_units_numeric[i,]<-new_row
}


### Showing some example depth profiles 
example_cruise<-no_units_numeric %>%
  filter(crn==314)

## Plotting profiles
temp_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=temp))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Temperature [C]')+
  theme_preset

## Density profile
density_profile<-ggplot(example_cruise)+
  geom_line(aes(x=press,y=sigma))+
  coord_flip()+
  scale_x_reverse()+
  xlab('Pressure [dbar]')+
  ylab('Potential Density [kg/m^3]')+
  theme_preset

## Salinity Profile
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

o2_chl<-chl_profile+o2_profile

## That was looking at basically what the water column looks like for any given
## trip in terms of structure and layers, but also changes over time 
## eg seasons

## 
ctd_with_dates<-no_units_numeric %>%
  mutate(julian_date=as.Date(julian,origin='1988-10-01'),
         month=month(julian_date)) %>%
  mutate(season=ifelse(between(month,6,8),'Summer',
                       ifelse(between(month,9,12),'Winter',
                              ifelse(between(month,1,3),'Winter',
                                     'Spring'))))
## Let's look at a small example first

cruises_2000s<-ctd_with_dates %>%
  filter(between(year(julian_date),2000,2002))

## Add in seasonal shading
ggplot(cruises_2000s) +
  geom_tile(aes(x=julian_date,
                y=press,fill=temp))+
  scale_fill_viridis_c(name='Temperature [C]')+
  scale_y_reverse()+
  theme_preset+
  ylab('Pressure [dbar]')+
  xlab('Date')

## Look at the entire time series
ggplot(ctd_with_dates)+
  geom_tile(aes(x=julian_date,
                y=press,
                fill=temp))+
  scale_fill_viridis_c(name='Temperature [C]')+
  scale_y_reverse()+
  theme_preset+
  ylab('Pressure [dbar]')+
  xlab('Date')



## Looking at seasonality in mixed layer
## Mixed layer depths determined using a potential temperature differential
## of 0.02 degrees C from 10 meters for 1988-2019. 
## Reading in the data
mld_data<-read.csv('data/hotdogsdata/mld_data.txt')
## Taking the units off
mld_units<-mld_data[1,]
## Switching observations from characters to numbers and changing
## the weird timekeeping from Julian days from October 1 1988 to actual
## dates
mld_no_units<-data.frame(apply(mld_data[-1,],2,as.numeric)) %>%
  mutate(julian_date=as.Date(julian,origin='1988-10-01')) %>%
  mutate(month=month(julian_date))

## Taking an initial look at the data
ggplot(mld_no_units)+
  geom_line(aes(x=julian_date,y=mean))+
  geom_errorbar(aes(x=julian_date,ymin=mean-stdDev,ymax=mean+stdDev))+
  theme_preset+
  ylab('Mixed Layer Depth [m]')+
  xlab('Date')

## Exploratory time series analysis
mixed_layer_time_series<-zoo(mld_no_units$mean,
                             mld_no_units$julian_date)

mld_aggregated<-aggregate(mixed_layer_time_series,
                          as.yearmon,mean)

mld_interp<-na.approx(mld_aggregated,
                      xout=as.yearmon(seq.Date(as.Date(start(mld_aggregated)),
                                                   as.Date(end(mld_aggregated)),
                                                   by='month')))

mld_interp_frame<-data.frame(time=as.Date(index(mld_interp)),
                             value=mld_interp)

## Sanity check that interpolation wasn't crazy
comparison_plot<-ggplot()+
  geom_line(data=mld_no_units,
            aes(x=julian_date,y=mean),
            col='black')+
  geom_line(data=mld_interp_frame,
            aes(x=time,y=value),
            col='blue')+
  theme_preset

## Getting the autocorrelation spectrum
autocorrelation_coefficients<-pacf(ts(mld_interp,frequency=12),
                                   lag.max=150) 

## As we expected we saw that there were spikes at 12 month intervals
## indicating an annual seasonal pattern in mixed layer depth throughout
## the entire timeseries

## Time series decomposition
mld_decomposition<-decompose(ts(mld_interp,frequency=12))

## Getting the seasonal coefficients - getting the coefficients for
## January-December
monthly_components<-mld_decomposition$seasonal[1:12]
names(monthly_components)<-c('Jan','Feb','March','Apr',
                             'May','Jun','July','Aug',
                             'Sept','Oct',
                             'Nov','Dec')

## We find a seasonal signal associated with MLD at station ALOHA
## with winter shoaling and summer upheaving. 


## Smushing together the mixed layer depths with the CTD observations
ctd_with_mld<-left_join(ctd_with_dates,
                        mld_no_units,
                        by=c('crn'='crn',
                             'julian'='julian',
                             'julian_date'='julian_date',
                             'month'='month'))
## Yay it worked

## Plotting calculated MLD over temperature
temperature_with_mld<-ggplot(ctd_with_mld)+
  geom_tile(aes(x=julian_date,y=press,fill=temp))+
  geom_line(aes(x=julian_date,y=mean))+
  scale_y_reverse()+
  scale_fill_viridis_c(name='Temperature [C]')+
  xlab('Date')+
  ylab('Pressure [dbar]')+
  theme_preset

## Only looking at one decade worth of cruises
zoomed_cruises<-ctd_with_mld %>%
  filter(crn>=275 & crn<=318)

## Plotting temperature profiles and MLD
zoomed_in<-ggplot(zoomed_cruises)+
  geom_tile(aes(x=julian_date,y=press,fill=temp))+
  geom_line(aes(x=julian_date,y=mean))+
  scale_y_reverse()+
  scale_fill_viridis_c(name='Temperature [C]')+
  xlab('Date')+
  ylab('Pressure\n[dbar]')+
  theme_preset+
  theme(legend.text=element_text(size=12))

## Plotting the seasonality coefficients
seasonality_reference<-data.frame(month=zoomed_cruises$month,
                                  seasonal_effect=monthly_components[zoomed_cruises$month])

season_zoom<-data.frame(zoomed_cruises,season_effect=seasonality_reference$seasonal_effect)


## Thanks for the great idea erikwr :D
seasonal_effect<-ggplot(season_zoom)+
  geom_point(aes(x=julian_date,y=season_effect))+
  scale_y_reverse()+
  theme_preset+
  xlab('Date')+
  ylab('Seasonal Effect\non MLD')

## Showing the final figure
full_figure<-seasonal_effect/zoomed_in

ggsave(full_figure,filename='mld_seasonality.png',device='png')

## We can also look at density
## Plotting temperature profiles and MLD
zoomed_in<-ggplot(zoomed_cruises)+
  geom_tile(aes(x=julian_date,y=press,fill=sigma))+
  geom_line(aes(x=julian_date,y=mean))+
  scale_y_reverse()+
  scale_fill_viridis_c(name='Density [kg/m^3]',direction=-1)+
  xlab('Date')+
  ylab('Pressure\n[dbar]')+
  theme_preset
