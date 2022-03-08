# REd Cedar Farm Soil Climate Monitoring
####Temperature Project Starts Here####
# install packages if you havent already 
install.packages("sharpshootR", dependencies = TRUE)
install.packages("latticeExtra", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("weathermetrics", dependencies = TRUE)
install.packages("sp", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("devtools", dependencies = TRUE)

#load packages
library(sharpshootR)
library(latticeExtra)
library(RColorBrewer)
library(plyr)
library(weathermetrics)
library(sp)
library(dplyr)
library(devtools)

# Optional: Get latest version of soilDB
devtools::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade_dependencies=FALSE)

# load soilDB
library(soilDB)

# get soil temperature averaged by day 
x <- fetchHenry(project='Red Cedar Demo Farm', what ="soiltemp", gran ='day', soiltemp.summaries=FALSE)

# check object structure:
str(x, 2)

# read csv with temp data 
site.data <- read.csv('C:/Paolucci/SoilClimate/RedCedarFarm/redcedar_sitedata.csv')

# read henry site data as dataframe
henry.site <- as.data.frame(x$sensors)

# merge site data with sensor data 
site.full <- merge(henry.site, site.data, by.x='sid', by.y='SID')
head(site.full)

# put full site data back into spatial points dataframe
x$sensors@data <- site.full

# convert celsius to fahrenheit and store in new column
x$soiltemp$sensor_value_f <- celsius.to.fahrenheit(x$soiltemp$sensor_value)

# rename doy column to day. Day equals day of year but day is easier for me to remember
x$soiltemp <- x$soiltemp %>%
  dplyr::rename(day = doy)

# temp data as dataframe
temp <- as.data.frame(x$soiltemp)

# merge site data in with dataframe 
temp <- merge(temp, site.data, by.x='sid', by.y='SID')

# Plot data to see where there is missing data
levelplot(factor(!is.na(sensor_value_f)) ~ day * factor(year) | sensor_name, 
          data=x$soiltemp,
          subset=sensor_depth == 5, 
          main='Daily Soil Temperature (Deg. F) at 5cm',
          col.regions=c('grey', 'RoyalBlue'), cuts=1, 
          colorkey=FALSE, as.table=TRUE, scales=list(alternating=3, cex=0.75), 
          par.strip.text=list(cex=0.85), strip=strip.custom(bg='yellow'), 
          xlab='Day of Year', ylab='Year')


#get integer values for months 
as.integer(format(seq(as.Date("2020-01-01"), to=as.Date("2020-12-31"), by="month"), "%j"))
# define scales example: scales=list(x=list(at=c(2, 5, 7, 9),labels=letters[1:4]))

# generate some better colors
cols.temp <- colorRampPalette(rev(brewer.pal(11, 'RdYlBu')), space='Lab', interpolate='spline')

# plot by sensor id 
levelplot(sensor_value_f ~ day * factor(year) | sensor_name, main='Daily Soil Temperature (Deg. F) at 5cm',
          data=temp, col.regions=cols.temp,
          subset=sensor_depth == 5 & year %in% 2016:2019,
          colorkey=list(space='top'), as.table=TRUE, scales=list(alternating=3, cex=0.75), 
          par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
          xlab='Day of Year', ylab='Year')


# plot data by day and color by soil activity
levelplot(sensor_value_f ~ day * factor(year) | sensor_name, main='Daily Soil Temperature (Deg. F) at 5cm',
          data=x$soiltemp, at=c(0, 50, 86, 100), col.regions=c("BLUE", "YELLOW", "RED"),
          subset=sensor_depth == 5 & year %in% 2016:2019,
          colorkey=list(space='top', at=c(20, 50, 86, 100)), as.table=TRUE, scales=list(alternating=3, cex=0.75, x=list(at=c(61, 122, 183, 245, 306), labels=c('mar', 'may', 'jul', 'sep', 'nov'))), par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), xlab='Day of Year', ylab='Year')

# plot by field id
levelplot(sensor_value_f ~ day * factor(year) | FIELD_NAME, main='Daily Soil Temperature (Deg. F) at 5cm: Field',
          data=temp, col.regions=cols.temp,
          subset=sensor_depth == 5 & year %in% 2016:2019,
          colorkey=list(space='top'), as.table=TRUE, scales=list(alternating=3, cex=0.75), 
          par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
          xlab='Day of Year', ylab='Year')

# plot by texture
levelplot(sensor_value_f ~ day * factor(year) | TEXTURE, main='Daily Soil Temperature (Deg. F) at 5cm: Texture',
          data=temp, col.regions=cols.temp,
          subset=sensor_depth == 5 & year %in% 2016:2019,
          colorkey=list(space='top'), as.table=TRUE, scales=list(alternating=3, cex=0.75), 
          par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
          xlab='Day of Year', ylab='Year')

# plot by tillage
levelplot(sensor_value_f ~ day * factor(year) | TILLAGE, main='Daily Soil Temperature (Deg. F) at 5cm: Tillage',
          data=temp, col.regions=cols.temp,
          subset=sensor_depth == 5 & year %in% 2016:2019,
          colorkey=list(space='top'), as.table=TRUE, scales=list(alternating=3, cex=0.75), 
          par.strip.text=list(cex=0.85), strip=strip.custom(bg='grey'), 
          xlab='Day of Year', ylab='Year')

# This is how you filter by date/time and sensor ID. Then you can remake any of the plots with the filtered data 
sub <- dplyr::filter(x$soiltemp, date_time >'2015-11-29 14:00:00'& date_time < '2015-11-29 20:00:00' & sid %in% c('1657', '1661'))

