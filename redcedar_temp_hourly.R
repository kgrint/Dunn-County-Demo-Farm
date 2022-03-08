

# Install packages if you havent already
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
library(ggplot2)

# Optional: Get latest version of soilDB
devtools::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade_dependencies=FALSE)

#load soilDB
library(soilDB)


# get soil temperature averaged by day 
x <- fetchHenry(project='Red Cedar Demo Farm', what ="soiltemp", gran ='hour', soiltemp.summaries=FALSE)

# check object structure:
str(x, 2)

# read csv with temp data 
site.data <- read.csv('C:/Paolucci/SoilClimate/RedCedarFarm/redcedar_sitedata.csv')
head(site.data)
# read henry site data as dataframe
henry.site <- as.data.frame(x$sensors)

# merge site data with sensor data 
site.full <- merge(henry.site, site.data, by.x='sid', by.y='SID')
head(site.full)

# put full site data back into spatial points dataframe
x$sensors@data <- site.full

# convert celsius to fahrenheit and store in new column
x$soiltemp$sensor_value_f <- celsius.to.fahrenheit(x$soiltemp$sensor_value)

# temp data as dataframe
temp <- as.data.frame(x$soiltemp)

# merge site data in with dataframe 
temp <- merge(temp, site.data, by.x='sid', by.y='SID')

#rename sensor_vale to sensor_value_C
temp <- temp%>%
  dplyr::rename(sensor_value_c = sensor_value)

temp <- tidyr::separate(temp, 'date_time',
                                 into = c('longdate', 'time'),
                                 sep= ' ') 

#inspect
head(temp)

#seperate year 
temp.daily <- temp %>%
  tidyr::separate('longdate',
                  into = c('year', 'month', 'day'),
                  sep= '-',
                  remove = FALSE)

head(temp.daily)

#calculate overall average, stdev, max, and min
temp.overall <-temp.daily %>%
  dplyr::group_by(SITE_NAME)%>%
  dplyr::summarise(meanoverall.F = mean(sensor_value_f), sdoverall.F=sd(sensor_value_f), maxoverall.F=max(sensor_value_f), minoverall.F=min(sensor_value_f))
head(temp.overall)

#combine with site data 
temp.overall<- merge(temp.overall, site.full, by='SITE_NAME')

#Overall barchart (Fahrenheit): Careful with data that contains missing values. Recommend subsetting so only matching records exist
# This is how you filter by date/time and sensor ID 
#Example how to subset#
#sub <- dplyr::filter(x$soiltemp, date_time >'2015-11-29 14:00:00'& date_time < '2015-11-29 20:00:00' & sid %in% c('1657', '1661'))
overall.bar.f <-ggplot(data=temp.overall, aes(x=SITE_NAME, y=meanoverall.F, fill=name)) +
  geom_bar(stat="identity")+
  geom_errorbar(width = 0.1, size=0.8, ymin=temp.overall$minoverall.F, ymax=temp.overall$maxoverall.F)+
  coord_cartesian(ylim=c(10,110))+
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100,110))+
  geom_text(aes(label=round(meanoverall.F, digits=1)), vjust=26)+
  #scale_fill_brewer(palette = "Spectral")+
  #facet_wrap(~month.abb)+
  labs(title="Red Cedar Farm",
       subtitle="Average Temperature (Fahrenheit) at 2 Inches. Error Bars Represent Max and Min Values",
       y="Degrees F")
overall.bar.f

# Calculate daily stats
head(temp.daily)
temp.daily.stats <- temp.daily %>%
  dplyr::group_by(longdate, month, day, year, sid)%>%
  dplyr::summarise(meantemp.f = mean(sensor_value_f), sdtemp.f = sd(sensor_value_f), maxtemp.f = max(sensor_value_f),  mintemp.f = min(sensor_value_f))

#combine with site data 
temp.daily.stats <- merge(temp.daily.stats, site.full, by='sid')
head(temp.daily.stats)

# subset sensors 45t (5 cm; no-till; Rasset; sil)
#                68t (5 cm; no-till; Finchford; ls)
#                200t (5 cm; tilled; Rasset; sil)
#                201t (5 cm; tilled; Finchford; ls)
# Date Range April 1. 2017-> May 2017 
sub <- dplyr::filter(temp.daily.stats, longdate >'2017-03-31'& longdate < '2017-06-01' & sid %in% c('1658', '1660','1670', '1672'))
head(sub)
#Plot temperature max (Fahrenheit; method ="loess")
mean.F <- ggplot(sub, aes(x=as.Date(longdate), y=meantemp.f))+
  geom_smooth(aes(colour=short_name), se=FALSE, span=0.11)+
  geom_line(y=50)+
  labs(title="Red Cedar Farm 2017", subtitle= "Mean Daily Temperature (Fahrenheit)", y="Mean Daily Temp. (°F)", x="Date")
mean.F








