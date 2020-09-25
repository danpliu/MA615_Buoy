library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

###Make URLs and read Data
##Make URLs
url1="http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2=".txt.gz&dir=data/historical/stdmet/"
years=c(1987:2016)
urls=paste0(url1,years,url2)
Dnames=paste0('D',years)
##Read the data from the URLs
for(i in years) assign(Dnames[i-years[1]+1],read_table2(urls[i-years[1]+1]))
coln=colnames(get(Dnames[1]))

###Combine the data into a frame
for(i in years){
  D=get(Dnames[i-years[1]+1])
  ##From Y2000 to Y2016, delete an additional variable of 'TIDE'
  if(i %in% 2000:2016){D=select(D,-TIDE)}
  ##From Y2005 to Y2016, delete an additional variable of 'mm'
  if(i %in% 2005:2016){D=select(D,-mm)}
  ##From Y2007 to Y2016, delete first row of units
  if(i %in% 2007:2016){D=D[-1,]}
  ##Check and unify col names and set data type as 'numeric'
  if(ncol(D)==length(coln)){colnames(D)=coln}
  D=sapply(D, as.numeric)
  ##From Y1987 to Y1999, transfer the Year from 'XX' to '19XX'
  D[,1][D[,1]<100]=D[,1][D[,1]<100]+1900
  ##Create and combine to form final data set Buoy
  if(i==years[1]){Buoy=D}
  else{Buoy=rbind.data.frame(Buoy,D)}
}

###Save data
save(Buoy,file='Buoy.Rdata')

# Remove NA values
`%notin%` <- Negate(`%in%`)
Buoysub <- filter(Buoy, ATMP%notin%c(999,99,"999","99","999.0","99.0") &
                  WTMP%notin%c(999,99,"999","99","999.0","99.0") & hh=="12")
# Convert string values to double
Buoysub$ATMP <- as.double(Buoysub$ATMP)
Buoysub$WTMP <- as.double(Buoysub$WTMP)
# Convert YY, MM, DD and hh to POSIXct and then to seconds from 1970/1/1
buoyTimeStr <- paste(Buoysub$YY, Buoysub$MM, Buoysub$DD, Buoysub$hh, sep="-")
Buoysub$TIME <- ymd_h(buoyTimeStr)
Buoysub$DATE <- unclass(Buoysub$TIME)
Buoysub$YY <- as.double(Buoysub$YY)

# Monthly
numMonths <- levels(factor(Buoysub$MM))
nameMonths <- c("January", "February", "March", "April", "May", "June", "July", "August",
                "September", "October", "November", "December")
fitA_list <- list()
fitW_list <- list()
coefAPerYear <- list()
coefWPerYear <- list()
for(i in 1:12){
  fitA_list[[i]] <- lm(ATMP~DATE, data=Buoysub[Buoysub$MM==numMonths[i],])
  fitW_list[[i]] <- lm(WTMP~DATE, data=Buoysub[Buoysub$MM==numMonths[i],])
  coefAPerYear <- 60*60*24*365*coef(fitA_list[[i]])[2]
  coefWPerYear <- 60*60*24*365*coef(fitW_list[[i]])[2]
}
Buoymonth <- data.frame(nameMonths, coefAPerYear, coefWPerYear)
kable(Buoymonth, col.names=c("Month", 
                           "Monthly air temperature change every year in celcius degrees", 
                           "Monthly water temperature change every year in celcius degrees"))
plotMonth <- ggplot(data=Buoysub)+
  geom_point(aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="Air"),method="lm", formula="y~x")+
  geom_smooth(aes(x=DATE,y=WTMP,color="Water"),method="lm", formula="y~x")+
  scale_color_manual("",breaks=c("Air","Water"),values=c("black","blue"))+
  ggtitle("Temperature vs. Time (Monthly)")+
  xlab("Time")+
  ylab("Tempurature")+
  theme(axis.text.x = element_blank())+
  facet_wrap(~factor(MM))
ggsave("plotMonth.jpg", plot=plotMonth)
plotMonth

# Anuual
avg <- Buoysub%>%group_by(YY)%>%summarize(meanATMP = mean(ATMP), meanWTMP = mean(WTMP))
plotAnnualAvg <- ggplot(data=avg)+
  geom_point(aes(x=YY, y=meanATMP), alpha=0.3, color="black")+
  geom_point(aes(x=YY, y=meanWTMP), alpha=0.3, color="#56B4E9")+
  geom_smooth(aes(x=YY,y=meanATMP,color="Air"),method="lm", formula="y~x")+
  geom_smooth(aes(x=YY,y=meanWTMP,color="Water"),method="lm", formula="y~x")+
  scale_color_manual("",breaks=c("Air","Water"),values=c("black","blue"))+
  ggtitle("Average Annual Temperature vs. Year")+
  xlab("Year")+
  ylab("Annual Average Tempurature")
ggsave("plotAnnualAvg.jpg", plot=plotAnnualAvg)
plotAnnualAvg
fitA_annual <- lm(meanATMP~YY, data=avg)
fitW_annual <- lm(meanWTMP~YY, data=avg)
cat(paste0("By average, the annual average air temperature increases ",
           as.character(coef(fitA_annual)[2]), 
           " celcius degrees \nand the annual average water temperature increases ",
           as.character(coef(fitW_annual)[2]), 
           " celcius degrees every year. ", 
           sep=""))
