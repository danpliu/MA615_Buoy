library(knitr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import Data
load("MR.Rdata")
# Remove NA values
`%notin%` <- Negate(`%in%`)
MRsub <- filter(MR, ATMP%notin%c(999,99,"999","99","999.0","99.0") &
                  WTMP%notin%c(999,99,"999","99","999.0","99.0") & hh=="12")
# Convert string values to double
MRsub$ATMP <- as.double(MRsub$ATMP)
MRsub$WTMP <- as.double(MRsub$WTMP)
# Convert YYYY, MM, DD and hh to POSIXct and then to seconds from 1970/1/1
buoyTimeStr <- paste(MRsub$YYYY, MRsub$MM, MRsub$DD, MRsub$hh, sep="-")
MRsub$TIME <- ymd_h(buoyTimeStr)
MRsub$DATE <- unclass(MRsub$TIME)
MRsub$YYYY <- as.double(MRsub$YYYY)

# Monthly
Since the temperature varies during different months, we decide to build linear regression models for each month respectively and for annual average. For better understanding, we make a table presenting how monthly air temperature and water temperature is changing from year to year. We also make plots of temperature versus time for every month. 
```{r}
numMonths <- levels(factor(MRsub$MM))
nameMonths <- c("January", "February", "March", "April", "May", "June", "July", "August",
                "September", "October", "November", "December")
fitA_list <- list()
fitW_list <- list()
coefAPerYear <- list()
coefWPerYear <- list()
for(i in 1:12){
  fitA_list[[i]] <- lm(ATMP~DATE, data=MRsub[MRsub$MM==numMonths[i],])
  fitW_list[[i]] <- lm(WTMP~DATE, data=MRsub[MRsub$MM==numMonths[i],])
  coefAPerYear <- 60*60*24*365*coef(fitA_list[[i]])[2]
  coefWPerYear <- 60*60*24*365*coef(fitW_list[[i]])[2]
}
MRmonth <- data.frame(nameMonths, coefAPerYear, coefWPerYear)
kable(MRmonth, col.names=c("Month", 
                           "Monthly air temperature change every year in celcius degrees", 
                           "Monthly water temperature change every year in celcius degrees"))
plotFit <- ggplot(data=MRsub)+
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
plotFit

# Anuual
avg <- MRsub%>%group_by(YYYY)%>%summarize(meanATMP = mean(ATMP), meanWTMP = mean(WTMP))
plotAnnualAvg <- ggplot(data=avg)+
  geom_point(aes(x=YYYY, y=meanATMP), alpha=0.3, color="black")+
  geom_point(aes(x=YYYY, y=meanWTMP), alpha=0.3, color="#56B4E9")+
  geom_smooth(aes(x=YYYY,y=meanATMP,color="Air"),method="lm", formula="y~x")+
  geom_smooth(aes(x=YYYY,y=meanWTMP,color="Water"),method="lm", formula="y~x")+
  scale_color_manual("",breaks=c("Air","Water"),values=c("black","blue"))+
  ggtitle("Average Annual Temperature vs. Year")+
  xlab("Year")+
  ylab("Annual Average Tempurature")
plotAnnualAvg
fitA_annual <- lm(meanATMP~YYYY, data=avg)
fitW_annual <- lm(meanWTMP~YYYY, data=avg)
cat(paste0("By average, the annual average air temperature increases ",
           as.character(coef(fitA_annual)[2]), 
           " celcius degrees \nand the annual average water temperature increases ",
           as.character(coef(fitW_annual)[2]), 
           " celcius degrees every year. ", 
           sep=""))
