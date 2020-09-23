library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
load("MR.Rdata")
`%notin%` <- Negate(`%in%`)
MRsub <- filter(MR, ATMP%notin%c(999,99,"999","99","999.0","99.0") & WTMP%notin%c(999,99,"999","99","999.0","99.0"))
MRsub$ATMP <- as.double(MRsub$ATMP)
MRsub$WTMP <- as.double(MRsub$WTMP)
buoyTimeStr <- paste(MRsub$YYYY, MRsub$MM, MRsub$DD, MRsub$hh, sep="-")
MRsub$TIME <- ymd_h(buoyTimeStr)
MRsub$DATE <- as.double(MRsub$YYYY)*10000 + as.double(MRsub$MM)*100 + as.double(MRsub$DD)
MRsub$YYYY <- as.double(MRsub$YYYY)

MR_Jan <- filter(MRsub, MM == "01" & hh == "12")
MR_Feb <- filter(MRsub, MM == "02" & hh == "12")
MR_Mar <- filter(MRsub, MM == "03" & hh == "12")
MR_Apr <- filter(MRsub, MM == "04" & hh == "12")
MR_May <- filter(MRsub, MM == "05" & hh == "12")
MR_Jun <- filter(MRsub, MM == "06" & hh == "12")
MR_Jul <- filter(MRsub, MM == "07" & hh == "12")
MR_Aug <- filter(MRsub, MM == "08" & hh == "12")
MR_Sep <- filter(MRsub, MM == "09" & hh == "12")
MR_Oct <- filter(MRsub, MM == "10" & hh == "12")
MR_Nov <- filter(MRsub, MM == "11" & hh == "12")
MR_Dec <- filter(MRsub, MM == "12" & hh == "12")

# MR_Jan
fitA_Jan <- lm(ATMP~DATE, MR_Jan)
fitW_Jan <- lm(WTMP~DATE, MR_Jan)
plot_Jan <- ggplot()+
  geom_point(data=MR_Jan, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Jan, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Jan,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Jan,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Jan)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Feb
fitA_Feb <- lm(ATMP~DATE, MR_Feb)
fitW_Feb <- lm(WTMP~DATE, MR_Feb)
plot_Feb <- ggplot()+
  geom_point(data=MR_Feb, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Feb, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Feb,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Feb,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Feb)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Mar
fitA_Mar <- lm(ATMP~DATE, MR_Mar)
fitW_Mar <- lm(WTMP~DATE, MR_Mar)
plot_Mar <- ggplot()+
  geom_point(data=MR_Mar, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Mar, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Mar,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Mar,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Mar)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Apr
fitA_Apr <- lm(ATMP~DATE, MR_Apr)
fitW_Apr <- lm(WTMP~DATE, MR_Apr)
plot_Apr <- ggplot()+
  geom_point(data=MR_Apr, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Apr, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Apr,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Apr,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Apr)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_May
fitA_May <- lm(ATMP~DATE, MR_May)
fitW_May <- lm(WTMP~DATE, MR_May)
plot_May <- ggplot()+
  geom_point(data=MR_May, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_May, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_May,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_May,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (May)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")
  
# MR_Jun
fitA_Jun <- lm(ATMP~DATE, MR_Jun)
fitW_Jun <- lm(WTMP~DATE, MR_Jun)
plot_Jun <- ggplot()+
  geom_point(data=MR_Jun, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Jun, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Jun,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Jun,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Jun)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Jul
fitA_Jul <- lm(ATMP~DATE, MR_Jul)
fitW_Jul <- lm(WTMP~DATE, MR_Jul)
plot_Jul <- ggplot()+
  geom_point(data=MR_Jul, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Jul, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Jul,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Jul,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Jul)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Aug
fitA_Aug <- lm(ATMP~DATE, MR_Aug)
fitW_Aug <- lm(WTMP~DATE, MR_Aug)
plot_Aug <- ggplot()+
  geom_point(data=MR_Aug, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Aug, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Aug,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Aug,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Aug)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Sep
fitA_Sep <- lm(ATMP~DATE, MR_Sep)
fitW_Sep <- lm(WTMP~DATE, MR_Sep)
plot_Sep <- ggplot()+
  geom_point(data=MR_Sep, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Sep, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Sep,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Sep,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Sep)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Oct
fitA_Oct <- lm(ATMP~DATE, MR_Oct)
fitW_Oct <- lm(WTMP~DATE, MR_Oct)
plot_Oct <- ggplot()+
  geom_point(data=MR_Oct, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Oct, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Oct,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Oct,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Oct)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Nov
fitA_Nov <- lm(ATMP~DATE, MR_Nov)
fitW_Nov <- lm(WTMP~DATE, MR_Nov)
plot_Nov <- ggplot()+
  geom_point(data=MR_Nov, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Nov, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Nov,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Nov,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Nov)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

# MR_Dec
fitA_Dec <- lm(ATMP~DATE, MR_Dec)
fitW_Dec <- lm(WTMP~DATE, MR_Dec)
plot_Dec <- ggplot()+
  geom_point(data=MR_Dec, aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(data=MR_Dec, aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),data=MR_Dec,method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),data=MR_Dec,method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date (Dec)")+
  xlab("Date in YYYYMMDD form")+
  ylab("Tempurature")

grid.arrange(plot_Jan, plot_Feb, plot_Mar, plot_Apr, plot_May, plot_Jun, plot_Jul, plot_Aug, plot_Sep, plot_Oct, plot_Nov, plot_Dec, ncol = 3)

print(paste("Slope of Linear regression model for Jan's ATMP & WTMP: ", as.character(coef(fitA_Jan)[2]), "&", as.character(coef(fitW_Jan)[2])))

print(paste("Slope of Linear regression model for Feb's ATMP & WTMP: ", as.character(coef(fitA_Feb)[2]), "&", as.character(coef(fitW_Feb)[2])))

print(paste("Slope of Linear regression model for Mar’s ATMP & WTMP: ", as.character(coef(fitA_Mar)[2]), "&", as.character(coef(fitW_Mar)[2])))

print(paste("Slope of Linear regression model for Apr’s ATMP & WTMP: ", as.character(coef(fitA_Apr)[2]), "&", as.character(coef(fitW_Apr)[2])))

print(paste("Slope of Linear regression model for May’s ATMP & WTMP: ", as.character(coef(fitA_May)[2]), "&", as.character(coef(fitW_May)[2])))

print(paste("Slope of Linear regression model for Jun’s ATMP & WTMP: ", as.character(coef(fitA_Jun)[2]), "&", as.character(coef(fitW_Jun)[2])))

print(paste("Slope of Linear regression model for Jul’s ATMP & WTMP: ", as.character(coef(fitA_Jul)[2]), "&", as.character(coef(fitW_Jul)[2])))

print(paste("Slope of Linear regression model for Aug’s ATMP & WTMP: ", as.character(coef(fitA_Aug)[2]), "&", as.character(coef(fitW_Aug)[2])))

print(paste("Slope of Linear regression model for Sep’s ATMP & WTMP: ", as.character(coef(fitA_Sep)[2]), "&", as.character(coef(fitW_Sep)[2])))

print(paste("Slope of Linear regression model for Oct’s ATMP & WTMP: ", as.character(coef(fitA_Oct)[2]), "&", as.character(coef(fitW_Oct)[2])))

print(paste("Slope of Linear regression model for Nov’s ATMP & WTMP: ", as.character(coef(fitA_Nov)[2]), "&", as.character(coef(fitW_Nov)[2])))

print(paste("Slope of Linear regression model for Dec’s ATMP & WTMP: ", as.character(coef(fitA_Dec)[2]), "&", as.character(coef(fitW_Dec)[2])))


