library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)

load("MR.Rdata")
`%notin%` <- Negate(`%in%`)
MRsub <- filter(MR, ATMP%notin%c(999,99,"999","99","999.0","99.0") & WTMP%notin%c(999,99,"999","99","999.0","99.0") & hh=="12")
MRsub$ATMP <- as.double(MRsub$ATMP)
MRsub$WTMP <- as.double(MRsub$WTMP)
buoyTimeStr <- paste(MRsub$YYYY, MRsub$MM, MRsub$DD, MRsub$hh, sep="-")
MRsub$TIME <- ymd_h(buoyTimeStr)
MRsub$DATE <- unclass(MRsub$TIME)
MRsub$YYYY <- as.double(MRsub$YYYY)
MR_Jan <- filter(MRsub, MM == "01")
MR_Feb <- filter(MRsub, MM == "02")
MR_Mar <- filter(MRsub, MM == "03")
MR_Apr <- filter(MRsub, MM == "04")
MR_May <- filter(MRsub, MM == "05")
MR_Jun <- filter(MRsub, MM == "06")
MR_Jul <- filter(MRsub, MM == "07")
MR_Aug <- filter(MRsub, MM == "08")
MR_Sep <- filter(MRsub, MM == "09")
MR_Oct <- filter(MRsub, MM == "10")
MR_Nov <- filter(MRsub, MM == "11")
MR_Dec <- filter(MRsub, MM == "12")
numMonths <- levels(factor(MRsub$MM))
nameMonths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
fitA_list <- list()
fitW_list <- list()
for(i in 1:12){
  fitA_list[[i]] <- lm(ATMP~DATE, data=MRsub[MRsub$MM==numMonths[i],])
  fitW_list[[i]] <- lm(WTMP~DATE, data=MRsub[MRsub$MM==numMonths[i],])
}

avg <- MRsub%>%group_by(YYYY)%>%summarize(meanATMP = mean(ATMP), meanWTMP = mean(WTMP))
plotAnnualAvg <- ggplot(data=avg)+
  geom_point(aes(x=YYYY, y=meanATMP), alpha=0.1, color="black")+
  geom_point(aes(x=YYYY, y=meanWTMP), alpha=0.1, color="#56B4E9")+
  geom_smooth(aes(x=YYYY,y=meanATMP,color="ATMP"),method="lm")+
  geom_smooth(aes(x=YYYY,y=meanWTMP,color="WTMP"),method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Annual Average Temperature vs. Year")+
  xlab("Year")+
  ylab("Annual Average Tempurature")

ggsave(file="plotAnnualAvg.jpg", plot=plotAnnualAvg)

# highATMP_May <- MR_May%>%
#   group_by(YYYY)%>%
#   slice_max(order_by = ATMP, n = 5)
# lowATMP_May <- MR_May%>%
#   group_by(YYYY)%>%
#   slice_min(order_by = ATMP, n = 5)
# highWTMP_May <- MR_May%>%
#   group_by(YYYY)%>%
#   slice_max(order_by = WTMP, n = 5)
# lowWTMP_May <- MR_May%>%
#   group_by(YYYY)%>%
  # slice_min(order_by = WTMP, n = 5)
# meanhighATMP_May <- mean(highATMP_May$ATMP)
# meanlowATMP_May <- mean(lowATMP_May$ATMP)
# meanhighWTMP_May <- mean(highWTMP_May$WTMP)
# meanlowWTMP_May <- mean(lowWTMP_May$WTMP)
# 
# meanATMP_Jun <- mean(MR_Jun$ATMP)
# meanWTMP_Jun <- mean(MR_Jun$WTMP)
# maxATMP_Jun <- aggregate(MR_Jun$ATMP, by=list(MR_Jun$YYYY), FUN=max)
# minATMP_Jun <- aggregate(MR_Jun$ATMP, by=list(MR_Jun$YYYY), FUN=min)
# maxWTMP_Jun <- aggregate(MR_Jun$WTMP, by=list(MR_Jun$YYYY), FUN=max)
# minWTMP_Jun <- aggregate(MR_Jun$WTMP, by=list(MR_Jun$YYYY), FUN=min)
# 
# 
# meanATMP_Jul <- mean(MR_Jul$ATMP)
# meanWTMP_Jul <- mean(MR_Jul$WTMP)
# maxATMP_Jul <- aggregate(MR_Jul$ATMP, by=list(MR_Jul$YYYY), FUN=max)
# minATMP_Jul <- aggregate(MR_Jul$ATMP, by=list(MR_Jul$YYYY), FUN=min)
# maxWTMP_Jul <- aggregate(MR_Jul$WTMP, by=list(MR_Jul$YYYY), FUN=max)
# minWTMP_Jul <- aggregate(MR_Jul$WTMP, by=list(MR_Jul$YYYY), FUN=min)
# 
# meanATMP_Aug <- mean(MR_Aug$ATMP)
# meanWTMP_Aug <- mean(MR_Aug$WTMP)
# maxATMP_Aug <- aggregate(MR_Aug$ATMP, by=list(MR_Aug$YYYY), FUN=max)
# minATMP_Aug <- aggregate(MR_Aug$ATMP, by=list(MR_Aug$YYYY), FUN=min)
# maxWTMP_Aug <- aggregate(MR_Aug$WTMP, by=list(MR_Aug$YYYY), FUN=max)
# minWTMP_Aug <- aggregate(MR_Aug$WTMP, by=list(MR_Aug$YYYY), FUN=min)

plotFit <- ggplot(data=MRsub)+
  geom_point(aes(x=DATE,y=ATMP),alpha=0.1,color="black")+
  geom_point(aes(x=DATE,y=WTMP),alpha=0.1, color = "#56B4E9")+
  geom_smooth(aes(x=DATE,y=ATMP,color="ATMP"),method="lm")+
  geom_smooth(aes(x=DATE,y=WTMP,color="WTMP"),method="lm")+
  scale_color_manual("",breaks=c("ATMP","WTMP"),values=c("black","blue"))+
  ggtitle("Temp vs. Date")+
  xlab("Date in number of seconds from 1970/1/1 form")+
  ylab("Tempurature")+
  facet_wrap(~factor(MM))
ggsave(file="plotFit.jpg", plot=plotFit)

plotDist_Jan <- ggplot(MR_Jan,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Jan.jpg", plot=plotDist_Jan)

plotDist_Feb <- ggplot(MR_Feb,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Feb.jpg", plot=plotDist_Feb)

plotDist_Mar <- ggplot(MR_Mar,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Mar.jpg", plot=plotDist_Mar)

plotDist_Apr <- ggplot(MR_Apr,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Apr.jpg", plot=plotDist_Apr)

plotDist_May <- ggplot(MR_May,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_May.jpg", plot=plotDist_May)

plotDist_Jun <- ggplot(MR_Jun,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Jun.jpg", plot=plotDist_Jun)

plotDist_Jul <- ggplot(MR_Jul,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Jul.jpg", plot=plotDist_Jul)

plotDist_Aug <- ggplot(MR_Aug,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Aug.jpg", plot=plotDist_Aug)

plotDist_Sep <- ggplot(MR_Sep,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Sep.jpg", plot=plotDist_Sep)

plotDist_Oct <- ggplot(MR_Oct,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Oct.jpg", plot=plotDist_Oct)

plotDist_Nov <- ggplot(MR_Nov,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Nov.jpg", plot=plotDist_Nov)

plotDist_Dec <- ggplot(MR_Dec,aes(x=ATMP))+geom_histogram()+facet_wrap(~factor(YYYY))
ggsave(file="plotDist_Dec.jpg", plot=plotDist_Dec)

  