rm(list=ls())

#Simple Plots showing spatial and temporal extent of data
#Input libraries
library(readxl)
library(googlesheets)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)

#set working directory
setwd("C:\\Users\\Harrison Carmody\\Documents\\Uni Documents\\Masters\\Project\\Spanish Mackerel Depredation\\Results\\R\\Data")
#read in
Dat_SphMkl <- read.csv("Analysis_SharkDepSphMkl.csv", header=T)
Dat_SphMkl$Year <- as.factor(Dat_SphMkl$Year)
Dat_SphMkl$Zone <- as.factor(Dat_SphMkl$Zone)
Dat_SphMkl$SessionNumber <- as.factor(Dat_SphMkl$SessionNumber)
Dat_SphMkl$MastersName <- as.factor(Dat_SphMkl$MastersName)
head(Dat_SphMkl)
names(Dat_SphMkl)
unique(Dat_SphMkl$Year)
view
plot(Dat_SphMkl$Year, Dat_SphMkl$NumberMackerelTaken)
boxplot(Dat_SphMkl$Year, Dat_SphMkl$NumberMackerelTaken)

Dat_SphMkl <- Dat_SphMkl[!is.na(Dat_SphMkl$NumberMackerelTaken),]
#Aggregate functions
#DPLYR
library(dplyr)
Dat_SphMkl%>%
  group_by(Dat_SphMkl$NumberMackerelTaken,Dat_SphMkl$Year, Dat_SphMkl$Zone) %>%
  summarise_all(funs(sum))
####
Year.Zone.NbrTkn <- Dat_SphMkl[,c("Year","Zone","NumberMackerelTaken")]
SumMklAll <- aggregate(NumberMackerelTaken ~ ., data = Year.Zone.NbrTkn, FUN=sum)

SumMkl <- aggregate(Dat_SphMkl$NumberMackerelTaken, by=list(Dat_SphMkl$Year,Dat_SphMkl$Zone), FUN=sum)
head(Dat_SphMkl$NumberMackerelTaken)
unique(Dat_SphMkl$NumberMackerelTaken)
unique(Dat_SphMkl$Year)
unique(SumMkl)
View(Dat_SphMkl)

ComMkl <- cbind(SumMkl$Group.1, SumMkl$Group.2)
C

AvgMkl <- aggregate(Dat_SphMkl$NumberMackerelTaken, by=list(Dat_SphMkl$Year), FUN=mean)
SumMklZne <- aggregate(Dat_SphMkl$NumberMackerelTaken, by=list(Dat_SphMkl$Zone), FUN=sum)

#So need to calculate what the zones are after by=list and then add that into the plot. Keep your Calcs separate from everything else.
#plots
plot(Year.Zone.NbrTkn[,1], Year.Zone.NbrTkn[,2])

ggplot(Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken,colour=Zone)) +
  stat_summary(fun.y=sum, geom="point") +
  stat_summary(fun.y=sum, geom="line")

help(plot)

plot(SumMkl[,1],SumMkl[,3],"o",
     col = SumMkl[,2],
     pch = 19,
     lty = 0,
     ylab = "Number of Depredation Events",
     xlab = "Year",
     ylim = c(0,1000),
     xlim = c(2006,2018))
    
help(levels)
legend (x=2007,y=1000, legend = levels(Dat_SphMkl$Zone),col = c(1:3),pch = 16)

plot(SumMklZne[,1],SumMklZne[,2],"o")

ggline.Year.Zone.NbrTkn <-ggplot(SumMkl, aes(x=SumMkl$Group.1, y=SumMkl$x, colour=SumMkl$Group.2)) + 
  stat_summary(fun.y=sum, geom="point") +
  stat_summary(fun.y=sum, geom="line") +
  theme_bw()+
  Theme1

ggplot(SumMkl, aes(x=SumMkl$Group.1,y=,SumMkl$x, colour = SumMkl$Group2))+
  stat_summary(fun.y=sum, geom="point") +
  stat_summary(fun.y=sum, geom="line") +
  theme_bw()+
  Theme1

help("ggplot2")
ggline.Year.Zone.NbrTkn

plot(SumMklAll[,1],SumMklAll[,2],"o")

plot(AvgMkl[,1],AvgMkl[,2],"o")
#theme
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = "top",
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    # axis.text.x=element_blank(),
    axis.text.y=element_text(size=14),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())


#Aggregate
Aggregate
#plots
#temporal plot number taken count (doesn't include zeroes so not total count for each year), total mackerel taken year on year, 
ggline.year.nbrtkn<-ggplot(data=Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken, fill = Zone)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  theme_bw()+
  Theme1

ggline.year.nbrtkn
unique(Dat_SphMkl)

plot(Dat_SphMkl$Year, NumberMackerelTaken,)
#percentage taken
ggline.year.pcgtkn<-ggplot(data=Analysis_SharkDepSphMkl, aes(x=Year, y=PercentageTaken, colour=Zone)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  theme_bw()+
  #Add a facet?
  Theme1
#need to get average percentage taken for each year for the plot, considering this is the percentage 
#taken when there has been some depredation not the actual percentage of depredation each year itself
ggline.year.pcgtkn

plot(Analysis_SharkDepSphMkl, aes(x=Year, y=NumberMackerelTaken))

mean(Dat_SphMkl$HoursFished)
## Actual plots
Dat_SphMkl
## Functions for standard errors
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)
## Scatter plot not really going to work considering you're getting the average for each year
plot(NumberMackerelTaken~Year, pch=as.numeric(Zone), xlab = "Year", ylab = "Number Mackerel Taken", data = Dat_SphMkl)
legend("topleft",legend=c("Zone 1", "Zone 2", "Zone 3"),pch=c(1,2,3), lty = c(0,0,0), bty ='n')
## Bar plot - mean Number mackerel taken over the years per zone
ggbarplot.NMTYRmean<-ggplot(Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation events per fishing session by Zone and Year")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
ggbarplot.NMTYRmean
## Bar plot average percentage Number Mackerel over the years per zone
BrPltPMTYRZ <-ggplot(Dat_SphMkl, aes(x=Year, y=PercentageTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session by Zone and Year")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
BrPltPMTYRZ
## Bar plot - total number mackerel taken over the years per zone
ggbarplot.NMTYRsum<-ggplot(Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken, fill = Zone)) + 
  stat_summary(fun.y=sum, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Sum Depredation events per fishing session")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
ggbarplot.NMTYRsum
## Bar plot mean number Mackerel Taken over the years per zone and per fishing session
BrPltNMTYRZFS <-ggplot(Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation events per fishing session across all Zones and fishing sessions")+
  theme_bw()+
  Theme1+
  facet_grid(SessionNumber~Zone)
BrPltNMTYRZFS
## Bar plot mean number Mackerel taken over the years just per fishing session
BrPltNMTYRFS <-ggplot(Dat_SphMkl, aes(x=Year, y=NumberMackerelTaken, fill = SessionNumber)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation events per fishing session across all years")+
  theme_bw()+
  Theme1+
  facet_grid(~SessionNumber)
BrPltNMTYRFS
## Barplot percentage Mackerel taken over the years per fishing session and zone
BrPltPMTYRZFS <-ggplot(Dat_SphMkl, aes(x=Year, y=PercentageTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session across all Zones and fishing sessions")+
  theme_bw()+
  Theme1+
  facet_grid(SessionNumber~Zone)
BrPltPMTYRZFS
## Barplot percentage Mackerel taken over the years just per fishing session
BrPltPMTYRFS <-ggplot(Dat_SphMkl, aes(x=Year, y=PercentageTaken, fill = SessionNumber)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session across all years")+
  theme_bw()+
  Theme1+
  facet_grid(~SessionNumber)
BrPltPMTYRFS
## Bar plot mean number Mackerel taken over the years per zone by Master
BrPltNMTYRZMN<-ggplot(Dat_SphMkl, aes(x=MastersName, y=NumberMackerelTaken)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation events per fishing session by Master")+
  theme_bw()+
  Theme1
BrPltNMTYRZMN
## Bar plot mean percentage Mackerel taken over the years per zone by Master
BrPltPMTYRZMN<-ggplot(Dat_SphMkl, aes(x=MastersName, y=PercentageTaken)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session by Master")+
  theme_bw()+
  Theme1
BrPltPMTYRZMN
## line plot mean percentage Mackerel taken over the years per zone by session length
LnPltPMTHFZFS <-ggplot(Dat_SphMkl, aes(x=HoursFished, y=PercentageTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="line")+
  ggtitle("Average Depredation rate per fishing session across all Zones and fishing sessions")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
LnPltPMTHFZFS
## Doesn't really work as it's not accumulating i.e. except for the shorter lengths there'll be large gaps in the number of hours fished
## Line plot mean percentage Mackerel taken per fishing session by zone and start time
LnPltPMTSTZFS <-ggplot(Dat_SphMkl, aes(x=StartTime, y=PercentageTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="line")+
  ggtitle("Average Depredation rate per fishing session across all Zones and fishing sessions")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
LnPltPMTSTZFS
## Barplot mean percentage Mackerel taken per fishing session by zone, year and method
BrPltPMTZMT <-ggplot(Dat_SphMkl, aes(x=Method, y=PercentageTaken, fill = Zone)) + 
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
BrPltPMTZMT
## Lineplot depredation rate vs temperatureBrPltPMTZMT <-ggplot(Dat_SphMkl, aes(x=Method, y=PercentageTaken, fill = Zone)) + 
LnPltDRSST <-ggplot(Dat_SphMkl, aes(x=SST, y=NumberMackerelTaken, fill = Zone)) +   
  stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
LnPltDRSST
## Lineplot depredation rate vs lunar phase
LnPltDRLP <-ggplot(Dat_SphMkl, aes(x=Fraction.moon.illuminated, y=NumberMackerelTaken, fill = Zone)) +   
  stat_summary(fun.y=mean, geom="line") +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
LnPltDRLP
## Barplot depredation rate vs temperature
BrPltDRSST <-ggplot(Dat_SphMkl, aes(x=SST, y=NumberMackerelTaken, fill = Zone)) +   
  stat_summary(fun.y=mean, geom="bar") +
  stat_summary(fun.ymin = se.min, fun.ymax = se.max, geom = "errorbar", width = 0.1) +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
BrPltDRSST
## Barplot depredation rate vs lunar phase
BrPltDRLP <-ggplot(Dat_SphMkl, aes(x=Fraction.moon.illuminated, y=NumberMackerelTaken, fill = Zone)) +   
  stat_summary(fun.y=mean, geom="bar") +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
BrPltDRLP
## Barplot depredation rate vs lunar phase
BrPltDRLP <-ggplot(Dat_SphMkl, aes(x=Fraction.moon.illuminated, y=PercentageTaken, fill = Zone)) +   
  stat_summary(fun.y=mean, geom="bar") +
  ggtitle("Average Depredation rate per fishing session by Zone and Method")+
  theme_bw()+
  Theme1+
  facet_grid(~Zone)
BrPltDRLP
