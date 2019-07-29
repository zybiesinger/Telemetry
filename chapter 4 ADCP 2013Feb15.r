################################################################################
# In this file I look at 2009 ADCP data and create plots
#########################################################################
# ADCP plots

library(reshape)
library(ggplot2)
library(grid) # for Ben's zmargin trick

source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


################################################################################
# deployment info
# md3 -hb1    -if43 -2009Jun01  
# md4 -sb1    -of43 -2009Jul10  
# md5 -sb2    -oh41 -2009Aug03  
# md6 -hb2    -if41 -2009Aug24  
# md7 -sb3    -os43 -2009Sep14 
# md8 -hb3    -if42 -2009Oct12 
# md9 -sb4    -of43 -2009Nov16

numexpt = 3:9

# look at a list of all the tags
for(i in numexpt){
  print(md[[i]]$deployment)
  print(md[[i]]$fishNames)
}





# import ADCP data
ad = importADCPdata()
# get just 2009
ad = ad[ad$utime > 1230768000, ]    #1230768000 = 2009 Jan 1 midnight GMT
ad$datiL = as.POSIXct(ad$datiL)
# pick only some columns
ad = ad[,c("datiL","tem","magL","dirL")]
ad[,3] = ad[,3]/1000




ad1 = ad

# add a deployment indicator, hb1, sb2, etc. and NA for all else
# add a year indicator
ad1$year = as.factor(as.POSIXlt(ad1$datiL)$year+1900)
 
ad1$deployment = NA
for (i in 1:length(numexpt)){
  keepers = (ad1$datiL > md[[numexpt[i]]]$startUtime) & 
    (ad1$datiL < md[[numexpt[i]]]$stopUtime)
  ad1[keepers,]$deployment = md[[numexpt[i]]]$deployment
}

# drop data not during a deployment, also drop sb1
ad1 = ad1[!is.na(ad1$deployment),]
ad1 = ad1[ad1$deployment != "sb1",]

# Now I want the lunar curve to cover the whole deployment even if there's no
# ADCP data.  To do this I'll add some lines with only dates in them.
# ... let's see, which deployments need this...
# ...ADCP in water: 1 June-20 Aug. 24 Aug-1 Oct. 13 Oct-27 Oct. 18 Nov - 26 Nov.
# ...hb1:3-17 June. sb2:4-20 Aug. hb2:25 Aug-8 Sept. sb3:16 Sept-1 Oct. 
# ...hb3:13-27 Oct. sb4:18-28 Nov.
#
# So sb4 is missing two days at the end...add dates for this
# ...it ends on 2009-11-26 10:59:00 and should go every 10min

# create empty data lines from the end to 2009-11-28 23:59:00
# 10 min = 600 sec for about 2.5 days or 360 10-min intervals
# ...also, add the year and deployment indicators
st = 1259251740 # utime = first made up time 
temp1 = data.frame(datiL = as.POSIXlt(seq(from=st, by=600, length.out=360),
  origin="1970-1-1", tz="EST5EDT"),year=2009,deployment="sb4")  
# this is about right, batteries died on Saturday


# now combine these dates with ad1
ad1=merge(ad1, temp1, by=c("datiL","year","deployment"), all=TRUE)



# now I want to add a sine wave showing the phase of the moon, but because
# 2009 ACDP data is not continuous the method I used for 2007/2008 won't work.
# I'll have to get the sine wave directly from the fractional day of the year


# FYI...from importALPSdata()
# read in the lunarIndex for each day of 2009
fn1 = "C:/zy/school closet/Telemetry/R summary files/lunar phases 2009.csv"
lunar2009 = read.table(file=fn1,header=T,sep=",",
  col.names=c("month","day","doy","lunarIndex"), 
  colClasses=c("character",rep("numeric",3))
)
# now pick the day of each datum in d9 and determine the lunarIndex
# luckily the order of lunar2009 is the same as the order as yday
#

# I want the plot to be a sine wave between 0 and 1 with a period equal to 31 
# days.  I already have a lunar index associated with each day (1 to 31) so I
# need to convert that to radians (0 to 2pi).  31 lunar Index = 2pi rads. 

# find the lunar Index from lunar2009 for each datum
ad1$doy = as.POSIXlt(ad1$datiL)$yday
ad1$lunarIndex = lunar2009$lunarIndex[ad1$doy]
# to smooth the curve, make it a fraction of how many seconds since midnight
ad1$lunarIndexFrac = ad1$lunarIndex + (as.POSIXlt(ad1$datiL)$hour*3600 + 
  as.POSIXlt(ad1$datiL)$min*60)/(24*60*60)
ad1$moonCurve = (sin(ad1$lunarIndexFrac*(2*pi/30) - 0.5*pi) + 1 )/2
plot(ad1$datiL,ad1$moonCurve,pch=19)

# rename the deployments
ad1$deployment[ ad1$deployment == "sb2" ] = "C - SB 2"
ad1$deployment[ ad1$deployment == "sb3" ] = "E - SB 3"
ad1$deployment[ ad1$deployment == "sb4" ] = "G - SB 4"
ad1$deployment[ ad1$deployment == "hb1" ] = "A - HB 1"
ad1$deployment[ ad1$deployment == "hb2" ] = "D - HB 2"
ad1$deployment[ ad1$deployment == "hb3" ] = "F - HB 3"

# drop what I don't want plotted
ad2 = ad1[,c("datiL","tem","deployment","moonCurve")]
ad3 = ad1[,c("datiL","tem","magL","dirL","deployment","moonCurve")]

# rename them pretty for the figure

names(ad2) = c("Date", "Temperature (Celsius)", "Deployment", "Lunar Phase")       
names(ad3) = c("Date", "Temperature (Celsius)", "Current Speed (m/s)",
  "Current Direction", "Deployment", "Lunar Phase")       

meltedad2 = melt(ad2, id.vars=c("Date", "Deployment"))
meltedad3 = melt(ad3, id.vars=c("Date", "Deployment"))

zmargin = opts(panel.margin=unit(0,"lines"))

ggplot(meltedad3, aes(x=Date, y=value)) + 
  geom_line() +
  facet_grid(variable~Deployment, space="fixed",scales="free") +  
  scale_x_datetime(major = "7 days", format="%b %d") + # specifying these works around a bug
  theme_bw() +
  scale_y_continuous(' ') + # instead of...opts(axis.title.y = theme_text(colour = 'white')) +
  opts(axis.title.x = theme_text(size=20)) +  
  #opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15)) +
  zmargin
 





aaa  Here's Ben's work on this figure.  see the original R code (freespace.R) in an email

g1 <- ggplot(meltedad3,aes(x=Date,y=value))+geom_line()+theme_bw()+
  scale_x_datetime(major="14 days")

g1 + facet_grid(variable~Date,scales="free",space="free")
g1 + facet_grid(variable~Date,scales="free",space="fixed")


library(gridExtra)

g2 <- ggplot(subset(meltedad,year==2007),
             aes(x=Date,y=value))+geom_line()+theme_bw()+
  scale_x_datetime(major="14 days")+
  facet_grid(variable~year,scale="free")

g3 <- g2 %+% subset(meltedad,year==2008)

## suppress labels
g2B <- g2+opts(strip.background=theme_blank(),
               strip.text.x=theme_blank(),strip.text.y=theme_blank())

g3B <- g3 + opts(axis.text.y=theme_blank(),axis.title.y=theme_blank())

n2007 <- length(d2007)
n2008 <- length(d2008)
tot <- n2007+n2008

grid.show.layout(grid.layout(1,2,widths=unit(c(n2007/tot,n2008/tot),"null")))
grid.arrange(g2B,g3B,ncol=2,widths=unit(c(n2007/tot,n2008/tot),"null"))


bbb End Ben's work on this figure




################################################################################
# Figure 3.  rose plots of current directions
ad = importADCPdata()
# get just 2007 and 2008
ad = ad[ad$utime < 1230768000, ]    #1230768000 = 2009 Jan 1 midnight GMT
ad$datiL = as.POSIXct(ad$datiL)
# pick just what I want
newad = ad[,c(3,14)]

# add a year indicator
#newad$year = as.factor(ifelse(
#  newad$datiL < as.POSIXct("2008-05-01", origin="1970-1-1", tz="EST5EDT"),
#  2007,2008))
# now add a column just for the figure captions, since I don't know how to change them manually  
newad$deployment = as.factor(ifelse(
  newad$datiL < as.POSIXct("2008-05-01", origin="1970-1-1", tz="EST5EDT"),
  "2007 Deployment","2008 Deployment"))  


ggplot(newad,aes(x=dirL))+
  geom_bar(binwidth=10)+
  facet_wrap(~deployment) +
  theme_bw() +
  coord_polar(start=-pi/20) + # I don't know why 0 isn't at top, but 'start' to fix it
  opts(axis.text.x = theme_text(size = 12)) +
  #opts(title="Water Flow Direction") +  
 # scale_y_continuous(' ') +   # instead of ...opts(axis.title.y = theme_text(colour = 'white')) +                         
  scale_x_continuous(' ') 

ggplot(newad,aes(x=dirL))+stat_bin(binwidth=10,aes(y=19*..density..))+
  scale_x_continuous(limits=c(0,360),breaks=seq(0,360,by=45))+
  #geom_bar(binwidth=10)+
  facet_wrap(~deployment) +
  theme_bw() +
  coord_polar() + # I don't know why 0 isn't at top, but 'start' to fix it
  opts(axis.text.x = theme_text(size = 12)) +
  #opts(title="Water Flow Direction") +  
 # scale_y_continuous(' ') +   # instead of ...opts(axis.title.y = theme_text(colour = 'white')) +                         
  labs(x="",y="Proportion")


