# In this chapter I use 2007 and 2008 data to describe fish movement.

library(gdata)  # for drop.levels
library(hexbin)
library(rgl)
#library(lattice) #or lme4 for 'histogram()'
library(mgcv)
#library(plotrix)
#library(lme4)
library(ggplot2)
library(MASS)
library(reshape)
library(rimage)

source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
        

# read in tagfm, z0, results, these are created from zzz? 
# 'make tagfm z0 2007 2008.r' and 'make tagfm z9 2009.r'
load("C:/zy/school closet/Telemetry/R summary files/tag 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/tagf 2011Mar16.rdata")

load("C:/zy/school closet/Telemetry/R summary files/tagfm 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/z0 2011May02.rdata")
load("C:/zy/school closet/Telemetry/R summary files/results 2011Mar16.rdata")



# I want the legend to say tags 1-5 not 60300, etc...so change the tagName and
# the order they appear.  DON'T FORGET THAT z0 IS NOW DIFFERENT.
z0$ID = rep(NA, nrow(z0))
z0$ID[z0$tagName == "f60200"] = 5
z0$ID[z0$tagName == "f60400"] = 4
z0$ID[z0$tagName == "f60900"] = 3
z0$ID[z0$tagName == "f60300"] = 1
z0$ID[z0$tagName == "f61100"] = 2
z0$ID = as.factor(z0$ID)

# for the 38/51 panel plots get the date in each box label
z0$Date = as.Date(z0$datiL)

# ...let's say the first 2/3 days are a tagging recovery period
z0short = z0[z0$dod > 3, ]

# for lunarIndex stuff...only the night
z0night = z0[z0$day=="night", ]
z0day = z0[z0$day=="day", ]



# for altitude stuff
z6 = z0[!is.na(z0$altitude), ]        
z6 = drop.levels(z6[(z6$tagName!="f60300")|(z6$tagName!="f61100"),],reorder=FALSE) 
z6night = z6[z6$day=="night", ]
z6day = z6[z6$day=="day", ]



#...or do you want one fish at a time
# main effects for one fish, change x and y for various relationships                                                                   
z1 = drop.levels(subset(z0,tagName=="f60200"),reorder=FALSE)
z2 = drop.levels(subset(z0,tagName=="f60400"),reorder=FALSE)
z3 = drop.levels(subset(z0,tagName=="f60900"),reorder=FALSE)
z4 = drop.levels(subset(z0,tagName=="f60300"),reorder=FALSE)
z5 = drop.levels(subset(z0,tagName=="f61100"),reorder=FALSE)


cTagNames = c("f60200", "f60400", "f60900", "f60300", "f61100")
cDeploymentNames = c(rep("hb2007",3),rep("hb2008",2))







################################################################################
# Figure 1.  Two panels, first the habitat map, then habitat use index
# ... this code was originally worked out in 'habitat maps.r'

### figure 1, left panel ##################
# my own plotting function
# plot.imagematrix.zy now lives in 'global functions.r'
plot.imagematrix.zy = function (x, ...) {
    colvec <- switch(attr(x, "type"), grey = grey(x), rgb = rgb(x[, 
        , 1], x[, , 2], x[, , 3]))
    if (is.null(colvec)) 
        stop("image matrix is broken.")
    colors <- unique(colvec)
    colmat <- array(match(colvec, colors), dim = dim(x)[1:2])
    image(x = 0:(dim(colmat)[2]), y = 0:(dim(colmat)[1]), 
      z = t(colmat[nrow(colmat):1, ]), 
      col = colors, bty="o", cex.lab=2, 
      xlab = "Easting (m)", ylab = "", 
      axes = FALSE, asp = 1, ...)
}

# import the image to use
rfile = "C:/zy/school closet/Telemetry/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg"
i1 = round(read.jpeg(rfile))
z plot(i1, useRaster=TRUE)
# md[[1]]$reefEN ~= (245174.8,3262391)
findHabType(e=md[[1]]$reefEN$easting, n=md[[1]]$reefEN$northing, 
  reference="if41", habmap=i1, 
  show=TRUE, pixels=T, dotCol="red", dotShape=4, dotSize=2) 


# pick out just the portion I want
bc = c(1590+1, 1805+1) # bc = box center...pixel location of reef
bw = 2600 # bw = box width, 2*130m = 2600pixels
bh = 2600 # bh = box height
bl = function(){bc[2] - bw/2}
br = function(){bc[2] + bw/2}
bt = function(){bc[1] - bh/2}  # row 1 is at the top in i1, but when plotting it's at the bottom
bb = function(){bc[1] + bh/2}

catmap = i1[bt():bb(), bl():br(), 3]
catmap1 =  i1[bt():bb(), bl():br(), ] # curse it, plotting needs 2D and findHabType needs 3D
# From this I calculate the eastings and northings of the edged of catmap, using
# 10pix/m.  Reef IF41/center is at (245174.8 easting, 3262391 northing).  The 
# image is 2600 pixels square, 260m square.  So:
# ...the top row is 3262391 N + 130m = 3262521 N
# ...the bottom row is 3262391 N - 130m = 3262261 N 
# ...the left column is 245175 E - 130m = 245045 E
# ...the right column is 245175 E + 130m = 245305 E
#                             
nx=bw+1
ny=bh+1 
r=10 # 10 pixels = 1m   

# look down to "plot fig 1" for the plotting code


  
### figure 1, right panel ##################
habmap = i1
rm(i1)

# location of IF41 in easting/northing or row,column.  row/column is right?
#ce = 245174.8-eastingOffset # center (meters) in easting direction
#cn =  3262391-northingOffset # center (meters) in northing direction
ce = 1805 # center column
cn = 1590 # center row
ne = dim(habmap)[2] # number of columns
nn = dim(habmap)[1] # number of rows
# compute the distance between pixel (ce,cn) and every other pixel
# ...the default is for outer to do the product, but this will add instead
d = sqrt(outer( (cn-(1:nn))^2, (ce-(1:ne))^2, "+")) 

 
# which tags are we dealing with
cTagNames = as.character(unique(z0$tagName))
# list to hold (tagName, radius, # positions, % time over HB)
allfish = list()
                  
                  
d2 = data.frame(radius=NA, percentHB=NA)
annulusThickness = 1  # that is 5m
rings = seq(annulusThickness,150, by=annulusThickness)
# but only go out to 50m
rings = head(rings,50)


for (i in 1:length(rings)){ 
  pixelsPerMeter = 10
  
  rOuter = rings[i] * pixelsPerMeter # r is in units of pixels, radius is in meters...10 pixels per meter
  if(i==1){rInner=0} else {rInner = rings[i-1] * pixelsPerMeter}
  
  kernel = function(z){(z > rInner) & (z < rOuter)} # if passed a matrix, this returns a T/F matrix 
  
  # k is an nx by ny matrix with TRUE(=1) everywhere [within 
  # rOuter of (i,j) and beyond rInner of (i,j)] and FALSE(=0) everywhere else
  k = kernel(d) 
  #k = k/sum(k) # this normalizes k so that it sums to one, probably not necessary but a good habit
  percentHB = 1 - sum(k * habmap[,,2]) / sum(k)
  
  
  # some exploration
  #d[(cn-10):(cn+10),(ce-10):(ce+10)]
  #habmap[(cn-10):(cn+10), (ce-10):(ce+10), 2]
  #k[(cn-10):(cn+10), (ce-10):(ce+10)]
  
  
  d2[i,] = c(rings[i], percentHB)
  #points(c(ce,ce+r,ce,ce-r),c(cn+r,cn,cn-r,cn),pch=19,cex=0.5,col="green")
  #points(ce,cn,pch=10, cex=0.5,col="red")
} # end for-loop     

# If you want to see it... 
# plot(d2,type="l",main="Landscape composition around IF41")


# for each fish, loop through all radii and calculate the %HB use
for (i in 1:length(cTagNames)){
  habUse = data.frame(tagName=NA, radius=NA, numPositions=NA, HBuse=NA, HBpreference=NA)
  cFish = z0[z0$tagName == cTagNames[i], ]
  for (j in 1:length(rings)){
    rOuter = rings[j] # rOuter is in units of m
    if(j==1){rInner=0} else {rInner = rings[j-1]}                                    
     
    cPositions = cFish[(cFish$dtr > rInner) & (cFish$dtr < rOuter), ]
    cHB = cPositions[cPositions$habType == "black", ]
     
    habUse[j,] = c(cTagNames[i], rings[j], 
      nrow(cPositions), nrow(cHB)/nrow(cPositions), 
      (nrow(cHB)/nrow(cPositions))/d2$percentHB[j]
    )
    allfish[[i]] = habUse
  }
}

# for all fish together calculate the %HB use at all radii

i=6
habUse = data.frame(tagName=NA, radius=NA, numPositions=NA, HBuse=NA, HBpreference=NA)
for (j in 1:length(rings)){
  # for each ring, calculate the mean of the habUse of all fish
  temp1 = vector(length=length(allfish))
  temp2 = vector(length=length(allfish))
  for (k in 1:length(allfish)){
    temp1[k] = as.numeric(allfish[[k]]$HBuse[j])
    temp2[k] = as.numeric(allfish[[k]]$HBpreference[j])
  }
  habUse[j,] = c("all", rings[j], NA, mean(temp1, na.rm=T), mean(temp2, na.rm=T))
}
allfish[[i]] = habUse 






### plot fig 1 ... a double plot for the paper ############################3
par(mfrow=c(1,2))  #stretch this to be as wide as you want
  
# draw the plot    
par(mar=c(5,6,3,2)+0.1)
plot.imagematrix.zy(imagematrix(catmap),useRaster=TRUE)
box(which = "plot", lty = "solid")
# add axes labels and numbers
mtext(text="Northing (m)", side=2, line=3.4, cex=2)
axis(1, 
  at=seq(308,2308,by=500), cex.axis=1.5, 
  labels=seq(-100,100,by=50)
)
axis(2, at=seq(300,2300,by=500), cex.axis=1.5, las=1, 
  labels=seq(-100,100,by=50)
)
# I've picked axis lables to put the reef at (0,0). The reef is at (1308,1300) 
# pixel locations, which you can see from where the diamond is drawn on the figure.
  

 

# I want to draw the locations of the reef and sdls on the image...
# ...these are my best estimates locations of the reef and sdl
# ... IF41 (245174.8 E, 3262391 N)
# ... 50m array spacing
md[[1]]$sdlEN
md[[1]]$sdlEN$easting + eastingOffset
md[[1]]$sdlEN$northing + northingOffset
# ... 125m spacing
md[[2]]$sdlEN
md[[2]]$sdlEN$easting + eastingOffset
md[[2]]$sdlEN$northing + northingOffset
# ...in the following you have to change the cpoint and the color
                         
# Because catmap1 is a cropped version of the full i1 image, the easting/northing
# and colmun/row mapping numbers are off. To plot things (SDLs or reef) in the 
# correct place you have to account for the croping. To do that shift E/N
# by the correct number of meters equivalent to the number of columns/rows, 
# bl()/bt()
# Remember that 1 meter = 10 pixel rows or columns, meaning that if 
# bl() = 506 columns that equals 50.6 meters 
#
# AND, the '+1' on the easting is clearly necessary (by visual inspection) 
# when marking the reef. It 
# must be a 'starting-at-0-or-1' issue   
 
# 50m spacings                                                        
for (cpoint in 1:5){
  dotColors = c("black", "white", "black", "black", "black")
  findHabType(
    e=md[[1]]$sdlEN$easting[cpoint]-(bl()/10)+1, 
    n=md[[1]]$sdlEN$northing[cpoint]+(bt()/10), 
    habmap=catmap1, reference="if41",
    show=TRUE, crosshairs=FALSE, pixels=TRUE, 
    dotCol=dotColors[cpoint], dotShape=4, dotSize=2) 
}
# 125m spacings
for (cpoint in 1:5){ 
  dotColors = c("black", "white", "black", "white", "black")
  findHabType(
    e=md[[2]]$sdlEN$easting[cpoint]-(bl()/10)+1, 
    n=md[[2]]$sdlEN$northing[cpoint]+(bt()/10), 
    habmap=catmap1, reference="if41",
    show=TRUE, crosshairs=FALSE, pixels=TRUE, 
    dotCol=dotColors[cpoint], dotShape=17, dotSize=2)  
}
# outline the western sdlEN to showup better
cpoint = 4
bob = findHabType(
    e=md[[2]]$sdlEN$easting[cpoint]-(bl()/10)+1, 
    n=md[[2]]$sdlEN$northing[cpoint]+(bt()/10), 
    habmap=catmap1, reference="if41",
    show=FALSE, crosshairs=FALSE, pixels=TRUE, 
    dotCol=dotColors[cpoint], dotShape=24, dotSize=2)
points(bob$pixelEN[1], bob$pixelEN[2], pch=24, col="black", bg="white", cex=2)    
     
# reef
cpoint=1
findHabType(
  e=md[[2]]$reefEN$easting[cpoint]-(bl()/10)+1, 
  n=md[[2]]$reefEN$northing[cpoint]+(bt()/10), 
  habmap=catmap1, reference="if41",
  show=TRUE, crosshairs=FALSE, pixels=TRUE, 
  dotCol="black", dotShape=18, dotSize=2) 
# figure improvement, perhaps unnecessary
# Look at the black hard-bottom in the upper left and notice that what was a 
# blue (or was it red) line in i1 is now a white line. I'll color it black in
# a terribly ugly way
points(90,1910,pch=19,col="black")   
points(80,1880,pch=19,col="black")   
points(80,1850,pch=19,col="black")   
points(80,1830,pch=19,col="black") 
  
  
  
# reefEN = (
#      "ID" = "reef",
#      "easting" = 8574.76,
#      "northing" =  691.2323  
#  
  


## label the reef with an arrow
#text(900, 900, "Reef", cex=1.5)
#arrows(950,950,1220,1220, length=0.1, lwd=2) 
#
# place the a)
text(130,2500,"a)", cex=2)   



### figure 1, right panel ##################
par(mar=c(5,6,3,2)+0.1)

plot(d2,type="l",lwd=4, las=1, bty="l", cex.lab = 2, cex.axis=2, 
  xlim=c(0,50), ylim=c(0,1), 
  xlab="Ring Number (m)",
  ylab="") #
mtext(text="Fraction of Cover or Use", side=2, line=3.7, cex=1.8)

# add individual fish with dashed lines
#for (i in 1:length(cTagNames)){
#  points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l")
#  text(x=100,y=as.numeric(tail(allfish[[i]]$HBuse,1))+0.01, labels=tail(allfish[[i]]$tagName,1))
#}

# f60200
i=1
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=1, lwd=2)
# f60400, lowest of triplet
i=2 
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=2, lwd=2)
# f60900, mid of triplet
i=3
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=3, lwd=2)
# f60300
i=4
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=4, lwd=2)
# f61100, highest of triplet
i=5
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=5, lwd=2)
# all fish 
i=6
points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", lty=2, lwd=4)
# place the b)
text(1,1,"b)", cex=2)   

# add a legend
legend(4, 1.04, legend=c("Hard-bottom cover", 
  "ID 1 Hard-bottom use", "ID 2 Hard-bottom use", 
  "ID 3 Hard-bottom use", "ID 4 Hard-bottom use", 
  "ID 5 Hard-bottom use", "All individuals"), seg.len=4,
  lty=c(1,4,5,3,2,1,2), lwd=c(4,2,2,2,2,2,4)) 


# check the stretching to that the map x and y axes cross at the zero hash marks




################################################################################
# Figure 2.  ADCP data and lunar phase in ggplot
#########################################################################
# ADCP plots
# ...for this plot I want the lunar phase to show from the beginning of the 
# ...2007 depoyment even though there's no water flow data...to do that I'll
# ...make up data lines which will be empty except for the lunar phase stuff

library(reshape)
library(ggplot2)
# import ADCP data
ad = importADCPdata()
# get just 2007 and 2008
ad = ad[ad$utime < 1230768000, ]    #1230768000 = 2009 Jan 1 midnight GMT
ad$datiL = as.POSIXct(ad$datiL)
# pick only some columns
ad = ad[,c(3,6,11,14)]
ad[,3] = ad[,3]/1000

# create empty data lines
# tagging start on 9 Dec 16:01:00, ADCP starts on 19 Dec 16:01:00.  That's 
# 11 days and 240 hours, with data every 10min...=1440 data lines
dday = rep(9:19,each=144)
hhour = rep(0:23,each=6,times=11)
mmin = rep(seq(1,51, by=10),264)

temp1 = paste("2007-12-",dday," ",hhour,":",mmin,":00",sep="")
temp2 = strptime(temp1, "%Y-%m-%d %H:%M:%S", tz="EST5EDT")
temp3 = as.POSIXct(temp2, origin="1970-1-1", tz="EST5EDT")
temp4 = data.frame(datiL = temp3[temp3 < ad$datiL[1]])


# now combine these
newad=merge(ad, temp4, by="datiL", all=TRUE)
# add a year indicator
newad$year = as.factor(ifelse(
  newad$datiL < as.POSIXct("2008-05-01", origin="1970-1-1", tz="EST5EDT"),
  2007,2008))
# now add a column just for the figure captions, since I don't know how to change them manually  
newad$deployment = as.factor(ifelse(
  newad$datiL < as.POSIXct("2008-05-01", origin="1970-1-1", tz="EST5EDT"),
  "2007 Deployment","2008 Deployment"))  



# add a column for creating a sine wave indicating lunar phase
newad$moon = NA
# split the years apart while the moon is added
nad7 = newad[newad$year == 2007, ]
nad8 = newad[newad$year == 2008, ]

# add an index column 2007...
# ...and each day has 24hrs with 6 lines each = 144 lines, and there are 30 days
# ..in the lunar cycle...30*144 = 4320 lines per 2 pi radians or (2160 lines = pi rads)

nad7$index = (1:nrow(nad7)*pi/2160) + pi # add pi to make it 1 at full moon and -1 at new
# ... and by chance 9 Dec (the first day) is a new moon so I don't have to 
# ... shift left or right except to make the full moon be up at 1

# create the lunar curve
nad7$moon = (cos(nad7$index)+1)/2 
# look and add lines where the new and full moons should be, as a check...looks good
plot(nad7$datiL, nad7$moon, type="l")
abline(v=as.POSIXct("2007-12-09", origin="1970-1-1", tz="EST5EDT")) #new moon
abline(v=as.POSIXct("2007-12-24", origin="1970-1-1", tz="EST5EDT")) #full
abline(v=as.POSIXct("2008-01-08", origin="1970-1-1", tz="EST5EDT")) #new

# add an index column 2008...
nad8$index = ((1:nrow(nad8))*(pi/2160)) - (497*(pi/2160))
# ... by looking at it I see that 14 Oct (the first full moon) is 497 lines down
# 
# create the lunar curve
nad8$moon = (cos(nad8$index)+1)/2
plot(nad8$datiL,nad8$moon, type="l")
abline(v=as.POSIXct("2008-10-15", origin="1970-1-1", tz="EST5EDT")) # full moon
abline(v=as.POSIXct("2008-10-29", origin="1970-1-1", tz="EST5EDT")) # new
abline(v=as.POSIXct("2008-11-13", origin="1970-1-1", tz="EST5EDT")) # full
abline(v=as.POSIXct("2008-11-28", origin="1970-1-1", tz="EST5EDT")) # new
abline(v=as.POSIXct("2008-12-13", origin="1970-1-1", tz="EST5EDT")) # full
abline(v=as.POSIXct("2008-12-27", origin="1970-1-1", tz="EST5EDT")) # new
#this looks close enough

# put them back together without the index or the year
newerad = rbind(nad7[,c(-5,-8)], nad8[,c(-5,-8)])
names(newerad) = c("Date", "Temperature (Celsius)", "Current Speed (m/s)",
  "Current Direction", "Deployment", "Lunar Phase")       

meltedad = melt(newerad, id.vars=c("Date", "Deployment"))
mad2007 = subset(meltedad,Deployment=="2007 Deployment")
mad2008 = subset(meltedad,Deployment=="2008 Deployment")


names(mad2007) = c("2007 Deployment", "year", "variable", "value")
names(mad2008) = c("2008 Deployment", "year", "variable", "value")

ggplot(meltedad, aes(x=Date, y=value)) + 
  geom_line() +
  facet_grid(variable~Deployment, space="fixed",scales="free") +  
  scale_x_datetime(major = "14 days", format="%b %d") + # specifying these works around a bug
  theme_bw() +
  scale_y_continuous(' ') + # instead of...opts(axis.title.y = theme_text(colour = 'white')) +
  opts(axis.title.x = theme_text(size=20)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15)) 
 


aaa  Here's Ben's work on this figure.  see the original R code (freespace.R) in an email

g1 <- ggplot(meltedad,aes(x=Date,y=value))+geom_line()+theme_bw()+
  scale_x_datetime(major="14 days")

g1 + facet_grid(variable~year,scales="free",space="free")
g1 + facet_grid(variable~year,scales="free",space="fixed")


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





################################################################################
# Figure 4.  two panel: a) Easting - Northing b) hourly fraction PS     
                                                           
# let's shift all the easting/northing plots so the reef is at (0,0)...see below
# for words on how I get these nubmers
# 
lleft=8434
bbottom=555
z0$easting = z0$easting - lleft
z0$northing = z0$northing - bbottom

# now the reef location is
md[[1]]$reefEN$easting - lleft       # = 140.76m E
md[[1]]$reefEN$northing - bbottom    # = 136.2m N


xLimits = md[[2]]$plotLimits$easting - lleft
yLimits = md[[2]]$plotLimits$northing - bbottom
                                                             
                                                             
                                                             
                                                             
                                                             
                                                             
                                                             
                                                             
cTag = 1 

# E-N plots with colors telling something
# ... for altitude I want the shallower points on top, so sort by altitude

z1sorted = z1[order(z1$altitude), ]
z2sorted = z2[order(z2$altitude), ]
z3sorted = z3[order(z3$altitude), ]
z4 = z4
z5 = z5
# make legend have a capital 'A'
z1sorted$Altitude=z1sorted$altitude
z2sorted$Altitude=z2sorted$altitude
z3sorted$Altitude=z3sorted$altitude
z4$Altitude=z4$altitude
z5$Altitude=z5$altitude

                       

# EN plot, pick different fish
d = z1

# calculate the contour lines...look in 'ggcont.R' from Ben...but this doesn't quite work now
# ... remember that the easting and northing have been shifted to show (0,0) in plot
# ... so shift the hrlims also...
hrlimsShifted = hrlims - c(lleft, lleft, bbottom, bbottom)

#
prob <- c(0.5,0.95)         ## utilization regions to plot
dens <- kde2d(d$easting,d$northing,n=250,lims=hrlimsShifted)
dx <- diff(dens$x[1:2])   
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy
levels <- sapply(prob, function(x) {
  approx(c1, sz, xout = 1 - x)$y
})
dC <- contourLines(dens$x, dens$y, dens$z, level = levels)
dC2 = dC 
## transform contourLines into a useful format
for (i in 1:length(dC))
  dC2[[i]] <- with(dC[[i]],data.frame(x,y,level=prob[match(level,levels)],f=i))
dC3 <- transform(do.call(rbind,dC2),level=factor(level))

d = z1sorted
ggplot(d, aes(x=easting, y=northing, colour=Altitude)) +     #      
  geom_point(alpha=0.05) + 
  #geom_path(data=dC3,aes(group=f,col="red"))     FIND THIS CODE AGAIN BELOW WHERE I GOT IT TO WORK 
  theme_bw() +
  coord_cartesian(xlim=c(0,280), ylim=c(0,280)) +
  scale_y_continuous("Northing (m)", breaks = seq(0,280,by=40)) +
  scale_x_continuous("Easting (m)", breaks = seq(0,280,by=40)) +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))
# add a)
grid.text("a)", x = unit(0.15, "npc"), y = unit(0.96, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 
# add contour lines

   
ggplot(z4, aes(x=easting, y=northing)) +
  geom_point(alpha=0.05) + 
  theme_bw() +
  coord_cartesian(xlim=c(0,280), ylim=c(0,280)) +
  scale_y_continuous("Northing (m)", breaks = seq(0,280,by=40)) +
  scale_x_continuous("Easting (m)", breaks = seq(0,280,by=40)) +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))
# add a)
grid.text("a)", x = unit(0.15, "npc"), y = unit(0.96, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 


plot(z1$easting, z1$northing, pch=19, alpha=0.1)



### figure 4, right panel ##################
# fraction of hourly PS before the minute mean...so re-filter the data

# read in 'tag' data then filter it without the minuteMean
load("C:/zy/school closet/Telemetry/R summary files/tag 2011Mar16.rdata")
ttf = list() # filtered tag data
for (i in 1:length(cTagNames)){
  print(cTagNames[i])
  ttf[[i]] = filterALPSdata(df1=tag[[i]], cnF=1.5, speedF=0.8, minuteMean=F)
}
# now remove 'tag'
rm(tag)


cTag=5 # pick one                     
dat = ttf[[cTag]]$data  
time = dat$datiL
dat <- subset(dat,select=c("northing","easting"))
timecat <- cut(time, breaks="hour")
datsplit <- split(dat,timecat)
hourlyFrac = length(datsplit)/(60*30)

fracHits = vector(length=length(datsplit))

for(i in 1:length(datsplit)){
  fracHits[i]=nrow(datsplit[[i]])/(60*30)
}

# draw the right plot of fig 4
# this code is for 2007 which does the dates nicely...
par(mar=c(5,6,2,2)+0.1)
dates = as.POSIXct(levels(timecat))
plot(dates,fracHits,type="l", las=1, bty="l", 
  cex.lab=2, cex.axis=1.7,
  ylim=c(0,0.8),
  xlab="Date", ylab=""
) 
text(as.POSIXlt(levels(timecat))[10], 0.8, "b)", cex=2)
mtext("Fraction of Transmissions", side=2, line=4, cex=2)


# this code is for 2008 which doesn't do dates nicely...
par(mar=c(5,6,2,2)+0.1)
dates = as.POSIXct(levels(timecat))
whichdates = seq(14,1204,by=336) # dates starting 18 Oct going every 14days
plot(dates,fracHits,type="l", las=1, bty="l", 
  cex.lab=2, cex.axis=1.7, xaxt="n",
  ylim=c(0,0.8),
  xlab="Date", ylab=""
) 
axis(1,at=dates[whichdates],  format(dates[whichdates], "%b %d"), cex.axis=1.5)
text(as.POSIXlt(levels(timecat))[10], 0.8, "b)", cex=2)
mtext("Fraction of Transmissions", side=2, line=4, cex=2)




#### figure 4, left panel ##################
## DON'T USE HEXBIN ANYMORE                hexbin of PS for one fish
#
#cTag = 5 #cmd=md[[2]]  # pick a tag
## I want the axes to start from zero or close...so subtract
## ... also I want to have the same scale for all 5 fish, so I'll pick a 
## ... south-east most point that I want the graphing limits to be, then to make
## ... the plotting limits be what I want, and uniform among all 5 fish I'll add
## ... a point in each of the four corners
##
## I get my range by looking at tagfm manually, and at md[[2]]$plotLimits which I
## made to be square...easting (8434-8721), northing (555,842)
## First, add points to all corners: bottom left, top left, top right, bottom right
#lleft=8434; rright=8721; bbottom=555; ttop=842;
#easting = c(tagfm[[cTag]]$data$easting, lleft, lleft, rright, rright) # - lleft ...already done at top
#northing = c(tagfm[[cTag]]$data$northing, bbottom, ttop, ttop, bbottom) # - bbottom ...already done at top
#eLimits = NA
#nLimits = NA
## In these plots the reef location is at 
#md[[1]]$reefEN$easting - lleft
#md[[1]]$reefEN$northing - bbottom
#
## for now, I can't figure out how to manually control the plotting limits in
## hexbin, so I'll add a point in each of the four corners
#
#bins = hexbin(easting, northing, xbins=40)
#               
##plot(bins,main="", xlab="Easting (m)", ylab="Northing (m)")
## is gplot.hexbin any better?
#
## 




par(mar=c(1,1,1,1)+0.1)
plot(bins,main="", xlab="Easting (m)", ylab="Northing (m)")

hexbinplot(bins, xbnds=c(0,500), ybnds=c(0,500))


# To make Appendix figures like these two just change cTag and re-evaluate








# I want an EN plot for the dissertation talk
# pick a fish tag, don't start with shifted EN data    
# this code has been moved to 'plots for dissertation talk...it needs z1 - z5 mostly

# remember

# now the reef location is
md[[1]]$reefEN$easting    # = 140.76m E
md[[1]]$reefEN$northing   # = 136.2mN

xLimits = md[[2]]$plotLimits$easting 
yLimits = md[[2]]$plotLimits$northing 

d = z3




################################################################################
# Figure 4 again, for publication after defense.
# I want to show EN scatterplots, with reef at (0,0), zoomed in a little, 
# and without altitude info (colors). Also, I won't include hourlyPsFrac plots

# IF NECESSARY, GO BACK AND READ IN z0 AGAIN SO IT'S NOT SHIFTED BY lleft
# OR bbottom. ALSO DO THE OTHER STUFF TO GET z1 - z5 WITH MODIFICATIONS.

# Shift everything so the reef is at (0,0), by subtracting 

y0 = z0
y0$easting = z0$easting - md[[1]]$reefEN$easting
y0$northing = z0$northing - md[[1]]$reefEN$northing
                                                              
y1 = drop.levels(subset(y0,tagName=="f60200"),reorder=FALSE)                   
y2 = drop.levels(subset(y0,tagName=="f60400"),reorder=FALSE)
y3 = drop.levels(subset(y0,tagName=="f60900"),reorder=FALSE)
y4 = drop.levels(subset(y0,tagName=="f60300"),reorder=FALSE)
y5 = drop.levels(subset(y0,tagName=="f61100"),reorder=FALSE)


# Look at some lots to pick good, common plotting ranges...and shift reef to (0,0)
plot(y5$easting, y5$northing, pch=".", xlim=c(-130,130),ylim=c(-130,130))

points(md[[2]]$sdlEN$easting-md[[1]]$reefEN$easting,md[[2]]$sdlEN$northing-md[[1]]$reefEN$northing)
points(md[[2]]$reefEN$easting-md[[1]]$reefEN$easting,md[[2]]$reefEN$northing-md[[1]]$reefEN$northing)

points(y4$easting, y4$northing, pch=".", col="red")
points(y3$easting, y3$northing, pch=".", col="green")
points(y2$easting, y2$northing, pch=".", col="blue")
points(y1$easting, y1$northing, pch=".", col="brown")

abline(v=c(-125,125), h=c(-116,134) ) # these will be my plotting ranges...
abline(h=0,v=0)

### so use y0-y5 and plot (-130,130) and (-130,130)


# EN plot, pick different fish
d = y2

# calculate the contour lines...look in 'ggcont.R' from Ben...but this doesn't quite work now
# ... remember that the easting and northing have been shifted to show (0,0) in plot
# ... so shift the hrlims also...
hrlimsShifted = hrlims - c(md[[1]]$reefEN$easting, md[[1]]$reefEN$easting, 
  md[[1]]$reefEN$northing, md[[1]]$reefEN$northing)

#
prob <- c(0.5,0.95)         ## utilization regions to plot
dens <- kde2d(d$easting,d$northing,n=250,lims=hrlimsShifted)
dx <- diff(dens$x[1:2])   
dy <- diff(dens$y[1:2])
sz <- sort(dens$z)
c1 <- cumsum(sz) * dx * dy
levels <- sapply(prob, function(x) {
  approx(c1, sz, xout = 1 - x)$y
})
dC <- contourLines(dens$x, dens$y, dens$z, level = levels)
dC2 = dC 
## transform contourLines into a useful format
for (i in 1:length(dC))
  dC2[[i]] <- with(dC[[i]],data.frame(x,y,level=prob[match(level,levels)],f=i))
dC3 <- transform(do.call(rbind,dC2),level=factor(level))
# separate 50% and 95% KDEs
dC3_50 = dC3[dC3$level == 0.5,]
dC3_95 = dC3[dC3$level == 0.95,]

# now that it works, recall that fish ID in paper are in a different order than y1-y5
# fish ID 1 = T60300 = y4
# fish ID 2 = T61100 = y5
# fish ID 3 = T60900 = y3
# fish ID 4 = T60400 = y2
# fish ID 5 = T60200 = y1
# ... make sure to label the panels correctly. Instead of panels a) b).. e) simply
# ... put the fish ID on the figure
   
ggplot(y2, aes(x=easting, y=northing)) +
  geom_point(alpha=0.05) + 
  geom_path(aes(x=dC3_50$x, y=dC3_50$y, group=dC3_50$f), size=1, colour="grey") +
  geom_path(aes(x=dC3_95$x, y=dC3_95$y, group=dC3_95$f), size=1, colour="black") +
  theme_bw() +
  coord_cartesian(xlim=c(-80,80), ylim=c(-80,80)) +
  scale_y_continuous("Northing (m)", breaks = seq(-80,80,by=40) ) +   # Northing (m)     , labels=c("","","","","")
  scale_x_continuous("Easting (m)", breaks = seq(-80,80,by=40)) +   # Easting (m)       , labels=c("","","","","")
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))
# add panel label
grid.text("Fish ID 4", x = unit(0.15, "npc"), y = unit(0.96, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 










################################################################################
# Figure 5.  four panels: histograms of interval length, DTR, SPDG, ALT

  
# dtr histogram
par(mfrow=c(2,3))
bks = c(20,20,20,30,30)
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$dtr, breaks=bks[i], freq=T, main=tagfm[[cTag]]$tagName,
    xlim=c(0,100))
  abline(v=median(tagfm[[cTag]]$data$dtr), col="red")
}

hist(z0$dtr, breaks=90, freq=F, xlim=c(0,100), ylim=c(0,0.05),
  col="grey", las=1,
  main="",
  xlab="Distance to the Reef (m)")
abline(v=median(z0$dtr),lwd=4)



# speed histogram
par(mfrow=c(2,3))
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$speed, breaks=30, freq=F, main=tagfm[[cTag]]$tagName)
  abline(v=mean(tagfm[[cTag]]$data$speed), col="red")
}


hist(z0$speed, breaks=30, freq=F, col="grey", las=1, 
  main="",
  xlab="Gag Speed (m/s)")
abline(v=median(z0$speed),col="black",lwd=4)




# interval histogram
# ... for this you want the filtered, but not minuteMeaned data
# ... too bad...I have to get that fresh now...you need some code from 'chapter 3 xxxxx.r"
cTagNames             
                                                                       
# import the 'tag' dataset
load("C:/zy/school closet/Telemetry/R summary files/tag 2011Mar16.rdata")
                                                    
ttf = list() # filtered tag data
for (i in 1:length(cTagNames)){
  print(cTagNames[i])
  ttf[[i]] = filterALPSdata(df1=tag[[i]], cnF=1.5, speedF=0.8, minuteMean=F)
}

# combine them 
tempInterval = c(ttf[[1]]$data$interval, ttf[[2]]$data$interval, 
  ttf[[3]]$data$interval, ttf[[4]]$data$interval, ttf[[5]]$data$interval)

                      
par(mfrow=c(2,3))
bks = seq(0,max(tempInterval),by=2)
for (i in 1:length(cTagNames)){
  cTag=i; 
  hist(ttf[[cTag]]$data$interval, breaks=bks, freq=T, main=ttf[[cTag]]$tagName,
    xlim=c(0,60))
  abline(v=median(ttf[[cTag]]$data$interval), col="red")
}                                                                             


par(mar=c(5,4,2,2)+0.1)
hist(tempInterval, breaks=bks, freq=F, col="grey", las=1, main="",
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,60), ylim=c(0,0.25), 
  xlab="Interval Length (s)", ylab="")
abline(v=median(tempInterval),lwd=4)
mtext("Density", side = 2, line = 4, cex = 1.5)

### Altitude distribution
par(mfrow=c(2,3))
possAlts = 1:13
 
for (i in 1:3){ # only the 2007 fish have altitudes

  # ...how do I want to round
  alt = ceiling(tagfm[[i]]$data$altitude[ tagfm[[i]]$data$altitude >= 0 ])
  altVec = c()
  for (j in 1:length(possAlts)){ 
    altVec = rbind(altVec, sum(alt == possAlts[j],na.rm=T))
  }

  altVec = altVec/sum(altVec,na.rm=T)
# remove altitudes 10-13 because they are or are close to zero  ... or not
#  altVec = head(altVec,9)
#  possAlts = head(possAlts,9)


  barplot(altVec, horiz=T, beside=T,
    main=paste(tagfm[[i]]$tagName,"Altitude"),
    xlab="Frequency", ylab="Altitude (m)", cex.axis=1.5, cex.names=1.5,
    cex.lab=1.5, names=possAlts,
    ylim=c(0,15))

#  gap.barplot(altVec,gap=c(0.25, 0.6),xlab="Altitude (m)", 
#    ytics=c(0,0.05, 0.1, 0.15, 0.2, 0.6383233),
#    xtics=possAlts, xaxlab=possAlts,
#    ylab="Frequency",horiz=T,
#    main=paste(tagfm[[i]]$tagName,"Distribution in the Water Column"), 
#    col=rep("grey",length(altVec)))

  abline(h=mean(tagfm[[i]]$data$altitude, na.rm=T),col="red",lwd=2)
}
  
# now do all fish together
alt = ceiling(z0$altitude[ z0$altitude >= 0 ])
altVec = c()
for (j in 1:length(possAlts)){ 
  altVec = rbind(altVec, sum(alt == possAlts[j],na.rm=T))
}

altVec = altVec/sum(altVec)

barplot(altVec, horiz=T, beside=T, col="grey", las=1, main="",
  xlab="Density",
  ylab="Gag Height Above Seafloor (m)", cex.axis=1.5, cex.names=1.5,
  cex.lab=1.5, names=possAlts,
  xlim=c(0,0.7), ylim=c(1,13)
)



# get a regular histogram for altitude distribution, but group the 0 into 1
hist(z0$altitude, breaks=30, freq=F, main="", col="grey", las=1,
  xlim=c(0,13), #ylim=c(0,30000), 
  xlab="Altitude (m)")
abline(v=median(z0$altitude,na.rm=T),lwd=4)





##############################################################################
##############################################################################
# a four panel figure...using stuff generated above
par(mfrow=c(2,2))                                          
# 1. Time interval between detections
par(mar=c(5,5,2,2)+0.1)
bks = seq(0,max(tempInterval),by=2)
hist(tempInterval, breaks=bks, freq=F, col="grey", las=1, main="",
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,40), ylim=c(0,0.25), 
  xlab="", ylab="", xaxt="n")
abline(v=median(tempInterval),lwd=3)
mtext("Density", side = 2, line = 3.8, cex = 1.5)
mtext("Interval Length (s)", side = 1, line = 3, cex = 1.5)
text(0,0.25,"a)", cex=1.5)
axis(1,at=seq(0,40,by=4), cex.axis=1.5)


# 2. distance to reef
par(mar=c(5,4,2,2)+0.1)
bks = seq(0,max(z0$dtr),by=1)
hist(z0$dtr, breaks=90, freq=F, col="grey", las=1, cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,80), ylim=c(0,0.05),
  xlab="", ylab="", main="", xaxt="n")
abline(v=median(z0$dtr),lwd=3)
mtext("Density", side = 2, line = 4, cex = 1.5)
mtext("Distance from Reef (m)", side = 1, line = 3, cex = 1.5)
text(0,0.05,"b)", cex=1.5)
axis(1,at=seq(0,80,by=10), cex.axis=1.5)

# 3. Gag Travel Speed
par(mar=c(5,5,2,2)+0.1)
hist(z0$speed, breaks=30, freq=F, col="grey", las=1, cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,0.8), ylim=c(0,4.5),
  xlab="", ylab="", main="", xaxt="n")
abline(v=median(z0$speed),col="black",lwd=4)
mtext("Density", side = 2, line = 2.8, cex = 1.5)
mtext("Travel Speed (m/s)", side = 1, line = 3, cex = 1.5)
text(0,4.5,"c)", cex=1.5)               
axis(1,at=seq(0,0.8,by=0.1), cex.axis=1.5)


# 4. altitude...this one from below
par(mar=c(5,4,2,2)+0.1)
barplot(altVec, horiz=T, beside=T, col="grey", las=1,
  xlab="",
  ylab="", cex.axis=1.5, cex.names=1.5,
  cex.lab=1.5, names=c(1,"",3,"",5,"",7,"",9,"",11,"",13),  #possAlts,
  xlim=c(0,0.7), ylim=c(1,13)
)
mtext("Altitude (m)", side = 2, line = 3, cex = 1.5)
mtext("Density", side = 1, line = 3, cex = 1.5)
text(0.025,13,"d)", cex=1.5)












################################################################################
# Figure 6.  two panels: KDE stabilization curves
  
# ...because I put 'results' in order of size and this wants them in original order
results = results[c(5,4,3,1,2) ,]

# something to hold the changing HR answers for all fish
allFishHR = list()                                          

cProb = 0.5
for (cTag in 1:length(tagfm)){  

  if(results$deployment[cTag] == "hb2007"){
    cmd = md[[1]]
    numUniqueDays = length(unique(tagfm[[cTag]]$data$datiL$yday))
  } else {
    cmd = md[[2]]
    numUniqueDays = length(unique(tagfm[[cTag]]$data$datiL$yday))
  }

  # get only the easting/northing data
  d1 = subset(tagfm[[cTag]]$data, select=c(utime,datiL,easting,northing))
  # create a vector of the index of the day of the run, 1-52 for example
  # ...there's got to be a prettier way of doing this, but...
  startOfFirstDay = unclass(as.POSIXct(
    strptime(cmd$taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
      origin="1970-1-1", tz="EST5EDT"))[1]
  endOfAllDays = startOfFirstDay + 86400 * 1:numUniqueDays
  
  # something to hold the changing HR for one fish
  hrVSdays = data.frame("numDays" = NA, "hrSize" = NA) 

  for (i in 3:numUniqueDays){                           # zyzyzy Try this without the first two days
    # grab only position solutions during the first i days
    d2 = d1[d1$utime < endOfAllDays[i],]                  
    # ...but drop data from the first two days
    d2 = d2[d2$utime > endOfAllDays[2] ,]
    print(i)  
    if(nrow(d2) > 0){
      # calculate the home range for these PS
      hrSize = homeRange(
        easting = d2$easting, 
        northing = d2$northing,  
        tagName = paste(tagfm[[cTag]]$tagName, ", ", i, " days", sep=""),
        lims = hrlims, #c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=FALSE
      )
    } else {hrSize = 0}
    # save the answer
    hrVSdays[i,] = c(i, hrSize) 
  } # end for-loop

  # a plot
  plot(hrVSdays$numDays, hrVSdays$hrSize, pch=19, 
    main=paste(cTagNames[cTag], ": ", cProb*100, "% HR", sep="")
  )
  
  # save the answer for this fish
  allFishHR[[cTag]] = hrVSdays
  
} # end cTag for-loop     

z hr50 = allFishHR 
z hr95 = allFishHR 




# a two panel plot, 
par(mfrow=c(1,2))
# left pane, 50% KDE
par(mar=c(4,5.5,1,0.5)+0.1)
plot(hr50[[4]],type="l", las=1, bty="l", 
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(2.3,60), ylim=c(46,1200),  # these are to make (0,0) at the corner
  xlab="", ylab=""
)
for (cTag in 1:length(hr50)){
  points(hr50[[cTag]], type="l", lwd=2)
}
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("50% KDE (    )", side=2, line=3.9, cex=1.7)
mtext(expression(m^2),side=2, line=4, adj=0.621, cex=1.7)

text(x=c(38,38,38,51,51)+4, y=results$kde50, labels=c("ID 5","ID 4",
  "ID 3","ID 1","ID 2"), cex=1.5)  #c(results$tagName))
# add the a)
text(2.3,1200,"a)",cex=1.5)
# f60200 = ID5,  f60400=ID4, f60900=ID3, f60300=ID1, f61100=ID2
#abline(h=results$kde50)
                                                      
# left pane, 95% KDE
par(mar=c(4,5.5,1,0.5)+0.1)
plot(hr95[[4]],type="l", las=1, bty="l", 
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(2.3,60), ylim=c(245,6400), # these are to make (0,0) the corner
  xlab="", ylab=""
)
for (cTag in 1:length(hr95)){
  points(hr95[[cTag]], type="l", lwd=2)
}
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("95% KDE (    )", side=2, line=4.4, cex=1.7)
mtext(expression(m^2),side=2, line=4.5, adj=0.621, cex=1.7)
text(x=c(38,38,38,51,51)+4, y=results$kde95, labels=c("ID 5","ID 4",
  "ID 3","ID 1","ID 2"), cex=1.5)  #c(results$tagName))
# add the b)
text(2.3,6400,"b)",cex=1.5)



# ...also create these same curves for 2008 fish as if there had only been 50m arrays
# ...you'll need 'rotate()' and 'chop()' which are in 'testing array spacing.r' for now
# ...the two rish of interest are 2008 f60300, f61100, tagfm[[4]], tagfm[[5]]

cmd=md[[2]]
s60300 = subset(tagfm[[4]]$data, select=c(utime,easting,northing))
r60300 = rotate(s60300$utime,s60300$easting, s60300$northing, spin=45)
c60300 = chop(r60300$fisht, r60300$fishx, r60300$fishy, spacing=50)
u60300 = rotate(c60300$fisht,c60300$fishx,c60300$fishy, spin=-45)
plot(s60300$easting,s60300$northing,pch=19)
points(u60300$fishx,u60300$fishy,pch=19,col="blue")


s61100 = subset(tagfm[[5]]$data, select=c(utime,easting,northing))
r61100 = rotate(s61100$utime,s61100$easting, s61100$northing)
c61100 = chop(r61100$fisht,r61100$fishx, r61100$fishy, spacing=50)  
u61100 = rotate(c61100$fisht,c61100$fishx,c61100$fishy, spin=-45)
plot(s61100$easting,s61100$northing,pch=19)
points(u61100$fishx,u61100$fishy,pch=19,col="blue")

# for 2008 fish see how things would have changed if the array spacing had been different
cProb = 0.5
for (cTag in 4:5){  

  cmd = md[[2]]
  numUniqueDays = length(unique(tagfm[[4]]$data$datiL$yday)) # data for 2008 deployment

  # get only the easting/northing data
  if(cTag==4){d1=u60300} else {d1=u61100}
  names(d1) = c("utime","easting","northing")
  
                          
  # create a vector of the index of the day of the run, 1-52 for example
  # ...there's got to be a prettier way of doing this, but...
  startOfFirstDay = unclass(as.POSIXct(
    strptime(cmd$taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
      origin="1970-1-1", tz="EST5EDT"))[1]
  endOfAllDays = startOfFirstDay + 86400 * 1:numUniqueDays
  
  # something to hold the changing HR for one fish
  hrVSdays = data.frame("numDays" = NA, "hrSize" = NA) 

  for (i in 3:numUniqueDays){                           # zyzyzy Try this without the first two days
    # grab only position solutions during the first i days
    d2 = d1[d1$utime < endOfAllDays[i],]                  
    # ...but drop data from the first two days
    d2 = d2[d2$utime > endOfAllDays[2] ,]
    print(i)  
    if(nrow(d2) > 0){
      # calculate the home range for these PS
      hrSize = homeRange(
        easting = d2$easting, 
        northing = d2$northing,  
        tagName = paste("u60300 or u61100", ", ", i, " days", sep=""),
        lims = hrlims, #c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=FALSE
      )
    } else {hrSize = 0}
    # save the answer
    hrVSdays[i,] = c(i, hrSize) 
  } # end for-loop

  # a plot
  plot(hrVSdays$numDays, hrVSdays$hrSize, pch=19, 
    main=paste(cTagNames[cTag], ": ", cProb*100, "% HR", sep="")
  )
  
  # save the answer for this fish
  allFishHR[[cTag+2]] = hrVSdays
  
} # end cTag for-loop                      

z hr50[6] = allFishHR[6]; hr50[7] = allFishHR[7]; 
z hr95[6] = allFishHR[6]; hr95[7] = allFishHR[7];


# a two panel plot, 
par(mfrow=c(1,2))
# left pane, 50% KDE
par(mar=c(4,5.5,1,0.5)+0.1)
plot(hr50[[4]],type="l", las=1, bty="l", 
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(2.3,60), ylim=c(46,1200),  # these are to make (0,0) at the corner 
  xlab="", ylab=""
)
for (cTag in 1:5){
  points(hr50[[cTag]], type="l", lwd=2)
}
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("50% KDE (    )", side=2, line=3.9, cex=1.7)
mtext(expression(m^2),side=2, line=4, adj=0.621, cex=1.7)
text(x=c(38,38,38,51,51)+4, y=results$kde50, labels=c("ID 5","ID 4",
  "ID 3","ID 1","ID 2"), cex=1.5)  #c(results$tagName))
# add the a)
text(2.3,1200,"a)",cex=1.5)
# f60200 = ID5,  f60400=ID4, f60900=ID3, f60300=ID1, f61100=ID2

# add the dashed lines for hr50 estimates with smaller array spacing
points(hr50[[6]], type="l", lty="dashed", lwd=2)
points(hr50[[7]], type="l", lty="dashed", lwd=2) 
                                                      
# right pane, 95% KDE
par(mar=c(4,5.5,1,0.5)+0.1)
plot(hr95[[4]],type="l", las=1, bty="l", 
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(2.3,60), ylim=c(245,6400), # these are to make (0,0) the corner
  xlab="", ylab=""
)
for (cTag in 1:5){
  points(hr95[[cTag]], type="l", lwd=2)
}
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("95% KDE (    )", side=2, line=4.4, cex=1.7)
mtext(expression(m^2),side=2, line=4.5, adj=0.621, cex=1.7)
text(x=c(38,38,38,51,51)+4, y=results$kde95, labels=c("ID 5","ID 4",
  "ID 3","ID 1","ID 2"), cex=1.5)  #c(results$tagName))
# add the b)
text(2.3,6400,"b)",cex=1.5)

points(hr95[[6]], type="l", lty="dashed", lwd=2)
points(hr95[[7]], type="l", lty="dashed", lwd=2)
# add one more "ID1" label
text(x=51+4,y=4300,labels="ID 1",cex=1.5) 


# now calcualte how much the final HR estiamte decreased for the 2008 fish
# from using the 125m array to using the similuated 50m array
# ...in hr50, list elements 4 and 6 go together
# ... and elements 5 and 7 go together
( hr50[[4]]$hrSize[51] - hr50[[6]]$hrSize[51] ) / hr50[[4]]$hrSize[51] #=3.8%
( hr50[[5]]$hrSize[51] - hr50[[7]]$hrSize[51] ) / hr50[[5]]$hrSize[51] #=1.5%


( hr95[[4]]$hrSize[51] - hr95[[6]]$hrSize[51] ) / hr95[[4]]$hrSize[51] #=12.5%
( hr95[[5]]$hrSize[51] - hr95[[7]]$hrSize[51] ) / hr95[[5]]$hrSize[51] #=6.6%





###############################################################################
###############################################################################
### Bootstrapping home range estimates
###############################################################################
###############################################################################
bootHR <- function(dat, by="day", nboot=100, prob=0.95, progressbar=FALSE,
  bootplot=FALSE, pts=FALSE, drawplot=FALSE, ...
  # 'by' will subset the data by whatever you choose, say 'day', then the 
  #   bootstrap will pick randomly from the 'days' 
  ) {
  if (progressbar) {                                     
    require(tcltk)
    pb <- tkProgressBar("hr bootstrap",min=0,max=nboot)
  }
  time <- dat$datiL
  dat <- subset(dat,select=c("northing","easting"))
  timecat <- cut(time, breaks=by)
  datsplit <- split(dat,timecat)
  nt <- length(levels(timecat))
  bootres <- numeric(nboot)
  if (bootplot) with(dat,plot(easting,northing,pch="."))
  for (i in 1:nboot) {
    bootsamp <- sample(nt,size=nt,replace=TRUE)
    bootdat <- do.call(rbind,datsplit[bootsamp])
    if (bootplot) with(bootdat,points(easting,northing,pch=".",col=i+1))
    bootres[i] <- with(bootdat,
      homeRange(easting, northing, prob=prob, pts=pts, drawplot=drawplot, ...)
    )
    if (progressbar) setTkProgressBar(pb,i)
  }
  if (progressbar) close(pb)
  bootres
} # end bootHR

    
system.time(boottest <- bootHR(dat=z0, by="day", #by = "2 days" or "week"
  prob=0.95, lims=hrlims,nboot=5, progressbar=TRUE, bootplot=TRUE))

# now to put this all together and run it 1000 for each tag
# create a list to hold the results
hrboots = list(
  list(
    tagName = NA, # which tag
    originalHR = NA, # a single number, HR estimate using all data
    bootResults = NA # a vector holding the 1000 boot strapped HR estimates
  ),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA) # this one for all fish combined
)


#####################################################################

cProb = 0.95           
for (i in 1:length(tagfm)){
  # grab things pertinent to this tag
  for (j in 1:length(md)){
    if(tagfm[[i]]$deployment == md[[j]]$deployment){ cmd = md[[j]] } 
  } # end for j loop
  
  # name
  hrboots[[i]]$tagName = tagfm[[i]]$tagName
  
  # find HR estimate will all data
  # ...except I don't want to use days 1 or 2
  hrUtime = tagfm[[i]]$data$utime
  hrEasting = tagfm[[i]]$data$easting
  hrNorthing = tagfm[[i]]$data$northing
  # ... figure out the end of day 2...pick out the day and add 2
  tempday = cmd$taggingDay
  substr(tempday,10,12) = as.character(as.numeric(substr(cmd$taggingDay,10,12))+2)
  tempUtime=as.POSIXct(strptime(tempday, "%Y/%b/%d", tz="EST5EDT"), origin="1970-1-1")
  # drop all data before tempUtime
  hrEasting = hrEasting[hrUtime > tempUtime ]
  hrNorthing = hrNorthing[hrUtime > tempUtime ]
  
  hrboots[[i]]$originalHR = homeRange(hrEasting, hrNorthing, 
    prob=cProb, lims=hrlims, tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, 
    sdlEN=cmd$sdlEN, pts=TRUE, drawplot=TRUE)  

  # now the boot strapping...first put the hrUtime, hrEasting, hrNorthing into a dataframe
  hrDF = data.frame(easting=hrEasting, northing=hrNorthing)
  hrboots[[i]]$bootResults = bootHR(tagfm[[i]]$data, #by = "day" or "2 days" or "week"
    prob=cProb, lims=hrlims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
    tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
    drawplot=FALSE
  )

}

 
# now calculate the 95% KDE for all fish combined...z0 is required
i=6
everyfish =     the answer I get with z0 seems funky...do it again with this
  = 
hrboots[[i]]$originalHR = bob=homeRange(z0$easting, z0$northing, 
  prob=cProb, lims=hrlims, tagName="All", reefEN=cmd$reefEN, 
  sdlEN=cmd$sdlEN, pts=FALSE, drawplot=FALSE)  

hrboots[[i]]$bootResults = sam=bootHR(z0, #by = "day" or "2 days" or "week"
  prob=cProb, lims=hrlims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
  tagName="All", reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
  drawplot=FALSE
)


cTag = 1
plot(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19, xlim=c(0,6), ylim=c(1000,6000))
cTag = 2
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 3
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 4                           
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 5
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19) 
cTag = 6       
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19) 
    

points(1:6,c(hrboots[[1]]$originalHR, hrboots[[2]]$originalHR,hrboots[[3]]$originalHR,
  hrboots[[4]]$originalHR, hrboots[[5]]$originalHR, hrboots[[6]]$originalHR),
  pch=19,col="red")

# now pick out the 95% confidence interval (2.5% and 97.5%  points), that is 
# 25, 975 our of 1000 different runs
cTag=4
plot(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)])
abline(v=c(25,975))
abline(h=hrboots[[cTag]]$originalHR)
print(hrboots[[cTag]]$tagName)
c(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][25],
  hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][975]
)


save("hrboots", file="C:/zy/Telemetry/R summary files/hrboots 2010April08.rdata")
load("C:/zy/Telemetry/R summary files/hrboots 2011April08.rdata")


################################################################################
# GGPLOT FIGURES

# Use this code for making many figures


# main effects change x and y for various relationships
# notes: 
#   tod x-axis breaks = c(0,6,8,12,17,19,24)  cyclic
#   dirL x-axis breaks = c(0,90,180,270,360)  cyclic
#   magL x-axis breaks = seq(0,0.35, by=0.05)
#   lunarIndex x-axis breaks = c(1,7,15,23,30)   cyclic
#   gag speed y-axis breaks = seq(0.08,0.26, by=0.02)
#   temperature x-axis breaks=seq(14,26,by=2)
#
#   altitude coord_cartesian(xlim=c(1,30), ylim=c(0,10)) 
#   altitude breaks=c(0,1,2,3,seq(4,10,by=2)))
#   dtr breaks = seq(6,28, by=2)  
     
# z0 is all fish      
# z6, z6day, z6night are just 2007 fish with altitude data                       
                                                                    
ggplot(z6night, aes(x=lunarIndex, y=altitude, group=ID, colour=ID, fill=ID)) + 
  geom_point(alpha=0.05) + 
  geom_smooth(method="gam",formula=y~s(x,bs="cc"),lwd=1.3) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +  #,bs="cc"
  coord_cartesian(xlim=c(1,30), ylim=c(0,10)) +    # range(z0$temperature,na.rm=T)    range(z0$magL,na.rm=T)
  theme_bw() + 
  scale_x_continuous("Lunar Index",breaks = c(1,7,15,23,30)) +
  scale_y_continuous("Altitude (m)", breaks=c(0,1,2,3,seq(4,10,by=2))) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))
                 
                    
  
  EXAMPLES
  geom_text(x=22, y=27, label="a)")
  opts(axis.title.y = theme_text(colour = 'red', angle = 45, size = 10, hjust = -0.2, vjust = 0.5, face = 'italic'))
  opts(axis.ticks.margin = unit(1, "cm"))
  opts(axis.title.y = theme_text(hjust = -0.2)) +
  opts(axis.ticks.margin = unit(1, "cm"))


# full time series of individuals dtr and alt 
ggplot(z5, aes(x=tod, y=speed))+#, group=tagName, colour=tagName, fill=tagName)) +
  geom_point(size=1, alpha=0.2) + 
  #geom_smooth(method="gam",formula=y~s(x)) +
  #geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
  coord_cartesian(xlim=c(0,24), ylim=c(0,0.8)) +
  theme_bw() + 
  facet_wrap(~Date) +
  scale_y_continuous("Travel Speed (m/s)", breaks=seq(0,0.80,by=0.20)) +
  scale_x_continuous("Time of Day", breaks = seq(0,18,by=6))
  
   +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))
  

                        
ggplot(z6night, aes(x=lunarIndex, y=altitude, group=tagName, colour=tagName, fill=tagName))+
  geom_point(alpha=0.05) + 
  geom_smooth(method="gam",formula=y~s(x,bs="cc")) +                #,bs="cc"
  geom_smooth(aes(group=1),colour="black", fill="black", lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +
  coord_cartesian(xlim=c(1,30), ylim=c(0,10)) +
  theme_bw() + 
  #facet_wrap(~dod) +
  scale_x_continuous("Lunar Index", breaks = c(1,7,15,23,30)) +
  scale_y_continuous("Altitude (m)",breaks=c(0,1,2,3,4,6,8,10))
  


# look at daily EN plots of individual fish
ggplot(z5, aes(x=easting, y=northing))+
  geom_point(alpha=0.05) + 
  #coord_cartesian(xlim=c(0,60), ylim=c(0,10)) +
  theme_bw() + 
  facet_wrap(~dod) +
  scale_x_continuous("Easting (m)")+#, breaks = seq(0,60,by=20)) +
  scale_y_continuous("Northing (m)")#,breaks=c(0,1,2,3,4,6,8,10))



# full time series of individuals dtr with fewer days...for the 2012 MarBio talk
# pick a few days
zTemp = z4[(z4$Date > "2008-11-01") & (z4$Date < "2008-11-11"),]

ggplot(zTemp, aes(x=tod, y=dtr))+#, group=tagName, colour=tagName, fill=tagName)) +
  geom_point(size=1, alpha=0.5) + 
  coord_cartesian(xlim=c(0,24), ylim=c(0,80)) +
  theme_bw() + 
  facet_wrap(~Date) +
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,80,by=20)) +
  scale_x_continuous("Time of Day", breaks = seq(0,18,by=6)) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))
  
  
# another for marBio 2012
ggplot(z4, aes(x=tod, y=dtr, colour=z4$ID[4]))+#, group=ID, colour=ID, fill=ID)) +
  geom_point(size=1, alpha=0.5) + 
  geom_smooth(method="gam",lwd=1.3,formula=y~s(x,bs="cc")) +   
  #geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +
  coord_cartesian(xlim=c(0,24), ylim=c(0,80)) +
  theme_bw() + 
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,80,by=10)) +
  scale_x_continuous("Time of Day", breaks = seq(0,24,by=6)) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))




# keep this for a publication plot dtr v tod, with points for one fish, curves for each, one curve for all
ggplot(z4, aes(x=tod, y=dtr)) +
  geom_point(size=1, alpha=0.2, colour="black") + 
  geom_smooth(            linetype=1,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +   
  geom_smooth(data=z1,linetype=2,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +
  geom_smooth(data=z2,linetype=3,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +
  geom_smooth(data=z3,linetype=4,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +
  geom_smooth(data=z5,linetype=5,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +
  coord_cartesian(xlim=c(0,24), ylim=c(0,80)) +
  theme_bw() + 
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,80,by=5)) +
  scale_x_continuous("Time of Day", breaks = seq(0,24,by=6)) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90)) +
  opts(legend.position="none")


                                                                             

# adapt this code for several plots 
ggplot(z0, aes(x=lunarIndex, y=dtr, group=ID, colour=ID, fill=ID)) +    #z6 for altitude plots
  #geom_point(size=1, alpha=0.3) + 
  geom_smooth(method="gam",lwd=1.3,formula=y~s(x,bs="cc")) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) + #   ,bs="cc"
  coord_cartesian(xlim=c(1,30), ylim=c(5,30)) +
  theme_bw() + 
  #scale_x_continuous("Time of Day", breaks = c(0,6,8,12,17,19,24)) +
  #scale_x_continuous("Distance From the Reef (m)", breaks = seq(0,50,by=10)) +
  scale_x_continuous("Lunar Index", breaks=c(1,7,15,23,30)) +
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,80,by=5)) +
  #scale_y_continuous("Gag Travel Speed (m/s)", breaks=seq(0.1,0.2,by=0.01)) +
  #scale_y_continuous("Altitude (m)", breaks=0:10) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))



















##############################################################################
# Otolith work, using Deb's otolith data
results
biometrics = importBiometricData()
# for some reason the very first element (year) has strange characters, so
# replace it and drop the bad factor level.
biometrics[1,1] = biometrics[2,1]
biometrics$year1 = drop.levels(biometrics$year1, reorder=FALSE)

# pick out the relevant data
b1 = biometrics[,c("year1","month1","reefID1","HBSB","replicate","weight1",
  "girth1",
  "TL1","tagID","mmNumber","recoveredTagID","year2","month2",
  "TL2","weight2","girth2","debAnnuli","debGrowth",
  "debAgeclass","resolvedAgeclass","otoRadius","ultimateAnnulus",
  "penultimateAnnulus","growthIncrement")]
# get some prettier names
names(b1[4]) <- "treatment"
names(b1)
# that didn't work





# narrow it to the right rows and columns
hb = b1[(b1$HBSB=="HB") & (!is.na(b1$HBSB)) & (!is.na(b1$mmNumber)) ,]  

sb = b1[(b1$HBSB=="SB") & (!is.na(b1$HBSB)) & (!is.na(b1$mmNumber)) ,]  

# some plots Deb asked for
# Is there a difference in the length weight relationship between the treatments



plot(hb$TL2, hb$weight2, pch=19, 
  xlab="Total Length (mm)", ylab="Weight (g)")  
points(sb$TL2, sb$weight2, pch=2, col="blue")  
# fit length-weight curves to these using nls
nlhb = nls(weight2 ~ aa*TL2^bb, data=hb, start=list(aa=0.00001, bb=2.7))
nlsb = nls(weight2 ~ aa*TL2^bb, data=sb, start=list(aa=0.00001, bb=2.7))
# add curves to plot      
fnhb = function(TL){coef(nlhb)[1] * TL^coef(nlhb)[2]}
fnsb = function(TL){coef(nlsb)[1] * TL^coef(nlsb)[2]}                      
curve(fnhb(x), add=TRUE)
curve(fnsb(x), add=TRUE, lty=2)



# Is there a difference between treatments in age class
plot(as.numeric(hb$resolvedAgeclass), hb$TL2,  pch=19, col="red", 
  xlab="Age Class", ylab="Total Length (mm)")  
points(sb$resolvedAgeclass, sb$TL2, pch=3, col="blue")  

# Is there a difference between treatments in back-calculated TL
plot(hb$TL2, hb$la,  pch=19, 
  xlab="Length at Capture (mm)", ylab="Back-calculated Length (mm)")    
points(sb$TL2, sb$la, pch=2)  
# fit lines to each treatment
reghb2 = lm(la ~ TL2, hb)
regsb2 = lm(la ~ TL2, sb)
abline(a=reghb2$coefficients[1], b=reghb2$coefficients[2])
abline(a=regsb2$coefficients[1], b=regsb2$coefficients[2], lty=2)



# look at TL and oto increments by treatment and rep
plot(x=rep(1,nrow(hb1)), y=hb1$TL2, pch=19,
  xlim=c(1,6), ylim=range(b1$TL2,na.rm=T))
points(x=rep(2,nrow(hb2)), y=hb2$TL2, pch=19)
points(x=rep(3,nrow(hb3)), y=hb3$TL2, pch=19)
points(x=rep(4,nrow(sb1)), y=sb1$TL2, pch=19)
points(x=rep(5,nrow(sb2)), y=sb2$TL2, pch=19)
points(x=rep(6,nrow(sb3)), y=sb3$TL2, pch=19)




# I want to compare the size of my fish, but they were all caught at different
# times so I can't really compare the length measurements I have.  I will
# back-calculate their lengths to 31 Dec using the equation is Deb's paper.
#
# Here are two equations
# la = [ (a + b * ra)/(a + b * rc) ] * lc
# lc = a + b * rcR
#
# la = back-calculated length to opaque zone 'a' 
# a = intercept from the linear regression of total length as a function of
#     otolith radius, rc
# b = slope from the same linear regression 
# ra = otolith radius to opaque zone 'a'
# rc = total otolith radius at time of capture
# lc = total length at time of capture

# do the length - otolith radius regression for each group
# hard bottom
reghb = lm(TL2 ~ otoRadius, hb)
summaryhb = summary(reghb)
ahb = summaryhb$coefficients[1]
bhb = summaryhb$coefficients[2]

# soft bottom
regsb = lm(TL2 ~ otoRadius, sb)
summarysb = summary(regsb)     
asb = summarysb$coefficients[1]
bsb = summarysb$coefficients[2]

# a plot
plot(hb$otoRadius, hb$TL2, pch=19, 
  xlim=c(0.5,1.5), ylim=c(300,800),
  xlab="Otolith Radius at Capture (mm)", ylab="Total Length at Capture (mm)")
abline(a=ahb, b=bhb)
points(sb$otoRadius, sb$TL2, pch=2)
abline(a=asb, b=bsb, lty=2)



reg1 = lm(TL2 ~ otoRadius, b1)
sum1 = summary(reg1)
aa = sum1$coefficients[1]                   
bb = sum1$coefficients[2]

plot(b1$TL2, b1$otoRadius, pch=19)
abline(a=aa, b=bb)

# back calculate length-at-age

hb$la = ( (ahb + bhb * hb$ultimateAnnulus)/(ahb + bhb * hb$otoRadius) ) * hb$TL2
sb$la = ( (asb + bsb * sb$ultimateAnnulus)/(asb + bsb * sb$otoRadius) ) * sb$TL2





b1$la = ( (aa + bb * b1$ultimateAnnulus)/(aa + bb * b1$otoRadius) ) * b1$TL2
# how do these compare to each other
plot(b1$TL2, b1$la, pch=19, xlab="Total Length at Capture",
  ylab="Back-calculated Total Length")
abline(0,1)





















































# OLD STUFF BELOW HERE

par(mfrow=c(2,3))

cTag = 1; cmd=md[[1]]
plot(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing,pch=19,cex=0.1,
  xlim=xLimits, ylim=yLimits,
  main=tagfm[[cTag]]$tagName)
points(cmd$reefEN$easting, cmd$reefEN$northing,pch=19,col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing,pch=19,col=4)

cTag = 2; cmd=md[[1]]
plot(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing,pch=19,cex=0.1,
  xlim=xLimits, ylim=yLimits,
  main=tagfm[[cTag]]$tagName)
points(cmd$reefEN$easting, cmd$reefEN$northing,pch=19,col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing,pch=19,col=4)

cTag = 3; cmd=md[[1]]
plot(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing,pch=19,cex=0.1,
  xlim=xLimits, ylim=yLimits, 
  main=tagfm[[cTag]]$tagName)
points(cmd$reefEN$easting, cmd$reefEN$northing,pch=19,col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing,pch=19,col=4)

cTag = 4; cmd=md[[2]]
plot(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing,pch=19,cex=0.1,
  xlim=xLimits, ylim=yLimits, 
  main=tagfm[[cTag]]$tagName)
points(cmd$reefEN$easting, cmd$reefEN$northing,pch=19,col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing,pch=19,col=4)

cTag = 5; cmd=md[[2]]
plot(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing,pch=19,cex=0.1,
  xlim=xLimits, ylim=yLimits, 
  main=tagfm[[cTag]]$tagName)
points(cmd$reefEN$easting, cmd$reefEN$northing,pch=19,col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing,pch=19,col=4)



# more pictures
par(mfrow=c(2,3))

cTag = 1; cmd=md[[1]]
plot(hexbin(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing), main=tagfm[[cTag]]$tagName)

cTag = 2; cmd=md[[1]]
plot(hexbin(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing),main=tagfm[[cTag]]$tagName)

cTag = 3; cmd=md[[1]]
plot(hexbin(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing),main=tagfm[[cTag]]$tagName)

cTag = 4; cmd=md[[2]]
bins = hexbin(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing, 
  xbins=30,xbnds=xLimits, ybnds=yLimits)
gplot.hexbin

plot(bins,
  main=paste("Positions of ",tagfm[[cTag]]$tagName,sep=""), 
  xlab="Easting (m)", ylab="Northing (m)"
)


text(0, 0, labels="bob")
mtext("Easting (m)", side=1, line=0, cex=2)
mtext("Northing (m)", side=2, line=0, cex=2)



xLimits 
yLimits 

cTag = 5; cmd=md[[2]]
plot(hexbin(tagfm[[cTag]]$data$easting,tagfm[[cTag]]$data$northing),main=tagfm[[cTag]]$tagName)
  
  
  
  
  
#########################################################################
# ADCP plots
# ...for this plot I want the lunar phase to show from the beginning of the 
# ...2007 depoyment even though there's no water flow data...to do that I'll
# ...make up data lines which will be empty except for the lunar phase stuff

# import ADCP data
ad = importADCPdata()
# get just 2007 and 2008
ad = ad[ad$utime < 1230768000, ]    #1230768000 = 2009 Jan 1 midnight GMT
ad$datiL = as.POSIXct(ad$datiL)
# pick only some columns
ad = ad[,c(3,6,11)]
ad[,3] = ad[,3]/1000

# create empty data lines
# tagging start on 9 Dec 16:01:00, ADCP starts on 19 Dec 16:01:00.  That's 
# 11 days and 240 hours, with data every 10min...=1440 data lines
dday = rep(9:19,each=144)
hhour = rep(0:23,each=6,times=11)
mmin = rep(seq(1,51, by=10),264)

temp1 = paste("2007-12-",dday," ",hhour,":",mmin,":00",sep="")
temp2 = strptime(temp1, "%Y-%m-%d %H:%M:%S", tz="EST5EDT")
temp3 = as.POSIXct(temp2, origin="1970-1-1", tz="EST5EDT")
temp4 = data.frame(datiL = temp3[temp3 < ad$datiL[1]])


# now combine these
newad=merge(ad, temp4, by="datiL", all=TRUE)
# add a year indicator
newad$yr = as.factor(ifelse(
  newad$datiL < as.POSIXct("2008-05-01", origin="1970-1-1", tz="EST5EDT"),
  2007,2008))



# add a column for creating a sine wave indicating lunar phase
newad$moon = NA
# split the years apart while the moon is added
nad7 = newad[newad$yr == 2007, ]
nad8 = newad[newad$yr == 2008, ]

# add an index column 2007...
# ...and each day has 24hrs with 6 lines each = 144 lines, and there are 30 days
# ..in the lunar cycle...30*144 = 4320 lines per 2 pi radians or (2160 lines = pi rads)

nad7$index = (1:nrow(nad7)*pi/2160) + pi # add pi to make it 1 at full moon and -1 at new
# ... and by chance 9 Dec (the first day) is a new moon so I don't have to 
# ... shift left or right except to make the full moon be up at 1

# create the lunar curve
nad7$moon = cos(nad7$index) 
# look and add lines where the new and full moons should be, as a check...looks good
plot(nad7$datiL, nad7$moon, type="l")
abline(v=as.POSIXct("2007-12-09", origin="1970-1-1", tz="EST5EDT")) #new moon
abline(v=as.POSIXct("2007-12-24", origin="1970-1-1", tz="EST5EDT")) #full
abline(v=as.POSIXct("2008-01-08", origin="1970-1-1", tz="EST5EDT")) #new

# add an index column 2008...
nad8$index = ((1:nrow(nad8))*(pi/2160)) - (497*(pi/2160))
# ... by looking at it I see that 14 Oct (the first full moon) is 497 lines down
# 
# create the lunar curve
nad8$moon = cos(nad8$index)
plot(nad8$datiL,nad8$moon, type="l")
abline(v=as.POSIXct("2008-10-15", origin="1970-1-1", tz="EST5EDT")) # full moon
abline(v=as.POSIXct("2008-10-29", origin="1970-1-1", tz="EST5EDT")) # new
abline(v=as.POSIXct("2008-11-13", origin="1970-1-1", tz="EST5EDT")) # full
abline(v=as.POSIXct("2008-11-28", origin="1970-1-1", tz="EST5EDT")) # new
abline(v=as.POSIXct("2008-12-13", origin="1970-1-1", tz="EST5EDT")) # full
abline(v=as.POSIXct("2008-12-27", origin="1970-1-1", tz="EST5EDT")) # new
#this looks close enough

# put them back together without the index
newerad = rbind(nad7[,-6], nad8[,-6])
names(newerad) = c("datiL", "Temperature (Celcius)", "Water Speed (m/s)", "yr",
  "Lunar Phase")       

meltedad = melt(newerad, id.vars=c("datiL", "yr"))
names(meltedad) = c("Date", "yr", "variable", "value")

ggplot(meltedad, aes(x=Date, y=value)) + 
  geom_line() +
  facet_grid(variable~yr, space="fixed",scales="free") +  
  scale_x_datetime(major = "14 days") + # specifying these works around a bug
  theme_bw() +
  opts(axis.title.y = theme_text(colour = 'white')) +
  opts(axis.title.x = theme_text(size=20)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))




# ztemp = z0[!is.na(z0$dirL),]  don't use this so lunarIndex is on same scale
ztemp = z0
ztemp$lunarIndex = ztemp$lunarIndex/15

ggplot(ztemp, aes(x=datiL,y=magL)) +
geom_line() +
facet_grid(.~yr, scales="free_x") +
#opts(title="Water Temperature") +
xlab("Date") + 
ylab("Water Speed (m/s)") +
theme_bw()  
  
ggplot(z0,aes(x=utime,y=magL))+geom_point()+facet_grid(.~yr,scales="free_x")
     opts(title="All Fish")

  
                
  
                      
    
# dtr histogram
par(mfrow=c(2,3))
bks = c(20,20,20,30,30)
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$dtr, breaks=bks[i], freq=T, main=tagfm[[cTag]]$tagName,
    xlim=c(0,100))
  abline(v=median(tagfm[[cTag]]$data$dtr), col="red")
}

hist(z0$dtr, breaks=30, freq=F, xlim=c(0,80), ylim=c(0,0.04),
  col="grey", las=1,
  main="",
  xlab="Distance to the Reef (m)")
abline(v=median(z0$dtr),lwd=4)



# speed histogram
par(mfrow=c(2,3))
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$speed, breaks=30, freq=F, main=tagfm[[cTag]]$tagName)
  abline(v=mean(tagfm[[cTag]]$data$speed), col="red")
}


hist(z0$speed, breaks=30, freq=F, col="grey", las=1, 
  main="",
  xlab="Gag Speed (m/s)")
abline(v=median(z0$speed),col="black",lwd=4)






# interval histogram...for this you want the filtered, but not minuteMeaned data
# ... too bad...I have to get that fresh now...you need some code from 'chapter 3 xxxxx.r"
cTagNames             

# import the 'tag' dataset
load("C:/zy/Telemetry/R summary files/tag 2011Mar16.rdata")
                                                    
ttf = list() # filtered tag data
for (i in 1:length(cTagNames)){
  print(cTagNames[i])
  ttf[[i]] = filterALPSdata(df1=tag[[i]], cnF=1.5, speedF=0.8, minuteMean=F)
}

# combine them 
tempInterval = c(ttf[[1]]$data$interval, ttf[[2]]$data$interval, 
  ttf[[3]]$data$interval, ttf[[4]]$data$interval, ttf[[5]]$data$interval)

par(mfrow=c(2,3))
bks = seq(0,max(tempInterval),by=2)
for (i in 1:length(cTagNames)){
  cTag=i; 
  hist(ttf[[cTag]]$data$interval, breaks=bks, freq=T, main=ttf[[cTag]]$tagName,
    xlim=c(0,60))
  abline(v=median(ttf[[cTag]]$data$interval), col="red")
}                                                                             



par(mar=c(5,4,2,2)+0.1)
hist(tempInterval, breaks=bks, freq=F, col="grey", las=1, main="",
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,60), ylim=c(0,0.25), 
  xlab="Interval Length (s)", ylab="")
abline(v=median(tempInterval),lwd=4)
mtext("Density", side = 2, line = 4, cex = 1.5)




# ... which element in temp1 represents the first 99% of all elements
temp1 = tempInterval[order(tempInterval)]
c1 = round(0.50*length(temp1))
c2 = round(0.95*length(temp1))

abline(v=c(temp1[c1],temp1[c2]), col=c("blue","blue"), lwd=2)

                                                     
                                                                              
##############################################################################
# I want to calcualte the mean number of position solutions per day...I've lost
# the code where I first did this...so redo it here...do this before the minuteMean
str(ttf)


temp1 = list() # each element will be a list for one fish...each element of that
       # sublist will be the number of hits each day
       #...don't use day 1, the tagging day

for (cTag in 1:length(ttf)){
  dat = ttf[[cTag]]$data
  time = dat$datiL
  timecat = cut(time, breaks="day")
  datsplit = split(dat,timecat)
  temp2 = c()
  for (i in 2:length(datsplit)){
    temp2 = c(temp2,nrow(datsplit[[i]])[1])
  }  
  temp1[[cTag]] = temp2
  mean(temp2)
}

# now look at the results for individual fish
mean(temp1[[5]])

# now look at results for all fish together...on day 1 how many hits did everyone get
# ...what's that for every day, what's the mean
temp3 = list()
# the first 37 days (38 - first day)
for (i in 1:37){
  temp3[[i]] = sum(temp1[[1]][[i]]+temp1[[2]][[i]]+temp1[[3]][[i]]+
    temp1[[4]][[i]]+temp1[[5]][[i]])
}                                 
# days with only 2008 fish
for (i in 38:50){
  temp3[[i]] = sum(temp1[[4]][[i]]+temp1[[5]][[i]])
} 
# find mean of all numbers in temp3
temp4 = c()
for (i in 1:length(temp3)){
  temp4 = c(temp4, temp3[[i]])
}
temp5 = mean(temp4/5)   # f=5 fish
### I've checked this answer two ways and get the same thing, but the number seems wrong


mean(temp3, rm.na=T)



# now do this for all fish combined
alltime = data.frame(ttf[[1]]$data$datiL, ttf[[2]]$data$datiL)





##############################################################################
##############################################################################
# a four panel figure...using stuff generated above
par(mfrow=c(2,2))
# 1. Time interval between detections

par(mar=c(5,4,2,2)+0.1)
bks = seq(0,max(tempInterval),by=2)
hist(tempInterval, breaks=bks, freq=F, col="grey", las=1, main="",
  cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,40), ylim=c(0,0.25), 
  xlab="", ylab="")
abline(v=median(tempInterval),lwd=4)
mtext("Density", side = 2, line = 3.8, cex = 1.5)
mtext("Interval Length (s)", side = 1, line = 3, cex = 1.5)
text(40,0.25,"a)", cex=1.5)

# 2. distance to reef
par(mar=c(5,4,2,2)+0.1)
bks = seq(0,max(z0$dtr),by=1)
hist(z0$dtr, breaks=90, freq=F, col="grey", las=1, cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,60), ylim=c(0,0.05),
  xlab="", ylab="", main="")
abline(v=median(z0$dtr),lwd=4)
mtext("Density", side = 2, line = 4, cex = 1.5)
mtext("Distance from Reef (m)", side = 1, line = 3, cex = 1.5)
text(60,0.05,"b)", cex=1.5)

# 3. Gag Travel Speed
hist(z0$speed, breaks=30, freq=F, col="grey", las=1, cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,0.6), ylim=c(),
  xlab="", ylab="", main="")
abline(v=median(z0$speed),col="black",lwd=4)
mtext("Density", side = 2, line = 3.8, cex = 1.5)
mtext("Gag Travel Speed (m/s)", side = 1, line = 3, cex = 1.5)
text(0.6,3.8,"c)", cex=1.5)

# 4. altitude...this one from below
barplot(altVec, horiz=T, beside=T, col="grey", las=1,
  xlab="",
  ylab="", cex.axis=1.5, cex.names=1.5,
  cex.lab=1.5, names=c(1,"",3,"",5,"",7,"",9),  #possAlts,
  xlim=c(0,0.7), ylim=c(1,10)
)
mtext("Altitude (m)", side = 2, line = 4, cex = 1.5)
mtext("Density", side = 1, line = 3, cex = 1.5)
text(0.6,10,"d)", cex=1.5)










### a figure
cTag = 5                                
cmd = md[[2]]
par(mfrow=c(1,3))


par(mar=c(5,4,2,2)+0.1)
plot(hexbin(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing,
  xbnds=cmd$plotLimits$easting, ybnds=cmd$plotLimits$northing, 
  xbins=40),
  #cex.lab=1.5, cex.axis=1.5, bty="l",
  xlab="Easting (m)", ylab="Northing (m)")

gplot.hexbin(
  hexbin(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing,
    xbnds=cmd$plotLimits$easting, ybnds=cmd$plotLimits$northing, 
    xbins=40),
    lcex=1,
  xlab="Easting (m)", ylab="Northing (m)"
)


plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=19, cex=1, col=2)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, cex=1, col=4)

plot(tagfm[[cTag]]$data$datiL, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  ylim=cmd$plotLimits$northing, main=tagfm[[cTag]]$tagName)
abline(h=cmd$reefEN$northing, col=2)
abline(h=cmd$sdlEN$northing, col=4)


plot(tagfm[[cTag]]$data$datiL, tagfm[[cTag]]$data$depth, pch=19, cex=0.2,
  ylim=rev(range(tagfm[[cTag]]$data$depth)))




################################################################################
# fraction of detections by hour  
cTag=3                      
dat = ttf[[cTag]]$data  
time = dat$datiL
dat <- subset(dat,select=c("northing","easting"))
timecat <- cut(time, breaks="hour")
datsplit <- split(dat,timecat)
hourlyFrac = length(datsplit)/(60*30)

fracHits = vector(length=length(datsplit))

for(i in 1:length(datsplit)){
  fracHits[i]=nrow(datsplit[[i]])/(60*30)
}
 

par(mar=c(5,4,2,2)+0.1)
plot(as.POSIXlt(levels(timecat)),fracHits,type="l", las=1, bty="l", 
  cex.lab=1.5, cex.axis=1.5,
  ylim=c(0,0.8),
  xlab="Date", ylab="Fraction of Recorded Positions") 



##########################################################################    
### Altitude distribution
par(mfrow=c(2,3))
possAlts = 1:13
 
for (i in 1:3){ # only the 2007 fish have altitudes

  # ...how do I want to round
  alt = ceiling(tagfm[[i]]$data$altitude[ tagfm[[i]]$data$altitude >= 0 ])
  altVec = c()
  for (j in 1:length(possAlts)){ 
    altVec = rbind(altVec, sum(alt == possAlts[j],na.rm=T))
  }
#  # make all the 'zero' values into 'one' values, and get rid of 'zero' element
#  altVec[2] = altVec[1] + altVec[2]; altVec[1]=0
#  altVec = altVec[2:14]
#  possAlts = 1:13

  altVec = altVec/sum(altVec,na.rm=T)
# remove altitudes 10-13 because they are or are close to zero
  altVec = head(altVec,9)
  possAlts = head(possAlts,9)


  barplot(altVec, horiz=T, beside=T,
    main=paste(tagfm[[i]]$tagName,"Altitude"),
    xlab="Frequency", ylab="Altitude (m)", cex.axis=1.5, cex.names=1.5,
    cex.lab=1.5, names=possAlts,
    ylim=c(0,15))

#  gap.barplot(altVec,gap=c(0.25, 0.6),xlab="Altitude (m)", 
#    ytics=c(0,0.05, 0.1, 0.15, 0.2, 0.6383233),
#    xtics=possAlts, xaxlab=possAlts,
#    ylab="Frequency",horiz=T,
#    main=paste(tagfm[[i]]$tagName,"Distribution in the Water Column"), 
#    col=rep("grey",length(altVec)))

  abline(h=mean(tagfm[[i]]$data$altitude, na.rm=T),col="red",lwd=2)
}
  
# now do all fish together
alt = ceiling(z0$altitude[ z0$altitude >= 0 ])
altVec = c()
for (j in 1:length(possAlts)){ 
  altVec = rbind(altVec, sum(alt == possAlts[j],na.rm=T))
}
#  # make all the 'zero' values into 'one' values, and get rid of 'zero' element
#  altVec[2] = altVec[1] + altVec[2]; altVec[1]=0
#  altVec = altVec[2:14]
#  possAlts = 1:13

altVec = altVec/sum(altVec)


barplot(altVec, horiz=T, beside=T, col="grey", las=1, main="",
  xlab="Density",
  ylab="Gag Height Above Seafloor (m)", cex.axis=1.5, cex.names=1.5,
  cex.lab=1.5, names=possAlts,
  xlim=c(0,0.7), ylim=c(1,13)
)



# get a regular histobram for altitude distribution, but group the 0 into 1
hist(z0$altitude, breaks=30, freq=F, main="", col="grey", las=1,
  xlim=c(0,13), #ylim=c(0,30000), 
  xlab="Altitude (m)")
abline(v=median(z0$altitude,na.rm=T),lwd=4)

barplot(z0$altitude, beside=T, col="grey", las=1, breaks=30)


# LINEAR REGRESSION?????
# TL FL

tlfl = lm(formula=results$TL ~ results$FL)
tlfl$coef
summary(tlfl)
fltl = lm(formula=results$FL ~ results$TL)
fltl$coef

# W TL
# this is the standard relationship
# W = a TL^b  -or-   log(W) = log (a) + b Log(TL)
logwtl = lm(formula = log(results$weight) ~ log(results$TL))
logwtl$coef
summary(logwtl)
plot(log(results$TL), log(results$weight))
abline (a = -22.124, b=3.607)



plot(results$FL, results$TL)
abline(a=tlfl$coef[[1]], b=tlfl$coef[[2]])
abline(tlfl)

summary(tlfl)$r.squared

tlfl$r.squared

# kde95 kde95
hrhr = lm(results$kde95 ~ results$kde50)
summary(hrhr)
plot(results$kde50, results$kde95)
abline(hrhr)

# kde95 TL
hrtl = lm(results$hr95 ~ results$TL)
summary(hrtl)
plot(results$TL, results$hr95)
abline(hrtl)


# KDE95 weight
hrwt = lm(results$kde95 ~ results$weight)
summary(hrwt)
plot(results$weight, results$kde95)
abline(hrwt)


# KDE50 weight
hrwt = lm(results$kde50 ~ results$weight)
summary(hrwt)
plot(results$weight, results$kde50)
abline(hrwt)



# dtr TL
dtrtl = lm(goodResults$dtr~goodResults$TL)
summary(dtrtl)
plot(goodResults$TL, goodResults$dtr)
abline(dtrtl)

                              
# dtr kde50
dtrhr = lm(results$medianDtr~results$kde50)
summary(dtrhr)
plot(results$kde50, results$medianDtr)
abline(dtrhr)

# dtr kde95
dtrhr = lm(results$medianDtr~results$kde95)
summary(dtrhr)
plot(results$kde95, results$medianDtr)
abline(dtrhr)





# dtr weight
dtrwt = lm(results$dtr~results$weight)
summary(dtrwt)
plot(results$weight, results$dtr)
abline(dtrwt)


# speed weight
speedwt = lm(results$meanSpeed~results$weight)
summary(speedwt)
plot(results$weight, results$meanSpeed)
abline(speedwt)



# dtr temp
dtrtemp = lm(z0$dtr~z0$temperature)
plot(z0$temperature, z0$dtr)
plot(dtrtemp)
summary.lm(dtrtemp)$r.squared

# dtr hod
dtrhod = lm(z0$dtr~z0$hod)
plot(dtrhod)
plot(z0$hod, z0$dtr)
summary.lm(dtrhod)$r.squared







par(mfrow=c(2,3))
plot(results$weight,results$dtr, pch=19, col="red")
points(goodResults$weight,goodResults$dtr, pch=19)

plot(results$weight,results$meanSpeed, pch=19, col="red")
points(goodResults$weight,goodResults$meanSpeed, pch=19)

plot(results$weight,results$homeRange95, pch=19, col="red")
points(goodResults$weight,goodResults$homeRange95, pch=19)

plot(results$dtr,results$homeRange95,  pch=19, col="red")
points(goodResults$dtr,goodResults$homeRange95,  pch=19)

plot(results$dtr,results$meanSpeed, pch=19, col="red")
points(goodResults$dtr,goodResults$meanSpeed, pch=19)

plot(results$weight,results$TL, pch=19, col="red")
points(goodResults$weight,goodResults$TL, pch=19)

# some box plots asking if the two years are different

plot(results$deployment, results$dtr, pch=19)
boxplot(list(results$deployment, results$dtr), notch=FALSE)





### a big combo plot for the paper
library(hexbin)

cTag=1
str(tagfm[[cTag]])



plot(hexbin(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing))

par(mfrow=c(3,2))
plot(tagfm[[cTag]]$data$datiL, tagfm[[cTag]]$data$easting, type="l")
plot(tagfm[[cTag]]$data$datiL, tagfm[[cTag]]$data$depth, type="l")









################################################################################
################################################################################
### Home Ranges
################################################################################
################################################################################
#
# THE LIMITS YOU USE WHEN CALCULATING THE KDE AFFECT THE ANSWER, SO FOR ALL 
# FISH MAKE SURE TO USE THE SAME LIMITS ON EASTING AND NORTHING.
#
# DO SOME WORK TO CHECK THE hr ESTIMATE SENSITIVITY TO THE LIMITS AND n GRID 
# CELLS IN EACH DIRECTION.
#
# USE BOOT STRAPPING TO FIND CONFIDENCE INTERVALS ON HR ESTIMATES.  ABOUT 1000
# RUNS OF THE BOOT STRAP IS ABOUT RIGHT.  BE SURE TO CHECK THAT THE MEAN OF 
# ALL THE BOOT STRAPS IS ABOUT EQUAL TO THE HR ESTIMATE WITH ALL THE DATA.  USE
# 95% QUANTILES TO APPROXIMATE THE 95% CI.




# calculate the home range
cTag = 5
if(results$deployment[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
cProb = 0.95
  
HRsize = homeRange(
  easting = tagfm[[cTag]]$data$easting, 
  northing = tagfm[[cTag]]$data$northing,  
  tagName = cTagNames[cTag],
  lims = hrlims, # c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing),
  reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
  prob=cProb, drawplot=TRUE
) 


################################################################################
# how does the HR change with number of days used...home range stabilization

# ...because I put 'results' in order of size and this wants them in original order
results = results[c(5,4,3,1,2) ,]

# something to hold the changing HR answers for all fish
allFishHR = list()                                          

cProb = 0.5
for (cTag in 1:length(tagfm)){  

  if(results$deployment[cTag] == "hb2007"){
    cmd = md[[1]]
    numUniqueDays = length(unique(tagfm[[cTag]]$data$datiL$yday))
  } else {
    cmd = md[[2]]
    numUniqueDays = length(unique(tagfm[[cTag]]$data$datiL$yday))
  }

  # get only the easting/northing data
  d1= subset(tagfm[[cTag]]$data, select=c(utime,datiL,easting,northing))
  # create a vector of the index of the day of the run, 1-52 for example
  # ...there's got to be a prettier way of doing this, but...
  startOfFirstDay = unclass(as.POSIXct(
    strptime(cmd$taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
      origin="1970-1-1", tz="EST5EDT"))[1]
  endOfAllDays = startOfFirstDay + 86400 * 1:numUniqueDays
  
  # something to hold the changing HR for one fish
  hrVSdays = data.frame("numDays" = NA, "hrSize" = NA) 

  for (i in 1:numUniqueDays){
    # grab only position solutions during the first i days
    d2 = d1[d1$utime < endOfAllDays[i],]
    print(i)  
    if(nrow(d2) > 0){
      # calculate the home range for these PS
      hrSize = homeRange(
        easting = d2$easting, 
        northing = d2$northing,  
        tagName = paste(tagfm[[cTag]]$tagName, ", ", i, " days", sep=""),
        lims = hrlims, #c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=FALSE
      )
    } else {hrSize = 0}
    # save the answer
    hrVSdays[i,] = c(i, hrSize) 
  } # end for-loop

  # a plot
  plot(hrVSdays$numDays, hrVSdays$hrSize, pch=19, 
    main=paste(cTagNames[cTag], ": ", cProb*100, "% HR", sep="")
  )
  
  # save the answer for this fish
  allFishHR[[cTag]] = hrVSdays
  
} # end cTag for-loop


plot(allFishHR[[4]],type="l", xlim=c(0,60),
  main=paste(cProb*100, "% KDEs", sep=""),
  xlab="Number of Days", ylab=paste(cProb*100, "% KDE", sep="")
)
for (cTag in 1:length(allFishHR)){
  points(allFishHR[[cTag]], type="l")
}
text(x=c(38,38,38,51,51)+4, y=results$kde95, labels=c(results$tagName))



# ...also create these same curves for 2008 fish as if there had only been 50m arrays
# ...you'll need 'rotate()' and 'chop()' which are in 'testing array spacing.r' for now
# ...the two rish of interest are 2008 f60300, f61100, tagfm[[4]], tagfm[[5]]

cmd=md[[2]]
s60300 = subset(tagfm[[4]]$data, select=c(utime,easting,northing))
r60300 = rotate(s60300$utime,s60300$easting, s60300$northing, spin=45)
c60300 = chop(r60300$fisht, r60300$fishx, r60300$fishy, spacing=50)
u60300 = rotate(c60300$fisht,c60300$fishx,c60300$fishy, spin=-45)
plot(s60300$easting,s60300$northing,pch=19)
points(u60300$fishx,u60300$fishy,pch=19,col="blue")


s61100 = subset(tagfm[[5]]$data, select=c(utime,easting,northing))
r61100 = rotate(s61100$utime,s61100$easting, s61100$northing)
c61100 = chop(r61100$fisht,r61100$fishx, r61100$fishy, spacing=50)  
u61100 = rotate(c61100$fisht,c61100$fishx,c61100$fishy, spin=-45)
plot(s61100$easting,s61100$northing,pch=19)
points(u61100$fishx,u61100$fishy,pch=19,col="blue")

for (cTag in 4:5){  

  cmd = md[[2]]
  numUniqueDays = length(unique(tagfm[[4]]$data$datiL$yday)) # data for 2008 deployment

  # get only the easting/northing data
  if(cTag==4){d1=u60300} else {d1=u61100}
  names(d1) = c("utime","easting","northing")
  
                          
  # create a vector of the index of the day of the run, 1-52 for example
  # ...there's got to be a prettier way of doing this, but...
  startOfFirstDay = unclass(as.POSIXct(
    strptime(cmd$taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
      origin="1970-1-1", tz="EST5EDT"))[1]
  endOfAllDays = startOfFirstDay + 86400 * 1:numUniqueDays
  
  # something to hold the changing HR for one fish
  hrVSdays = data.frame("numDays" = NA, "hrSize" = NA) 

  for (i in 1:numUniqueDays){
    # grab only position solutions during the first i days
    d2 = d1[d1$utime < endOfAllDays[i],]
    print(i)  
    if(nrow(d2) > 0){
      # calculate the home range for these PS
      hrSize = homeRange(
        easting = d2$easting, 
        northing = d2$northing,  
        tagName = paste("u60300 or u61100", ", ", i, " days", sep=""),
        lims = c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=FALSE
      )
    } else {hrSize = 0}
    # save the answer
    hrVSdays[i,] = c(i, hrSize) 
  } # end for-loop

  # a plot
  plot(hrVSdays$numDays, hrVSdays$hrSize, pch=19, 
    main=paste(cTagNames[cTag], ": ", cProb*100, "% HR", sep="")
  )
  
  # save the answer for this fish
  allFishHR[[cTag+2]] = hrVSdays
  
} # end cTag for-loop



plot(allFishHR[[4]],type="l", xlim=c(0,60), 
  main="Home Range Stabilization", xlab="Number of Days", 
  ylab=paste(cProb*100, "% KDE", sep="")
)
for (cTag in 1:5){
  points(allFishHR[[cTag]], type="l", lwd=2)
}
points(allFishHR[[6]], type="l", lty="dashed", lwd=2)
points(allFishHR[[7]], type="l", lty="dashed", lwd=2) 
text(x=c(38,38,38,51,51)+4, y=results$kde50, labels=c(results$tagName))











###############################################################################
###############################################################################
### Bootstrapping home range estimates
###############################################################################
###############################################################################
bootHR <- function(dat, by="day", nboot=100, prob=0.95, progressbar=FALSE,
  bootplot=FALSE, pts=FALSE, drawplot=FALSE, ...
  # 'by' will subset the data by whatever you choose, say 'day', then the 
  #   bootstrap will pick randomly from the 'days' 
  ) {
  if (progressbar) {                                     
    require(tcltk)
    pb <- tkProgressBar("hr bootstrap",min=0,max=nboot)
  }
  time <- dat$datiL
  dat <- subset(dat,select=c("northing","easting"))
  timecat <- cut(time, breaks=by)
  datsplit <- split(dat,timecat)
  nt <- length(levels(timecat))
  bootres <- numeric(nboot)
  if (bootplot) with(dat,plot(easting,northing,pch="."))
  for (i in 1:nboot) {
    bootsamp <- sample(nt,size=nt,replace=TRUE)
    bootdat <- do.call(rbind,datsplit[bootsamp])
    if (bootplot) with(bootdat,points(easting,northing,pch=".",col=i+1))
    bootres[i] <- with(bootdat,
      homeRange(easting, northing, prob=prob, pts=pts, drawplot=drawplot, ...)
    )
    if (progressbar) setTkProgressBar(pb,i)
  }
  if (progressbar) close(pb)
  bootres
} # end bootHR

    
system.time(boottest <- bootHR(dat=z0, by="day", #by = "2 days" or "week"
  prob=0.95, lims=hrlims,nboot=5, progressbar=TRUE, bootplot=TRUE))

# now to put this all together and run it 1000 for each tag
# create a list to hold the results
hrboots = list(
  list(
    tagName = NA, # which tag
    originalHR = NA, # a single number, HR estimate using all data
    bootResults = NA # a vector holding the 1000 boot strapped HR estimates
  ),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA) # this one for all fish combined
)


#####################################################################

cProb = 0.95
for (i in 1:length(tagfm)){
  # grab things pertinent to this tag
  for (j in 1:length(md)){
    if(tagfm[[i]]$deployment == md[[j]]$deployment){ cmd = md[[j]] } 
  } # end for j loop
  
  # name
  hrboots[[i]]$tagName = tagfm[[i]]$tagName
  
  # find HR estimate will all data 
  hrboots[[i]]$originalHR = homeRange(tagfm[[i]]$data$easting, tagfm[[i]]$data$northing, 
    prob=cProb, lims=hrlims, tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, 
    sdlEN=cmd$sdlEN, pts=TRUE, drawplot=TRUE)  

  # now the boot strapping
  hrboots[[i]]$bootResults = bootHR(tagfm[[i]]$data, #by = "day" or "2 days" or "week"
    prob=cProb, lims=hrlims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
    tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
    drawplot=FALSE
  )

}

 
# now calculate the 95% KDE for all fish combined...z0 is required
i=6
everyfish =     the answer I get with z0 seems funky...do it again with this
  = 
hrboots[[i]]$originalHR = bob=homeRange(z0$easting, z0$northing, 
  prob=cProb, lims=hrlims, tagName="All", reefEN=cmd$reefEN, 
  sdlEN=cmd$sdlEN, pts=FALSE, drawplot=FALSE)  

hrboots[[i]]$bootResults = sam=bootHR(z0, #by = "day" or "2 days" or "week"
  prob=cProb, lims=hrlims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
  tagName="All", reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
  drawplot=FALSE
)


cTag = 1
plot(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19, xlim=c(0,6), ylim=c(1000,6000))
cTag = 2
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 3
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 4                           
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19)
cTag = 5
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19) 
cTag = 6       
points(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19) 
    

points(1:6,c(hrboots[[1]]$originalHR, hrboots[[2]]$originalHR,hrboots[[3]]$originalHR,
  hrboots[[4]]$originalHR, hrboots[[5]]$originalHR, hrboots[[6]]$originalHR),
  pch=19,col="red")

# now pick out the 95% confidence interval (2.5% and 97.5%  points), that is 
# 25, 975 our of 1000 different runs
cTag=6
plot(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)])
abline(v=c(25,975))
abline(h=hrboots[[cTag]]$originalHR)
print(hrboots[[cTag]]$tagName)
c(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][25],
  hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][975]
)


save("hrboots", file="C:/zy/Telemetry/R summary files/hrboots 2010April08.rdata")
load("C:/zy/Telemetry/R summary files/hrboots 2011April08.rdata")

    
###############################################################################
###############################################################################
### Variance Decomposition
###############################################################################
###############################################################################
#
# This codes comes from examples by Ben in 'var_decomp_BMB.r'

ggplot(z0, 
   aes(x=tod,y=speed,colour=tagName,fill=tagName)) + 
   geom_point()

(a1 <- lmer(dtr ~ 1 + (1|tag)+(1|dod)+(1|hod), z0))

## variances
estvar = c(unlist(VarCorr(a1)), err=lme4:::sigma(a1)^2)
percentVariances = 100 * estvar / sum(estvar)
dd <- data.frame(comp=names(estvar), val=estvar, perVar=percentVariances)
             
ggplot(dd,aes(x=val,y=comp))+geom_point()




################################################################################
# ggplots


# 
ggplot(z0, 
  aes(x=dtr, y=speed, group=tagName, colour=tagName, fill=tagName))+
  geom_point(alpha=0.1) +
  geom_smooth()+
  #facet_wrap(~tagName) + 
  xlab("Time of Day") +
  coord_cartesian(ylim=c(0.0,0.4), xlim=c(0,60)) +
  scale_x_continuous(breaks = seq(0,40,by=5)) +
  scale_y_continuous(breaks = seq(0.05,0.2,by=0.01)) +
  
       
     

# main effects change x and y for various relationships                                                                   
ggplot(z0, aes(x=temperature, y=speed, group=tagName, colour=tagName, fill=tagName)) + 
  geom_point(alpha=0.03) + 
  geom_smooth(method="gam",formula=y~s(x)) +
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
  coord_cartesian(ylim=c(0.1,0.2)) +
  scale_y_continuous(breaks = seq(0.1,0.2,by=0.01)) +
  theme_bw() +
  scale_x_continuous("Temperature (Celcius)") +
  scale_y_continuous("Travel Speed (m)") +
  opts(axis.text.x = theme_text(size = 20)) +
  opts(axis.text.x = theme_text(size = 20), axis.text.y = theme_text(size = 20)) +               
  opts(axis.ticks.x = theme_text(size = 20)) +
  geom_text()


# this for making my main archives of pictures  
# main effects change x and y for various relationships                                                                   
ggplot(z0, aes(x=tod, y=dtr, group=tagName, colour=tagName, fill=tagName)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="gam",formula=y~s(x)) +
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
  coord_cartesian(ylim=c(6,28)) +
  scale_y_continuous(breaks = seq(0,30,by=1)) +
  theme_bw() + 
  facet_wrap(~dod)

ggplot(z0night, aes(x=lunarIndex, y=altitude)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="gam",formula=y~s(x)) +
  #coord_cartesian(ylim=c(0.1,0.2)) +
  #scale_y_continuous(breaks = seq(0.1,0.2,by=0.010)) +
  facet_wrap(~lunarIndex)


 
# main effects by fish, change x and y for various relationships                                                                   
ggplot(z0, aes(x=tod, y=altitude, group=tagName, colour=tagName, fill=tagName)) + 
  geom_point(alpha=0.1, cex=1) + 
  geom_smooth() +                                      
  #geom_smooth( method="gam",formula=y~s(x)) +                                      
  geom_smooth(aes(group=1),colour="black",lwd=1.1,method="gam",formula=y~s(x)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0,13)) +
  scale_y_continuous(breaks = seq(0,13,by=2)) +
  facet_wrap(~dod)  

# for altitude and depth
# what I want to look at
#altitude v tod, lunarIndex, temperature, dirL, magL, dod 
# magL, dirL. temp - no pattern.
z6 = z0[!is.na(z0$altitude), ]        
z6 = drop.levels(z6[(z6$tagName!="f60300")|(z6$tagName!="f61100"),],reorder=FALSE) 

z6night = z6[z6$day=="night", ]
z6day = z6[z6$day=="day", ]
z6$date = as.Date(z6$datiL)


       
z7 = z0[!is.na(z0$depth), ]        
z7 = drop.levels(z7[(z7$tagName!="f60300")|(z7$tagName!="f61100"),],reorder=FALSE)
setwd("C:/zy/Ch 3 Preliminary Data/Figures/temp figures")                                                        

                                                                
ggplot(z6, aes(x=tod, y=altitude)) +#, group=tagName, colour=tagName, fill=tagName)) + 
  geom_point(alpha=0.4, cex=1) + 
  #geom_smooth(lwd=1) +                                      
  #geom_smooth( method="gam",formula=y~s(x)) +                                      
  #geom_smooth(aes(group=1),colour="black",lwd=1.1,method="gam",formula=y~s(x)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0,13)) +
  scale_x_continuous(breaks = seq(6,24,by=6)) +
  scale_y_continuous(breaks = seq(0,10,by=5)) +
  facet_wrap(~date) + 
  opts(axis.label.x = "Time of Day") +
  opts(axis.label.y = "Altitude from Seafloor (m)") +
  opts(axis.text.x = theme_text(size = 20), axis.text.y = theme_text(size = 20)) +               
  opts(axis.ticks.x = theme_text(size = 20)) 

  
# interactions, change x and y for various relationships                                                                   
ggplot(z0, aes(x=tod, y=dtr, colour=tagName)) + theme_bw() +
  geom_point(alpha=0.3) +
  geom_smooth( method="gam",formula=y~s(x,bs="cc"), colour="black") +                                     
  geom_smooth(aes(group=1,colour=NA),colour="black",method="gam",formula=y~s(x,bs="cc")) +
  facet_wrap(~dod) + 
  coord_cartesian(ylim=c(0,13)) +
  scale_y_continuous(breaks = seq(0,13,by=4))
  # 



aaa
   how to change colour to be 2nd variable
   
X00s$timechunk <- cut(X00s$utime,breaks=8)
ggplot(X00s,aes(x=magL ,y=dtr,colour=utime))+geom_point(alpha=0.1)+geom_path(alpha=0.4)
## +   facet_wrap(~timechunk)


ggplot(Y00,aes(x=hr,y=dtr,colour=tag,fill=tag))+geom_smooth()+coord_cartesian(ylim=c(10,30))+
  geom_smooth(aes(colour=NA,fill=NA),colour="black",fill="black")



### something to try
ggplot(z0,aes(x=tod,y=dtr,colour=tagName,fill=tagName,group=interaction(tagName,tod)))+
  geom_boxplot()+stat_smooth(aes(group=tagName))




bbb


z4$date = as.Date(z4$datiL)
z5$date = as.Date(z5$datiL)

# one fish at a time                                                                  
ggplot(z5, aes(x=tod, y=dtr), xlab="s") + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="gam",formula=y~s(x),colour="black") +
  #geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
  coord_cartesian(ylim=c(0,100)) +
  scale_x_continuous(breaks = seq(6,24,by=6)) +
  scale_y_continuous(breaks = seq(30,90,by=60)) +
  theme_bw() +
  facet_wrap(~date) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15))                
  #opts(axis.ticks.x = theme_text(size = 20)) 



## for looking at tod and lunarIndex interactions
z8 = z0[z0$day=="night",]

# main effects by fish, change x and y for various relationships                                                                   
ggplot(z8, aes(x=lunarIndex, y=speed, group=tagName, colour=tagName, fill=tagName)) + 
  facet_wrap(~hod) + 
  geom_point(alpha=0.1, cex=1) + 
  geom_smooth() +                                      
  #geom_smooth( method="gam",formula=y~s(x)) +                                      
  #geom_smooth(aes(group=1),colour="black",lwd=1.1,method="gam",formula=y~s(x)) +
  theme_bw() + 
  coord_cartesian(ylim=c(0,0.3)) +
  scale_y_continuous(breaks = seq(0,0.3,by=0.1)) 
  



ggplot(z0, aes(x=hod, y=spd, group=dod)) + 
  #geom_point(alpha=0.01) + 
  geom_smooth( method="gam",formula=y~s(x)) +
  #theme_bw() + 
  coord_cartesian(ylim=c(0,50)) +
  scale_y_continuous(breaks = seq(0,50,by=5)) +
  facet_wrap(~dod) +
  opts(title="All Fish")


 
 
 
 
 
 
 
 
 
 
 
 
 
 
##### water flow direction...dirL
### ...use raw adcp data not what's attached to z0
ad=importADCPdata()
ad$datiL = as.POSIXct(ad$datiL)
ad$yr = as.factor(ifelse(ad$utime>1.21e9,2008,2007))
 
 library(CircStats)
 
bob = ad[!is.na(ad$dirL),]

hist(ad$dirL) 
rose.diag((bob$dirL)*pi/180, bins=36,  prop=3, main="distribution of dirL") 


ggplot(ad,aes(x=dirL))+geom_bar(binwidth=10)+facet_wrap(~yr) +
  theme_bw() +
  coord_polar() +
  opts(title="Water Flow Direction")                         
















z0$btr = as.numeric(z0$btr) 
 
ggplot(z0, aes(x=dirL, y=btr)) 

plot(z0$dirL,z0$btr,pch=19,cex=0.1)

bob=as.numeric(z0$btr)
 
 
ggplot(z0,aes(x=as.numeric(btr)))+geom_bar(binwidth=10)+facet_wrap(~tagName) +
  theme_bw() +
  coord_polar() +
  opts(title="Water Flow Direction")    
  
ggplot(z0,aes(x=dtr, y=as.numeric(btr))) + 
  geom_point(alpha=0.01) +
  facet_wrap(~tagName) +
  theme_bw() 
                       






plot(hexbin(z0$dtr, as.numeric(z0$btr)))

 
 
 
 
 
ggplot(z5, aes(x=easting, y=northing)) +
  coord_cartesian(ylim=c(500,800), xlim=c(8450,8750)) +
  #stat_binhex() +
  stat_binhex(bins=40) +
  #stat_binhex(binwidth=c(10,10))
  
  #geom_point(x=md[[2]]$reefEN$easting, y=md[[2]]$reefEN$northing) +
  geom_point(x=8500, y=550, alpha=10) 
 
  


# individual fish movement by day
ggplot(z5,
  aes(x=tod,y=speed)) +# geom_point() + 
  geom_smooth(method="gam",formula=y~s(x)) +  
  #coord_cartesian(ylim=c(0,130)) +      
  #scale_y_continuous(breaks = seq(0,130,by=40)) +
  facet_wrap(~dod) + theme_bw() +
  opts(title = z5$tag[1])

   


# altitude v tod
ggplot(z0,
  aes(x=tod,y=altitude)) + geom_line() + 
  geom_smooth() + ## geom_smooth(method="gam",formula=y~s(x)) + ## 
   theme_bw() 

head(z1)

junk=head(z3[!is.na(z3$depth) ,],10000)
plot(junk$datiL,junk$depth,type="b")



# individual fish altitude
ggplot(z0,
  aes(x=magL,y=altitude)) + geom_point(alpha=0.1) + 
  geom_smooth() + ## geom_smooth(method="gam",formula=y~s(x)) + ## 
  coord_cartesian(ylim=c(0,5)) + 
  theme_bw() 




# individual fish altitude by day
ggplot(z2,
  aes(x=tod,y=altitude)) +# geom_line() + 
  geom_smooth() + ## geom_smooth(method="gam",formula=y~s(x)) + ## 
  coord_cartesian(ylim=c(0,5)) + 
  facet_wrap(~dod) + theme_bw() 




  zz = rbind(z1,z2)
  
  
  
  
plot(z1$datiL,z1$depth,type="l",col="red")
points(z2$datiL,z2$depth,type="l",col="blue")
points(z2$datiL,z2$waterDepth,type="l",col="green")  
plot(z2$datiL,z2$altitude,type="b",col="green")  




