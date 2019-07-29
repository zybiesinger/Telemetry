##############################################################################
### This file evaluates the performance of the array inside the array.
### We deployed the array at 100m and placed ten tags at many different
### locations in order to determine whether different locations within the
### array result in different frequencies of "positions recorded"
###
### 
##############################################################################

### Needs:
#'global variables.r'
library("PBSmodelling")
library("PBSmapping") # for converting LL to UTM


source("C:/zy/Telemetry/R Data Processing/global variables.r")
source("C:/zy/Telemetry/R Data Processing/global functions.r")
source("C:/zy/Telemetry/R Data Processing/global metadata.r")

# This all happened during one deployment...
# 9 Tags: 43-52, but no 49 because it broke 
cmd = md[[3]]
cDeployment = cmd$deployment
cTagNames = c(cmd$otherNames,cmd$beaconNames,cmd$sentinelNames) 


# nine buoys with tags as follows:
# Orange-12 had tag 43
# GPS had tags 44 (~3ft up) and 45 (~6ft up)
# O-26 had tag 46
# O-5 had tag 47
# Yellow-4 had tag 48
# O-6 had tag 49, this tag died and had no detections
# Y-8 had tag 50
# O-24 had tag 51
# Y-11 had tag 52


### which tags went with which buoys
d1 = data.frame(
  tagID = c("43","44","45","46","47","48","49","50","51","52"), 
  
  buoyNumber = c(1,2,2:9), 
  
  buoyName = c("O12", "GPS", "GPS", "O26", "O5", "Y4", 
    "O6", "Y8", "O24", "Y11")# as they correspond to tagIDs
)

### get the dates and times matched
startDate = c(rep("01/06/2009",46),rep("03/06/2009",10))
  
startTime = c( # EDT = GMT-4hrs.  The times shown are EDT
  "13:57:27", #time of deployment at the reef on first day, named reef1
  "14:03:26","14:03:26", # GPS buoy with tags 44 and 45 
  "14:06:03", "14:15:26", "14:10:58", "14:19:43", 
  "14:23:23", "14:25:27", "14:27:50", 
  "15:18:14", "15:18:14", # GPS buoy with tags 44 and 45
  "15:24:00", "15:27:40", 
  "15:32:20", "15:34:25", "15:37:43", "15:41:00", "15:45:30", 
  "16:12:45", "16:12:45", # GPS buoy with tags 44 and 45
  "16:16:59", "16:22:30", "16:25:00", "16:28:20", "16:31:50", "16:35:30", 
  "16:42:47", 
  "17:14:05", "17:14:05", # GPS buoy with tags 44 and 45
  "17:17:42", "17:21:34", "17:24:42", "17:28:08", 
  "17:31:03", "17:34:10", "17:36:25", 
  "17:54:11", "17:54:11", # GPS buoy with tags 44 and 45
  "17:56:36", "17:59:32", 
  "18:02:17", "18:05:00", "18:06:28", "18:09:31", "18:11:27", 
  "09:28:07", # time of deployment at the reef on second day, named reef2
  "09:29:39", "09:31:06", "09:32:03", "09:33:06", "09:34:20", "09:34:52", 
  "09:35:44", 
  "09:36:59", "09:36:59" # GPS buoy with tags 44 ad 45
)
 
stopTime = c( # EDT = GMT-4hrs, Times shown are EDT
  "18:52:15", #reef1
  "15:14:50", "15:14:50", # GPS buoy with tags 44 and 45
  "15:22:29", "15:29:30", "15:25:10", "15:33:00", "15:35:10", 
  "15:38:50", "15:42:20", 
  "16:10:43", "16:10:43", # GPS buoy with tags 44 and 45
  "16:14:15", "16:18:56", "16:23:30", 
  "16:26:30", "16:29:45", "16:32:50", "16:36:00", 
  "17:11:00", "17:11:00", # GPS buoy with tags 44 and 45
  "17:14:40", 
  "17:18:30", "17:22:21", "17:25:52", "17:29:02", "17:32:10", "17:35:10", 
  "17:51:29", "17:51:29", # GPS buoy with tags 44 and 45
  "17:55:02", "17:57:22", "18:00:49", "18:02:56", "18:05:36", 
  "18:07:52", "18:10:44", 
  "18:46:20", "18:46:20", # GPS buoy with tags 44 and 45
  "18:47:16", "18:48:48", "18:50:10", 
  "18:50:54", "18:48:09", "18:51:25", "18:53:18", 
#  "13:40:00", # reef2
#  "13:40:00", "13:40:00", "13:40:00", "13:40:00", "13:40:00", "13:40:00", 
#  "13:40:00", "13:40:00", "13:40:00" 
  "13:00:00", # reef2
  "13:00:00", "13:00:00", "13:00:00", "13:00:00", "13:00:00", "13:00:00", 
  "13:00:00", "13:00:00", "13:00:00" 
)

d2start = paste(startDate, startTime)
d2stop = paste(startDate, stopTime)
d2datiStart = strptime(d2start, "%d/%m/%Y %H:%M:%S")
d2datiStop = strptime(d2stop, "%d/%m/%Y %H:%M:%S")


### now match the set, location, buoy name, start time, and stop time 
d2 = data.frame(
  set=c("reef1", rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),"reef2",rep(6,8)), 
  
  location=c("reef1",1:40,"reef2",41:48), 
  
  # note that the sets 1 and 6 have the buoys in different orders than 2-5
  buoyName = c(
    "O12",                                                 # reef1
    "GPS", "O26", "Y4", "O5", "O6", "Y8", "O24", "Y11",    # set 1
    "GPS", "O26", "O5", "Y4", "O6", "Y8", "O24", "Y11",    # set 2
    "GPS", "O26", "O5", "Y4", "O6", "Y8", "O24", "Y11",    # set 3
    "GPS", "O26", "O5", "Y4", "O6", "Y8", "O24", "Y11",    # set 4
    "GPS", "O26", "O5", "Y4", "O6", "Y8", "O24", "Y11",    # set 5
    "O6",                                                  # reef2
    "O24", "O5", "Y11", "O12", "Y4", "Y8", "O26", "GPS"    # set 6
  )
) # end d2



### now get the target locations into R
latitude = c(
  29.462783	, # reef1
  29.46305317, 29.46297413, 29.46278333, 29.46259253, 
  29.4625135,  29.46259253, 29.46278333, 29.46297413, 29.46323305, 
  29.4631728,  29.46300819, 29.46278333, 29.46255847, 29.46239386, 
  29.46233361, 29.46239386, 29.46255847, 29.46278333, 29.46300819, 
  29.4631728,  29.46341294, 29.46332859, 29.46309814, 29.46278333, 
  29.46246853, 29.46223808, 29.46215372, 29.46223808, 29.46246853, 
  29.46278333, 29.46309814, 29.46332859, 29.46287562, 29.46294629, 
  29.46316493, 29.46339149, 29.46304397, 29.46278333, 29.46278333, 
  29.46350015,
  29.462783, # reef2
  29.462119, 29.46231597, 29.4625135, 29.46266619, 
  29.46290047, 29.46305317, 29.4632507, 29.46344767
)
    
longitude = c(
  83.624433, # reef1
  83.62443333, 83.62462413, 83.62470317, 83.62462413, 
  83.62443333, 83.62424253, 83.6241635, 83.62424253, 83.62443333, 
  83.62465819, 83.6248228, 83.62488305, 83.6248228, 83.62465819, 
  83.62443333, 83.62420847, 83.62404386, 83.62398361, 83.62404386, 
  83.62420847, 83.62443333, 83.62474814, 83.62497859, 83.62506294, 
  83.62497859, 83.62474814, 83.62443333, 83.62411853, 83.62388808, 
  83.62380372, 83.62388808, 83.62411853, 83.62417977, 83.62382518, 
  83.62405173, 83.62427038, 83.6243635, 83.62362384, 83.62429842, 
  83.62437062, 
  83.624433, # reef1
  83.62431619, 83.6241635, 83.62396597, 83.623769, 
  83.623769, 83.62396597, 83.6241635, 83.62431619)

d3 = data.frame(
  location = c("reef1", 1:40, "reef2", 41:48),
  
  # lat long as decimal degrees 
  latDM=latitude,
  longDM=longitude,
      
   # show latitude, longitude as degree decimal minutes
  latDDM = paste("(",floor(latitude), "_", 
    round((latitude-floor(latitude))*60, 3),")", sep=""),
  longDDM = paste("(",floor(longitude), "_", 
    round((longitude-floor(longitude))*60, 3),")", sep="")
)

# lat long as UTM
tagsLL =data.frame(X = -longitude, Y = latitude)
attr(tagsLL, "zone") <- 17
attr(tagsLL, "projection") <- "LL"   
tagsUTM = convUL(tagsLL,km=FALSE)  #(longitude, latitude)
d3a = cbind(d3, targetNorthing = tagsUTM$Y-northingOffset, targetEasting = tagsUTM$X-eastingOffset)  


# now merge d1, d2, and d3
d4 = merge(d1, d2, by="buoyName", sort=FALSE)  
d5 = merge(d3a, d4, by="location", sort=FALSE)
d5a = cbind(d5, datiStart=d2datiStart, datiStop=d2datiStop)
d5b = cbind(d5a, elapsedTime = d5a$datiStop-d5a$datiStart)
d6 = d5b[,c(1,6,7,9,12,13,14)]

# drop tag 49, it was broken
d6 = d6[d6$tagID != 49,]

# and add columns for the fraction of positions at each tag spot, with unfiltered
# ... and filtered data
d6$psFracy = NA
d6$psFracn = NA
# ... also columns for the best estimate position for filtered data
d6$foundNorthing = NA
d6$foundEasting = NA


pause this work...it's taking too long for the benefit, maybe come back to it...
# now add more lines to d6 for the beacons and sentinel tags
# b1 on C45, b2 on N41, b79400 on E42, b79500 on S44
i=1 # first beacon
d6$location[nrow(d6)+i] = "center"
d6$targetNorthing[nrow(d6)+i] = NA
d6$targetEasting[nrow(d6)+i] = NA 
d6$tagID[nrow(d6)+i] = 1
d6$datiStart[nrow(d6)+i] =  
...pause this work...it's taking too long for the benefit, maybe come back to it









### now import data for a particular tag used during the test, the tag
# numbers are listed at the top of this file.  The data file names (ALPS output)
# are named like "T61000B1.txt".
## import, filter ALPS data
itag = list() # raw tag data
itagfn = list() # unfiltered tag data
itagfy = list() # filtered tag data
for (i in 1:length(cTagNames)){
  print(cTagNames[i])
  itag[[i]] = importALPSdata(deployment=cDeployment,tagName=cTagNames[i], 
    chopTimes=FALSE)
  itagfn[[i]] = filterALPSdata(df1=itag[[i]], cnF=0, minuteMean=F)
  itagfy[[i]] = filterALPSdata(df1=itag[[i]], cnF=1.5, speedF=0.8, minuteMean=F)
}
# now pair these down to just the things I want
# ... I end up with just utime, datiL, easting, and northing in $data
tagn = itagfn
tagy = itagfy
for (i in 1:length(cTagNames)){
  tagn[[i]]$data = itagfn[[i]]$data[,c(1,3,8,9)]
  tagy[[i]]$data = itagfy[[i]]$data[,c(1,3,8,9)]
}


# now for each location, or spot calculate the number of detections and the 
# fraction of position solutions, detPS

# lists to hold the easting northing data for each tag spot
spoty = list()
spotn = list()

# lists to hold the PS for each tag spot
psFracy = list()
psFracn = list()


for (i in 1:nrow(d6)){ # d6 is a data.frame of each deployment spot 
  # for each spot ...
  # ... pick out only the data from tagy and tagn which are for the whichTag
  whichTag = as.character(d6[i,]$tagID)
  for (j in 1:length(tagy)){
    if(grepl(paste("o",whichTag,sep=""), tagy[[j]]$tagName)){
      spoty[[i]] = tagy[[j]]$data
      spotn[[i]] = tagn[[j]]$data
      spoty[[i]]$tagName = tagy[[j]]$tagName
      spotn[[i]]$tagName = tagn[[j]]$tagName
    }
  }
  # now that I have all the data for this tag...  
  # ... pick out only the data between startDati and stopDati for this spot 
  spoty[[i]] = spoty[[i]][
    (spoty[[i]]$datiL > d6$datiStart[i]) & (spoty[[i]]$datiL < d6$datiStop[i]),]
  spotn[[i]] = spotn[[i]][(
    spotn[[i]]$datiL > d6$datiStart[i]) & (spotn[[i]]$datiL < d6$datiStop[i]),]
  numPings = as.numeric(d6$elapsedTime[i]) * 30 # num min* 30 pings per min
  # calculate the fraction of PS for this tag/spot during this interval
  d6$psFracy[i] = nrow(spoty[[i]]) / numPings
  d6$psFracn[i] = nrow(spotn[[i]]) / numPings
  # calculate the best position estimates
  d6$foundNorthing[i] = mean(spoty[[i]]$northing)
  d6$foundEasting[i] = mean(spoty[[i]]$easting)
}  # end i for loop

# that's all the roaming tags, now what about the stationary beacons/sentinel
# tagy has the data for these tags.  For datiStart and datiStop I'll use the
# full trial time as extracted from d6, that will be almost 2 days.
allstart = min(d6$datiStart)
allstop = max(d6$datiStop)
numBeaconPings = as.numeric(allstop-allstart)*24*60*3
numSentinelPings = as.numeric(allstop-allstart)*24*300 # 300pings/hr
# now extract data from tagy for beacons/sentinel and chop to right times
stillStuff = list()
for (i in 1:5){ # 5 stationary tags
  stillStuff[[i]] = tagy[[9+i]]
  # chop times
  stillStuff[[i]]$data = 
  stillStuff[[i]]$data[(stillStuff[[i]]$data$datiL>allstart)&
  (stillStuff[[i]]$data$datiL<allstop) ,]
  stillStuff[[i]]$foundEN = c(mean(stillStuff[[i]]$data$easting),
    mean(stillStuff[[i]]$data$northing))
  # now calculate the psFracy for all stationary tags, the sentinel will be wrong
  # but I'll redo it next  
  stillStuff[[i]]$psFracy = nrow(stillStuff[[i]]$data)/numBeaconPings  
} 
# correct the sentinel psFracy, the last element of stillStuff
stillStuff[[5]]$psFracy = nrow(stillStuff[[5]]$data)/numSentinelPings




# now look at a couple plots
# First, show the clouds of points at each tag spot.  Second look at the psFrac 
#   at each spot
#
# Plot 1: The clouds

# the raw plot
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", 
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing)
# the reef and sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=1.5)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1.5)
# the target locations
points(d6$targetEasting, d6$targetNorthing, pch=19, col="green", cex=0.5)
# the best estimate locations
points(d6$foundEasting, d6$foundNorthing, pch=21, col="black", cex=0.5)

# add the clouds of recorded position solutions for each spot
for (i in 1:length(spoty)){
  #points(spotn[[i]]$easting, spotn[[i]]$northing, pch=19, cex=1, col=plotColors[i])
  points(spoty[[i]]$easting, spoty[[i]]$northing, pch=19, cex=0.5, col=plotColors[i])
}
# add the clouds of stationary tags
for (i in 1:length(stillStuff)){
  points(stillStuff[[i]]$data$easting, stillStuff[[i]]$data$northing, pch=19,
    cex=0.5, col=plotColors[i])
}




# Plot 2: the psFrac
# the raw plot
par(mar=c(4,5,1,1)+0.1)
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", bty="l", las=1,
  xlab="", ylab="", cex.lab=1.5, cex.axis=1.5,
  xlim=c(8789,8969), ylim=c(300,548)
  #xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing
)
# the sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=1.5)
mtext(text="Easting (m)",side=1,line=2.5,cex=1.5)
mtext(text="Northing (m)",side=2,line=3.5,cex=1.7)
 

# add points at the best estimate locations with size = fracPS
#points(d6$foundEasting, d6$foundNorthing, pch=19, cex=(1+d6$psFracn))
points(d6$foundEasting, d6$foundNorthing, pch=21, col="black", 
  cex=(1+2*d6$psFracy) )
   
# add points that have data before filtering but not after
emptyPts = d6[is.nan(d6$foundNorthing) ,] 
points(emptyPts$targetEasting, emptyPts$targetNorthing, pch=19, col="black", 
  cex=1 ) 
  
# add the stationary tags' circles
# add the clouds of stationary tags
for (i in 1:length(stillStuff)){
  points(stillStuff[[i]]$foundEN[1], stillStuff[[i]]$foundEN[2], pch=21,
    cex=(1+2*stillStuff[[i]]$psFracy), col="black")
}

# figure out the legend details
# ...find the smallest circle and the psFracy that goes with it
# ...find the largest circle and the psFracy that goes with it
psmax = max(d6$psFracy) 
psmin = min(d6$psFracy[d6$psFracy>0])# smallest non-zero psFracy 
# I see that stillStuff are all in this range

# add legend  
legend(8790,550,legend=c(paste("Fraction =",round(psmax,3)), 
  paste("Fraction =",round(psmin,3)),"Fraction = 0", "Hydrophone"), 
  pch=c(21,21,19,17), pt.cex=c(1+2*psmax,1+2*psmin,1,1.5), cex=1.2) 
 
# add A and B and C and D with arrows
text(8830,480,"A",cex=1.5)
arrows(8833,479,8873,455,lwd=2,length=0.15)
text(8910,520,"B",cex=1.5)
arrows(8906,519,8891,505,lwd=2,length=0.15)
text(8820,380,"D",cex=1.5)
arrows(8824,380,8848,390,lwd=2,length=0.15)
text(8850,300,"C",cex=1.5)
arrows(8853,305,8865,324,lwd=2,length=0.15)

# add clouds for two tags
i=4 # south beacon
points(stillStuff[[i]]$data$easting, stillStuff[[i]]$data$northing, pch=4,cex=0.5)
   
i=15 # a roaming spot
points(spoty[[i]]$easting, spoty[[i]]$northing, pch=2, cex=0.5)





# the target locations
points(d6$targetEasting, d6$targetNorthing, pch=19, col="green", cex=0.5)
# the best estimate locations
points(d6$foundEasting, d6$foundNorthing, pch=19, col="red", cex=0.5)




################### redo this plot for the paper
# Plot 2 again: the psFrac
# NOTE THAT TO GET THE X- AND Y-AXES SCALES TO MATCH I DO A MANUALLY RESIZE 
# THE WINDOW AFTER DRAWING THE PLOT

# the raw plot
par(mar=c(4,5,1,1)+0.1)
# shift easting and northing to start at (0,0)
tempEshift = 8769
tempNshift = 300

plot(cmd$sdlEN$easting-tempEshift, cmd$sdlEN$northing-tempNshift, 
  type="n", bty="l", las=1,
  xlab="", ylab="", cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,220), ylim=c(0,230)
  #xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing
)
# the sdls
points(cmd$sdlEN$easting-tempEshift, cmd$sdlEN$northing-tempNshift, 
  pch=17, col="black", cex=1.5)
mtext(text="Easting (m)",side=1,line=2.5,cex=1.5)
mtext(text="Northing (m)",side=2,line=3.5,cex=1.7)
 

# add points at the best estimate locations with size = fracPS
#points(d6$foundEasting, d6$foundNorthing, pch=19, cex=(1+d6$psFracn))
points(d6$foundEasting-tempEshift, d6$foundNorthing-tempNshift, 
  pch=21, col="black", cex=(1+2*d6$psFracy) )
   
# add points that have data before filtering but not after
emptyPts = d6[is.nan(d6$foundNorthing) ,] 
points(emptyPts$targetEasting-tempEshift, emptyPts$targetNorthing-tempNshift, 
  pch=19, col="black", cex=1 ) 
  
# add the stationary tags' circles
for (i in 1:length(stillStuff)){
  points(stillStuff[[i]]$foundEN[1]-tempEshift, stillStuff[[i]]$foundEN[2]-tempNshift, 
    pch=21, cex=(1+2*stillStuff[[i]]$psFracy), col="black")
}

# figure out the legend details
# ...find the smallest circle and the psFracy that goes with it
# ...find the largest circle and the psFracy that goes with it
psmax = max(d6$psFracy) 
psmin = min(d6$psFracy[d6$psFracy>0])# smallest non-zero psFracy 
# I see that stillStuff are all in this range

# add legend  
legend(132,45,
  legend=c(paste("Fraction =",round(psmax,3)," (Max)"), 
  paste("Fraction =",round(psmin,3),"(Min)"),"Fraction = 0", "Hydrophone",
    "Individual position solution"), 
  pch=c(21,21,19,17,4), pt.cex=c(1+2*psmax,1+2*psmin,1,1.5,1), cex=1.0) 


# add clouds for two tags
i=4 # south beacon
points(stillStuff[[i]]$data$easting-tempEshift, 
  stillStuff[[i]]$data$northing-tempNshift, pch=4,cex=0.5)
   
i=15 # a roaming spot
points(spoty[[i]]$easting-tempEshift, 
  spoty[[i]]$northing-tempNshift, pch=2, cex=0.5)

# add A and B 

text(95,25,"A",cex=1.5)

text(76,89,"B",cex=1.5)










#################################################################################
#############################################################################
# below here is the old way                               
tagCount = length(tagn)   

  
### some checks
# number of hours over two days
(tail(tagn[[1]]$data$utime,1) - tagn[[1]]$data$utime[1])/3600
(tail(tagy[[1]]$data$utime,1) - tagy[[1]]$data$utime[1])/3600

#
as.POSIXlt(tagn[[1]]$data$utime[1], origin="1970-1-1")
as.POSIXlt(tail(tagn[[1]]$data$utime,1), origin="1970-1-1")
#
as.POSIXlt(tagy[[1]]$data$utime[1], origin="1970-1-1")
as.POSIXlt(tail(tagy[[1]]$data$utime,1), origin="1970-1-1")



# our best estimates of reef and sdl locations, in UTM
cmd$reefEN
cmd$sdlEN


# a plot
tagIndex = 3 
cTag = tagy[[tagIndex]]
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="black", cex=1.5,
  main=paste("Tag",tagy[[tagIndex]]$tagName))
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=19, col="red", cex=1.5)
oneTag = d6[d6$tagID == substr(cTagNames[tagIndex],2,3),]
points(oneTag$targetEasting, oneTag$targetNorthing, pch=19, col="blue", cex=1)


################################################################################
# pick only positions during the right time intervals 
tagIndex = 8 #tag43=1, 44=2, 45=3, 46=4, 47=5, 48=6, 49=7, 50=8, 51=9, 52=10
cTag = tagy[[tagIndex]]
oneTag = d6[d6$tagID == substr(cTagNames[tagIndex],2,3),]

# gather all cTag lines between each of the oneTag$datiStart and oneTag$datiStop times
# ...in other words...remove the points recorded when the buoy was being moved
ct2 = vector(length=nrow(oneTag))
loc = vector("list",nrow(oneTag))
for (i in 1:nrow(oneTag)){   
  loc[[i]] = cTag$data[
    (cTag$data$utime > oneTag$datiStart[i]) & 
    (cTag$data$utime < oneTag$datiStop[i]), ]
  ct2[[i]] = loc[[i]]  
}

# look at this so far for 1 June 2009
# one tag at a time
june02 = as.POSIXlt("2009-06-02")

ct3 = ct2[[1]][ct2[[1]]$utime < june02,]
plot(ct3$utime, rep(1,nrow(ct3)), pch=19,, cex=0.5, 
  xlim = c( as.POSIXct("2009-06-01 13:50:00"), as.POSIXct("2009-06-01 19:00:00") ))
abline(v=d6$datiStart, col="red")
abline(v=d6$datiStop, col="blue")

# look at this so far for 3 June 2009
# one tag at a time
june02 = as.POSIXlt("2009-06-02")
ct3 = ct2[ct2$utime > june02,]
plot(ct3$utime, rep(1,nrow(ct3)), pch=19,, cex=0.5,
  xlim = c( as.POSIXct("2009-06-03 09:20:00"), as.POSIXct("2009-06-03 13:50:00") ))
abline(v=d7$datiStart, col="red")
abline(v=d7$datiStop, col="blue")
                                             
zzz why do all the tags stop giving positions before the tags were pulled out of the water?  Is it 
possible the watch was different from the computer and SDLs?  Don't delete points before startTime and 
and see if the points are recorded before startTime.   Also, look at detections and "position solutions"

################################################################################
# draw the temporal picture of how often each tag gave a position resolution
allTagsParsed = vector("list",tagCount)

for (i in 1:tagCount){
  currentTag = allTagsFiltered[[i]] # get ALPS data for tag i 
  d7 = d6[d6$tagID == whichTag[i],]
  
  # gather all currentTag lines between each of the d7$datiStart and d7$datiStop times
  # ...in other words...remove the points recorded when the buoy was being moved
  ct2 = currentTag[ !(currentTag$utime<d7$datiStart[1]) ,]
  for ( j in 1:(nrow(d7)-1) ) {   
    ct2 = ct2[
      (ct2$utime < d7$datiStop[j]) | 
      (ct2$utime > d7$datiStart[j+1]),]
  }
  ct2 = ct2[ct2$utime < d7$datiStop[nrow(d7)],]
  
  # now save this tag's data and move on the the next tag
  allTagsParsed[[i]] = ct2        
}           

# now plot all tags on one plot
plot(allTagsParsed[[1]]$utime, rep(1,nrow(allTagsParsed[[1]])),
  type="n", cex=0.5, ylim=c(0,11), 
  xlim=c( as.POSIXct("2009-06-01 13:50:00"), as.POSIXct("2009-06-01 19:00:00")))
for (i in 1:tagCount){
  # draw points for tag positions through time
  points(allTagsParsed[[i]]$utime, rep(i, nrow(allTagsParsed[[i]])),
    pch=19, cex=0.5)
  # draw lines for datiStart and datiStop
  d7 = d6[d6$tagID == whichTag[i],]  
  for (j in 1:nrow(d7)){
    points(x=rep(d7[j,]$datiStart,2), y=c(i-0.4, i+0.4), type="l", col="red")
    points(x=rep(d7[j,]$datiStop,2), y=c(i-0.4, i+0.4), type="l", col="blue")
  }  
}


 
                                             
                                                                                
################################################################################
# find the spatial picture of frequency of position solutions

# add columns to d6 to hold the 'number of positions' in each buoy location
d9 = cbind(d6, posCount=rep(-1,56), posFrac=rep(-1,56), 
  meanLat=rep(-1,56), meanLong=rep(-1,56))

# count the 'number of positions' recorded at each buoy location
for (i in 1:nrow(d9)){
  # for each row in d6, pick out the data.frame from the 'allTagsFiltered' list
  whichrow = d9[i,] # pick a row from d8
  whichtag = whichrow$tagID # find out which tag, that row describes
  tagIndex = which(d1$tagID == whichtag) # find the tagID for the tag on that row
  currentTag = allTagsFiltered[[tagIndex]] # get all the position solutions for that tag
  # pick only position solutions during the times listed on 'whichrow'
  currentTag = currentTag[
    (currentTag$utime>whichrow$datiStart) & (currentTag$utime<whichrow$datiStop),]
  posCount = nrow(currentTag)
  d9[i,]$posCount = posCount # count position solutions during that buoy location
  d9[i,]$posFrac = posCount/(as.double(d9[i,]$elapsedTime) * 30)
  d9[i,]$meanLat = mean(currentTag$northing)
  d9[i,]$meanLong = mean(currentTag$easting)
}


# plot the results

# a plot
plot(sdls$easting, sdls$northing, pch=2, col="black", cex=1.5, main="")
points(reef[1], reef[2], pch=19, col="red", cex=1.5)
points(d9$meanLong, d9$meanLat, pch=19, col="green", cex=5*d9$posFrac)
points(d9$targetLong, d9$targetLat, pch=1, col="blue", cex=1)

also...make this same plot, one  set at a time














# This is try two ... "Try it again"
setwd("C:/zy/Telemetry/R summary files")
fileName = "internal performance locations used 2011Mar25.txt"
colNames = c("set", "location", "buoy", "tag", "date", "startTime", "stopTime",
  "latitude", "longitude")
colClasses.z = c("factor", "numeric", "character", "factor", "character",
  "character", "character", "numeric", "numeric")

bob = read.table(fileName, header=TRUE, col.names=colNames)
