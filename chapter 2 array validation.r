################################################################################
################################################################################
################################################################################
################################################################################
### Testing Array Performance
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
### detections
################################################################################
#
# These are the data sets which can be used for this:
# 1. 7 Dec 2007 50m deployment, B79100-B79500
# 2. 22 July 2008 single SDL/single tag detection trials at 200m and 300m, T60800
# 3. 9 Oct 2008 125m deployment, B79100-79500
# 4. 23 April 2009 spacing trial, 125m
# 5. 7 May 2009 spacing trials, 150m and 100m  
# 6. 1 June 2009 internal performance, 100m
# 7-13. 1 June 2009 - 30 Nov 2009 100m deployments

# From these days I want the distance between each tag and each SDL, then I 
# want to calculate the hourly and total detection rate.  For this use *.toa
# data.
#
# I also want to calculate the position solution rate for each tag depending
# on the array spacing, and how it changes through time.




# First read/execute another file which creates 'tagList[[ ]]'.
# This list has tag info for these tags from these deployments...t.r")
#
# 5 beacons in 2007 deployment
# Don't do this for the single SDL detection trial...
# 5 beacons in 2008 deployment
# 3 fish tags, 2 beacons in 125m spacing trial
# 2 fish tag, 4 beacons, 1 sentinel in 150 spacing trial
# 2 fish tag, 4 beacons, 1 sentinel in 100m spacing trial
# 40 fish tags first internal performance.  Acutally, 8 tags at 40 places
#    (The 4 beacons, 1 sentinel will be included in first 2009 fish study) 
# 10 fish tags first internal performance
#    (The 4 beacons, 1 sentinel will be included in first 2009 fish study) 
# 4 beacon, 1 sentinel - fish study
# 4 beacon, 1 sentinel - fish study
# 4 beacon, 1 sentinel - fish study
# 4 beacon, 1 sentinel - fish study
# 4 beacon, 1 sentinel - fish study
# 4 beacon - fish study
# 4 beacon - fish study
#
# -or in other words-
# 5+5+5+5+5+5+5+4+4 = 43 in fish studies
# 19 in spacing trials + 43 = 62
# 56 in internal performance + 62 = 118
#
# Each element in tagList has: tagName, deployment, tagLocation, startUtime, stopUtime
load("C:/zy/school closet/Telemetry/R summary files/tagList 2011Apr09.rdata")

# get the global code
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


library(ggplot2)
library(plotrix) # for multhist
library(TeachingDemos) # for subplot

# get the info for internal performance 
("C:/zy/school closet/Telemetry/R summary files/internal performance locations used 2011Mar25.txt")



   
tagList[[1]]
  
# to decide how to handle things, how long is each tag in the water
outTimes = data.frame(tag=NA, time=NA)
for (i in 1:length(tagList)){
  outTimes[i,] = c(i,(tagList[[i]]$stopUtime-tagList[[i]]$startUtime)/3660)
}

plot(outTimes,pch=19)
plot(outTimes,pch=19,ylim=c(0,6))
# I'll use 5hrs as a cut off for doing hourly calculations or just one, I'm 
# thinking mostly of the internal array performance tags, not out for very long


################################################################################
# A function in which I specify one tag, one deployment and calculate the
# detection frequency at different distances, through time, and maybe at 
# different water conditions.  I also calculate the position solution frequency
# at different array spacings, through time, and maybe at different water 
# conditions.

testTheTag = function(cTag, showme=FALSE, filterMe=TRUE, psrMe=TRUE){
  # cTag = 1;  tagName = tagList[[cTag]]$tagName; deployment = tagList[[cTag]]$deployment; tagLocation = tagList[[cTag]]$tagLocation; startUtime=tagList[[cTag]]$startUtime; stopUtime=tagList[[cTag]]$stopUtime
  # maybe later get the ADCP data
  
  tagName=cTag$tagName 
  deployment=cTag$deployment 
  tagLocation=cTag$tagLocation 
  startUtime=cTag$startUtime 
  stopUtime=cTag$stopUtime
  
  tagType = substr(tagName,1,1)
  tagID = substr(tagName,2,10)
  numSdl = 5
  showRed = FALSE  # switch for plotting in red when fractions are above 1
  
  # get info from md[[]]
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      print(i)
      spacing = md[[i]]$spacing
      homeDir = paste(md[[i]]$homeDir,"/ALPS 2011Feb14", sep="")
      bestBeacon = substr(md[[i]]$bestBeacon,2,10)
      secondBestBeacon = substr(md[[i]]$secondBestBeacon,2,10)
      reefEN = md[[i]]$reefEN
      sdlEN = md[[i]]$sdlEN 
      # is this beacon/sentinel at the center or not, find out which SDL it was on...      
   
      location = "lost"
      for (j in 1:nrow(md[[i]]$beaconEN)){                
        if(tagName == md[[i]]$beaconEN$beaconID[j]){
          location = md[[i]]$beaconEN$location[j]
        } 
      } # end for-loop
       
      # in a couple cases the preceeding lines won't find the right location...
      if(location == "lost"){
        # you might have one of the problem 2008 tags, where codes 
        # get used in place of beacons
        if ((tagName == "b80") | (tagName == "b81" )) {location=42}
        if ((tagName == "b85") | (tagName == "b86" )) {location=44}
        if (tagName == "b130") {location=45}
        # or you're using a fish tag in one of the trials
        if (tagType == "f"){location="inside"}  # these tags are all over the place
        # now the sp150/sp100...read the metadata about these...
        # N41 had B2, B79400.  C45 had B1, B79500
        # The sentinel s79600 and T61000 were at the reef
        if ((tagName == "b2") | (tagName == "b79400")){location="41"}
        if ((tagName == "b1") | (tagName == "b79500")){location="45"}
      }
      
      # ...then designate it as center or not
      center = ifelse(((location==45)|(location=="reef")|(location=="inside")),T,F) 
    }  
  }
  setwd(homeDir)
  
  # if cTag == bestBeacon, then use secondBestBeacon
  cBeacon = ifelse(grepl(bestBeacon,tagName), secondBestBeacon, bestBeacon)
  
  # calculate the nubmer of transmissions this tag made during the entire deployment
  numSec = stopUtime - startUtime 
  numMin = numSec/60 # this is for visually weighting data points
  if (tagType == "f"){
    numPings = numSec / 2
    pingsPerHour = 30 * 60
  } else if (tagType == "b"){
    numPings = numSec / 20
    pingsPerHour = 180
  } else if(tagType == "s"){ 
    # it's a sentinel 5min on, 25min off, 2 sec burst interval = 300 pings/hr 
    # Luckily all the sentinels were out longer than 1 hour...makes it easier
    numPings = (numSec / 3600) * 300
    pingsPerHour = 300
  } else {print("This tag isn't right")}
  
  # calculate the distance between this tag and 5 SDLs
  sdlDist = sqrt( (sdlEN$easting-tagLocation[1])^2 + 
    (sdlEN$northing-tagLocation[2])^2 )

  # read in the *.toa files for this deployment
  fileNames = list.files(pattern=paste("TxId",tagID,".toa",sep=""), recursive=TRUE, ignore.case=TRUE)
  d1 = lapply( as.list (fileNames), read.table, header=FALSE)
  d2 = do.call("rbind", d1)    # I'M NOT SURE THESE ARE IN CHRONOLOGICAL ORDER
  # gather the non -1 detections times during the interval by each SDL individually
  sdlList = list() # a list of 5 SDLs
  sdlList[[1]] = d2$V1[(d2$V1 != -1) & (d2$V1 > startUtime) & (d2$V1 < stopUtime)]   
  sdlList[[2]] = d2$V6[(d2$V6 != -1) & (d2$V6 > startUtime) & (d2$V6 < stopUtime)]
  sdlList[[3]] = d2$V11[(d2$V11 != -1) & (d2$V11 > startUtime) & (d2$V11 < stopUtime)]
  sdlList[[4]] = d2$V16[(d2$V16 != -1) & (d2$V16 > startUtime) & (d2$V16 < stopUtime)]
  sdlList[[5]] = d2$V21[(d2$V21 != -1) & (d2$V21 > startUtime) & (d2$V21 < stopUtime)]
  
  # in a couple cases (tags 46 and 48) during the internal array performance 
  # test there were no tag detections by some SDLs so check for and fix this
  # so that the logic test in the next for-loop doesn't fail when it reaches
  # and NA
  for (i in 1:numSdl){
    if(length(sdlList[[i]]) == 0){sdlList[[i]][1] = startUtime}
  }      
  
  # to make the 'cut()' results uniform add a detection at the start and stopUtimes
  # if they're not already there
  for (i in 1:numSdl){ 
    if(sdlList[[i]][1] != startUtime){sdlList[[i]] = c(startUtime, sdlList[[i]])}
    if(sdlList[[i]][length(sdlList[[i]])] != stopUtime){sdlList[[i]] = c(sdlList[[i]],stopUtime)}
  }

  ### DETECTIONS ###
  # Total detections over the entire deployment by each SDL
  detFrac=vector(length=numSdl)
  for (i in 1:numSdl){ detFrac[i] = length(sdlList[[i]])/numPings }
  
  # a check that my fractions are less than 1
  redFlag = FALSE
  for (i in 1:numSdl){
    if((detFrac[i] > 1)&(showRed))  {redFlag=TRUE}
  }
  if(redFlag){cColors = "red"} else {cColors = "black"}
  
  par(mfrow=c(2,2))
  plot(sdlDist, detFrac, pch=19, main=paste(tagName, deployment), ylim=c(0,1),
    col=cColors)

  # Look at hourly detections
  datiG = list()
  dSplit = list()
  for (i in 1:numSdl){
    datiG[[i]] = as.POSIXlt(sdlList[[i]], origin="1970-1-1", tz="GMT")
    dSplit[[i]] = split(sdlList[[i]], cut(datiG[[i]], breaks="hour")) 
  }
  
  # How many hours were in this deployment 
  deploymentHour = 1:length(dSplit[[1]])  #hourList = attr(dSplit[[1]], "names")
  
  # Create a list of lists to hold the hourly detection fraction:
  # ... a list of 5 elements, each element a list with one element for each hour
  # ...but the first and last hours are incomplete...drop them or figure the 
  # ...fraction of the hour   
  hourlyDetFrac = list()  
  for (i in 1:numSdl){ hourlyDetFrac[[i]] = vector(length=length(dSplit[[i]])) }
  
  # Calculate the hourly detection fraction of this tag by each SDL each hour
  # ... for tags out less than 5hrs (I'm thinking of the internal performance test)
  # ... just calculate one detection fraction. 5hrs = 18000sec
  if (numSec > 18000){ # if it's more than 5hrs...
       
    for (i in 1:numSdl){
      # the 'first' (and 'last') hour started mid way through the hour...I could 
      # find the fraction of the hour actually used...but with hundreds of 
      # hours this won't change the answer much
      
      # Don't use this code for special first hour calculations
      #j=1
#      # look at the startUtime to figure out which hour it's in
#      fd=as.POSIXlt(startUtime, origin="1970-1-1", tz="GMT")
#      # now find the utime of the start of that hour...what a pain
#      temp1 = paste(fd$year+1900,"-",fd$mon+1,"-",fd$mday," ",fd$hour,":00:00", sep="")
#      startOfFirstHour = as.POSIXct(strptime(temp1, "%Y-%m-%d %H:%M:%S", tz="GMT"), 
#        origin="1970-1-1", tz="GMT")
#      notused = fd-startOfFirstHour # this is how much of the hour was not used
#      # how many pings during the partial hour
#      partialPings = floor(pingsPerHour * (60-notused[[1]])/60) 
#      # fraction of partialPings detected
#      hourlyDetFrac[[i]][[j]] = length(dSplit[[i]][[j]]) / partialPings

      # now calculate hourlyDetFrac
      for (j in 1:(length(dSplit[[1]]))){ 
        hourlyDetFrac[[i]][[j]] = length(dSplit[[i]][[j]]) / pingsPerHour 
      }   
    } # end for (i in 1:numSdl) loop
  } else { # else numSec < 18000 so calculate just one fracDet
    for (i in 1:numSdl){ # for each SDL calculate one overall detFrac
      # make hourlyDetFrac[[i]] be only one element long
      hourlyDetFrac[[i]] = hourlyDetFrac[[i]][1]
      
      j=1 # the only one
      totalHours = numSec/3600
      totalPings = floor(pingsPerHour * totalHours)
      hourlyDetFrac[[i]][[j]] = length(sdlList[[i]]) / totalPings
    } 
  } # end else (stopUtime-startUtime)<2hrs so calculate just one fracDet
  
  
  # a check that my fractions are less than 1
  redFlag = FALSE
  for (i in 1:numSdl){ if((max(hourlyDetFrac[[i]]) > 1)&(showRed)) {redFlag=TRUE} }
  
  # show the hourlyDetFrac  
  plot(hourlyDetFrac[[1]], type="n", xlab="hour of deployment", ylim=c(0,1))
  for (i in 1:numSdl){                               
    if(redFlag){cColors = rep("red",numSdl)} else {cColors = plotColors}
    points(hourlyDetFrac[[i]], type="b", col=cColors[i]) 
  }
  
  # Read in ADCP data   
  ad = importADCPdata()
  # Pick out data within the current deployment
  ad1 = ad[(ad$utime > startUtime) & (ad$utime < stopUtime),]

  if(nrow(ad1)>0){
    # To make ADCP data cover the same time as the detection data...
    # ...change the first and last time to start and stopUtime, make the data=NA
    ad2 = ad1
    ad2$utime[1] = startUtime
    ad2$datiG[1] = as.POSIXlt(startUtime, origin="1970-1-1", tz="GMT")       
    ad2$datiL[1] = as.POSIXlt(startUtime, origin="1970-1-1", tz="EST5EDT")   
    ad2[1,4:16] = NA 
    ad2$utime[nrow(ad2)] = stopUtime
    ad2$datiG[nrow(ad2)] = as.POSIXlt(stopUtime, origin="1970-1-1", tz="GMT")       
    ad2$datiL[nrow(ad2)] = as.POSIXlt(stopUtime, origin="1970-1-1", tz="EST5EDT")   
    ad2[nrow(ad2),4:16] = NA
  
    # calculate hourly averages for each deployment hour
    aSplit = split(ad2, cut(ad2$datiG, breaks="hour"))
    temp1 = length(aSplit)
    aHourlyMeans = data.frame("tem"=rep(NA,temp1), "dep"=rep(NA,temp1), 
      "eaaL"=rep(NA,temp1), "eaaM"=rep(NA,temp1), "eaaU"=rep(NA,temp1), 
      "magL"=rep(NA,temp1), "magM"=rep(NA,temp1), "magU"=rep(NA,temp1), 
      "dirL"=rep(NA,temp1), "dirM"=rep(NA,temp1), "dirU"=rep(NA,temp1))
    for (i in 1:temp1){
      aHourlyMeans[i,] = colMeans(aSplit[[i]][,6:16])     
    }
  
    # show water conditions v. hourlyDetFrac
    if (showme){ # I don't see any patterns in here
      par(mfrow=c(2,3))
      plot(aHourlyMeans$tem, hourlyDetFrac[[1]], type="n", ylim=c(0,1))
      for(i in 1:numSdl){
        points(aHourlyMeans$tem,hourlyDetFrac[[i]],pch=19,cex=0.2,col=plotColors[i])
      }
      plot(aHourlyMeans$dep, hourlyDetFrac[[1]], type="n", ylim=c(0,1))
      for(i in 1:numSdl){
        points(aHourlyMeans$dep,hourlyDetFrac[[i]],pch=19,cex=0.2,col=plotColors[i])
      }
      plot(aHourlyMeans$eaaL, hourlyDetFrac[[1]], type="n", ylim=c(0,1))
      for(i in 1:numSdl){
        points(aHourlyMeans$eaaL,hourlyDetFrac[[i]],pch=19,cex=0.2,col=plotColors[i])
      }
      plot(aHourlyMeans$magL, hourlyDetFrac[[1]], type="n", ylim=c(0,1))
      for(i in 1:numSdl){
        points(aHourlyMeans$magL,hourlyDetFrac[[i]],pch=19,cex=0.2,col=plotColors[i])
      }
      plot(aHourlyMeans$dirL, hourlyDetFrac[[1]], type="n", ylim=c(0,1))
      for(i in 1:numSdl){
        points(aHourlyMeans$dirL,hourlyDetFrac[[i]],pch=19,cex=0.2,col=plotColors[i])
      }
    }
  } else { # end if(nrow(ad1)>0)
    showme=FALSE
    aHourlyMeans = data.frame("tem"=NA, "dep"=NA, 
      "eaaL"=NA, "eaaM"=NA, "eaaU"=NA, 
      "magL"=NA, "magM"=NA, "magU"=NA, 
      "dirL"=NA, "dirM"=NA, "dirU"=NA)
  }
  
  ##############################################33 
  
  ### POSITION SOLUTIONS ###
  d1 = importALPSdata(deployment=cTag$deployment, tagName=cTag$tagName, psr=psrMe)
  # if there are any data in d1 then filter it
  if ( nrow(d1[1]$data) > 1 ){
    if (filterMe){
      d2 = filterALPSdata(df1=d1, cnF=1.5, speedF=0.8, minuteMean=FALSE)$data
    } else {
      d2 = filterALPSdata(df1=d1, minuteMean=FALSE)$data
    }
    # gather the PS times during the interval
    d3 = d2$utime[(d2$utime > startUtime) & (d2$utime < stopUtime)]
    # to make the number of hours consistent with detection stuff above add
    # a PS at the start and stopUtime
    d3 = c(startUtime, d3, stopUtime) 
  
    # Total PS over the entire deployment 
    psFrac = length(d3)/numPings
  } else { # there are no data in d1, so make the answers = 0
    d3 = c(startUtime, stopUtime)
    psFrac = 0
  }
  
  
  # Look at hourly PS
  datiG = as.POSIXlt(d3, origin="1970-1-1", tz="GMT")
  pSplit = split(d3, cut(datiG, breaks="hour"))
  
  # check that the number of hours is the same
  if (length(dSplit[[1]]) != length(pSplit)){
    print("The number of hours isn't right")
  }
  
  # Create a list to hold the hourly PS fraction:
  # ... with one element for each hour  
  hourlyPsFrac = vector(length=length(pSplit)) 
 
  # Calculate the hourly PS fraction of this tag each hour
  # ... for tags out less than 5hrs (I'm thinking of the internal performance test)
  # ... just calculate one detection fraction. 5hrs = 18000sec
  if (numSec > 18000){ # if it's more than 5hrs...
    # the first and last hours could be handled differently because they're 
    # partial hours, but I won't do that now.
  
    # Calculate the hourly PS fraction of this tag by the array
    for (i in 1:length(pSplit)){hourlyPsFrac[i]=length(pSplit[[i]])/pingsPerHour}
  } else { # else numSec < 18000 so calculate just one PsFrac
    # make hourlyPSFrac be only one element long
    hourlyPsFrac = hourlyPsFrac[1]
    # calculate the one PsFrac
    i=1 # the only one
    # totalHours and totalPings from above
    hourlyPsFrac[i] = psFrac
  }
 
   # a check that my fractions are less than 1
  redFlag = FALSE
  if((max(hourlyPsFrac) > 1)&(showRed)) {redFlag=TRUE}
  
  # show the hourlyPsFrac
  if(redFlag){cColor="red"} else {cColor="black"} 
  plot(hourlyPsFrac, ylim=c(0,1), xlab="hour of deployment", 
    type="b", col=cColor)  
  
  # show the average hourlyDetFrac v hourlyPsFrac
  meanHourlyDetFrac = rowMeans(cbind(hourlyDetFrac[[1]], hourlyDetFrac[[2]], hourlyDetFrac[[3]], 
    hourlyDetFrac[[4]], hourlyDetFrac[[5]]))
  plot(meanHourlyDetFrac, hourlyPsFrac, pch=19, xlim=c(0,1), ylim=c(0,1))
  
  
  # show water conditions v. hourlyPsFrac
  if (showme){ # I don't see any patterns in here
    par(mfrow=c(2,3))
    plot(aHourlyMeans$tem, hourlyPsFrac, pch=19,cex=0.2,ylim=c(0,1))
    plot(aHourlyMeans$dep, hourlyPsFrac, pch=19,cex=0.2,ylim=c(0,1))
    plot(aHourlyMeans$eaaM, hourlyPsFrac, pch=19,cex=0.2,ylim=c(0,1))
    plot(aHourlyMeans$magM, hourlyPsFrac, pch=19,cex=0.2,ylim=c(0,1))
    plot(aHourlyMeans$dirM, hourlyPsFrac, pch=19,cex=0.2,ylim=c(0,1))
  }

  # combine hourly data for ggplots later on
  hourlyMeans = data.frame(
    hour=1:length(hourlyDetFrac[[1]]),
    detFrac1=hourlyDetFrac[[1]],
    detFrac2=hourlyDetFrac[[2]],
    detFrac3=hourlyDetFrac[[3]],
    detFrac4=hourlyDetFrac[[4]],
    detFrac5=hourlyDetFrac[[5]],
    psFrac=hourlyPsFrac,
    temperature=aHourlyMeans$tem,
    magL=aHourlyMeans$magL,
    eaaL=aHourlyMeans$eaaL,
    eaaM=aHourlyMeans$eaaM,
    eaaU=aHourlyMeans$eaaU
  )
    

  ### RETURN RESULTS ###
  answer=list(hourlyMeans=hourlyMeans,tagName=tagName, deployment=deployment, 
    numMin=numMin, 
    spacing=spacing, center=center, sdlDist=sdlDist, detFrac=detFrac, 
    psFrac=psFrac)
  return(answer)

} # end testTheTag() function



bob = testTheTag(cTag=tagList[[1]], showme=F)




allresults = list()
filterMeNow = T
psrMeNow = F

# 2007 deployment
for (i in 1:5){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(cTag=tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}

# 2008 deployment...the problem child
for (i in 6:10){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(cTag=tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}
# cTag 6 and 7 are b80 and b81, which are b79200
# ...something wrong with these so I won't use them...
# cTag 8 and 9 are b85 and b86, which are b79400
# cTag 10 is b130, which is b79500
allresults[[6]]$detFrac = rep(NA,5) #allresults[[6]]$detFrac + allresults[[7]]$detFrac
allresults[[7]]$detFrac = rep(NA,5)
allresults[[6]]$psFrac = NA #allresults[[6]]$psFrac + allresults[[7]]$psFrac
allresults[[7]]$psFrac = NA

allresults[[8]]$detFrac = allresults[[8]]$detFrac + allresults[[9]]$detFrac
allresults[[9]]$detFrac = rep(NA,5)
allresults[[8]]$psFrac = allresults[[8]]$psFrac + allresults[[9]]$psFrac
allresults[[9]]$psFrac = NA


plot(allresults[[1]]$sdlDist, allresults[[1]]$detFrac, pch=19, xlim=c(0,250), 
  ylim=c(0,1), type="n")
for (i in 1:10){
  points(allresults[[i]]$sdlDist, allresults[[i]]$detFrac, pch=19, col=plotColors[i])
}
                                
# hb1:11-15. sb1:16-20. sb2:21-25. hb2:26-30.  sb3:31-35. hb3:36-39. sb4:40-43.
# ...remember these are only the stationary tags
for (i in 11:43){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}

# spacing trials...44-62
# 
for (i in 44:62){ # cycle through all tags in the spacing trials
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}

# inter performance trials...
for (i in c(63:112)){ # cycle through all tags in the internal performance trial
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}
                            
               
   

######
# 
# I want to do plots with unfiltered and filtered psFrac...always using psr
zz rawResults = allresults  # This used to be 'rawResults' ...unfiltered
zz filteredResults = allresults  # This used to be 'filteredResults' 


# gather some things into one dataframe for regressions
# add regression lines
# gather data into one place

distVec = c(); detFrac = c(); detNumMin = c();
spacingVec = c(); psFracRaw = c(); psFracFiltered = c();  psNumMin = c();
for (i in 1:length(rawResults)){ # for each
  # detections
  distVec = c(distVec, rawResults[[i]]$sdlDist)
  detFrac = c(detFrac, rawResults[[i]]$detFrac)
  detNumMin = c(detNumMin, 
    rep(rawResults[[i]]$numMin, length(rawResults[[i]]$detFrac))) 
  
  # PS
  spacingVec = c(spacingVec, rawResults[[i]]$spacing)
  psFracRaw = c(psFracRaw, rawResults[[i]]$psFrac)
  psFracFiltered = c(psFracFiltered, filteredResults[[i]]$psFrac) 
  psNumMin = c(psNumMin, rawResults[[i]]$numMin) 
  
}

detections = data.frame(distVec,detFrac,detNumMin)
solutions = data.frame(spacingVec,psFracRaw,psFracFiltered,psNumMin)

plot(distVec,detFrac,pch=19)




##############################################################################
# a plot of distance v detFrac, filtering and psr don't affect this so it's okay
# to use allresults.
plColors = rep("black",length(allresults))
plType = rep(19,length(allresults))
plSize = rep(1,length(allresults))
for(i in 1:length(allresults)){
  if(allresults[[i]]$numMin < 3600){
    plType[i]=4; plColors[i]="red"; plSize[i]=1.2;
  }
}

# plot 1
# a plot of distance v detFrac
par(mar=c(5,5,1,1)+0.1)
plot(allresults[[1]]$sdlDist, allresults[[1]]$detFrac, las=1, type="n", bty="l",
  cex.axis = 1.5, cex.lab=1.5,
  xlab="Distance Between Transmitter and Hydrophone (m)",
  ylab="",  
  xlim=c(0,300), ylim=c(0,0.99))
for (i in 1:length(allresults)){
  points(allresults[[i]]$sdlDist, allresults[[i]]$detFrac, pch=plType[i], 
    cex=plSize[i], col="black")
}
abline(h=0)
# add regression lines caluclated below
abline(longerlm,lwd=2)
abline(shorterlm,lwd=2,lty=2)

mtext("Detection Fraction", side=2, line=3.5, cex=1.7)
# now add the single HP-single tag detection trial results... find them in
# "C:\zy\The closets\data closet\Telemetry\2008\2008 Jul 22 SDL detection trials"
# "SDL detection trial results from ALPS.xls"
# at 200m for 7 min, detection fraction is 0.679
# at 300m for 5 min, detection fraction is 0.245
points(c(200,300), c(0.679, 0.245), pch=4, cex=1.2, col="black" )
# add a legend
legend(220,1.0,legend=c("Longer than 6 hr","Shorter than 6 hr"),pch=c(19,4))

# add regression lines
# I need to know which are longer and which are shorter. I'll gather them in
# separate lists. This would have simplified
# the preceeding plotting commands.

longerDist =  longerFrac = longerMin = shorterDist = shorterFrac = shorterMin = c()
for(i in 1:length(allresults)){
  if(allresults[[i]]$numMin < 3600){
    shorterDist = c(shorterDist,allresults[[i]]$sdlDist)  
    shorterFrac = c(shorterFrac,allresults[[i]]$detFrac)  
    shorterMin = c(shorterMin,allresults[[i]]$numMin) # for check 
  } else {
    longerDist = c(longerDist,allresults[[i]]$sdlDist)  
    longerFrac = c(longerFrac,allresults[[i]]$detFrac)  
    longerMin = c(longerMin,allresults[[i]]$numMin) # for check 
  }
}
# add single HP-tag detections to shorter list
shorterDist = c(shorterDist,200,300)
shorterFrac = c(shorterFrac,0.679,0.245)


plot(longerDist,longerFrac,pch=19,xlim=c(0,300),ylim=c(0,1))
points(shorterDist,shorterFrac,pch=4)
# now fit regression lines and make them ready to put on the last plot

longerlm = lm(longerFrac~longerDist)   #intercept = 0.817384. slope = -0.001248
shorterlm = lm(shorterFrac~shorterDist) #intercept = 0.883628. slope = -0.001841
abline(longerlm)
abline(shorterlm)

# crap all over it.
longdata = cbind(longerDist,longerFrac,"longer")
shortdata = cbind(shorterDist,shorterFrac,"shorter")
alldata = rbind(longdata,shortdata)
moreall = data.frame(sdlDist=as.numeric(alldata[,1]), detFrac=as.numeric(alldata[,2]), 
  numMin=as.factor(alldata[,3]))

# now the ancova
ancova = lm(detFrac ~ numMin * sdlDist, data=moreall)
summary(ancova)

#Call:
#lm(formula = detFrac ~ numMin * sdlDist, data = moreall)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.75368 -0.12319  0.05948  0.16312  0.52527 
#
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            0.8173841  0.0310284  26.343  < 2e-16 ***
#numMinshorter          0.0662437  0.0407720   1.625    0.105    
#sdlDist               -0.0012478  0.0002872  -4.345 1.67e-05 ***
#numMinshorter:sdlDist -0.0005934  0.0003767  -1.575    0.116    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#
#Residual standard error: 0.2299 on 543 degrees of freedom
#  (15 observations deleted due to missingness)
#Multiple R-squared: 0.1227,     Adjusted R-squared: 0.1179 
#F-statistic: 25.32 on 3 and 543 DF,  p-value: 2.414e-15 



## BMB> 'intercept' is for the first treatment (alphabetically) -- the expected average
## BMB> detFrac of a 'longer' tag with sdlDist zero, the intercept is signifiantly 
##     different from zero (p=2e-16). 

# - there is no effect of 'numMin' (p=0.105) ...BUT WHY IS 'shorter' ADDED TO THE NAME?
## BMB> because this is the effect of level "shorter" relative to the baseline level "longer"
## BMB> if there were a third treatment (say "medium") its difference
## BMB>  from the baseline level would be listed as numMinmedium

# - there is an effect of sdlDist on detFrac, p=1.67e-5 ... the slope is greater than zero
## BMB> yes

# - there is no interaction between numMin and detFrac ... the slopes are not 
#   significantly different
#
# #

# IN THE END I DON'T REALLY CARE ABOUT WHETHER THERE'S A DIFFERENCE IN SLOPES, 
# THIS GRAPH JUST SHOWS THE RANGE FROM FILTERED TO UNFILTERED
 
 
 

##############################################################################
# Look at detFrac through time, I looked all all tagList 1-43 and I like 26 the
# best because it's a central beacon, good detections, good PS, places when
# drops in detFracs agree among some sdls and places when some don't agree.


# pick one central and one marginal beacon...central i=26, marginal i=12
i=26
# for shorthand pick out just the pertinent data and drop the first and last hours
r1 = filteredResults[[i]]$hourlyMeans[,c("hour","detFrac1","detFrac2",
  "detFrac3","detFrac4","detFrac5","psFrac")]
r1 = head(r1,-1)  




   
### what about a spatial arrangement for the figure ###

par(oma=c(4,4,1,1))
par(mfrow=c(3,3))
figmargins = c(1,1,1,1)
showboxes=F
shadeboxes=T
shadecolor = rgb(190, 190, 190, alpha=150, maxColorValue=255)
shadeB = -1
shadeT = 1.0


# pane 1 empty
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac1, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
mtext(text="a)",side=2,line=0,cex=1.5,las=1,padj=-3)
if(showboxes){box("figure"); box("plot");}

# pane 2. north sdl
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac1, cex.axis=1.3,
  type="l", bty="l", las=1, xaxt="n",
  main="North Hydrophone", xlab="", ylab="")
#mtext("Fraction of Detections",side=2,line=2.8,cex=1.4)
axis(1,seq(0,350,by=50),cex.axis=1.3, labels=NA)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
if(showboxes){box("figure"); box("plot");}
# I want the black lines on top
points(r1$hour, r1$detFrac1, type="l")



# pane 3 empty
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac1, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
if(showboxes){box("figure"); box("plot");}

# pane 4. west sdl
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac4, cex.axis=1.3,
  type="l", bty="l", las=1, xaxt="n",
  main="West Hydrophone", xlab="", ylab="")
mtext("Detection Fraction",side=2,line=2.8,cex=1.4)
#mtext("Hour of Deployment",side=1,line=2.8,cex=1.3)
axis(1,seq(0,350,by=50),cex.axis=1.3)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
if(showboxes){box("figure"); box("plot");}
# I want the black lines on top
points(r1$hour, r1$detFrac4, type="l")

# pane 5. center sdl
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac5,
  type="l", bty="l", las=1, xaxt="n", yaxt="n",
  main="Central Hydrophone", xlab="", ylab="")
#mtext("Hour of Deployment",side=1,line=2.8,cex=1.3)
axis(1,seq(0,350,by=50),cex.axis=1.3, labels=NA)
axis(2,seq(0,1,by=0.2),cex.axis=1.3, labels=NA)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
if(showboxes){box("figure"); box("plot");}
# I want the black lines on top
points(r1$hour, r1$detFrac5, type="l")

# pane 6. east sdl
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac2,
  type="l", bty="l", las=1, xaxt="n", yaxt="n",
  main="East Hydrophone", xlab="", ylab="")
axis(1,seq(0,350,by=50),cex.axis=1.3)
axis(2,seq(0,1,by=0.2),cex.axis=1.3, labels=NA)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
if(showboxes){box("figure"); box("plot");}
# I want the black lines on top
points(r1$hour, r1$detFrac2, type="l")

# pane 7 empty
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac1, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
if(showboxes){box("figure"); box("plot");}

# pane 8. south sdl
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac3,
  type="l", bty="l", las=1, xaxt="n", yaxt="n",
  main="South Hydrophone", xlab="", ylab="")
mtext("Hour of Deployment",side=1,line=2.8,cex=1.3)
axis(1,seq(0,350,by=50),cex.axis=1.3)
axis(2,seq(0,1,by=0.2),las=1,cex.axis=1.3)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
if(showboxes){box("figure"); box("plot");}
# I want the black lines on top
points(r1$hour, r1$detFrac3, type="l")

# pane 9 empty
par(mar=figmargins+0.1)
plot(r1$hour, r1$detFrac1, type="n", xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
if(showboxes){box("figure"); box("plot");}



# plot for PS frac
par(mar=c(4.5,4,1,1)+0.1)
plot(r1$hour, r1$psFrac, cex.axis=1.3, cex.lab=1.5,
  type="l", bty="l", las=1, #xaxt="n",yaxt="n",
  main="", xlab="Hour of Deployment", ylab="Position Solution Fraction", 
  ylim=c(0,1)
)
text(0,1,"b)",cex=1.5)
# add shaded regions
if(shadeboxes){
  rect(0,shadeB,30,shadeT-0.05,col=shadecolor, border=NA)
  rect(210,shadeB,280,shadeT,col=shadecolor, border=NA)
}
# I want the black lines on top
points(r1$hour, r1$psFrac, type="l")




   
   















par(mfrow=c(1,2))
# plot X THIS HAS MORE THINGS ADDED TO IT BELOW...
# a plot of spacing v psFrac
plot(rawResults[[1]]$spacing, rawResults[[1]]$psFrac, las=1, type="n", bty="l",
  cex.axis=1.5, cex.lab=1.5, xaxt="n",
  xlab="Array Spacing (m)",
  ylab="Fraction of Position Solutions",
  xlim=c(50,155), 
  ylim=c(0,1))
# label the x-axis 
axis(1, at = seq(50,225,by=25),    
  labels = seq(50,225,by=25), 
  tick = TRUE, cex.axis=1.5)  
# I want to jitter the 100m column more than other columns 
 
for (i in 1:length(rawResults)){
  points(jitter(rawResults[[i]]$spacing, factor=2), rawResults[[i]]$psFrac, 
    pch=plType[i], col="black")#plotColors[i])
}


# now for plot 2, I see that the 50m spacing is lower than it should be, the 
# problem is that it has no psr (collected in symbol mode) and so doesn't really 
# compare with the psr results of all other deployments.  So to account for that
# I'll run 'testTheTag()' for everything except 2007 
# (also, because 2008 if funky don't use it, and don't use spacing trials 
# or interal performance, they're too short, so just 2009 really) using non-psr results,
# then again using psr results.  I'll calculate the percent increase in PS for 
# each point, find the mean percent increase, finally apply that increase to the
# 2007 points.
#
# I'll follow this process both for the unfiltered and filtered data.
#
# In the end, I'll run 'testTheTag()' (on the 2009 stuff): 
#  - unfiltered no-psr
#  - unfiltered psr
#  - filtered no-psr
#  - filtered psr
# then ...
# run 2007 data:
#  - unfiltered not psr adjusted
#  - unfiltered psr adjusted
#  - filtered not psr adjusted
#  - filtered psr adjusted
#
# run the code above for each of the first 4 cases and save the results here...

zz unno = allresults   # in this I'll just be looking at 2008 data
zz unpsr = rawResults #---OR---allresults  # in this I'll just be looking at 2008 data
zz filtno = allresults  # in this I'll just be looking at 2007 data
zz filtpsr = filteredResults #---OR---allresults  # in this I'll just be looking at 2007 data

# now that I have all the 'allresults' calculated for the four cases, calculate
# the percent increase for everything (but 2007)

unPercentIncrease = c()
filtPercentIncrease = c()
for (i in 11:43){ # 11 is first 2009, 43 is the last before the spacing trails
  unPercentIncrease = c(unPercentIncrease, 
    (unpsr[[i]]$psFrac - unno[[i]]$psFrac)/unno[[i]]$psFrac)
  filtPercentIncrease = c(filtPercentIncrease,
    (filtpsr[[i]]$psFrac - filtno[[i]]$psFrac)/filtno[[i]]$psFrac)
}
# now calculate the mean percent increase for unfiltered and filtered
meanun = mean(unPercentIncrease)  #=1.47, stdev = 2.108, range= (0.07, 10.07)
range(unPercentIncrease)
meanfilt = mean(filtPercentIncrease) #=5.27, stdev = 10.912, range= (-0.63, 58.71)
range(filtPercentIncrease)

---or a short cut ---

meanun = 1.47
meanfilt = 5.27

# now apply this percentIncrease to the 2007 data, there are only five data points
# I want to do plots with unfiltered and filtered psFrac
#
# to keep nomenclature consistend
zz rawResults = unpsr  # unfiltered with psr
zz filteredResults = filtpsr  # filtered with psr


# now calculate how much the 2007 data might increase if it had PSR
# ... and when they go above 100%, limit them to 100%
un2007 = list()
filt2007 = list()
for (i in 1:5){   # for hb2007 unpsr = unno and filtpsr=filtno
  un2007[[i]] = min(unpsr[[i]]$psFrac * (1+meanun),1)    
  filt2007[[i]] = min(filtpsr[[i]]$psFrac * (1+meanfilt),1)
}
  

# pick the symbol:
# ... closed circle: beacon on center sdl, longer than 360min
# ... open circle: beacon not on center, loner than 360min
# ... 
plColors = rep("black",length(unpsr))
plType = rep(23,length(unpsr)) # none should end up with this symbol
plSize = rep(1,length(unpsr))
for(i in 1:length(unpsr)){
  # use unpsr because the 'numMin' and 'center' won't change
  if((unpsr[[i]]$numMin < 360)&(unpsr[[i]]$center)){plType[i]=4}
  if((unpsr[[i]]$numMin < 360)&(!unpsr[[i]]$center)){plType[i]=4}
  if((unpsr[[i]]$numMin > 360)&(unpsr[[i]]$center)){plType[i]=19}
  if((unpsr[[i]]$numMin > 360)&(!unpsr[[i]]$center)){plType[i]=1}
}


# now reproduce figure 2 with the extra 2007 data points
par(mfrow=c(1,2))
par(mar=c(5,5,1,1)+0.1)
# plot 2a WITH ADDED 2007 WITH PRETEND PSR
# a plot of spacing v psFrac
plot(unpsr[[1]]$spacing, unpsr[[1]]$psFrac, las=1, type="n", bty="l",
  cex.axis=1.5, cex.lab=1.5, xaxt="n",
  xlab="Array Spacing (m)",
  ylab="",
  xlim=c(50,155), 
  ylim=c(0,1.1))
# label the x-axis 
axis(1, at = seq(50,150,by=25),    
  labels = seq(50,150,by=25), 
  tick = TRUE, cex.axis=1.5)  
# I want to jitter the 100m column more than other columns 
for (i in 1:length(unpsr)){
  points(jitter(unpsr[[i]]$spacing, factor=2), unpsr[[i]]$psFrac, 
    pch=plType[i], col="black")#plotColors[i])
}
# add the 2007 data that have been increased as if they used PSR
points(jitter(rep(50,length(un2007)),2),un2007,pch=c(2,2,2,2,17))
abline(h=0)
# add y-axis label
mtext(text="Position Solution Fraction", side=2, line=3.5, cex=1.5)
# add the "a)"
text(50,1.1,"a)", cex=1.5)



# plot 2b
# a plot of spacing v psFrac
par(mar=c(5,3,1,1)+0.1)
plot(filtpsr[[1]]$spacing, filtpsr[[1]]$psFrac, las=1, type="n", bty="l",
  cex.axis=1.5, cex.lab=1.5, xaxt="n",
  xlab="Array Spacing (m)",
  ylab="",
  xlim=c(50,155), 
  ylim=c(0,1.1))
# label the x-axis 
axis(1, at = seq(50,225,by=25),    
  labels = seq(50,225,by=25), 
  tick = TRUE, cex.axis=1.5)  
# I want to jitter the 100m column more than other columns 
for (i in 1:length(filtpsr)){
  points(jitter(filtpsr[[i]]$spacing, factor=2), filtpsr[[i]]$psFrac, 
    pch=plType[i], col="black")#plotColors[i])
}
# add the 2007 data that have been increased as if they used PSR
# ... make the center triangle filled in instead of open
points(jitter(rep(50,length(filt2007)),2),filt2007,pch=c(2,2,2,2,17))
abline(h=0)
# add the "b)"
text(50,1.1,"b)", cex=1.5)

# add a legend
legend(111, 1.14, legend=c(
  "Central, longer than 6 hr",
  "Marginal, longer than 6 hr", 
  "All locations, shorter than 6 hr",
  "Central, Deployment A",
  "adjusted for PSR",
  "Marginal, Deployment A",
  "adjusted for PSR"),
  pch=c(19,1,4,17,NA,2,NA) 
)

# in this plot I want to know how many points are actually on the zero line
temp1 = c()
for (i in 1:length(filtpsr)){
  temp1 = c(temp1,filtpsr[[i]]$psFrac*100)
}
min(temp1,na.rm=T)













# how much is lost with filtering?  What percent decrease is there in the 
# number of PS, for central and marginal locations
# From above we have...
rawResults
filteredResults

percentRemovedCentral = vector(length=0)
percentRemovedMarginal = vector(length=0)
# loop through all and calculate the percent decrease after filtering
for (i in 1:length(rawResults)){
  
  # extract psFrac and numMin, then calculate the number of PS
  rawPScount = rawResults[[i]]$numMin * rawResults[[i]]$psFrac
  filteredPScount = filteredResults[[i]]$numMin * filteredResults[[i]]$psFrac
  if(rawResults[[i]]$center){
    percentRemovedCentral[i] = 100*(rawPScount-filteredPScount)/rawPScount
  } else if (!rawResults[[i]]$center){
    percentRemovedMarginal[i] = 100*(rawPScount-filteredPScount)/rawPScount
  }
}

range(percentRemovedCentral, na.rm=T)
mean(percentRemovedCentral, na.rm=T)

range(percentRemovedMarginal, na.rm=T)
mean(percentRemovedMarginal, na.rm=T)
















###############################################################################
# What is the position solution accuracy?  Two methods:
# 1. match ALPS mean position solution with GPS best estimates

# import and filter beacon, etc. data
i=15
cTag = tagList[[i]]

psAccuracy = function(cTag, filterMe=TRUE, psrMe=TRUE){
  # cTag = 1;  tagName = tagList[[cTag]]$tagName; deployment = tagList[[cTag]]$deployment; tagLocation = tagList[[cTag]]$tagLocation; startUtime=tagList[[cTag]]$startUtime; stopUtime=tagList[[cTag]]$stopUtime
  # maybe later get the ADCP data
  
  tagName=cTag$tagName 
  deployment=cTag$deployment 
  tagLocation=cTag$tagLocation 
  startUtime=cTag$startUtime 
  stopUtime=cTag$stopUtime
  
  tagType = substr(tagName,1,1)
  tagID = substr(tagName,2,10)
#  numSdl = 5
  
  # get info from md[[]]
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      print(i)
      spacing = md[[i]]$spacing
      homeDir = paste(md[[i]]$homeDir,"/ALPS 2011Feb14", sep="")
      bestBeacon = substr(md[[i]]$bestBeacon,2,10)
      secondBestBeacon = substr(md[[i]]$secondBestBeacon,2,10)
      reefEN = md[[i]]$reefEN
      sdlEN = md[[i]]$sdlEN  
      beaconEN = md[[i]]$beaconEN 
      plotLimits = md[[i]]$plotLimits

      # is this beacon/sentinel at the center or not, find out which SDL it was on...
      location = "lost" # keep this if you're using a funky 2008 beacon code, or a 
           # non-beacon which 
           # probably doesn't have a position estimate.  I won't do the 
           # sentinels now either.
      for (j in 1:nrow(md[[i]]$beaconEN)){                
        if(tagName == md[[i]]$beaconEN$beaconID[j]){
          location = md[[i]]$beaconEN$location[j]
        } 
      } # end for-loop
       
      # in a couple cases the preceeding lines won't find the right location...
      if(location == "lost"){
        # you might have one of the problem 2008 tags, where codes 
        # get used in place of beacons
        if ((tagName == "b80") | (tagName == "b81" )) {location=42}
        if ((tagName == "b85") | (tagName == "b86" )) {location=44}
        if (tagName == "b130") {location=45}
        # or you're using a fish tag in one of the trials
        if (tagType == "f"){location="inside"}  # these tags are all over the place
        # now the sp150/sp100...read the metadata about these...
        # N41 had B2, B79400.  C45 had B1, B79500
        # The sentinel s79600 and T61000 were at the reef
        if ((tagName == "b2") | (tagName == "b79400")){location="41"}
        if ((tagName == "b1") | (tagName == "b79500")){location="45"}
      }
      # ...then designate it as center or not
      center = ifelse(((location==45)|(location=="reef")|(location=="inside")),T,F)
    }  
  }
  setwd(homeDir)
  
  
  
  
#  # if cTag == bestBeacon, then use secondBestBeacon
#  cBeacon = ifelse(grepl(bestBeacon,tagName), secondBestBeacon, bestBeacon)
  
  ### POSITION SOLUTIONS ###
  d1 = importALPSdata(deployment=cTag$deployment, tagName=cTag$tagName, psr=psrMe)
  # if there are any data in d1 then filter it
  if ( nrow(d1[1]$data) > 1 ){
    if (filterMe){
      d2 = filterALPSdata(df1=d1, cnF=1.5, speedF=0.8, minuteMean=FALSE)$data
    } else {
      d2 = filterALPSdata(df1=d1, minuteMean=FALSE)$data
    }
    # gather the PS during the interval
    d3 = d2[(d2$utime > startUtime) & (d2$utime < stopUtime),c("datiL","easting","northing")]
  } else { # there are no data in d1, so make the answers = 0
    print("No data imported")
  }
  
  # now find the position of that location
  gpsEN = data.frame(easting=NA,northing=NA) # keep this if you don't find a match
  for (i in 1:length(sdlEN$ID)){
    if (as.character(location) == sdlEN$ID[[i]]){
      gpsEN = data.frame(easting=sdlEN$easting[[i]], northing=sdlEN$northing[[i]])
    }
  }
  # if it's the sentinel on the reef
  if(as.character(location) == reefEN$ID){
    gpsEN = data.frame(easting=reefEN$easting, northing=reefEN$northing)
  }
  # now calculate the distance between the gpsEN and the mean PS
  meanPS = data.frame(easting=mean(d3$easting, na.rm=T), 
    northing=mean(d3$northing, na.rm=T))
  distOff = sqrt( (gpsEN$easting-meanPS$easting)^2 + (gpsEN$northing-meanPS$northing)^2)  
  
  # determine the size of the 90% cloud of PS locations around the mean PS using
  # just the Northing axis
  # ...gather all the northing PS and sort
  d4 = sort(d3$northing)
  # find the bounds containing 90% of all PSs
  fiveP = max(floor(0.05*length(d4)),1)
  ninteyfiveP = floor(0.95*length(d4))
  northingCloudSize = d4[ninteyfiveP] - d4[fiveP]
 
  ### RETURN RESULTS ###
  answer=list(data=d3, tagName=tagName, deployment=deployment, spacing=spacing, 
    meanPS=meanPS,northingCloudSize=northingCloudSize,gpsEN=gpsEN,
    distOff=distOff,reefEN=reefEN, sdlEN=sdlEN, 
    plotLimits=plotLimits,center=center)
  return(answer)

} # end psAccuracy() function




# now pick a deployment, gather all the tagList tags for that deployment 
# and run psAccuracy for each tag, then plot

drawPlot = function(deployment, showClouds=F){
  # pick deployemnt hb2007 hb2008 hb1-3, sb1-4
  cDeployment = deployment
  # get tagList tags for that deployment
  shortList = list() # holding just the tags in this deployment          
  tagCounter = 1   # how many tags have I found so far belonging to this deployment
  for (i in 1:length(tagList)){
    if (tagList[[i]]$deployment == cDeployment){
      shortList[[tagCounter]] = tagList[[i]]
      tagCounter = tagCounter + 1
    }
  }

  # for all tags in shortList run psAccuracy
  psList = list() # for holding psAccuracy output
  for (i in 1:length(shortList)){
    psList[[i]] = psAccuracy(cTag=shortList[[i]])
  }
  
  # now plot sdl, these are the same in all psLists
  plot(psList[[1]]$sdlEN$easting, psList[[1]]$sdlEN$northing, pch=17,
    xlim=psList[[1]]$plotLimits$easting, ylim=psList[[i]]$plotLimits$northing
  )
  # add clouds if desired
  for (i in 1:length(psList)){
    # draw the clouds of ps
    if (showClouds){
      points(psList[[i]]$data$easting, psList[[i]]$data$northing, pch=19, cex=0.5,
        col=plotColors[i+1])  
    }
    # redraw the sdls
    points(psList[[1]]$sdlEN$easting, psList[[1]]$sdlEN$northing, pch=17)
    # draw the ALPS mean PSs
    points(mean(psList[[i]]$data$easting,na.rm=T), mean(psList[[i]]$data$northing,na.rm=T),
      pch=15
    )  
  }
} # end drawPlot()

# pick deployemnt hb2007 hb2008 hb1-3, sb1-4
drawPlot(deployment="sb4", showCloud=T)


# now make a histogram of all tags / all deployments of distance between
# GPS position estimate and ALPS mean PS

# for all tags in shortList run psAccuracy
psList = list() # for holding psAccuracy output
distOffVec = vector(length=0)
centerVec = vector(length=0)
northingCloudSizeVec = vector(length=0)
           
for (i in c(1:5,8:43)){# only from fish deployments.  not b80 or b81 from hb2008    
  psList[[i]] = psAccuracy(cTag=tagList[[i]])
  distOffVec[i] = psList[[i]]$distOff
  centerVec[i] = psList[[i]]$center
  northingCloudSizeVec[i] = psList[[i]]$northingCloudSize
}
                         
# how far off was each of the beacon tags



# now a plot for the paper ############################################
# histogram of "distance between position estiamtes"
# Panel A
par(mfrow=c(1,2))
par(mar=c(5,4,1,1)+0.1)
# pick the right data
centertags = distOffVec[ centerVec==T ]
offcentertags = distOffVec[ centerVec==F ]                  
bob=multhist(list(centertags,offcentertags), breaks=80, 
  cex.axis=1, cex.names=1, col=c("black","grey"), space=c(0,0), 
  axis.lty=0, axes=F, 
  xlim=c(1,60),
  ylim=c(0,9), legend.text=c("Central transmitters", "Marginal transmitters"),
  xlab="",
  ylab="",
  names.arg = seq(1,81,by=1)
    #c(1,"","","",5,"","","","",10,"","","","",15,"","","","",20,"","","","",
    #25,"","","","",30,"","","","",35,"","","","",40,"","","","",45,"","","","",50,"","","","",
    #55,"","","","",60,"","","","",65,"","","","",70,"","","","",75,"","","","",80,"")
)
axis(2,seq(0,8,by=1),labels=seq(0,8,by=1), las=1, cex.axis=1.5)
mtext("Difference Between Transmitter Position Estimates (m)    ",1,3, cex=1.5)
mtext("Frequency",2,2.5,cex=1.7)
text(1,8.5,"a)",cex=1.5)

# histogram of 90% variation in PS locations...the size of the clouds
# panel B
#
# now the histogram
par(mar=c(5,3,1,1)+0.1)
# pick the right
centertags = northingCloudSizeVec[ centerVec==T ]
offcentertags = northingCloudSizeVec[ centerVec==F ]                  
sam=multhist(list(centertags,offcentertags), breaks=40, 
  cex.axis=1.5, cex.names=1, col=c("black","grey"),space=c(0,0), 
  axis.lty=0, xaxt="n", axes=F,
  #xlim=c(1,46),
  ylim=c(0,13), 
  xlab="",
  ylab="",
  names.arg = seq(0,139,by=5)
)
axis(2,seq(0,12,by=1),labels=seq(0,12,by=1), las=1, cex.axis=1.5)
mtext("90% Range in Northing Position Estimates (m)",1,3, cex=1.5)
mtext("Frequency",2,2.5,cex=1.7)
text(1,12.4,"b)",cex=1.5)







###################################################################### 
# Appendix Figure
# look at accuracy over time, pick a tag from psList
par(mfrow=c(1,2))
par(mar=c(5,5,1,1)+0.1)
cex.pt = 0.6

i = 1
plot(psList[[i]]$data$datiL, psList[[i]]$data$northing, pch=1, 
  cex=cex.pt, cex.lab=1.5, cex.axis=1.5, bty="l", 
  yaxt="n",
  ylim=psList[[i]]$plotLimits$northing,
  xlab="Date", ylab="")
mtext("Northing (m)", side=2,line=3.5, cex=1.7) 
axis(2,seq(600,800,by=25),cex.axis=1.5,las=1) 
abline(h=psList[[i]]$sdlEN$northing[1],lwd=2)   
i=3
points(psList[[i]]$data$datiL, psList[[i]]$data$northing,pch=2,cex=cex.pt)
abline(h=psList[[i]]$sdlEN$northing[3],lwd=2)  
i=5
points(psList[[i]]$data$datiL, psList[[i]]$data$northing,pch=0,cex=cex.pt)
abline(h=psList[[i]]$sdlEN$northing[5],lwd=2)  
# on figure labels
text(psList[[1]]$data$datiL[1],765,"a)",cex=1.5)
text(psList[[1]]$data$datiL[29000],735,"North beacon",cex=1.5)
text(psList[[1]]$data$datiL[29000],690,"Center beacon",cex=1.5)
text(psList[[1]]$data$datiL[29000],645,"South beacon",cex=1.5)

# details for the caption
i=1
mean(psList[[i]]$data$northing,na.rm=T)   # mean=742.8m northing, range=(738.8,755.7)
psList[[i]]$sdlEN$northing[i]   # = 742
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)

i=3
mean(psList[[i]]$data$northing,na.rm=T)   # mean=630.0m northing, range=(621.8,637.0)
psList[[i]]$sdlEN$northing[i]   # = 639
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)

i=5
mean(psList[[i]]$data$northing,na.rm=T)   # mean=698.3m northing, range=(694.7,706.7)
psList[[i]]$sdlEN$northing[i]   # = 699
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)


# panel 2
par(mar=c(5,5,1,1)+0.1)
i=21
plot(psList[[i]]$data$datiL, psList[[i]]$data$northing, pch=0, 
  las=1, cex=cex.pt, cex.lab=1.5, cex.axis=1.5, bty="l", 
  #yaxt="n",
  ylim=psList[[i]]$plotLimits$northing,
  xlab="Date", ylab="")
mtext("Northing (m)", side=2,line=4.2, cex=1.7) 
axis(2,seq(600,800,by=25),cex.axis=1.5) 
abline(h=psList[[i]]$sdlEN$northing[1],lwd=2)
i=22
points(psList[[i]]$data$datiL, psList[[i]]$data$northing,pch=1,cex=cex.pt)
abline(h=psList[[i]]$sdlEN$northing[3],lwd=2)
i=24
points(psList[[i]]$data$datiL, psList[[i]]$data$northing,pch=2,cex=cex.pt)
abline(h=psList[[i]]$sdlEN$northing[5],lwd=2)  
# on figure labels
text(psList[[i]]$data$datiL[1],2190,"b)",cex=1.5)
text(psList[[21]]$data$datiL[42000],2150,"North beacon",cex=1.5)
text(psList[[21]]$data$datiL[42000],2050,"Center beacon",cex=1.5)
text(psList[[21]]$data$datiL[42000],1970,"South beacon",cex=1.5)

# details for the caption
i=21 # center beacon
mean(psList[[i]]$data$northing,na.rm=T)   # mean=2068m northing, range=(1981,2088)
psList[[i]]$sdlEN$northing[5]   # = 2070
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)

i=22 #north beacon
mean(psList[[i]]$data$northing,na.rm=T)   # mean=2169m northing, range=(1980,2205)
psList[[i]]$sdlEN$northing[1]   # = 2165m
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)

i=24 # south beacon
range(psList[[i]]$data$northing,na.rm=T)   # mean=1955m northing, range=(1927,2132)
psList[[i]]$sdlEN$northing[3]   # = 1956
abline(h=c(mean(psList[[i]]$data$northing,na.rm=T)),lty=2,lwd=2)




# now I want to be able to say something like, 95% of ps were within 2 m of
# the GPS position estimate for central tags and 85% for marginal tags.
# Also, same thing for mean PS and GPS
cDist = cDistMean = c();  
mDist = mDistMean = c()
for(i in 1:length(tagList)){
  z = psAccuracy(tagList[[i]])
  # find distance between all position solutions and gpsEN
  distance = sqrt((z$data$easting-z$gpsEN$easting)^2 +(z$data$northing-z$gpsEN$northing)^2)
  # find distance between mean S and gpsEN
  meanDist = sqrt((z$meanPS$easting-z$gpsEN$easting)^2 +(z$meanPS$northing-z$gpsEN$northing)^2)
  # add it to the right running list
  if((nrow(z$data)!=0) & (!is.na(z$gpsEN$northing))){
    plot(distance, main=paste(z$tagName,z$center), pch=".")
  }  
  if(z$center){
    cDist = c(cDist,distance)
    cDistMean = c(cDistMean,meanDist)
  } else {
    mDist = c(mDist,distance)
    mDistMean = c(mDistMean,meanDist)
  }
}

plot(cDist,pch=".")
plot(cDistMean,pch=19)
plot(mDist,pch=".")
plot(mDistMean,pch=19)

hist(cDist[cDist<5])  #,breaks=c(seq(0,10,by=0.5),seq(20,110,by=10)))
hist(mDist[mDist<20])     #,breaks=c(seq(0,10,by=0.5),seq(20,380,by=20)))

# how many < 2 or 3 m
length(cDist[cDist<2])/length(cDist) # 0.8685896 = 87%
length(cDist[cDist<3])/length(cDist) # 0.9722533 = 97%
length(mDist[mDist<2])/length(mDist) # 0.384421 = 38%
length(mDist[mDist<3])/length(mDist) # 0.4253941 = 43%

# Now what about the mean PS
cDistMean = cDistMean[!is.na(cDistMean)]
mDistMean = mDistMean[!is.na(mDistMean)]

length(cDistMean[cDistMean<2])/length(cDistMean) # 0.8571429
length(cDistMean[cDistMean<3])/length(cDistMean) #  0.952381
range(cDistMean)                                 # 0.2211213 11.8434389
length(mDistMean[mDistMean<2])/length(mDistMean) #  0.2333333
length(mDistMean[mDistMean<3])/length(mDistMean)  #   0.3
length(mDistMean[mDistMean<10])/length(mDistMean)  #  0.7333333
range(mDistMean)                                   # 0.7039804 320.4740412




### internal array trial
# SEE 'internal array test.r'







###############################################################################
###############################################################################
### How does the chosen sound speed affect accuracy, PS requency?
# The 2008 deployment had a temperature range of almost 10 degC, but you can
# only pick one sound speed.  The question is how much does the wrong sound 
# speed/water temperature affect PS accuaracy and frequency?

### I'll use the 2009Aug03 OH41 deployment because it has a small temperature 
#   range.  I'll increase and decrease the temp by +/- 5 and +/- 10 degC.
# I'll run ALPS five times total for the beacons (1,2,79400,79500) and 
# sentinel (79600).  Then I'll compare the estimated positions.  I'll also
# compare the fraction of PS over time.
#
# 20 degC = 1521 m/s
# 25 degC = 1533 m/s
# 30 degC = 1545 m/s
# 35 degC = 1554 m/s
# 40 degC = 1562 m/s

# read in all five data sets
alltags = list(beacon1=list(),beacon2=list(),beacon3=list(),beacon4=list())
alltagsf = list(beacon1=list(),beacon2=list(),beacon3=list(),beacon4=list())
                    # minus10, minus5, normal, plus5, plus10 
numTemps = 5 # this is the number of different temperatures I'm using                    

# pick the deployment and tags
cmd=md[[5]]
cDeploymentNames = cmd$deployment
cTagNames = c(cmd$beaconNames)

altDir = paste("C:/zy/The closets/data closet/Telemetry/2009/2009Aug03 OH41/",
   c("ALPS minus 10 degC","ALPS minus 5 degC",
     "ALPS normal", "ALPS plus 5 degC",
     "ALPS plus 10 degC"), sep="")
   
   
numPings = (cmd$stopUtime - cmd$startUtime)/20             
for (i in 1:length(cTagNames)){
  for (j in 1:numTemps){ 
    # pick one temperature ALPS run, calculate sound speed and drop things I don't need 
    temp1 = importALPSdata(deployment=cDeploymentNames,tagName=cTagNames[i],
      altDir=altDir[j])
    temp2 = filterALPSdata(df1=temp1, minuteMean=F)
    temp3 = filterALPSdata(df1=temp1, cnF=1.5, speedF=0.8, minuteMean=F)
    # pick important columns
    temp2$data = temp2$data[,c('utime','datiL','easting','northing')]
    temp3$data = temp3$data[,c('utime','datiL','easting','northing')]
    # now get only data between start and stopUtimes
    temp2$data = temp2$data[(temp2$data$utime > cmd$startUtime) & 
      (temp2$data$utime < cmd$stopUtime),]
    temp3$data = temp3$data[(temp3$data$utime > cmd$startUtime) & 
      (temp3$data$utime < cmd$stopUtime),]
    # calculate the psFrac
    temp2$psFrac = nrow(temp2$data)/numPings
    temp3$psFrac = nrow(temp3$data)/numPings
    # calculate the beacon position estimate
    temp2$easting = mean(temp2$data$easting)
    temp2$northing = mean(temp2$data$northing)
    temp3$easting = mean(temp3$data$easting)
    temp3$northing = mean(temp3$data$northing)
    # which temperature is this
    temp2$alpsdir = altDir[j]
    temp3$alpsdir = altDir[j]
    # save the answer, ready for plotting
    alltags[[i]][[j]] = temp2
    alltagsf[[i]][[j]] = temp3
  }
}  



# from these results how what's the biggest move in estimated beacon position
# ... I see that the biggest temp range always gives the biggest distance 
# ... difference...so find the dist between the extremes for each tag
i = 5
sqrt( (alltagsf[[i]][[5]]$easting - alltagsf[[i]][[1]]$easting)^2 +
  (alltagsf[[i]][[5]]$northing - alltagsf[[i]][[1]]$northing)^2 )  

# beacon position estimates moves 1.9m with over the entire temperature range
  # see below for how far from the nominal position any one point moves

# a plot of how the psFrac changed for each beacon over 5 distances
# extract data from alltagsf
psresults = data.frame(b1=NA, b2=NA, b3=NA, b4=NA,temperatures=c(-10,-5,0,5,10),
  soundSpeeds=c(1521, 1533, 1545, 1554, 1562))
for (i in 1:length(cTagNames)){
  for (j in 1:numTemps){
    psresults[j,i] = alltagsf[[i]][[j]]$psFrac
  }
}

# how much does the psFrac drop
(psresults[3,1] - psresults[2,1]) / psresults[3,1]
# ...so there's the worst is an 18% drop from nominal to -5degC

# a plot of how each beacon's psFrac changes with temperature changes
par(mfrow=c(2,1))
# inner beacon
par(mar=c(1,6,1,1)+0.1)
plot(psresults$soundSpeeds, psresults$b1, pch=19, las=1, bty="l", cex.lab=1.5,
  cex.axis=1.3, xaxt="n", yaxt="n",
  ylim=c(0.55,0.75),
  xlab="", ylab="")
axis(side=1, at=psresults$soundSpeeds, cex.axis=1.3, labels=NA)
axis(side=2, at=seq(0.55,0.75,by=0.02), 
  #labels=c(0.57,"",0.61,"",0.65,"",0.69,"",0.73), 
  cex.axis=1.3, las=1)
# add a)
text(1521,0.744,labels="a)",cex=1.7) 
# legend
legend(1525,0.75,legend=c("Central","North","East","South"),pch=c(19,2,3,4)) 
  
# outer beacons  
par(mar=c(5,6,0,1)+0.1)
plot(psresults$soundSpeeds, psresults$b2, pch=2, las=1, bty="l", cex.lab=1.5,
  cex.axis=1.5, xaxt="n", yaxt="n",
  ylim=c(0,0.065), 
  xlab="Sound Speed (m/s)", ylab="")
points(psresults$soundSpeeds, psresults$b3, pch=3)
points(psresults$soundSpeeds, psresults$b4, pch=4)
axis(side=1, at=psresults$soundSpeeds, cex.axis=1.3)
axis(side=2, at=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06), cex.axis=1.3, las=1)
abline(h=0)
# add y-axis label
mtext(text="Position Solution Fraction", side=2, line=4.3, cex=1.7, adj=-1.5)





# now that I've got the data, look at the picture
par(mfrow=c(1,1))
par(mar=c(4,6,1,1)+0.1)
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", las=1, bty="l",
  cex.axis = 1.5, cex.lab=1.5, 
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing,
  xlab="", ylab="")
mtext("Easting (m)",side=1,line=2.5,cex=1.5)
mtext("Northing (m)", side=2, line=4.5, cex=1.7) 
# add the reef and sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=1.5)
#points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1.5)
# add recorded positions
# point types different for each temperature
plType = c(3,4,20,2,0)
for(i in 1:length(cTagNames)){ # for each tag
  for(j in 1:numTemps){
    points(alltagsf[[i]][[j]]$easting, alltagsf[[i]][[j]]$northing, pch=plType[j],
    cex=2)
  }
}
text(325,2190,"b)",cex=1.7)
# legend
legend(x=470,y=2200,legend=c("1521 m/s", "1533 m/s", "1545 m/s", "1554 m/s", 
  "1562 m/s","GPS estimate"), pt.cex=2,
  pch=c(plType,17), cex=1.3)


# redo this with smaller x and y limits for an inset
# now that I've got the data, look at the picture
par(mfrow=c(1,1))
par(mar=c(6,9.5,1,1.5)+0.1)
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", las=1, bty="l",
  cex.axis = 3,  bty="o",
  xlim=c(441,445), ylim=c(2066,2070),
  xaxt="n",
  xlab="", ylab="")
mtext("Easting (m)",side=1,line=2.5, cex=3, padj=1)
mtext("Northing (m)", side=2, line=4.5, cex=3, padj=-1.6) 
axis(1,441:445,labels=c(441,"",443,"",445),cex.axis=3, padj=0.5)
# add the reef and sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=4)
#points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1.5)
# add recorded positions
# point types different for each temperature
plType = c(3,4,20,2,0)
for(i in 1:length(cTagNames)){ # for each tag
  for(j in 1:numTemps){
    points(alltagsf[[i]][[j]]$easting, alltagsf[[i]][[j]]$northing, pch=plType[j],
    cex=4)
  }
}   remember to stretch to the right size



# but how far away do the center beacon mean position solutions move?
centerbeacon = 1
alltagsf[[centerbeacon]][[1]]$easting  # plus
alltagsf[[centerbeacon]][[2]]$easting  # x
alltagsf[[centerbeacon]][[3]]$easting  # dot
alltagsf[[centerbeacon]][[4]]$easting  # open triangle
alltagsf[[centerbeacon]][[5]]$easting  # square
# pick the one farthest from the center, plus to x (look at easting and northing)
x1 = alltagsf[[centerbeacon]][[1]]$easting  # x
y1 = alltagsf[[centerbeacon]][[1]]$northing  # x
x2 = alltagsf[[centerbeacon]][[2]]$easting  # dot
y2 = alltagsf[[centerbeacon]][[2]]$northing  # dot
sqrt( (x2-x1)^2 + (y2-y1)^2 )
# so the -10degC change moves the mean ps the most and it it 0.9m 
 
# how far do the marginal beacon mean position solutions move?
mb = 4 # 2 or 3 or 4
alltagsf[[mb]][[1]]$easting  # plus
alltagsf[[mb]][[2]]$easting  # x
alltagsf[[mb]][[3]]$easting  # dot
alltagsf[[mb]][[4]]$easting  # open triangle
alltagsf[[mb]][[5]]$easting  # square
# pick the one farthest from the center. mb2: plus(1) to dot(3)
x1 = alltagsf[[mb]][[1]]$easting  # plus
y1 = alltagsf[[mb]][[1]]$northing  # plus
x2 = alltagsf[[mb]][[3]]$easting  # dot
y2 = alltagsf[[mb]][[3]]$northing  # dot
sqrt( (x2-x1)^2 + (y2-y1)^2 )











###############################################################################
###############################################################################
### How does the estimated SDL position affect accuracy, PS frequency?

### I'll use the 2009Aug03 OH41 deployment because it has a small temperature 
#   range.  I'll move the center sdl (and beacon) 2, 4, 6, 8, and 10m to the
#   east.  
# I'll run ALPS five times total for the beacons (1,2,79400,79500).  
# Then I'll compare the estimated positions.  I'll also
# compare the fraction of PS over time.

# read in all five data sets
alltags = list(beacon1=list(),beacon2=list(),beacon3=list(),beacon4=list(),sentinel=list())
alltagsf = list(beacon1=list(),beacon2=list(),beacon3=list(),beacon4=list(),sentinel=list())
                    # minus10, minus5, normal, plus5, plus10 
numDistances = 6 # this is the number of different distances I'm using                    

# pick the deployment and tags
cmd=md[[5]]
cDeploymentNames = cmd$deployment
cTagNames = c(cmd$beaconNames,cmd$sentinelNames)

altDir = c("C:/zy/data closet/Telemetry/2009/2009Aug03 OH41/ALPS 2011Feb14",
  paste(
    "C:/zy/data closet/Telemetry/2009/2009Aug03 OH41/ALPS c45 ",
     c("2m east","4m east","6m east","8m east","10m east"), sep=""
))
     
   
   
numPingsB = ((cmd$stopUtime - cmd$startUtime)/3600) * 180 # 180 pings per hour             
numPingsS = ((cmd$stopUtime - cmd$startUtime)/3600) * 300 # 300 pings per hour         
for (i in 1:length(cTagNames)){
  for (j in 1:numDistances){ 
    # pick one temperature ALPS run, calculate sound speed and drop things I don't need 
    temp1 = importALPSdata(deployment=cDeploymentNames,tagName=cTagNames[i],
      altDir=altDir[j])
    temp2 = filterALPSdata(df1=temp1, minuteMean=F)
    temp3 = filterALPSdata(df1=temp1, cnF=1.5, speedF=0.8, minuteMean=F)
    # pick important columns
    temp2$data = temp2$data[,c('utime','datiL','easting','northing')]
    temp3$data = temp3$data[,c('utime','datiL','easting','northing')]
    # now get only data between start and stopUtimes
    temp2$data = temp2$data[(temp2$data$utime > cmd$startUtime) & 
      (temp2$data$utime < cmd$stopUtime),]
    temp3$data = temp3$data[(temp3$data$utime > cmd$startUtime) & 
      (temp3$data$utime < cmd$stopUtime),]
    # calculate the psFrac
    if ( substr(temp3$tagName,1,1)=="b" ){
      temp2$psFrac = nrow(temp2$data)/numPingsB
      temp3$psFrac = nrow(temp3$data)/numPingsB
    } else if ( substr(temp3$tagName,1,1)=="s" ) {
      temp2$psFrac = nrow(temp2$data)/numPingsS
      temp3$psFrac = nrow(temp3$data)/numPingsS
    } else {print("It's not a beacon or sentinel")}
    # calculate the position estimate
    temp2$easting = mean(temp2$data$easting)
    temp2$northing = mean(temp2$data$northing)
    temp3$easting = mean(temp3$data$easting)
    temp3$northing = mean(temp3$data$northing)
    # which temperature is this
    temp2$alpsdir = altDir[j]
    temp3$alpsdir = altDir[j]
    # save the answer, ready for plotting
    alltags[[i]][[j]] = temp2
    alltagsf[[i]][[j]] = temp3
  }
}  



# from these results how what's the biggest move in estimated beacon position
# ... I see that the biggest artificial displacement always gives the biggest distance 
# ... difference...so find the dist between the extremes for each tag
i = 1
sqrt( (alltagsf[[i]][[5]]$easting - alltagsf[[i]][[1]]$easting)^2 +
  (alltagsf[[i]][[5]]$northing - alltagsf[[i]][[1]]$northing)^2 )  
# with a 10m artificial displacement the beacon mean ps moves 3.2m

# a plot of how the psFrac changed for each beacon over 5 distances
# extract data from alltagsf
psresults = data.frame(b1=NA, b2=NA, b3=NA, b4=NA,s79600=NA,distance=c(0,2,4,6,8,10))
for (i in 1:length(cTagNames)){
  for (j in 1:numDistances){
    psresults[j,i] = alltagsf[[i]][[j]]$psFrac
  }
}

# what percent decrease is there from nominal position to 10m artificial displacement
(psresults[1,1] - psresults[6,1])/psresults[1,1]
#   16.5%

# a plot of how each beacon's psFrac changes with c45 moves
par(mfrow=c(2,1))
# inner beacon
par(mar=c(1,6,1,1)+0.1)
plot(psresults$distance, psresults$b1, pch=19, las=1, bty="l", cex.lab=1.5,
  cex.axis=1.3, xaxt="n", yaxt="n",
  ylim=c(0.55,0.75),
  xlab="", ylab="")
points(psresults$distance,psresults$s79600)   # this doesn't actually show on the plot
axis(side=1, at=psresults$distance, cex.axis=1.3, labels=NA)
axis(side=2, at=seq(0.55,0.75,by=0.02), cex.axis=1.3, las=1)
# add a)
text(0,0.744,labels="a)",cex=1.7) 
# legend
legend(8,0.75,legend=c("Central","North","East","South"),pch=c(19,2,3,4)) 
  
par(mar=c(5,6,0,1)+0.1)
plot(psresults$distance, psresults$b2, pch=2, las=1, bty="l", cex.lab=1.5,
  cex.axis=1.5, xaxt="n", yaxt="n",
  ylim=c(0,0.065), 
  xlab="Artificial Displacement of Central Hydrophone (m)", ylab="")
points(psresults$distance, psresults$b3, pch=3)
points(psresults$distance, psresults$b4, pch=4)
axis(side=1, at=psresults$distance, cex.axis=1.3)
axis(side=2, at=c(0, 0.01,0.02, 0.03,0.04,0.05,0.06), cex.axis=1.3, las=1)
abline(h=0)
# add y-axis label
mtext(text="Position Solutions Fraction", side=2, line=4.3, cex=1.7, adj=-1.5)





# now that I've got the data, look at the picture
par(mfrow=c(1,1))
par(mar=c(4,6,1,1)+0.1)
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", las=1, bty="l",
  cex.axis = 1.5, cex.lab=1.5, 
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing,
  xlab="", ylab="")
mtext("Easting (m)",side=1,line=2.5,cex=1.5)
mtext("Northing (m)", side=2, line=4.5, cex=1.7) 

# add the reef and sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=1.5)

# point types different for each distance
plType = c(20,3,4,2,0,5)
for(i in 1:(length(cTagNames)-1)){ # for each tag, but don't do the sentinel
  for(j in 1:numDistances){
    points(alltagsf[[i]][[j]]$easting, alltagsf[[i]][[j]]$northing, 
    pch=plType[j],cex=2)
  }
}
text(325,2190,"b)",cex=1.7)
legend(x=470,y=2200,legend=c("0 m","2 m","4 m","6 m","8 m","10 m","GPS estimate"),
  pch=c(plType,17), cex=1.3, pt.cex=2)
      

# redo this with smaller x and y limits for an inset     THIS ISN'T RIGHT YET
# now that I've got the data, look at the picture
par(mfrow=c(1,1))
par(mar=c(6,9.5,1,1.5)+0.1)
plot(cmd$sdlEN$easting, cmd$sdlEN$northing, type="n", las=1, bty="l",
  cex.axis = 3,  bty="o",
  xlim=c(440,450), ylim=c(2065,2075),
  #xaxt="n",
  xlab="", ylab="")
mtext("Easting (m)",side=1,line=2.5, cex=3, padj=1)
mtext("Northing (m)", side=2, line=4.5, cex=3, padj=-1.6) 
axis(1,441:445,labels=c(441,"",443,"",445),cex.axis=3, padj=0.5)
# add the reef and sdls
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="black", cex=4)
#points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1.5)
# add recorded positions
# point types different for each temperature
plType = c(3,4,20,2,0)
for(i in 1:length(cTagNames)){ # for each tag
  for(j in 1:numDistances){
    points(alltagsf[[i]][[j]]$easting, alltagsf[[i]][[j]]$northing, pch=plType[j],
    cex=4)
  }
}   remember to stretch to the right size



# but far away do the center beacon mean position solutions move?
centerbeacon = 1    # northing and easting show same pattern
alltagsf[[centerbeacon]][[1]]$northing  # dot
alltagsf[[centerbeacon]][[2]]$northing  # plus
alltagsf[[centerbeacon]][[3]]$northing  # x
alltagsf[[centerbeacon]][[4]]$northing  # open triangle
alltagsf[[centerbeacon]][[5]]$northing  # square
alltagsf[[centerbeacon]][[6]]$northing  # diamond
# pick the one farthest from the center, plus to dot
x1 = alltagsf[[centerbeacon]][[1]]$easting  # dot
y1 = alltagsf[[centerbeacon]][[1]]$northing  # dot
x2 = alltagsf[[centerbeacon]][[3]]$easting  # x
y2 = alltagsf[[centerbeacon]][[3]]$northing  # x
sqrt( (x2-x1)^2 + (y2-y1)^2 )

mb = 4 # 2 or 3 or 4
# pick the one farthest from the center, plus to dot
x1 = alltagsf[[mb]][[1]]$easting  # dot
y1 = alltagsf[[mb]][[1]]$northing  # dot
x2 = alltagsf[[mb]][[6]]$easting  # 
y2 = alltagsf[[mb]][[6]]$northing  # 
sqrt( (x2-x1)^2 + (y2-y1)^2 )




# don't add this for the paper
for(i in 1:length(cTagNames)){ # for each tag
  for(j in 1:numDistances){
    points(alltags[[i]][[j]]$easting, alltags[[i]][[j]]$northing, pch=plType[j],
    cex=2, col="red")
  }
}













### How does the number of position solutions change as the CN filtering cut-off 
#   decreases

cTag=tagList[[1]]


justFilter = function(cTag, psrMe=TRUE){
  
  tagName=cTag$tagName 
  deployment=cTag$deployment 
  tagLocation=cTag$tagLocation 
  startUtime=cTag$startUtime 
  stopUtime=cTag$stopUtime
  tagType = substr(tagName,1,1)
  tagID = substr(tagName,2,10)
 
  # get info from md[[]]
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      print(i)
      spacing = md[[i]]$spacing
      homeDir = paste(md[[i]]$homeDir,"/ALPS 2011Feb14", sep="")
      bestBeacon = substr(md[[i]]$bestBeacon,2,10)
      secondBestBeacon = substr(md[[i]]$secondBestBeacon,2,10)
      reefEN = md[[i]]$reefEN
      sdlEN = md[[i]]$sdlEN 
      
      # is this beacon/sentinel at the center or not, find out which SDL it was on...
      location = "lost" # keep this if you're using a funky 2008 beacon code, or a 
           # non-beacon which 
           # probably doesn't have a position estimate.  I won't do the 
           # sentinels now either.
      for (j in 1:nrow(md[[i]]$beaconEN)){                
        if(tagName == md[[i]]$beaconEN$beaconID[j]){
          location = md[[i]]$beaconEN$location[j]
        } 
      } # end for-loop
       
      # in a couple cases the preceeding lines won't find the right location...
      if(location == "lost"){
        # you might have one of the problem 2008 tags, where codes 
        # get used in place of beacons
        if ((tagName == "b80") | (tagName == "b81" )) {location=42}
        if ((tagName == "b85") | (tagName == "b86" )) {location=44}
        if (tagName == "b130") {location=45}
        # or you're using a fish tag in one of the trials
        if (tagType == "f"){location="inside"}  # these tags are all over the place
        # now the sp150/sp100...read the metadata about these...
        # N41 had B2, B79400.  C45 had B1, B79500
        # The sentinel s79600 and T61000 were at the reef
        if ((tagName == "b2") | (tagName == "b79400")){location="41"}
        if ((tagName == "b1") | (tagName == "b79500")){location="45"}
      }
      # ...then designate it as center or not
      center = ifelse(((location==45)|(location=="reef")|(location=="inside")),T,F) 
    }  
  }
  setwd(homeDir)
    
  # import ALPS data 
  d1 = importALPSdata(deployment=cTag$deployment, tagName=cTag$tagName, psr=psrMe)
  # set the range of CN filters 
  cnVec = seq(0.1,20,by=0.1)
  # create a data.frame to hold results
  psDecay = data.frame(cn=NA, psNum=NA, psFrac=NA)
  
  # if there are any data in d1 then filter it
  if ( nrow(d1[1]$data) > 1 ){
    for (i in 1:length(cnVec)){
      d2 = filterALPSdata(df1=d1, cnF=cnVec[i])$data
      psDecay[i,] = c(cnVec[i], nrow(d2), NA) # do the psFrac later
    }
  } else {print("No Data in d1")} # no action necesary, psDecay just stays empty
  
  # I really want the fraction not the number...  
  psDecay$psFrac = psDecay$psNum / tail(psDecay$psNum,1)
  
  answer = list(tagName=tagName, deployment=deployment, center=center, 
    psDecay=psDecay, 
    dataUnfiltered=d1$data[,c("utime","datiL","easting","northing","depth",
      "cn","rn","dop")]
  )
  
  return(answer)
}  # end just Filter    
# bob = justFilter(cTag)    
   
   
   
# now that justFilter works, apply it to the fish trials
filterResults = list()
# 2007 deployment
for (i in 1:5){ # cycle through all tags in this deployment
  filterResults[[i]]=justFilter(tagList[[i]])
  print(i)
}
                                                                          
# 2008 deployment...the problem child
for (i in 6:10){ # cycle through all tags in this deployment
  filterResults[[i]]=justFilter(tagList[[i]])   
  print(i)
}
# cTag 6 and 7 are b80 and b81, which are b79200
# ...something wrong with these so I won't use them...
# cTag 8 and 9 are b85 and b86, which are b79400
# cTag 10 is b130, which is b79500
#filterResults[[6]]$detFrac = rep(NA,5) #allresults[[6]]$detFrac + allresults[[7]]$detFrac
#filterResults[[7]]$detFrac = rep(NA,5)
#filterResults[[6]]$psFrac = NA #allresults[[6]]$psFrac + allresults[[7]]$psFrac
#filterResults[[7]]$psFrac = NA
#
#filterResults[[8]]$detFrac = allresults[[8]]$detFrac + allresults[[9]]$detFrac
#filterResults[[9]]$detFrac = rep(NA,5)
#filterResults[[8]]$psFrac = allresults[[8]]$psFrac + allresults[[9]]$psFrac
#filterResults[[9]]$psFrac = NA
#
## hb1:11-15. sb1:16-20. sb2:21-25. hb2:26-30.  sb3:31-35. hb3:36-39. sb4:40-43.
# ...remember these are only the stationary tags
for (i in 11:43){ # cycle through all tags in this deployment
  filterResults[[i]]=justFilter(tagList[[i]])
  print(i)
}
 
    
    
plot(bob$psDecay$cn, bob$psDecay$psFrac), #type="n",
  type="l", xlim=c(), ylim=c())   
    
    
# now plot the results
plType = vector()
for (i in 1:(length(filterResults)-1)){plType[i]=ifelse(filterResults[[i]]$center,1,2)}    
par(mar=c(5,4.5,1,1)+0.1)
# create the main plot
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
  las=1, cex.lab=1.5, cex.axis=1.5, bty="l",
  xlim=c(0,20), ylim=c(0,1),
  xlab="Condition Number", ylab="Fraction of Position Solutions")   
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# add a a)
text(0,1,"a)",cex=1.5)
# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)
# add a legend
legend(8,0.2, c("Central transmitter","Marginal transmitter",
  "Central transmitter shown in c) and d)"), 
  lty = c(1, 2, 1), lwd=c(1,1,2)
)    


# plot again zoomed in on the x-axis
plType = vector()
for (i in 1:(length(filterResults)-1)){plType[i]=ifelse(filterResults[[i]]$center,1,2)}    
par(mar=c(5,4.5,1,1)+0.1)
# create the main plot
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
  las=1, cex.lab=1.5, cex.axis=1.5, bty="l",
  xlim=c(1,3), ylim=c(0,1),
  xlab="Condition Number", ylab="Fraction of Position Solutions")   
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# add a b)
text(1,1,"b)",cex=1.5)
# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)



## add an inset plot
#tmp = subplot(
#  plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
#    las=1,xlim=c(0.8,2.5), ylim=c(0,1),xlab="",ylab=""),
#  17,0.29,size=c(2,2)    
#)
#op = par(no.readonly=TRUE)
#par(tmp)
#for (i in 1:(length(filterResults)-1)){
#  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
#    type="l", lty=plType[[i]], las=1)
#}
## now darken the line of the tag shown in panel A, which is tagList[[11]].
## note that tagList[[11]] ~ and filterResults[11]]
#i=11
#points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
#    type="l", lty=plType[[i]], lwd=3)
#mtext("Condition Number",1,2)
#mtext("Fraction of Unfiltered",2,3.5) 
#mtext("Position Solutions",2,2.5)
#par(op)
#








### more filtering
# how does the northing position change with CN of unfiltered data

# get the raw data, probably best not to use a 2007 tag...no psr
i = 11; psrMe=T;
cTag=tagList[[i]]
d1 = importALPSdata(deployment=cTag$deployment, tagName=cTag$tagName, psr=psrMe)

# plot(d1$data$cn, d1$data$northing, pch=19, cex=0.5)


# a two panel plot, each with an insert

# Panel A is here
# a plot with an inset...first the main plot
#split.screen(figs=c(1,2))
#screen(1)
#   I CAN'T GET MFROW() OR SPLIT.SCREEN OR LAYOUT TO WORK SO JUST MAKE 
# TWO SEPERATE PLOTS

par(mar=c(5,4.5,1,1)+0.1)

plot(d1$data$cn, d1$data$northing, pch=19, cex=0.5, las=1, bty="l", cex.axis=1.5, 
  xlab="", ylab="",  
  xlim=c(1,3),ylim=c(428,436)    
)

abline(h=cTag$tagLocation[2])
mtext("Condition Number",1,2.5, cex=1.5)
mtext("Northing (m)",2,3.5,cex=1.5)  
text(1,436,"b)", cex=1.5)
# because they don't work with the code below, add the inset axes labels now
text(2.78, 433, "Condition Number")
mtext("Northing (m)",2,-18, adj=0.9) 

# add an inset plot

subplot(
  plot(d1$data$cn, d1$data$northing, pch=19, cex=0.5, las=1,
  ylim=c(400,480),
    xlab="",ylab=""),
  x=2.8,y=435,size=c(1.5,1.5)
)    
# unfortunately these end up in panel b
#op = par(no.readonly=TRUE)
#par(tmp)
#mtext("Condition Number",1,2)
#mtext("Northing (m)",2,2.5)
#par(op)
#


# how many of these points remain after filtering CN<1.5
should still be i=11 
filterResults[[i]]$psDecay



### try this out with a hexbin plot ########################################
# ... it appears I can't restrict the binning and plotting to smaller than the
# ... data ranges, so before binning, delete to the range I want.
library("hexbin")
range(d1$data$cn); range(d1$data$northing);

bindata = d1$data
xrange = c(0,3)
yrange = c(428,436)


# filter data to smaller plotting window
bindata = bindata[(bindata$cn<xrange[2]) & 
  (bindata$northing>yrange[1]) & (bindata$northing<yrange[2]),]

# create the hexbins
thebins = hexbin(bindata$cn, bindata$northing, xbins=60)
sum(thebins@count[thebins@count>1000])

# plot the hexbins
hplt <- plot(thebins, style = "colorscale", #xaxt="n",
  xlab="", ylab="")

pushHexport(hplt$plot.vp)

grid.lines(c(0.89,3.11), c(cTag$tagLocation[2],cTag$tagLocation[2]), 
  gp=gpar(col = 1), default.units = "native")

# but where do the really big bins (>1000) lie and how many points are in them
#grid.lines(c(0.94,2.59), c(431.3,431.3), 
#  gp=gpar(col = 1), default.units = "native")
#  
#grid.lines(c(1.25,1.25), c(430,433), 
#  gp=gpar(col = 1), default.units = "native")

# add labels to the big plot
grid.text(label="Condition Number", x = unit(2, "native"), 
  y = unit(426.5, "native"), gp=gpar(fontsize=20))
  
grid.text(label="Northing (m)", x = unit(0.55, "native"), 
  y = unit(432, "native"), rot=90, gp=gpar(fontsize=20))

# add b)
text(0.95,434.6,"b)", cex=1.5)

## add an inset plot
subplot(
  plot(d1$data$cn, d1$data$northing, pch=19, cex=0.5, las=1,
  ylim=c(400,480),
    xlab="",ylab=""),
  x=2.28,y=433.85,size=c(1.3,1.3)
)    

# add labels to the little plot
grid.text(label="Condition Number", 
  x = unit(2.75, "native"), y = unit(432.7, "native"), gp=gpar(fontsize=12))
  
grid.text(label="Northing (m)", 
  x = unit(2.1, "native"), y = unit(434.9, "native"), rot=90, gp=gpar(fontsize=12))






###################################################################
### I want to redo this plot for publication with two panels...
# now plot the results
plType = vector()
for (i in 1:(length(filterResults)-1)){plType[i]=ifelse(filterResults[[i]]$center,1,2)}    
par(mar=c(5,4.5,1,1)+0.1)
# create the main plot
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
  las=1, cex.lab=1.8, cex.axis=1.5, bty="l", xaxt="n",
  xlim=c(1,20), ylim=c(0,1),
  xlab="Condition Number", ylab="Fraction of Unfiltered Position Solutions")   
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# add x-axis
axis(1,c(1,3,5,10,15,20),cex.axis=1.3)
# add a)
text(0.8,1,"a)",cex=1.5)

# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)

# add a legend    
#legend(12.24, 0.62, c("Central transmitter","Central transmitter in b)",
#  "Marginal transmitter"), lty = c(1,1, 2), lwd=c(1,2,1))    

## another shape legend
legend(14.25, 0.75, c("Central","transmitter","Central", "transmitter in b)",
  "Marginal", "transmitter"), lty = c(1,NA,1,NA,2,NA), lwd=c(1,NA,2,NA,1,NA))    


## add an inset plot
#tmp = subplot(
#  plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
#    las=1,xlim=c(0.9,3), ylim=c(0,1),xlab="",ylab=""),
#  17,0.34,size=c(2,2.7)    
#)
#
tmp = subplot(
  plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
    las=1,xlim=c(0.9,3), ylim=c(0,1),xlab="",ylab=""),
  14.8,0.27,size=c(3.3,2)    
)


op = par(no.readonly=TRUE)
par(tmp)
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)
mtext("Condition Number",1,2)
mtext("Fraction of Unfiltered",2,3.5) 
mtext("Position Solutions",2,2.5)
par(op)






###################################################################
### I want to redo this plot for publication with four panels...

four plot


# create the hexbins
allbins = hexbin(bindata$cn, bindata$northing, xbins=60)
sum(allbins@count[allbins@count>1000])


# my try
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))

# panel A
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
grid.rect(gp=gpar(col="red", lwd=1))
par(plt=gridPLT(), oma=c(2,4,0,0)) #new=TRUE, 

plType = vector()
for (i in 1:(length(filterResults)-1)){plType[i]=ifelse(filterResults[[i]]$center,1,2)}    
#par(mar=c(3,3.5,1,1)+0.1)
# create the main plot
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
  las=1, cex.lab=1, cex.axis=1, bty="l", xaxt="n",
  xlim=c(1,20), ylim=c(0,1),
  xlab="", ylab="")   
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}

# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)

# add a legend    
#legend(12.24, 0.62, c("Central transmitter","Central transmitter in b)",
#  "Marginal transmitter"), lty = c(1,1, 2), lwd=c(1,2,1))    

## another shape legend
legend(8, 0.48, c("Central","transmitter","Central", "transmitter in b)",
  "Marginal", "transmitter"), lty = c(1,NA,1,NA,2,NA), lwd=c(1,NA,2,NA,1,NA))    

# add x-axis 
axis(1,c(1,3,5,10,15,20),cex.axis=1)
# label x- and y-axes
mtext("Condition Number",side=1,line=2)
mtext("Fraction of Unfiltered Position Solutions",side=2,line=2.4)
# add a)
text(1,1,"a)",cex=1)
grid.rect(gp=gpar(col="red", lwd=1))

popViewport()


# panel B
pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0)) #new=TRUE, 

#par(plt=gridPLT(), oma=c(2,4,0,0)) #new=TRUE, 
plot(allbins, xlab = "Condition Number", ylab = "Northing (m)", 
  legend=F, newpage=FALSE)
grid.rect(gp=gpar(col="red", lwd=1))

bindata = d1$data



pushHexport(hplt$plot.vp)

grid.lines(c(0.89,3.11), c(cTag$tagLocation[2],cTag$tagLocation[2]), 
  gp=gpar(col = 1), default.units = "native")














popViewport()






pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
par(plt=gridPLT())
par(new=TRUE)
plot(0,0)
grid.rect(gp=gpar(col="red", lwd=1))
popViewport()


pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
grid.rect(gp=gpar(col="red", lwd=1))
plot(hbin, legend=0, xlab = "X", ylab = "Y", newpage=FALSE)
popViewport()










aaa

library(hexbin) 
library(lattice)
library(gridBase)

bindata = d1$data

# create the hexbins
allbins = hexbin(bindata$cn, bindata$northing, xbins=60)
sum(allbins@count[allbins@count>1000])

# plot the hexbins
hplt <- plot(allbins, style = "colorscale", #xaxt="n",
  xlab="", ylab="", newpage=F)

pushHexport(hplt$plot.vp)

grid.lines(c(0.89,3.11), c(cTag$tagLocation[2],cTag$tagLocation[2]), 
  gp=gpar(col = 1), default.units = "native")












# add b)
text(0.95,434.6,"b)", cex=1.5)
bbb


# panel C
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, 
  type="n", bty="l",
  las=1,xlim=c(0.9,3), ylim=c(0,1),xlab="",ylab="")

for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)
    
# label x- and y-axes
mtext("Condition Number",side=1,line=2)
mtext("Fraction of Unfiltered Position Solutions",side=2,line=2.3)    
    
box("figure",col="red")

# panel D
# an empty plot to make sure hexplot ends up in right spot
par(mar=c(3,3.5,1,1)+0.1)
plot(0,0,type="n",xlab="",ylab="",xaxt="n",yaxt="n")
box("figure",col="red")


aaa
### try this out with a hexbin plot ########################################
# ... it appears I can't restrict the binning and plotting to smaller than the
# ... data ranges, so before binning, delete to the range I want.
library("hexbin")
range(d1$data$cn); range(d1$data$northing);

bindata = d1$data
xrange = c(0,3)
yrange = c(428,436)


# filter data to smaller plotting window
bindata = bindata[(bindata$cn<xrange[2]) & 
  (bindata$northing>yrange[1]) & (bindata$northing<yrange[2]),]

# create the hexbins
thebins = hexbin(bindata$cn, bindata$northing, xbins=60)
sum(thebins@count[thebins@count>1000])

# plot the hexbins
hplt <- plot(thebins, style = "colorscale", #xaxt="n",
  xlab="", ylab="")

pushHexport(hplt$plot.vp)

grid.lines(c(0.89,3.11), c(cTag$tagLocation[2],cTag$tagLocation[2]), 
  gp=gpar(col = 1), default.units = "native")

# but where do the really big bins (>1000) lie and how many points are in them
#grid.lines(c(0.94,2.59), c(431.3,431.3), 
#  gp=gpar(col = 1), default.units = "native")
#  
#grid.lines(c(1.25,1.25), c(430,433), 
#  gp=gpar(col = 1), default.units = "native")

# add labels to the big plot
grid.text(label="Condition Number", x = unit(2, "native"), 
  y = unit(426.5, "native"), gp=gpar(fontsize=20))
  
grid.text(label="Northing (m)", x = unit(0.55, "native"), 
  y = unit(432, "native"), rot=90, gp=gpar(fontsize=20))

# add b)
text(0.95,434.6,"b)", cex=1.5)

## add an inset plot
subplot(
  plot(d1$data$cn, d1$data$northing, pch=19, cex=0.5, las=1,
  ylim=c(400,480),
    xlab="",ylab=""),
  x=2.28,y=433.85,size=c(1.3,1.3)
)    

# add labels to the little plot
grid.text(label="Condition Number", 
  x = unit(2.75, "native"), y = unit(432.7, "native"), gp=gpar(fontsize=12))
  
grid.text(label="Northing (m)", 
  x = unit(2.1, "native"), y = unit(434.9, "native"), rot=90, gp=gpar(fontsize=12))



bbb







# first create an empty plot
library(lattice)
library(gridBase)

plot.new()
pushViewport(viewport())
xvars <- rnorm(25)
yvars <- rnorm(25)
xyplot(yvars~xvars)
pushViewport(viewport(x=.6,y=.8,width=.25,height=.25,just=c("left","top")))
grid.rect()
par(plt = gridPLT(), new=TRUE)
plot(xvars,yvars)
popViewport()

pushViewport(viewport(x=0.3,y=0.3,width=0.25,height=0.25))
grid.rect()
par(plt = gridPLT(), new=TRUE)
plot(xvars,yvars)
popViewport()









# push the same viewport several times














grid.newpage()
vp <- viewport(width=0.5, height=0.5)
pushViewport(vp)
grid.rect(gp=gpar(col="blue"))
grid.text("Quarter of the device",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))

  bob=grob(plot(1,1))
  
grid.draw(bob)
  
  
  
pushViewport(vp)    


grid.rect(gp=gpar(col="red"))
grid.text("Quarter of the parent viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
popViewport(2)
# push several viewports then navigate amongst them
grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.text("Top-level viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="grey"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.7, name="A"))
grid.rect(gp=gpar(col="blue"))
grid.text("1. Push Viewport A",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.1, width=0.3, height=0.6,
  just="left", name="B"))
grid.rect(gp=gpar(col="red"))
grid.text("2. Push Viewport B (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
if (interactive()) Sys.sleep(1.0)
upViewport(1)
grid.text("3. Up from B to A",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.5, width=0.4, height=0.8,
  just="left", name="C"))
grid.rect(gp=gpar(col="green"))
grid.text("4. Push Viewport C (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="green"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.6, name="D"))
grid.rect()
grid.text("5. Push Viewport D (in C)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
upViewport(0)
grid.text("6. Up from D to top-level",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="grey"))
if (interactive()) Sys.sleep(1.0)
downViewport("D")
grid.text("7. Down from top-level to D",
  y=unit(1, "npc") - unit(2, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("B")
grid.text("8. Seek from D to B",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="red"))
pushViewport(viewport(width=0.9, height=0.5, name="A"))
grid.rect()
grid.text("9. Push Viewport A (in B)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("A")
grid.text("10. Seek from B to A (in ROOT)",
  y=unit(1, "npc") - unit(3, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
seekViewport(vpPath("B", "A"))
grid.text("11. Seek from\nA (in ROOT)\nto A (in B)")
popViewport(0)

bbb












pushHexport(hplt$plot.vp)




plType = vector()
for (i in 1:(length(filterResults)-1)){plType[i]=ifelse(filterResults[[i]]$center,1,2)}    
par(mar=c(5,4.5,1,1)+0.1)
# create the main plot
plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
  las=1, cex.lab=1.8, cex.axis=1.5, bty="l", xaxt="n",
  xlim=c(1,20), ylim=c(0,1),
  xlab="Condition Number", ylab="Fraction of Unfiltered Position Solutions")   
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# add x-axis
axis(1,c(1,3,5,10,15,20),cex.axis=1.3)
# add a)
text(0.8,1,"a)",cex=1.5)

# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)

# add a legend    
#legend(12.24, 0.62, c("Central transmitter","Central transmitter in b)",
#  "Marginal transmitter"), lty = c(1,1, 2), lwd=c(1,2,1))    

## another shape legend
legend(14.25, 0.75, c("Central","transmitter","Central", "transmitter in b)",
  "Marginal", "transmitter"), lty = c(1,NA,1,NA,2,NA), lwd=c(1,NA,2,NA,1,NA))    


## add an inset plot
#tmp = subplot(
#  plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
#    las=1,xlim=c(0.9,3), ylim=c(0,1),xlab="",ylab=""),
#  17,0.34,size=c(2,2.7)    
#)
#
tmp = subplot(
  plot(filterResults[[1]]$psDecay$cn, filterResults[[1]]$psDecay$psFrac, type="n",
    las=1,xlim=c(0.9,3), ylim=c(0,1),xlab="",ylab=""),
  14.8,0.27,size=c(3.3,2)    
)


op = par(no.readonly=TRUE)
par(tmp)
for (i in 1:(length(filterResults)-1)){
  points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], las=1)
}
# now darken the line of the tag shown in panel A, which is tagList[[11]].
# note that tagList[[11]] ~ and filterResults[11]]
i=11
points(filterResults[[i]]$psDecay$cn, filterResults[[i]]$psDecay$psFrac, 
    type="l", lty=plType[[i]], lwd=3)
mtext("Condition Number",1,2)
mtext("Fraction of Unfiltered",2,3.5) 
mtext("Position Solutions",2,2.5)
par(op)










### what's the range of ps removed when filtering at cn=1.5 ###################
filterResults[[1]]$psDecay$cn[15]
filterResults[[1]]$psDecay$psFrac[15]

vec1 = c()
for(i in 1:length(filterResults)){
  vec1[i] = round(filterResults[[i]]$psDecay$psFrac[15]*100,2)
}

range(vec1, na.rm=T)

### for panel b what percent of points are in the three blackest dots and how 
### far are they from the GPS position estimate
str(d1$data)
plot(thebins@count)
# from looling at this plot, I want the three bins with more than 1000 points
# I'll manually count them
bigbins = 1381+4049+2345
bigbins/sum(thebins@count)             danger will robinson, these hexbin numbers
                                      have lots of data chopped of so the plotting
                                      comes out right.










































# examples of how filtering affects EN plots

# remember these 
load("C:/zy/Telemetry/R summary files/tag 2011Mar16.rdata")






















# pick a tag
cTag = tagList[[4]]

tagName=cTag$tagName 
deployment=cTag$deployment 
tagLocation=cTag$tagLocation 
startUtime=cTag$startUtime 
stopUtime=cTag$stopUtime
  
tagType = substr(tagName,1,1)
tagID = substr(tagName,2,10)
numSdl = 5

# get the raw data
d1 = importALPSdata(deployment=cTag$deployment, tagName=cTag$tagName, psr=TRUE)
# filter or no
d2 = filterALPSdata(df1=d1, minuteMean=FALSE)$data
d3 = filterALPSdata(df1=d1, cnF=1.5, speedF=0.8, minuteMean=FALSE)$data

# plots
par(mfrow=c(1,2))
plot(d2$easting, d2$northing, pch=19, cex=0.5)
plot(d3$easting, d3$northing, pch=19, cex=0.5)
cTag$tagName
nrow(d2)
nrow(d3)










#######################################################################
# how do water conditions affect hourly detection fraction and hourly ps fraction
#
# run testTheTag again just for the main array deployments lasting longer than 
# a day.  Filter and PSR.
allresults=list(); filterMeNow=T; psrMeNow=T;


# 2007 deployment
for (i in 1:5){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}

# 2008 deployment...the problem child
for (i in 6:10){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
}
# cTag 6 and 7 are b80 and b81, which are b79200
# ...something wrong with these so I won't use them...
# cTag 8 and 9 are b85 and b86, which are b79400
# cTag 10 is b130, which is b79500
allresults[[6]]$detFrac = rep(NA,5) #allresults[[6]]$detFrac + allresults[[7]]$detFrac
allresults[[7]]$detFrac = rep(NA,5)
allresults[[6]]$psFrac = NA #allresults[[6]]$psFrac + allresults[[7]]$psFrac
allresults[[7]]$psFrac = NA
allresults[[6]]$hourlyMeans = 
  allresults[[6]]$hourlyMeans[allresults[[6]]$hourlyMeans$detFrac1>999 ,]
allresults[[7]]$hourlyMeans = 
  allresults[[7]]$hourlyMeans[allresults[[7]]$hourlyMeans$detFrac1>999 ,]

allresults[[8]]$detFrac = allresults[[8]]$detFrac + allresults[[9]]$detFrac
allresults[[9]]$detFrac = rep(NA,5)
allresults[[8]]$psFrac = allresults[[8]]$psFrac + allresults[[9]]$psFrac
allresults[[9]]$psFrac = NA

allresults[[8]]$hourlyMeans$detFrac1 = allresults[[8]]$hourlyMeans$detFrac1 + allresults[[9]]$hourlyMeans$detFrac1
allresults[[8]]$hourlyMeans$detFrac2 = allresults[[8]]$hourlyMeans$detFrac2 + allresults[[9]]$hourlyMeans$detFrac2
allresults[[8]]$hourlyMeans$detFrac3 = allresults[[8]]$hourlyMeans$detFrac3 + allresults[[9]]$hourlyMeans$detFrac3
allresults[[8]]$hourlyMeans$detFrac4 = allresults[[8]]$hourlyMeans$detFrac4 + allresults[[9]]$hourlyMeans$detFrac4
allresults[[8]]$hourlyMeans$detFrac5 = allresults[[8]]$hourlyMeans$detFrac5 + allresults[[9]]$hourlyMeans$detFrac5
allresults[[8]]$hourlyMeans$psFrac = allresults[[8]]$hourlyMeans$psFrac + allresults[[9]]$hourlyMeans$psFrac

allresults[[9]]$hourlyMeans = 
  allresults[[9]]$hourlyMeans[allresults[[9]]$hourlyMeans$detFrac1>999 ,]

# hb1:11-15. sb1:16-20. sb2:21-25. hb2:26-30.  sb3:31-35. hb3:36-39. sb4:40-43.
# ...remember these are only the stationary tags
# ...40-41 has no ADCP data
for (i in 11:39){ # cycle through all tags in this deployment
  allresults[[i]]=testTheTag(tagList[[i]], filterMe=filterMeNow, psrMe=psrMeNow)
} 

# now that I've got all the hourly data in allresults, compile it all into 
# a single data.frame
hd1 = data.frame(hour=NA, detFrac1=NA, detFrac2=NA, detFrac3=NA, detFrac4=NA,
  detFrac5=NA, psFrac=NA, temperature=NA, magL=NA, eaaL=NA, eaaM=NA, eaaU=NA, tagName=NA,
  center=NA)

for (i in c(1:5,8,10:39)){        #1:length(allresults)){
  temp1 = allresults[[i]]$hourlyMeans
  temp1$tagName = allresults[[i]]$tagName
  temp1$center = allresults[[i]]$center
  # append this one to the whole list
  hd1 = rbind(hd1, temp1)
}    

# finally ready for ggplot
ggplot(hd1, aes(x=eaaU, y=eaaM, group=center, colour=center)) +
  geom_point(alpha=0.5)                                     
                                                              
ggplot(z6, aes(x=magL, y=altitude, group=ID, colour=ID, fill=ID)) + 
  geom_point(alpha=0.05) + 
  geom_smooth(method="gam",formula=y~s(x),lwd=1.3) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +  #,bs="cc"
  coord_cartesian(xlim=range(z0$magL,na.rm=T), ylim=c(0,10)) +    # range(z0$temperature,na.rm=T)
  theme_bw() + 
  scale_x_continuous("Current Speed (m/s)") +
  scale_y_continuous("Altitude (m)", breaks = c(0,1,2,3,seq(4,10,by=2)))
   


















































###########################################################################
BELOW HERE IS THE OLD
 WAY OF DOING THINGS.
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################








###############################################################################
# First Dec 2007 deployment
cmd = md[[2]]

cmd$beaconNames
cmd$sdlEN

cTagNames = cmd$beaconNames --or-- cTagNames = c("c80", "c81", "c85", "c86", "c130")



# set the database and connect
dbName = paste("db",cmd$deployment  ,sep="")# connect to the database
dbcon = dbConnect(MySQL(), user="root", password="zy0014", dbname=dbName)

detections = list() # this will hold lists for all cTagNames 

for (i in 1:length(cTagNames)){
  # grab data from dbTable
  dbphrase = paste("select * from toa", cTagNames[i], ";", sep="")   
  res = dbGetQuery(dbcon, dbphrase)                
  
  # calculate hourly detection frequencies and total detection frequency of this beacon by all SDLs
  sdlNumber = 41:45
  sdl = list() # list to compute detections of beacon i by all j sdls
  totalFrac = data.frame("tag"=NA, "sdl"=NA, "distance"=NA, "totalFrac"=NA) # to hold the total detection fraction of tag i by sdl j
  for (j in 1:length(cmd$sdlEN$ID)){
    # pick only detections at this SDL
    sdl[[j]] = res[res$sdlNumber == sdlNumber[j],2:4]
    # convert character times to POSIXlt
    sdl[[j]]$datiG = as.POSIXlt(strptime(sdl[[j]]$datiG, 
      "%Y-%m-%d %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT")
    sdl[[j]]$datiL = as.POSIXlt(strptime(sdl[[j]]$datiG, 
      "%Y-%m-%d %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
  
    # Because hb2007 runs over the change of year...
    # create a vector of the index of the day of the run, 1-52 for example
    # ...there's got to be a prettier way of doing this, but...
    numUniqueDays = length(unique(sdl[[j]]$datiL$yday))
    startOfFirstDay = unclass(as.POSIXct(
      strptime(cmd$startDay, "%Y/%B/%d", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))[1]
    endOfAllHours = startOfFirstDay + 3600 * 1:(numUniqueDays*24)
   
    # something to hold hourly detection frequencies for this sdl
    hrlyFrac = data.frame("hourOfDeployment"=NA, "frac"=NA)  
    # calculate the hourly detection frequency of beacon i by sdl j
    for (k in 1:((numUniqueDays*24)-1)){
       temp1 = sdl[[j]][ (sdl[[j]]$utime > endOfAllHours[k]) & (sdl[[j]]$utime < endOfAllHours[k+1]), ]
       hrlyFrac[k,] = c(k, nrow(temp1)/180)
    }
    plot(hrlyFrac$hourOfDeployment, hrlyFrac$frac, type="l", 
      main=paste("Beacon ", cTagNames[[i]], "; SDL ", sdlNumber[j], sep=""))
    
    # find the position of the beacon and distance to each SDL
    temp1 = paste("4", substr(cTagNames[i],4,4), sep="")
    for (k in 1:length(cmd$sdlEN$ID)){
      if (temp1 == cmd$sdlEN$ID[k]){
        tagPos = c(cmd$sdlEN$easting[k], cmd$sdlEN$northing[k]) 
      } 
    }
    
    distance = sqrt( 
      (tagPos[1]-cmd$sdlEN$easting[j])^2 + (tagPos[2]-cmd$sdlEN$northing[j])^2 
    )
    
    # now compute the total detection frequency of all hours with some detections
    temp1 = hrlyFrac[hrlyFrac$frac > 0   ,]
    totalFrac[j,] = c(cTagNames[i], sdlNumber[j], distance, mean(temp1$frac))
  } # end for j loop over all slds     
  
#  # find the position of the beacon and distance to each SDL
#  temp1 = paste("4", substr(cTagNames[i],4,4), sep="")
# 
#  for (j in 1:length(cmd$sdlEN$ID)){
#    if (temp1 == cmd$sdlEN$ID[j]){
#      tagPos = c(cmd$sdlEN$easting[j], cmd$sdlEN$northing[j]) 
#    }
#    totalFrac$distance[j] = sqrt( 
#      (tagPos[1]-cmd$sdlEN$easting[j])^2 + (tagPos[2]-cmd$sdlEN$northing[j])^2 
#    )
#  }
  
 

  # put everything into the detections list
  detections[[i]] = totalFrac
}

# pick one of the following to save the current detection results
detections7 = detections
detections8 = detections






# while I'm with Ben, I won't have the db, so save detections as a file to take with me...
write.csv(detections, 
  "C:/zy/Telemetry/R summary text files/2007 detection distance trial 2010Oct28.csv", row.names=FALSE) 


###############################################################################
# Second...22 July 2008 detection trials at 200m and 300m, T60800
# On this day we placed tag T60800 at these distances at these times:
#
# distance 300m (diver probably between tag and sdl
# 13:29:00 EDT - 13:34:00 EDT  (1216747740-1216748040 utime GMT)
# 
# distance 200m (diver not between tag and sdl
# 13:52:00 EDT - 13:57:00 EDT  (1216749120-1216749420 utime GMT)
# distance 200m (diver between tag and sdl)
# 13:58:00 EDT - 14:00:00 EDT  (1216749480-1216749600 utime GMT)
#
# sdl data is in two files named "SN265045_22Jul08.txt": one is the converted 
#   *.bin file and one is the ALPS generated record of all detections by sdl45
#   on 22Jul08.txt.
# There's also "TxId60800.toa" ...the times T60800 has detections at sdl45
# 
# Although it would be interesting to see how these compare, the right thing to
#   do for comparisons of detections is to use the *.toa file, which only shows
#   detections when a full symbol was possible.  No PSR is involved at the 
#   detection stage.

# read in the data
cDir = setwd("E:/DATA/Telemetry/2008/2008 Jul 22 SDL detection trials/ALPS output/20080722")
filename = "TxId60800.toa"
d1 = read.table(filename, header=FALSE, 
  col.names = c("utime", "fraction", "power", "sType", "sValue"))

# now calculate detection fractions during the three time periods...and also for
# periods 2 and 3 together, just for fun

# distance 300m (diver probably between tag and sdl
utime1 = c(1216747740, 1216748040) # 5 min
utime2 = c(1216749120, 1216749420) # 5 min
utime3 = c(1216749480, 1216749600) # 2 min
utime4 = c(1216749120, 1216749600) # 8 min

detections1 = nrow(d1[ (d1$utime > utime1[1]) & (d1$utime < utime1[2]) ,])
numTransmissiona1 = (utime1[2] - utime1[1])/2
frac1 = detections1/numTransmissiona1

detections2 = nrow(d1[ (d1$utime > utime2[1]) & (d1$utime < utime2[2]) ,])
numTransmissiona2 = (utime2[2] - utime2[1])/2
frac2 = detections2/numTransmissiona2

detections3 = nrow(d1[ (d1$utime > utime3[1]) & (d1$utime < utime3[2]) ,])
numTransmissiona3 = (utime3[2] - utime3[1])/2
frac3 = detections3/numTransmissiona3

detections4 = nrow(d1[ (d1$utime > utime4[1]) & (d1$utime < utime4[2]) ,])
numTransmissiona4 = (utime4[2] - utime4[1])/2
frac4 = detections4/numTransmissiona4

# combine those points that are really meaningful for comparison
oneDayDistance = c(300, 200, 200) 
oneDayFrac = c(frac1, frac2, frac3)

### look at differences between minutes...pick one time period at a time
startTime = utime3[1] # change this to change time periods
numMin = 2 # change this to change time periods

minuteBreaks = seq(from=0, by=60, length.out = numMin+1) 
bins = startTime + minuteBreaks
# count detections each minute
minFreqs = vector(mode="numeric", length=numMin)
for (i in 1:numMin){
  minFreqs[i] = nrow(d1[ (d1$utime > bins[i]) & (d1$utime < bins[i+1]) ,])/30
}
plot(minFreqs, pch=19)
abline(h= mean(minFreqs), col="red")

###############################################################################
# Third...Oct 2008 deployment...remember this one must work on codes not on full 
# symbol.  So remember that the comparison won't be exactly nice becasue
#
# BE CAREFUL WITH THESE, THERE ARE TIMES WHEN CODES COME EVERY 2s EVEN THOUGH 
# THEY ARE 20s BEACONS.  ALSO, REMEMBER THAT INDIVIDUAL CODES ONLY REPRESENT
# HALF OF A TAG.  THEY WILL NEED TO BE COMBINED AFTER.
#
# These are the codes that uniquely identify a beacon
# b79500 - c130
# b79200 - c80, c81
# b79400 - c85, c86
# 
# Besides these...
# ...when only beacons were in the water:
# b79100 - c78, c79
# ...and when all tags are in the water:
# b79100 - c79 occurs uniquely with b79100 every other transmission...every 40 sec

# first do the 'normal' ones
cmd = md[[2]]
cmd$beaconNames
cmd$sdlEN

cTagNames = c("c80", "c81", "c85", "c86", "c130") #cmd$beaconNames

# set the database and connect
dbName = paste("db",cmd$deployment  ,sep="")# connect to the database
dbcon = dbConnect(MySQL(), user="root", password="zy0014", dbname=dbName)

detections = list() # this will hold lists for all cTagNames 

for (i in 1:length(cTagNames)){
  # grab data from dbTable
  dbphrase = paste("select * from toa", cTagNames[i], ";", sep="")   
  res = dbGetQuery(dbcon, dbphrase)                
  
  # calculate hourly detection frequencies and total detection frequency of this code by all SDLs
  sdlNumber = 41:45
  sdl = list() # list to compute detections of beacon i by all j sdls
  totalFrac = data.frame("tag"=NA, "sdl"=NA, "distance"=NA, "totalFrac"=NA) # to hold the total detection fraction of tag i by sdl j
  for (j in 1:length(cmd$sdlEN$ID)){
    # pick only detections at this SDL
    sdl[[j]] = res[res$sdlNumber == sdlNumber[j],2:4]
    # convert character times to POSIXlt
    sdl[[j]]$datiG = as.POSIXlt(strptime(sdl[[j]]$datiG, 
      "%Y-%m-%d %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT")
    sdl[[j]]$datiL = as.POSIXlt(strptime(sdl[[j]]$datiG, 
      "%Y-%m-%d %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
  
    # This is unnecessary in 2008 but easier to leave it
    # create a vector of the index of the day of the run, 1-52 for example
    # ...there's got to be a prettier way of doing this, but...
    numUniqueDays = length(unique(sdl[[j]]$datiL$yday))
    startOfFirstDay = unclass(as.POSIXct(
      strptime(cmd$startDay, "%Y/%B/%d", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))[1]
    endOfAllHours = startOfFirstDay + 3600 * 1:(numUniqueDays*24)
   
    # something to hold hourly detection frequencies for this sdl
    hrlyFrac = data.frame("hourOfDeployment"=NA, "frac"=NA)  
    # calculate the hourly detection frequency of beacon i by sdl j
    for (k in 1:((numUniqueDays*24)-1)){
       temp1 = sdl[[j]][ (sdl[[j]]$utime > endOfAllHours[k]) & (sdl[[j]]$utime < endOfAllHours[k+1]), ]
       hrlyFrac[k,] = c(k, nrow(temp1)/180)
    }
    plot(hrlyFrac$hourOfDeployment, hrlyFrac$frac, type="l", 
      main=paste("Beacon ", cTagNames[[i]], "; SDL ", sdlNumber[j], sep=""))
    
    # find the position of the beacon and distance to each SDL
    if( (cTagNames[i] == "c80") | (cTagNames[i] == "c81") ){
      tagPos = c(cmd$sdlEN$easting[2], cmd$sdlEN$northing[2])
    } else if ( (cTagNames[i] == "c85") | (cTagNames[i] == "c86") ){ 
      tagPos = c(cmd$sdlEN$easting[4], cmd$sdlEN$northing[4])
    } else if (cTagNames[i] == "c130"){
      tagPos = c(cmd$sdlEN$easting[5], cmd$sdlEN$northing[5])
    } else {
      tagPos = NULL
      print("This tag appears to not be a beacon")
    }
    
    distance = sqrt( 
      (tagPos[1]-cmd$sdlEN$easting[j])^2 + (tagPos[2]-cmd$sdlEN$northing[j])^2 
    )
    
    # now compute the total detection frequency of all hours with some detections
    temp1 = hrlyFrac[hrlyFrac$frac > 0   ,]
    totalFrac[j,] = c(cTagNames[i], sdlNumber[j], distance, mean(temp1$frac))
  } # end for j loop over all slds     
  
#  # find the position of the beacon and distance to each SDL
#  temp1 = paste("4", substr(cTagNames[i],4,4), sep="")
# 
#  for (j in 1:length(cmd$sdlEN$ID)){
#    if (temp1 == cmd$sdlEN$ID[j]){
#      tagPos = c(cmd$sdlEN$easting[j], cmd$sdlEN$northing[j]) 
#    }
#    totalFrac$distance[j] = sqrt( 
#      (tagPos[1]-cmd$sdlEN$easting[j])^2 + (tagPos[2]-cmd$sdlEN$northing[j])^2 
#    )
#  }
  
  # put everything into the detections list
  detections[[i]] = totalFrac
}

# now combine 80/81 and 85/86
d1 = detections
d2 = d1
#
d2[[1]][,1] = "c80c81"
d2[[1]][1,4] = as.numeric(d1[[1]][1,4]) + as.numeric(d1[[2]][1,4])
d2[[1]][2,4] = as.numeric(d1[[1]][2,4]) + as.numeric(d1[[2]][2,4])
d2[[1]][3,4] = as.numeric(d1[[1]][3,4]) + as.numeric(d1[[2]][3,4])
d2[[1]][4,4] = as.numeric(d1[[1]][4,4]) + as.numeric(d1[[2]][4,4])
d2[[1]][5,4] = as.numeric(d1[[1]][5,4]) + as.numeric(d1[[2]][5,4])
#
d2[[3]][,1] = "c85c86"
d2[[3]][1,4] = as.numeric(d1[[3]][1,4]) + as.numeric(d1[[4]][1,4])
d2[[3]][2,4] = as.numeric(d1[[3]][2,4]) + as.numeric(d1[[4]][2,4])
d2[[3]][3,4] = as.numeric(d1[[3]][3,4]) + as.numeric(d1[[4]][3,4])
d2[[3]][4,4] = as.numeric(d1[[3]][4,4]) + as.numeric(d1[[4]][4,4])
d2[[3]][5,4] = as.numeric(d1[[3]][5,4]) + as.numeric(d1[[4]][5,4])

d3 = list(d2[[1]], d2[[3]], d2[[5]])


detections8 = d3

# while I'm with Ben, I won't have the db, so save detections as a file to take with me...
write.csv(detections8, 
  "C:/zy/Telemetry/R summary text files/2008 detection distance trial 2010Oct28.csv", row.names=FALSE) 





###############################################################################
# look at results so far
# put the interesting bits into one list


plot(detections7[[1]]$distance, detections7[[1]]$totalFrac, xlim=c(0,300), ylim=c(0,1.5), pch=19)
points(detections7[[2]]$distance, detections7[[2]]$totalFrac, pch=19, col="red")
points(detections7[[3]]$distance, detections7[[3]]$totalFrac, pch=19, col="blue")
points(detections7[[4]]$distance, detections7[[4]]$totalFrac, pch=19, col="green")
points(detections7[[5]]$distance, detections7[[5]]$totalFrac, pch=19, col="orange")

points(oneDayDistance, oneDayFrac, pch=15, col="yellow")

points(detections8[[1]]$distance, detections8[[1]]$totalFrac, pch=17, col="black")
points(detections8[[2]]$distance, detections8[[2]]$totalFrac, pch=17, col="red")
points(detections8[[3]]$distance, detections8[[3]]$totalFrac, pch=17, col="blue")


################################################################################
### position solutions
################################################################################


################################################################################
### position solutions through time
################################################################################

################################################################################
### signal traffic
################################################################################

################################################################################
### position solutions within array variation
################################################################################


