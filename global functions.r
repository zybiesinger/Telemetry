##############################################################################
### This file defines common functions which are used in other
### files.  This way I only have to change them in one place.
##############################################################################
### It requires variables defined in 'global variables.r'

library(plyr)
library(zoo)
library(circular)


###############################################################################
###############################################################################
###############################################################################
# Index # Index # Index # Index # Index # Index # Index # Index # Index # Index 
# importALPSdata
# filterALPSdata
# subsample
# kielFilter
# positionStats1
# importADCPdata
# mergeAlpsAdcpData
# findHabType
# mergeSonarData
# importGPSdata
# importTideData
# importSunData
# importRawSDLdata  # this one not yet written...this data in MySQL db
# importToaData
# toaStats1 
# importBatteryData
# importBiometricData
# circles3d
# clocks3d
# anglefun
# bearing.ta
# meanAngle
# homeRange
# rotate
# chop
# kde2dplot
# kde2dplot2
# mymovie
# importFishData
# plot.imagematrix.zy


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### This file reads in fish movement data from the daily files and combines
### them into a single data frame.  It filters out bad points with
### erroneous (-1) depth values.  It filters on cn.  Also, sets the 
### 'zero' unix time.    
##############################################################################

### This requires variables defined in 'commonVariables.r'

##############################################################################
### IMPORT DATA      NOTE: this was renamed from 'importSdlData()'
##############################################################################

importALPSdata = function(deployment, tagName, beaconName="useBest", psr=TRUE, 
  offset=TRUE, whichDays=NA, chopTimes=TRUE, altDir="n"){
  # 'deployment' is the experiment designation, i.e. IF43 for 2009 experiments
  # 'tagName' is the desired fish tag, i.e. f18 
  # 'beaconName' is the desired beacon, i.e. b1
  # 'psr'  use the psr or no psr output file
  # 'offset' use the global offsets when reporting positions?
  # 'whichDays' is the list of day indices, 1:38 for 2007, 1:52 for 2008, not used for 2009
  # 'chopTimes' when true remove all data before and after start and stopUtimes
  #
  # 'altDir' is added in case you want to use an alternate directory, for example
  #      when testing ALPS with different sound speeds you want to use multiple
  #      directories.  It should be "n" or set to the desired directory.
  # This function no longer filters or subsamples data, see function 'subsample()'
  
  # deployment="hb2007"; tagName="b79100"; beaconName="useBest"; psr=TRUE; offset=TRUE; whichDays=12;  
  # deployment="hb1"; tagName="f11"; beaconName="b1"; psr=TRUE; offset=TRUE; whichDays=NA;  
    
  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      year = md[[i]]$year
      SDLmode = md[[i]]$SDLmode
      timezone = md[[i]]$timezone
      if (beaconName == "useBest"){beaconName = md[[i]]$bestBeacon} 
        # else make no changes and use 'beaconName'
      # ...but double check were not looking at the best beacon
      if(tagName == beaconName){beaconName = md[[i]]$secondBestBeacon}  
      trueNames = md[[i]]$trueNames
      alpsDir = paste(md[[i]]$homeDir,"/ALPS 2011Feb14",sep="")
      startUtime = md[[i]]$startUtime
      stopUtime = md[[i]]$stopUtime
      taggingDay = md[[i]]$taggingDay
      #reefEN = md[[i]]$reefEN
      sdlEN = md[[i]]$sdlEN  
    }  
  }
  print(paste("importALPSdata:",deployment,"-",tagName,"-",beaconName))
  
  # if altDir = F then use the alpsDir just set.  Otherwise use the directory
  # specified by altDir
  if (altDir != "n"){alpsDir = altDir}
  
  # other stuff      
  tagID = substr(tagName,2,100)
  tagType = substr(tagName,1,1)
  beaconID = substr(beaconName,2,100)
  oDir=getwd()
  setwd(alpsDir) 
  
  ### names of columns to be read from the ALPS output files
  columnNames = c("utime", "easting", "northing", "depth", "cn",
    "rn", "dop", "hid", "hcount", "mystery1", "mystery2")              
     
  # because in 2007/2008 the data for each tag are divided into multiple files...
  # ...there's a folder and file for each day  
  if (year == 2007){ # this year is run in symbol mode
    # create a pattern for all files for this beacon, tag combination
    namePattern = paste("T",tagID,beaconName,sep="")
    
    # gather all the file names for the tag  
    fileNames = list.files(pattern = namePattern, recursive=TRUE, ignore.case=TRUE)
    ### if you've asked for more days than are present, don't worry about it
    if (length(whichDays) > length(fileNames)) {whichDays = 1:length(fileNames)}
  
    ### pick one or more days from the middle because there's not enough memory
    ### Remember they might not be in chronological order...
    ### these file names include the subdirectory from the ~\ALPS directory
    if (whichDays=="all" || is.na(whichDays)){whichDays=1:length(fileNames)}
    fileNames = fileNames[whichDays] #1=09Dec2007, 24=01Jan2008;     
    
  } else if (year == 2008){ # this year ends up being run in symbol mode
    # and there are two "code" files for each tag
    # B79200: 80, 81
    # B79400: 85, 86
    # B79500: 130
    # F60100: 66, 67
    # F60300: 70, 71
    # F61100: 46, 47
    # F61200: 48, 49
    # F61300: 51, 52
    # o61500: 55, 56 - the REMUS tag
    
    originalTagNames = c("b79200", "b79400", "b79500", "f60100", "f60300", 
      "f61100", "f61200", "f61300", "o61500")
    tagCodes = list(
      c("80", "81"), 
      c("85", "86"), 
      c("130"), 
      c("66", "67"), 
      c("70", "71"),
      c("46", "47"), 
      c("48", "49"),
      c("51", "52"), 
      c("55", "56")
    )   
    
    # Sometimes I sent these tagNames as b79200 and sometimes as b80 and b81...
    # ...determine which and proceed accordingly
    if (nchar(tagName) < 6){ 
      # if you send only one code number, then proceed with only that one
      currentCodes = substr(tagName,2,5) 
    } else if (nchar(tagName==6)){
      # if you send one symbol number, then proceed with both relevant codes
      # ...pick the right codes for the current tag
      for (i in 1:length(originalTagNames)){
        if (tagName == originalTagNames[i]){ currentCodes = tagCodes[[i]] }
      }
    } else {
       # if the tagName doesn't fit the pattern print the error flag.
       print("Tag name is wrong. Search for 'Error 1' ")
    } 
     
    # create a pattern for all files for this beacon, tag combination
    namePatterns = paste("T",currentCodes,beaconName,sep="")
    
    # gather all the file names for the tag  
    fileNames1 = list.files(pattern = namePatterns[1], recursive=TRUE, 
      ignore.case=TRUE)
    fileNames = fileNames1  
    # if there's a second fileName, add it here
    if(length(namePatterns) == 2){   
      fileNames2 = list.files(pattern = namePatterns[2], recursive=TRUE, 
        ignore.case=TRUE)
      fileNames = c(fileNames1, fileNames2)  
    } 
    # don't worry about picking only whichDays, there is so little data, just
    # get everything everytime
    
  } else if (year==2009){ # ...but in 2009 there's one file per tag
    
    # create the filename 'with psr' or 'without psr'
    if (psr){
      fileNames = paste("T",tagID,"B",beaconID," psr.txt", sep="")
    } else {
      fileNames = paste("T",tagID,"B",beaconID," no psr.txt", sep="")
    }      
  }
  
  ### ...then read in data from that file
  d1 = lapply( as.list (fileNames), read.table, header=FALSE, 
    col.names=columnNames)
 
  if (nrow(d1[[1]]) > 0){ # then do the normal things, if there are no data,         
                             #   make a blank answer for ourput  
    d2 = do.call("rbind", d1)    # this doesn't ensure chronological order
    d3=d2
    ### some years the SDLs were set to local (EST or EDT) time instead of GMT
    ### GMT = EDT+4hrs = EST+5hrs. 1hr = 60min * 60sec = 3600sec.
    ###
    ### Read "Time zones in ALPS 2011Mar26.docx" for my final word on this.
    ###
    ### I am confident that the text version of the *.bin files with 
    ### readable dati show which ever timezone I synchronized the SDL clock to, 
    ### local or GMT.  I am also confident that ALPS output files showing 
    ### position solutions (showing unix time) show time as GMT.
    ### 
    ### So no utime correction required
  
    d4=d3  
    d5=d4
                                                                              
    ### To offset or not to offset easting and northing numbers
    if (offset){ eOffsetTemp = eastingOffset; nOffsetTemp = northingOffset;
    } else { eOffsetTemp = 0; nOffsetTemp = 0; }
    d5$easting = d4$easting - eOffsetTemp
    d5$northing = d4$northing - nOffsetTemp
    ### Convert from psi to depth in meters.  ALPS output is   
    ### in psi.  The max psi is 19, and the location is 40ft=13m.
    ### Find this worked out in 'convert pressure to depth.xlsx'.
    d5$depth = (34/50)*d4$depth 
    # change the negative values to NA...this may be all
    d5$depth[d5$depth < 0] = NA    
  
    d6=d5
      
    ### Ensure chronological order
    # ...because I'm a sloppy programmer...in one case there's only one line
    # ...so trying to reorder it makes a mess...
    if (nrow(d6)>1){
      d7 = data.frame( cbind(
        "utime" = d6$utime, 
        "easting" = d6$easting, 
        "northing" = d6$northing, 
        "depth" = d6$depth, 
        "cn" = d6$cn,
        "rn" = d6$rn,                         
        "dop" = d6$dop,
        "hid" = d6$hid,
        "hcount" = d6$hcount,
        "mystery1" = d6$mystery1,
        "mystery2" = d6$mystery2)
        [order(d6$utime), ]
      )
    } else { d7 = d6 }
    
    # Add datiG in the data.frame...for some reason the cbind step 
    # undoes these conversions, so do this now
    d8 = cbind(
      utime = d7$utime,
      datiG = d7$utime,
      datiL = d7$utime,
      subset(d7,select=c("easting", "northing", "depth", "cn", "rn", "dop", 
        "hid", "hcount", "mystery1", "mystery2"))
    )
  
    d8$datiG = as.POSIXlt(d8$datiG, origin="1970-1-1", tz="GMT")
    d8$datiL = as.POSIXlt(d8$datiL, origin="1970-1-1", tz="EST5EDT") 
  

    ### I REPLACED THIS SECTION FROM THE LINES THAT FOLLOW IMMEDIATELY...THIS 
    ### IS GOING TO BREAK SOME OTHER CODE SOMEPLACE
    # for fish and beacon tags, only keep positions after and before the start 
    # and stop utimes.  Sometimes "other" tags were used by divers during 
    # deployments or recovery and so lie outside the official start and stop
    # times.  Don't filter them here, but use their ALPS results with caution
    # to be sure you only use points obtained while the array was in place.
    #if (tagType == "o"){ # do nothing
    #  d9=d8
    #} else { # keep positions only after and before the start and stop utimes.
    #  d9 = d8[((d8$utime>startUtime) & (d8$utime<stopUtime)) ,] 
    #}
    if (chopTimes){
      d9 = d8[((d8$utime>startUtime) & (d8$utime<stopUtime)) ,] 
    } else {
      d9=d8
    }                               
  
  
    # to calculate the day of deployment is easy if the deployment didn't cover
    # the new year, like the 2007 deployment.
    
    if (year == 2007) {
      # this is the old way, 
      # It has a flaw, but luckily worked fine for most fish.
      # The problem is that 'd9$datiL - tagDay' must have units of hours
      # in order for the code to work, and whatever units the first element has
      # are the units used for all.  So for most fish the first tag reception
      # comes hours (not minutes or days) after midnight of tagDay...but not all
      # fish.  But since 2007 runs over the new year simply using $yday doesn't
      # work...so here's an ugly if.statement that happens to work with my data
      tagDay = as.POSIXlt(strptime(taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
        origin="1970-1-1")
      d9$dod = floor(as.numeric(d9$datiL - tagDay)/24)+1
      
      if(sum(d9$dod<1)>0){print("Some dod values are negative and have been changed to 1")}
      d9$dod[(d9$dod < 1)] = 1
    } else if ((year == 2008) | (year == 2009)){
      # add a 'day of deployment' column, which counts number of days after tagging
      tagDay = as.POSIXlt(strptime(taggingDay, "%Y/%B/%d", tz="EST5EDT"), 
        origin="1970-1-1")$yday 
      d9$dod = d9$datiL$yday - tagDay + 1
    } else { print("Error in importALPSdata.  'year' is wrong") }
    
  
    # add a 'time of day' column a fractional value between 0 and 23.99772
    # 3600 sec/hr.  60 sec/min.
    d9$tod = ((( (d9$datiL$hour*3600) + (d9$datiL$min)*60 + (d9$datiL$sec) )/86400)*24)
    # add an 'hour of day' column - THIS MIGHT BE COMPLETELY REPLACED BY TOD
    d9$hod = d9$datiL$hour 
  
    # add 'lunarPhase' column.  1 = new moon through 16 or 17 = full moon.  
    # See'lunar phases.r' and 'lunar phases.xlsx' for more.
    # There's a pretty way to do this, but this is much faster
    if (year == 2007){ 
      temp1 = c(1:30,1:8)
      d9$lunarIndex = temp1[d9$dod]
      # note you could read in the 'lunar phases 2007.csv' file, but because
      # this deployment goes over the new year, that file won't work...
      # fortunately, that's only true of 2007      
    } else if (year == 2008) {
      # read in the lunarIndex for each day of 2009
      fn1 = "C:/zy/school closet/Telemetry/R summary files/lunar phases 2008.csv"
      lunar2008 = read.table(file=fn1,header=T,sep=",",
        col.names=c("month","day","doy","lunarIndex"), 
        colClasses=c("character",rep("numeric",3))
      )
      # now pick the day of each datum in d9 and determine the lunarIndex
      # luckily the order of lunar2008 is the same as the order as yday
      # 
      d9$lunarIndex = lunar2008$lunarIndex[d9$datiL$yday]
    
      # this is the old way, which fails when I try and run beacons, which were
      # in the water before taggingDay so that they have negative dod values...
      # temp1 = c(19:29,1:30,1:30,1:5)
      # d9$lunarIndex = temp1[d9$dod]
    } else if (year == 2009){
      # read in the lunarIndex for each day of 2009
      fn1 = "C:/zy/school closet/Telemetry/R summary files/lunar phases 2009.csv"
      lunar2009 = read.table(file=fn1,header=T,sep=",",
        col.names=c("month","day","doy","lunarIndex"), 
        colClasses=c("character",rep("numeric",3))
      )
      # now pick the day of each datum in d9 and determine the lunarIndex
      # luckily the order of lunar2009 is the same as the order as yday
      # 
      d9$lunarIndex = lunar2009$lunarIndex[d9$datiL$yday]
    }
  } else { # there were no data in d1, so make a blank data set
    d9 = data.frame(
      utime=NA, datiG=NA, datiL=NA, easting=NA, northing=NA, depth=NA, cn=NA, 
      rn=NA, dop=NA, hid=NA, hcount=NA, mystery1=NA, mystery2=NA, dod=NA, 
      tod=NA, hod=NA, lunarIndex=NA)
  }
  ### put directory back to what it was
  setwd(oDir)
                          
  # return the data (as a data.frame) and other stuff
  list("data"=d9, "tagName"=tagName, "beaconName"=beaconName, "psr"=psr, 
    "deployment"=deployment)                      
}  #### end importData ########################################################
###############################################################################
# bob = importALPSdata(deployment="sb3", tagName="f39", beaconName="b1", psr=TRUE, offset=TRUE)
# bob = importALPSdata(deployment="hb2007", tagName="f60200", beaconName="b79200", psr=TRUE, offset=TRUE, whichDays=c(1:3))
 

###############################################################################
# filter raw ALPS output using depth, cn, rn, dop, hid, or hcount
filterALPSdata = function(  
  ### this functions is for filtering raw ALPS output using ALPS output values
  df1, # the dataframe holding ALPS output from 'importALPSdata()'
  depthF=F, cnF=F, rnF=F, dopF=F, hidF=F, hcountF=F, # T/F filter using these
  speedF=F, # if filtering on this, specify a speed in m/s
  minuteMean=F # a switch to compute the mean easting, etc for each minute

){ 
  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (df1$deployment == md[[i]]$deployment){
      reefEN = md[[i]]$reefEN
    }  
  }
  print(paste("filterALPSdata:",df1$deployment,"-",df1$tagName))
  
  d1=df1$data
  
  # what to do if there are lines of data in d1
  if (nrow(d1) > 0){ # then filter as normal    
    # filter using depth, etc.
    if (depthF) {d1 = d1[d1$depth < depthF,]} 
    if (cnF) {d1 = d1[d1$cn < cnF,]}
    if (rnF) {d1 = d1[d1$rn < rnF,]}
    if (dopF) {d1 = d1[d1$dop < dopF,]}
    if (hidF) {print("I don't yet know how to filter on 'hid'")}
    if (hcountF) {d1 = d1[d1$hcount >= hcountF,]}
  } else { # don't do anything, just make data.frame look right
    d3=data.frame(
      utime=d1$utime,
      datiG=d1$datiG,
      datiL=d1$datiL,
      dod=d1$dod,
      tod=d1$tod,
      lunarIndex=d1$lunarIndex,
      easting=d1$easting,
      northing=d1$northing,
      depth=d1$depth,
      dtr=d1$depth, # I don't know how to make the rest empty the real way
      btr=d1$depth,
      interval = d1$depth,
      speed = d1$depth,
      turnAngle = d1$depth,
      npos = d1$depth
    )
  } 
  
  # now check again to see if there are any lines of data remaining
  if (nrow(d1) > 3){ # then continue as normal...you need four points for some calculations
  
    # calculate travel speed and turning angles
    # gather times and positions at the 'last', 'now' and 'next' times, 
    # these vectors will be 2 shorter than the basic columns
    lastUtime = head(d1$utime, -2)
    lastEasting = head(d1$easting, -2)
    lastNorthing = head(d1$northing, -2)
    nowUtime = tail(head(d1$utime, -1), -1)
    nowEasting = tail(head(d1$easting, -1), -1)
    nowNorthing = tail(head(d1$northing, -1), -1)
    nextUtime = tail(d1$utime, -2) 
    nextEasting = tail(d1$easting, -2)
    nextNorthing = tail(d1$northing, -2)
    # bearing.ta calculates the turning angle between the two steps: 
    # (last to now), and (now to next).  It also gets the distances between
    # the three points.  bearing.ta takes 3 2-column data.frames...
    p1 = data.frame(lastEasting, lastNorthing)    
    p2 = data.frame(nowEasting, nowNorthing)    
    p3 = data.frame(nextEasting, nextNorthing)  
       
    utimeDiff = c(nowUtime-lastUtime, 
      nextUtime[length(nextUtime)]-nowUtime[length(nowUtime)],1) # 1 in last place to avoid division by zero   
    # run bearing.ta and pick out answers
    temp1 = bearing.ta(p1,p2,p3, as.deg=TRUE, replaceNaN=TRUE)    
    turnAngle = c(0,temp1$ta,0)               
    speed = c(temp1$dist1, temp1$dist2[length(temp1$dist2)], 0) / utimeDiff
    # calculate the 2-D distance to reef (dtr) of each point
    dtr = sqrt((d1$easting-reefEN$easting)^2 + (d1$northing-reefEN$northing)^2)
    # calculate the 2-D bearing to the reef (btr) of each point
    # ...first find the easting/northing difference between fish and reef
    # ...do reef-fish so that a fish directly N of reef has a bearing to reef of 180deg 
    etemp = reefEN$easting-d1$easting
    ntemp = reefEN$northing-d1$northing
    # ...anglefun() can't do two points in exactly the same place so... 
    ntemp[etemp==0 & ntemp==0] = 7777777 # make this one directly north, only direction matters here
    # ...now calculate the bearing to fish from reef
    btr = anglefun(etemp, ntemp, bearing=TRUE, as.deg=TRUE)
    # change the class of btr to 'circular' so you can take the circular mean correctly
    btr = circular(btr, units="degrees", zero=pi/2, rotation="clock",  modulo="2pi")
    # ... as a note, if you ever want to use these as regular numbers just do...
    # ... attributes(btr) = NULL
        
    # calculate the time interval between two successive points
    interval = utimeDiff
     
    # add dtr, speed and turnAngle to the data.frame
    d2 = cbind(
      subset(d1, select=c("utime", "datiG", "datiL", "dod", "tod", "hod", 
        "lunarIndex", "easting", "northing", "depth")),
      dtr = dtr,
      btr = btr,
      interval = interval,
      speed = speed,
      turnAngle = turnAngle
    )
    
    # filter using gag travel speed
    if (is.numeric(speedF)){d2 = d2[speed < speedF,]}
    d3 = d2
    # do you want to use every recorded point or the mean of all points each minute
    if (minuteMean){
      d3$min = cut(d3$datiG, breaks="min")
      d3 = ddply(d3,"min",
        function(x){ with(x,data.frame(
          # min=min, # this one gets put in automaticaly as a factor
          utime=unclass(as.POSIXct(min, origin="1970-1-1", tz="GMT")[1]), 
          datiG=7777777,# I can't get this to be GMT instead of EST so leave it for later...as.POSIXct(min, tz="GMT")[1],
          datiL=7777777,
          dod = mean(dod),
          tod = mean(tod),
          hod = mean(hod),
          lunarIndex = mean(lunarIndex),
          easting=mean(easting), 
          northing=mean(northing), 
          depth=mean(depth),
          dtr=mean(dtr), 
          btr=mean.circular(btr), 
          interval=mean(interval),
          speed=mean(speed), 
          turnAngle=meanAngle(turnAngle),      
          npos=nrow(x)# how many positions went into this minute average
        ))} # end function/with/data.frame
      ) # end ddply()
      
      # now get rid of the $min column.  The last row is/might be NA 
      # because of calculating the min averages, so drop it
        
      d3 = subset(d3, select = -c(min))
      d3 = d3[-nrow(d3),]                        
      # fix the dati 
      d3$datiG = as.POSIXlt(d3$utime, origin="1970-1-1", tz="GMT")
      d3$datiL = as.POSIXlt(d3$utime, origin="1970-1-1", tz="EST5EDT")  
      
      # round some figures
      d3$dtr = round(d3$dtr,2)
      d3$btr = round(d3$btr,0)
      d3$interval = round(d3$interval,0)
      d3$speed = round(d3$speed,5)
      d3$turnAngle = round(d3$turnAngle,2)
      
    } else { # don't do anything, just make the data.frame look right
      attr(d3$data$btr,"circularp") = NULL
      d3$npos = NA 
    }# end if minuteMean                               
  } else { # else there are no data rows, just make the data.frame look right              
    d3=data.frame(
      utime=d1$utime,
      datiG=d1$datiG,
      datiL=d1$datiL,
      dod=d1$dod,
      tod=d1$tod,
      lunarIndex=d1$lunarIndex,
      easting=d1$easting,
      northing=d1$northing,
      depth=d1$depth,
      dtr=d1$depth, # I don't know how to make the rest empty the real way
      btr=d1$depth,
      interval = d1$depth,
      speed = d1$depth,
      turnAngle = d1$depth,
      npos = d1$depth
    )                              
  }
  
  d9=d3
  # return the data (as a data.frame) and other stuff
  list("data"=d9, "tagName"=df1$tagName, "beaconName"=df1$beaconName, 
    "psr"=df1$psr, "deployment"=df1$deployment)     
} # end 'filterALPS'
# sam = filterALPSdata(bob, cnF=3)


#################################################################################\
# subsample raw ALPS output
subsample = function(df1, subSample=1, ...){     # this has been changed and not checked for accuracy ###############################################
  # if 'subSample' = "kiel" then 
  # do kielFilter(), if 'subSample' = an integer then it becomes the 
  # subsampling frequency.  So subSample=1 is equivalent to taking all points

  ### apply the kielFilter or simply take a subset
  if (subSample == "kiel"){d9=kielFilter(df1, ...)}
  # else take some fraction of hits, but if 24 or fewer hits per day on average, take all  
  else if ( nrow(df1) <= (24*length(whichDays)) ) {d9=df1}
  else {
    ii = seq(1,nrow(df1), by=subSample)       
    d9=df1[ii,]                                                
  } 
}


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### Kiel Filter - to filter data like Brian Kiel
##############################################################################
kielFilter = function(df1, ...){
  # repeat Kiel's "first hit of the day" sampling 
  # His samples were one hit per day usually between 10am and 2pm
  # I want to pick out one hit each day between 10:00 and 14:00
  # If there are no points between those hours return NA
  
  ### set utime to "seconds since 'startTime'" depending on which year  
  if(year==2007) {startTime=startTime2007} else 
    if (year==2008) {startTime=startTime2008} else startTime = 0
  
  seconds = (df1$utime-startTime) %% 86400 # seconds since midnight, %%=module divide, gives only the remainder
  temp1 = cbind(seconds, df1)                                      # %/%=integer divide, gives only the integer
  # 10:00=36000 sec since midnight; 14:00=50400
  cutoffs=c(36000,50400)      
  # take only rows between 10:00 and 14:00
  temp2 = subset(temp1, temp1$seconds>cutoffs[1] &  temp1$seconds<cutoffs[2]) 
  firstDay = (temp2$utime[1] %/% 86400)
  day = temp2$utime %/% 86400 - firstDay + 1 # start counting days at 1
  temp3 = cbind(day,temp2)
  # split the entire dataframe into a list of smaller data frames, one for each day
  daylist = split(temp3, day)
  # a function to take one row from a data frame
  sample1 = function(x){x[sample(nrow(x), size=1),]}
  # apply 'sample1()' to each data frame in 'daylist' 
  temp4 = do.call("rbind", lapply(daylist,sample1))
  temp4[,3:6] # take only some columns     
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# calculate position stats, percent of position solutions for a given tag
#
# This function takes the output of 'importALPSdata()' and 'filterALPSdata()'
# and calculates ther fraction of position solutions of a tag for a given 
# time interval, for example, every 60 min.
# It produces a picture and returns a list of XX items.  1-5 are...6-8 are ...

positionStats1 = function(positionData, mLines=TRUE){
  # 'positionData' is the output of 'importALPSdata()' or 'filterALPSdata()'
  #   which is a list of 4 items: 1-data, 2-tagName, 3-beaconName, 4-deployment 
  #  'mLines' is a switch for plotting mean lines or not
  
  tagID = substr(positionData$tagName,2,100)
  tagType = substr(positionData$tagName,1,1)
  beaconID = substr(positionData$beaconName,2,100)

  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (positionData$deployment == md[[i]]$deployment){
      startUtime = md[[i]]$startUtime
      stopUtime = md[[i]]$stopUtime  
    }  
  }

  # some 'time' book keeping
  totalSec = stopUtime - startUtime
  thirtyMinBins = seq(from=startUtime, to=stopUtime, by=1800) # 30min * 60sec
  sixtyMinBins  = seq(from=startUtime, to=stopUtime, by=3600)
  bins = sixtyMinBins; mins=60; # if you choose a different bin size, fix the sentinel tpi in the lines below
  bins2 = as.POSIXct(bins, origin="1970-1-1", tz="GMT")  

  if (tagType == "f"){
    tpm = 30 # transmissions per minute for a fish tag
    tpi = tpm * mins # transmission per bin
  } else if (tagType == "b"){
    tpm = 3 # transmissions per minute for a beacon
    tpi = tpm * mins # transmission per bin
  } else if (tagType == "s"){
    tpm = 30 # transmissions per minute for the sentinel
    tpi = tpm * 5 * 2 # transmissions ber bin of 60 minutes
  } else { 
    print("Please pick a tag type")
  }

  # count position solutions per time intervals for 'tag'  
  freqList = data.frame("bin"=bins2[1], "frequency"=0) 
  for (j in 2:length(bins2)){ # j counts time bins
    temp1 = positionData$data[
      ((positionData$data$utime>bins[j-1]) & (positionData$data$utime<bins[j])),
    ]
    freqList = rbind(freqList, 
      data.frame("bin"=bins2[j-1], "frequency"=nrow(temp1)/tpi))  
  } # end j loop  
  
  # plot the results
  plot(freqList$bin, freqList$frequency, type="b", cex=1,
    xlim=c(startUtime, stopUtime), ylim=c(0,1.1),
    main=paste("Tag", tagID, "Position Solution Frequencies", sep=" "),
    sub=paste("Bin size:", mins, "min", sep=" "),xlab="Time", ylab="Frequency"
  )
  
  # show the means
  if (mLines){  
    abline(h=mean(freqList$frequency)) 
  }
  
  # return the position solution frequencies for 'tag' 
  list("positionSolutionFrequency"=freqList, "tagName"=positionData$tagName,
    "beaconName"=positionData$beaconName, "psr"=positionData$psr,
    "deployment"=positionData$deployment)
} # end positionStats1
# fred = positionStats1(sam, mLines=TRUE)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### IMPORT ADCP Data 
###       At the bottom is code writing the result to a *.csv file
##############################################################################
importADCPdata = function(recalculate=FALSE){
  # 'recalculate' is a switch to either import all ADCP data and re-calculate
  #          everything (=TRUE) or to simply read in the final useable 
  #          version saved as a text file (=FALSE)

  oDir = getwd()
  # to import or read in ADCP data  
  if (!recalculate){ # 'getSummary' == TRUE
    # ask the user for the file to use
    #filename = getFile(directory = "C:/zy/Data/ADCP summary text files/")
    #setwd(filename$dir)    
    cDir = setwd(paste(dataDir, adcpDir, sep=""))
    filename = list.files(pattern="adcpDataSummary")
    #filename = "adcpDataSummary.csv"
    adcpData = read.csv(filename, header=TRUE, 
      colClasses=c("numeric", "character", "character", rep("numeric", 13))
    )
    print(paste("Read data from", filename)) 
    # for some reason I can't read the dati in as POSIXct anymore, so convert now
    adcpData$datiG = as.POSIXlt(strptime(adcpData$datiG, "%Y-%m-%d %H:%M:%S", 
      tz="GMT"), origin="1970-1-1", tz="GMT")
    adcpData$datiL = as.POSIXlt(strptime(adcpData$datiL, "%Y-%m-%d %H:%M:%S", 
      tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
  } else { # 'recalculate' == FALSE

    ### change to desired ADCP directory
    cDir = paste(dataDir, "/ADCP Data 2010Sep-Final/for R", sep="")
    setwd(cDir)
    # get names for all the files
    filenames = list.files(pattern=".csv")
    # a list to hold the results
    adcpData = list()
    # for each of the adcp files in the folder...
    for (i in 1:length(filenames)){ 
      # settings that change for each ADCP deployment
      for (j in 1:length(adcpmd)){ # i loops through all deployments
        if (substr(filenames[[j]],1,10) == adcpmd[[i]]$deployment){
          startUtime = adcpmd[[j]]$startUtime
          stopUtime = adcpmd[[j]]$stopUtime
          deployment = adcpmd[[j]]$deployment
        }  
      }
    
      # open the connection
      rfile = file(filenames[i], open="rt") 
      # read in the column headers, units, and bin numbers...on 3 lines
      headers = tolower(as.data.frame(read.csv(rfile, header=FALSE, skip=12, nrows=1, 
        colClasses="character")))
      units1 = read.csv(rfile, header=FALSE, nrows=1)
      binNum = read.csv(rfile, header=FALSE, nrows=1)
   
      # construct unique column names
      colNames=c()
      for (j in 1:7){colNames[j] = headers[j]}
      # column 8 name is duplicate and column is unnecessary.  col 9 is empty
      colNames[8] = "junk1"; colNames[9] = "junk2";
      for (j in 10:13){colNames[j] = headers[j]}
      for (j in 14:length(headers)){colNames[j] = paste(headers[j], binNum[j], sep="_")}
      colNames = unlist(colNames)
    
      # if this is the one dataset which collected data every second instead of
      # an average every ten minutes...then calculate a 10 min average and drop
      # the rest of the data
      if (filenames[i] == "2009adcp05.csv"){
        ### read 600 lines, calculate and keep only the average, 10min = 600 sec
        # for holding the 10-minute mean values
        meanDF = data.frame(matrix(data=NA, nrow=1, ncol=length(colNames)))
        means = vector(mode="numeric", length=124)
       
        # skip one blank line, and just to make the output times pretty, skip 
        # the first data line
        readLines(rfile, n=2) 
        go=TRUE; # a switch to indicate eof
        counter = 1
        while(go){ # while the file still has lines to read in
          # read in 600 lines
          temp1 = read.csv(rfile, header=FALSE, nrows=600, col.names=colNames)
          # calculate the means of columns 10:133 (133-9 = 124)
          for (j in 1:124){means[j] = mean(temp1[j+9],na.rm=T)}
          # save the means with the dati of the last row
          meanDF[counter,] = c(temp1[ nrow(temp1), 1:9], means)
          # check to see if you got all the lines
          if(nrow(temp1)<600){go=FALSE}
          counter = counter+1
        } # end while the file still has lines
        # save the results and give their names back
        df1 = meanDF
        names(df1) = colNames
      } else { # else the choosen ADCP data file already had data every 10min
        df1 = read.csv(rfile, header=FALSE, skip=1, col.names=colNames)
      
      }
      print(paste("Read data from", filenames[i])) 
      close(rfile)
      df2 = df1
    
      # Columns are as follows
      # 1 ensemble number
      # 2-8 dati; GMT 
      # 9 blank; 10 pitch; 11 roll; 12 temp; 13 depth; 
      # 14-43 average echo amplitude, all bins
      # 44-73 velocity magnitude, all bins (mm/sec)
      # 74-103 current direction in 10ths of a degree, all bins
      # 104-133 percent good 4 
   
      # create dati from the separate columns
      temp1 = paste("200", df2$yr,"/", df2$mo,"/", df2$da, " ", df2$hh, 
        ":", df2$mm, ":00", sep="")
      # convert from chr to utime (GMT)
      datiVec1 = unclass(as.POSIXct(strptime(temp1, "%Y/%m/%d %H:%M:%S", tz="GMT"), 
        origin="1970-1-1", tz="GMT"))
      # remove the tz attribute from datiVec1
      attributes(datiVec1) = NULL
      # convert to datiG
      datiVec2 = as.POSIXlt(datiVec1, origin="1970-1-1", tz="GMT")  
      #convert to datiL                                
      datiVec3 = as.POSIXlt(datiVec1, origin="1970-1-1", tz="EST5EDT")      
    
      # See 'Notes on processing ADCP data.docx' for notes on the bin decisions.
      # Ignore bins 21:30.  
      # lower water layer is bins 1-4.  
      # mid water layer is bins 5-11
      # upper layer is 12-20 
      # ...that is, I want the row means over the columns containing data 
      #    for bins 1-4, 5-11, and 12-20
      eaaL = rowMeans(df2[,14:17], na.rm=TRUE)
      eaaM = rowMeans(df2[,18:24], na.rm=TRUE) 
      eaaU = rowMeans(df2[,25:33], na.rm=TRUE)
      
      magL = rowMeans(df2[,44:47], na.rm=TRUE)
      magM = rowMeans(df2[,48:54], na.rm=TRUE)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      magU = rowMeans(df2[,55:63], na.rm=TRUE)
      
      dirL = rowMeans(df2[,74:77], na.rm=TRUE)
      dirM = rowMeans(df2[,78:84], na.rm=TRUE) 
      dirU = rowMeans(df2[,85:93], na.rm=TRUE)
   
      # replace these dati columns and only keep wanted columns:
      df3 = cbind("utime"=datiVec1, "datiG"=datiVec2, "datiL"=datiVec3, 
        df2[,c(10:13)], 
        "eaaL" = eaaL, "eaaM" = eaaM, "eaaU"=eaaU, 
        "magL"=magL, "magM"=magM, "magU"=magU,
        "dirL"=dirL, "dirM"=dirM, "dirU"=dirU
      )
    
      # now remove the leading and trailing times when ADCP was out of water
      df4 = df3[(df3$utime > startUtime) & (df3$utime < stopUtime),]
      df5 = df4
      
      # there are some anomalous times when water depth seems wrong (i.e. less 
      # than 10m) ... 1. remove lines with depth < 10m and 2. find the depth 
      # change from one time to the next.  Delete any changes greater than 0.2m
      df5 = df5[df5$dep > 10 ,]
      
      nextTime = c(tail(df5$dep, -1), NA)
      depChange = abs(nextTime - df5$dep)
      # now to get the index right, move the NA from the tail to the head
      depChange = c(FALSE, head(depChange, -1))
      # now delete rows with depth changes which are too large, >0.2m
      df5 = df5[!(depChange > 0.2),]
      
      # now if the current file is from deployment "2009adcp02", then only 
      # use temperature and depth info.  The SDL apparently tipped over almost
      # immediately.
      if (deployment == "2009adcp02") {
        # delete all data but  temperature and depth
        df5$eaaL = NA; df5$eaaM = NA; df5$eaaU = NA;
        df5$magL = NA; df5$magM = NA; df5$magU = NA;
        df5$dirL = NA; df5$dirM = NA; df5$dirU = NA;
      }
      df6=df5
      adcpData[[i]] = df6    
    } # end i-loop over each of the ADCP files 

    # combine all ADCP data into one list
    adcpData1 = rbind(adcpData[[1]], adcpData[[2]], adcpData[[3]], adcpData[[4]], 
      adcpData[[5]], adcpData[[6]],adcpData[[7]])
      
    adcpData = adcpData1    
  } # end 'import' == FALSE
  
  ### return the oDir
  setwd(oDir)
                                  
  ### return the data
  return(adcpData)  
} ### end importADCPdta()
###############################################################################
###############################################################################
# adcpData = importADCPdata(recalculate=TRUE)
# write.csv(adcpData, "C:/zy/Telemetry/R summary files/adcpDataSummary 2011Jun22.csv", row.names=FALSE) 
# adcpData = importADCPdata(recalculate=FALSE)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### MERGE ALPS OUTPUT, ADCP DATA, AND TIDE DATA
##############################################################################
# This function imports ALPS output and ADCP output and merges them together
# The function name was changed from 'joinSdlAdcp()'
mergeAlpsAdcpData = function(alpsData, ...){
  # alpsData is a dataframe from importALPSdata and/or filterALPSdata
  # temp1 = importALPSdata(deployment="hb2007", tagName="f60200", beaconName="b79200", psr=FALSE, offset=TRUE, whichDays=c(11:14))
  # temp1 = importALPSdata(deployment="hb2", tagName="f32", beaconName="b1", psr=FALSE, offset=TRUE, whichDays=c(11:14))
  # alpsData = filterALPSdata(temp1, cnF=3)

  # adcpData is a dataframe from importADCPdata
  # adcpData = importADCPdata()
  
  print(paste("mergeAlpsAdcpData:",alpsData$deployment, "-", alpsData$tagName))
  
  d1 = alpsData$data
  tagType = substr(alpsData$tagName,1,1)     
  
  # what to do if there are/aren't lines of data in d1
  if (nrow(d1) > 0){ # do the normal thing... 
  
    d2 = importADCPdata(...)    
    # convert magL, magM, magU units to m/s from mm/s
    d2$magL=d2$magL/1000; d2$magM=d2$magM/1000; d2$magU=d2$magU/1000;
    
    tideData = importTideData()         
    deployment = alpsData$deployment
  
    # change things for each deployment
    for (i in 1:length(md)){ # i loops through all deployments
      if (deployment == md[[i]]$deployment){
        startUtime = md[[i]]$startUtime
        stopUtime = md[[i]]$stopUtime
      }  
    }

    # only keep ADCP data 30min before and after the SDL array start and stop utimes
    # in order to be sure I don't interpolate ADCP date between, say, Jan 2008
    # and Oct 2008.  However, if this is an 'other' tag, its position times
    # might lie outside the normal start and stop utimes, so be more lax
    # in deleting ADCP data
    if (tagType == "o") { # don't delete as much ADCP data
      d3 = d2
    } else { # do the normal thing
      d3 = d2[((d2$utime>startUtime-30*60) & (d2$utime<stopUtime+30*60)) ,] 
    }
    
    
    # merge doesn't work if datiG and datiL have $name attributes, so delete
    # these columns here and add them back in at the end
    d1short = subset(d1, select = -c(datiG, datiL))
    d3short = subset(d3, select = -c(datiG, datiL))       

    # now merge
    d4 = merge(d1short, d3short, by="utime", all=TRUE)     
    
    # remove any completely empty rows
    d4 = d4[rowSums(d4, na.rm=T) != 0,]
    
    # in one case, when the ADCP fell over I think, there is temp and depth 
    # data but no other ADCP data...so interpolation won't work.  Instead 
    # of adding an if() and writing two streams of code, I'll add 7777777
    # in place of the NAs, interpolate using the code as written, then at
    # the end I'll replace the 7777777s witn NAs
    #
    # check if d4 has ADCP data, do c(pit, rol, tem, dep) and c(eaaL-dirL) 
    # separately because once the ADCP fell over and has some data (not flow)
    # and once the ADCP just missed the entire deployment.
    if(sum(d4$eaaL, na.rm=TRUE) == 0){
      d4[,c("eaaL","eaaM","eaaU","magL","magM","magU","dirL","dirM","dirU")] = 7777777
    }
    if(sum(d4$pit, na.rm=TRUE) == 0){
      d4[,c("pit","rol","tem","dep")] = 7777777
    }
    
    
    # I want to interpolate ADCP data for each time a position is known, but if 
    # there are position times before the first ADCP data or after the last 
    # ADCP data, na.approx will simply not perform the calculation and 
    # the result will be a shorter list than d4$utime.  Then when I cbind
    # everything together later things won't match up at the correct time.
    # To fix this, 1. find out how many positions are known before the first ADCP
    # data, 2. find out how many positions are known after the last ADCP data.
    # 3. use na.approx to interpret ADCP data, resulting in a vector potentially
    # shorter than d4$utime, 4. to each of the interpolated ADCP data columns, 
    # add the right number of NAs at heads and tails.
   
    
    # count fish positions before the first ADCP data
    go = TRUE; headCount = 1;
    while (go){
      if (is.na(d4$dep[headCount])){ 
        headCount = headCount + 1
      } else {
        firstADCPutime = d4$utime[headCount]
        headNAs = headCount - 1
        go = FALSE 
      }
    } # end while-loop
  
    # count fish positions after last ADCP data
    go = TRUE; tailCount = 0;
    while (go){
      if ( is.na(d4$dep[nrow(d4)-tailCount]) ){
        tailCount = tailCount + 1
      } else {
        #lastADCPutime = d4$utime[tailCount] # I believe this should be d4$utime[nrow(d4)-tailCount] but this never gets used ???
        tailNAs = tailCount
        go = FALSE 
      }
    } # end while-loop
   
    # for a check, count some things 
    interpTimes = length(d4$utime) - headNAs - tailNAs
    
    # If there are at least two ADCP data lines then interpolate, otherwise ...
    
    
    # now interpolate ADCP data for all positions in interpTimes 
    tempTem = c(rep(NA,headNAs),na.approx(object=d4$tem, x=d4$utime),rep(NA,tailNAs))
    
    # AS OF 2011 JUNE 24 I'VE REALIZED THIS CHECK IS WRONG BUT LUCKILY IT WAS
    # STILL GIVING THE RIGHT ANSWER...
    
    # a check: length(tempTem+headNAs+tailNAs) should = length(d4$utime)
    #if (length(tempTem+headNAs+tailNAs) != length(d4$utime)){
    #  print("Failed check during interpolation in 'mergeAlpsAdcpData()'")}
    # REPLACE IT WITH THE CORRECT CHECK...
    if (length(tempTem) != length(d4$utime)){
      print("Failed check during interpolation in 'mergeAlpsAdcpData()'")} 
      
    tempDep = c(rep(NA,headNAs),na.approx(object=d4$dep, x=d4$utime),rep(NA,tailNAs))
    tempEaaL = c(rep(NA,headNAs),na.approx(object=d4$eaaL, x=d4$utime),rep(NA,tailNAs))
    tempEaaM = c(rep(NA,headNAs),na.approx(object=d4$eaaM, x=d4$utime),rep(NA,tailNAs))
    tempEaaU = c(rep(NA,headNAs),na.approx(object=d4$eaaU, x=d4$utime),rep(NA,tailNAs))
    tempMagL = c(rep(NA,headNAs),na.approx(object=d4$magL, x=d4$utime),rep(NA,tailNAs))
    tempMagM = c(rep(NA,headNAs),na.approx(object=d4$magM, x=d4$utime),rep(NA,tailNAs))
    tempMagU = c(rep(NA,headNAs),na.approx(object=d4$magU, x=d4$utime),rep(NA,tailNAs))
    tempDirL = c(rep(NA,headNAs),na.approx(object=d4$dirL, x=d4$utime),rep(NA,tailNAs))
    tempDirM = c(rep(NA,headNAs),na.approx(object=d4$dirM, x=d4$utime),rep(NA,tailNAs))
    tempDirU = c(rep(NA,headNAs),na.approx(object=d4$dirU, x=d4$utime),rep(NA,tailNAs))
  
    d5 = cbind(
      subset(d4, select = c(utime, dod, tod, hod, lunarIndex, easting, northing, 
        depth, dtr, btr, interval, speed, turnAngle, npos)), 
      "temperature"=tempTem, "waterDepth"=tempDep, 
      "eaaL"=tempEaaL, "eaaM"=tempEaaM, "eaaU"=tempEaaU, 
      "magL"=tempMagL, "magM"=tempMagM, "magU"=tempMagU, 
      "dirL"=tempDirL, "dirM"=tempDirM, "dirU"=tempDirU
    )
    
    # check to see if ADCP data is my 7777777 holder and replace them with NA
    if(mean(d5$eaaL, na.rm=TRUE) == 7777777){  
      d5[,c("eaaL","eaaM","eaaU","magL","magM","magU","dirL","dirM","dirU")] = NA
    }
    if(mean(d5$temperature, na.rm=TRUE) == 7777777){  
      d5[,c("temperature","waterDepth")] = NA
    }
    
   
    # now that interpolation of ADCP data is done, remove lines without fish
    # position data, only ADCP data
    d6 = d5[!is.na(d4$easting),]
    
    # what is the fish altitude above the seafloor (waterDepth - fish depth)
    
    d6$altitude = d6$waterDepth - d6$depth
    # its ugly but I want this in a different order
    d6a = cbind(subset(d6, select=c(utime, dod, tod, hod, lunarIndex, easting,
      northing, depth, altitude, dtr, btr, interval, speed, turnAngle, npos,
      temperature, waterDepth, eaaL, eaaM, eaaU, magL, magM, magU, dirL, dirM,
      dirU)))
    
  
    # merge tide data with ALPS/ADCP data...again don't bother with the datiG and datiL    
    d7 = merge(d6a, subset(tideData, select=c(utime, tidalHeight)), all=TRUE)              
    
    ## Tide Data comes every hour and I'm going to interpolate it to every 10 min
    ## Look to see how well this interpolation will be.  It looks great!
    #  ad = d7;  
    #  plot(ad$utime, ad$tidalHeight, pch=19, cex=0.5, xlim=c(ad$utime[1], ad$utime[50]))
    #  ?how long am I looking at  in this plot? (ad$utime[50]-ad$utime[1])/3600 = 49 hours

    # interpolate tidal heights at all times when there is ALPS data (every 10 min)
    d7$tidalHeight = na.approx(object=d7$tidalHeight, x=d7$utime)
    # now remove rows/utimes when there are no ALPS position data
    d8 = d7[!is.na(d7$easting),]
  
    # fill in missing datiG and datiL, you can't make it POSIXlt inside the 
    # data.frame call so do it after
    d9 = data.frame(
      utime = d8$utime,
      datiG = d8$utime, 
      datiL = d8$utime,
      subset(d8, select=-utime)
    )
    
    d9$datiG = as.POSIXlt(d9$datiG, origin="1970-1-1", tz="GMT")     
    d9$datiL = as.POSIXlt(d9$datiL, origin="1970-1-1", tz="EST5EDT")
  } else { # if there are no lines of data in d1, just add and delete the right
    # columns to/from $data

    d2 = data.frame(d1[1:length(d1)],
      "temperature"=numeric(0), "waterDepth"=numeric(0),
      "eaaL"=numeric(0), "eaaM"=numeric(0), "eaaU"=numeric(0), 
      "magL"=numeric(0), "magM"=numeric(0), "magU"=numeric(0), 
      "dirL"=numeric(0), "dirM"=numeric(0), "dirU"=numeric(0), 
      "tidalHeight"=numeric(0)
    )
    d9=d2
  }  
  
  # return answers
  list("data"=d9, "tagName"=alpsData$tagName, "beaconName"=alpsData$beaconName, 
    "psr"=alpsData$psr, "deployment"=alpsData$deployment)                      
} # end 'mergeAlpsAdcpData()'





##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
# This function takes a single (easting, northing) location and a 
# black and white categorical habitat map as *.jpg.  It determines whether 
# each (E,N) location is in the white/black part of the map.  It was originally
# worked out in 'chapter 3 part 2.r'.  There are 
# more details there about getting easting/northing correctly referenced to
# column/row.  
# 
# It is written to be used in conjunction with mergeSonarData() below. 
# 
# This function is used in various places, and as I have gone along, I changed
# this to work in one place, only to break it in another place. I've lost track
# of where and why it works. So I'm starting a list of all the locations it is
# used: 'chapter 3 prelin data zzzzzzzzz.r', 'chapter 4 reef map.r',
#       'habitat maps zzzzzzzzz.r'
#       used in 'mergeSonarData()'
# My most recent effort it to make it work in 'chapter 3 prelim data.r'



findHabType = function(
  e, n, # easting and northing location of fish, easting and northing offset applied 
  reference, # which point to use for relating easting/northing to row/column
    # IF41, IF42, IF43, OH41, OS43, OF43
    # OLD ...  "blue box", "if41", "if42", "other"-for when I use only a piece of full hab map
  # erange = NA, # OLD DON'T USE# easting range to use if 'reference' is 'other'.  i.e. =c(0,50)
  # nrange = NA, # OLD DON'T USE # northing range to use if 'reference' is 'other'
  habmap = NA, # categorical habitat map matrix, should be 3-dimensions, a full 3-D jpeg.
  crosshairs = FALSE, show = FALSE, # draw something on a plot
  pixels = FALSE, # include the pixel location in the output?
  dotCol="green", # what color do you want the dot to be
  dotShape = 19,
  dotSize = 1
)
{ 
  # a check if the point is off the image map
  outOfBounds=FALSE 
 
# THIS REFERENCING SYSTEM IS OLD AND ONLY WORKS FOR IF41_IF42.   
#  ## all 4 blue box corners give the same answer
#  if (reference == "blue box"){
#    emin=244982-eastingOffset; emax=245506-eastingOffset; # easting of the left/western most column
#    nmin=3262241-northingOffset; nmax=3262556-northingOffset; # northing of the top/northern most row
#  } else if (reference == "if41"){
#    emin=244994-eastingOffset; emax=245518-eastingOffset; 
#    nmin=3262235-northingOffset; nmax=3262550-northingOffset; 
#  } else if (reference == "if42"){
#    emin=244984-eastingOffset; emax=245508-eastingOffset; 
#    nmin=3262237-northingOffset; nmax=3262552-northingOffset; 
#  } else if(reference == "other"){
#    emin=erange[1]-eastingOffset; emax=erange[2]-eastingOffset;
#    nmin=nrange[1]-northingOffset; nmax=nrange[2]-northingOffset;
#  } else print("Please specify a reference: blue box, if41, if42.")

  # pick the correct deployment for the given habitat map...
  # ...there are four maps, two of them have two reefs on them
  # 
  # These are the choice of habitat maps, make sure you've choosen a reef actually
  # ... on this habmap
  # IF41_IF42_lines_aligned_HBandSB_bluebox.jpg
  # IF43_lines_aligned_HB_SB_bluebox.jpg
  # OH41_OS43_lines_SBonly_bluebox.jpg
  # OF43_lines_SBonly_bluebox.jpg
  
  # get the somar image to use
  if(is.na(habmap)){ #otherwise use the habmap passed to findHabType()
    library(rimage)
    if((reference == "if41") | (reference == "if42")){
      rfile = paste("C:/zy/school closet/Telemetry/R summary files/",
        "IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep="")
    } else if(reference == "if43"){
      rfile = paste("C:/zy/school closet/Telemetry/R summary files/",
        "IF43_lines_aligned_HB_SB_bluebox.jpg",sep="")
    } else if((reference == "oh41") | (reference == "os43")){
      rfile = paste("C:/zy/school closet/Telemetry/R summary files/",
        "OH41_OS43_lines_SBonly_bluebox.jpg",sep="")   
    } else if(reference == "of43"){
      rfile = paste("C:/zy/school closet/Telemetry/R summary files/",
        "OF43_lines_SBonly_bluebox.jpg",sep="")
    } else {print("You're not using a standard reference and habitat map.")
    }
    
    # now read in the correct habitat map.  This should be a 3-D jpeg
    habmap = round(read.jpeg(rfile))    
    print("Reading in habitat map")                
  } 

  # what are the dimensions of habmap  
  ne = dim(habmap)[2] # number of columns
  nn = dim(habmap)[1] # number of rows

  # we need to calculate the easting and northing edges of the image, see
  #   'habitat map procedures.xlsx' and 'defining IF41 IF42 imagery.xlsx' 
  #   for more on this.
  #   ...but in short, get the reefEN and make sure it's not offset, count the 
  #   ...number of pixels from it to the edges, and divide by 10, add/subtract
  #   ...that number of meters.
  #   First pick the right md[[i]] for each reef so you can get the reefEN
  #   gimpCol and gimpRow are the (column, row) location of the reef in GIMP
  
  if       (reference == "if41") { dn = 1; gimpCol = 1805; gimpRow = 1590; 
  } else if(reference == "if42") { dn = 8; gimpCol = 3345; gimpRow = 1455; 
  } else if(reference == "if43") { dn = 3; gimpCol = 2515; gimpRow = 1387; 
  } else if(reference == "oh41") { dn = 5; gimpCol = 3232; gimpRow = 2288;
  } else if(reference == "os43") { dn = 7; gimpCol = 2413; gimpRow = 1472;
  } else if(reference == "of43") { dn = 4; gimpCol = 1966; gimpRow = 1473;
  } else {print("You're not using a standard reference and habitat map.")
  }
  reefEasting = md[[dn]]$reefEN$easting + eastingOffset
  reefNorthing = md[[dn]]$reefEN$northing + northingOffset
  # now calculate UTM at image edges 
  #   ...the ne-1 is because GIMP starts at 0 and R starts at 1
  emin = (reefEasting - (gimpCol-0)/10) - eastingOffset
  emax = (reefEasting + (ne-1 - gimpCol)/10) - eastingOffset
  nmin = (reefNorthing - (nn-1 - gimpRow)/10) - northingOffset
  nmax = (reefNorthing + (gimpRow-0)/10) - northingOffset
  
  # checks
  if ((e < emin) | (e > emax)){print("e is out of bounds"); outOfBounds=TRUE}
  if ((n < nmin) | (n > nmax)){print("n is out of bounds"); outOfBounds=TRUE}
  
  if(!outOfBounds){ 
    # given the fish is at (e,n) what pixel location is it at
    eanswer = floor(( ((e-emin)/(emax-emin)) * ne))-1
    nanswer = floor(( ((n-nmin)/(nmax-nmin)) * nn))-1 
    # the term '(e-emin)/(emax-emin)' produces a fraction of how far across the 
    # easting direction the fish is.  Multiply that by the number of columns, ne,
    # and you get which column is occupied.  This needs careful checking to see if
    # I should use round/floor/ceiling.  Note that GIMP row/column counts start 
    # at 0, while R starts them at 1.
    #
    # If the fish position is ever outside the range of the image, this code will
    # need more work.
  
    # determine the habitat type.  Apparently for plotting, (0,0) is the bottom
    # left of the figure, but for referencing the underlying array(jpg) (0,0)
    # is the top left, so when picking the habitat type, count from the top down
    # but for drawing dots, count from the bottom up.
  
    if(habmap[nn-nanswer,eanswer,2] == 1){habType = "white"
    } else if (habmap[nn-nanswer,eanswer,2] == 0){habType="black"
    } else {habType="unknown"} 
  
    if(show){
      # this only works if is the map is already showing
      #if (habType=="white"){dotCol="blue"} else if (habType=="black"){dotCol="red"
      #  } else {dotCol="yellow"} 
      
      if(crosshairs){abline(h=nanswer, v=eanswer,col="green",lwd=1)}
      points(eanswer, nanswer, col=dotCol, pch=dotShape, cex=dotSize)
      
    }
    
  } else { # it is outOfBounds
    habType = NA
    outOfBounds = FALSE
  } # end if(outOfBounds)
  
  # given what pixel location the fish is at, what habitat type is it occupying
  # 1 = white = sand, 0 = black = HB
  
  if (pixels){return(list("pixelEN"=c(eanswer, nanswer), "habType" = habType)) 
  } else {return(habType)}
} # end findHabType


#### some testing code
#library(rimage)
#rfile = paste("C:/zy/The closets/school closet/Telemetry/R summary files/",
#  "IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep="")
#i1 = round(read.jpeg(rfile))
#par(mar=c(0.2,0.2,0.2,0.2))
#plot(i1)
#box("plot", col="red") 
### IF41 is at (245174.8 E, 3262391 N)
#findHabType(e=245174.8-eastingOffset, n=3262391-northingOffset,  
#  habmap=i1, reference="if41", dotCol="green", show=TRUE, crosshairs=TRUE) 
#
#i=i+100                                                               
#findHabType(e=tagfm[[4]]$data$easting[i], n=tagfm[[4]]$data$northing[i], 
#    habmap=i1, reference="if41", dotCol="green", show=TRUE, crosshairs=FALSE)
#
#findHabType(e=245250-eastingOffset, n=3262500-northingOffset,  
#  habmap=i1, reference="if41", dotCol="blue", show=TRUE, crosshairs=FALSE) 
#                                             
#cFish = 4  
#i=i+100                
#findHabType(e=tagfm9[[cFish]]$data$easting[i], n=tagfm9[[cFish]]$data$northing[i], 
#    habmap=i1, reference=cmd$site, dotCol="red", show=TRUE, crosshairs=FALSE)
#
#library(rimage)
#rfile = "C:/zy/Telemetry/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg"
#rfile = "C:/zy/Telemetry/R summary files/IF43_lines_aligned_HB_SB_bluebox.jpg"
#rfile = "C:/zy/Telemetry/R summary files/OH41_OS43_lines_SBonly_bluebox.jpg"   
#rfile = "C:/zy/Telemetry/R summary files/OF43_lines_SBonly_bluebox.jpg"
#
#i1 = round(read.jpeg(rfile))
#plot.imagematrix(i1, useRaster=T)
#
## if41 ---> e=245174.8-eastingOffset, n=3262391-northingOffset,
## if42 ---> e=245318.1-eastingOffset, n=3262407-northingOffset,
## if43 ---> e=245478-eastingOffset, n=3262128-northingOffset,
## oh41 ---> e=237034.9-eastingOffset, n=3263760-northingOffset,
## os43 ---> e=236946.8-eastingOffset, n=3263839-northingOffset,
## of43 ---> e=237897.1-eastingOffset, n=3263128-northingOffset,
#
#findHabType( e=237897.1-eastingOffset, n=3263128-northingOffset, habmap=i1,
#  reference="of43", show=TRUE, crosshairs=TRUE) 
#findHabType(e=245350-eastingOffset, n=3262100-northingOffset,  
#  reference="if43", show=TRUE, crosshairs=FALSE) 





##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
# This function takes a data.frame with easting, northing columns and 
# applies findHabType() to each one.  There's probably a faster vectorized
# way to do this.  
#
# So far this has only been tested with one image...
# rfile = "C:/zy/Telemetry/R summary files/IF41_IF42_lines_aligned_HBandSB_2.jpg"


mergeSonarData = function(
  alpsData, # typically this is one of tagfm  
  habmap, # categorical habitat map matrix, should be 3-dimensions, a full 3-D jpeg.
  reference # which point to use for relating easting/northing to row/column
    #  "blue box", "if41", "if42"...
)
{
  d1 = alpsData$data
  d1$habType = NA
  
  for (i in 1:nrow(d1)){                                      
    d1$habType[i] = 
      findHabType(e=d1$easting[i], n=d1$northing[i], 
        habmap=habmap, reference=reference)
  }
  
  # return answers
  list("data"=d1, "tagName"=alpsData$tagName, "beaconName"=alpsData$beaconName, 
    "psr"=alpsData$psr, "deployment"=alpsData$deployment)  
} # end mergeSonarData




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
### in this function, supply the filename, and the start and stop times marking
### the GPS locations to extract
importGPSdata = function(
  fn,  # filename of Garmin output file
  startTime, # start time in local time of GPS buoy at good position
  stopTime, # 
  filter = 100, # for removing clear errors
  offset = TRUE, # use the global offset or not?
  plotThem = TRUE # show the plot or not
){ 
  
  # fn="42-E GPS track.txt"; day=23; startTime="12:33:44"; stopTime="14:26:00"; filter=100;  # 125m
  # fn="44-W GPS track.txt"; startTime="12:42:30"; stopTime="14:32:00"; filter=10;  # 125m
  # fn="41-N GPS track.txt"; day=7; startTime="10:29:35"; stopTime="13:23:00"; # 150m 
  # fn="43-S GPS track.txt"; day=7; startTime="11:07:00"; stopTime="13:56:00"; # 150m
   
   
   
  ### names of columns to be read in
  columnNames = c("type", "ident", "lat", "long", "y_proj", "x_proj", "new_seq", 
    "display", "color", "altitude", "depth", "temp", "time", "model", 
    "filename", "ltime")
  columnClasses =  c('character','character','numeric','numeric','numeric',
    'numeric','character','character','integer','numeric','integer','integer',
    'character','character','character','character')   

  # read in data
  temp1 = read.table(fn, header=TRUE, sep=",", col.names=columnNames, 
    colClasses = columnClasses  
  )
  
  # extract lat/long and convert to UTM, UTM increases northward and eastward
  temp2 = data.frame(X = temp1$lon, Y = temp1$lat)
  attr(temp2, "zone") <- 17
  attr(temp2, "projection") <- "LL"   
  temp3 = convUL(temp2,km=FALSE)  #X is easting in m, Y is northing in m

  # add date and time, to offset or not to offset?
  if (offset) {
    temp4 = data.frame(
      dati = strptime(temp1$ltime, "%Y/%m/%d %H:%M:%S"),
      northing = temp3$Y-northingOffset, easting = temp3$X-eastingOffset)
  } else {
    temp4 = data.frame(
      dati = strptime(temp1$ltime, "%Y/%m/%d %H:%M:%S"),
      northing = temp3$Y, easting = temp3$X)  
  }
  
  # filter for only the desired points
  # for the 125m spacing trial the date is 23 April 2009, so only take points from that day
  # for the 150m and 100m trials, the date is 8 May 2009, so...
  temp5 = temp4
  # take only points during the choosen period, (startTime, stopTime)
  temp6 = chron(times=format(temp5$dati, format="%H:%M:%S"))  
  temp7 = temp5[((temp6 > startTime) & (temp6 < stopTime)),]
  
  # filter out any obvious outliers, this might have to be done carefully each time
  temp8 = temp7[abs(temp7$easting-mean(temp7$easting))<filter,]
  # or don't filter temp8=temp7
  
  # now find the average position and ranges
  average = data.frame(northing=mean(temp8$northing), easting=mean(temp8$easting))
  ranges = data.frame(northing=max(temp8$northing)-min(temp8$northing), 
    easting = max(temp8$easting)-min(temp8$easting))
 
  # plot them
  if (plotThem){
    plot(temp8$easting, temp8$northing, type="b",  #pch=19,
      main=fn
    )               
    points(average$easting,average$northing, col="red", pch=19 ,cex=2)    
  }
  
  return(list("average"=average, "ranges"=ranges, "data"=temp8))
  # mean = northing UTM, easting UTM.  UTM increases to N and E
  # ranges =  
} # end importGPSdata
# setwd("C:/zy/telemetry/2009/2009Aug24 IF41/GPS data")
#bob=importGPSdata(fn="2009Aug24 GPS data.txt", startTime = "11:25:00", stopTime="13:48:00", filter=99999, offset=TRUE, plotThem=TRUE)
#bob=importGPSdata... fn="2009Oct12 GPS data.txt"; startTime = "10:39:45"; stopTime="10:53:00"; filter=100; offset=FALSE; plotThem=TRUE;


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### IMPORT tide Data
##############################################################################
importTideData = function(){
  ### record current directory and change to desired directory
  oDir = getwd()
  cDir = paste(dataDir, 
    "/Environmental data 2011Apr-Final/Tide data/", sep="")
  setwd(cDir)
  # get names for all the files
  filenames = list.files(pattern="tide")

  # read in all the data files
  df1 = list()
  for (i in 1:length(filenames)){ # for each of the files in the folder...
    rfile = file(filenames[i], open="rt") # open the connection
    df1[[i]] = read.table(rfile, skip=13, fill=TRUE, col.names=c("station", "date", 
      "time", "predicted", "tidalHeight"))
    print(paste("Read data from", filenames[i])) 
    close(rfile)  
  }
  # combine all lists into one df
  df2 = rbind(df1[[1]], df1[[2]], df1[[3]])  
  
  # compare predicted with actual height
  #plot(tideData$predicted, tideData$height, pch=19, cex=0.5) 
   
  # create dati from the separate columns, the times are already GMT
  temp1 = paste(df2$date, df2$time, sep=" ")
  # convert from chr to utime (GMT)
  datiVec1 = unclass(as.POSIXct(strptime(temp1, "%Y%m%d %H:%M", tz="GMT"), 
    origin="1970-1-1", tz="GMT"))
  # remove the tz attribute from datiVec1
  attributes(datiVec1) = NULL
  # convert to datiG
  datiVec2 = as.POSIXlt(datiVec1, origin="1970-1-1", tz="GMT")  
  #convert to datiL                                
  datiVec3 = as.POSIXlt(datiVec1, origin="1970-1-1", tz="EST5EDT")
  
  # replace missing 'height' with 'predicted'  
  df3 = df2
  df3[is.na(df3$tidalHeight),]$tidalHeight = df3[is.na(df3$tidalHeight),]$predicted
  
  # replace these dati columns and only keep wanted columns:
  df4 = data.frame("utime"=datiVec1, "datiG"=datiVec2, "datiL"=datiVec3,
     "tidalHeight"=df3[,5])
  
  # set the directory back
  setwd(oDir)
  
  return(df4) 
} # end importTideData
# tideData = importTideData()




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### IMPORT sun/moon rise/set Data
##############################################################################

importSunData = function(){
  ### record current directory and change to desired directory
  oDir = getwd()
  cDir = paste(dataDir, 
    "/DATA/Environmental data 2010Aug-Final/sun moon rise set", sep="")
  setwd(cDir)
  # get names for all the sun files
  filenames = list.files(pattern="sun")

  # read in all the data files      Times are all EST, no EDT
  df1 = list()
  for (i in 1:length(filenames)){ # for each of the files in the folder...
    rfile = file(filenames[i], open="rt") # open the connection
    # construct headers
    headers = c("day", "jan.r", "jan.s", "feb.r", "feb.s", "mar.r", "mar.s", 
      "apr.r", "apr.s", "may.r", "may.s", "jun.r", "jun.s", "jul.r", "jul.s", 
      "aug.r", "aug.s", "sep.r", "sep.s", "oct.r", "oct.s", "nov.r", "nov.s", 
      "dec.r", "dec.s") 
    # read table
    df1[[i]] = read.fwf(rfile, skip=9, fill=TRUE, col.names=headers, n=31,
      widths=c(4,  rep(c(5,6),12)))
    print(paste("Read data from", filenames[i]))   
    close(rfile)
    
    # re-arrange, there are 31 days in all months, some have NA
    temp1 = df1[[i]][,1] # the day
    bymonth = list()
    for (j in 1:12){
      bymonth[[j]] = cbind("month"=j, "day"=temp1, "r"=df1[[i]][,j*2], 
        "s"=df1[[i]][,j*2+1])
    }
    # combine into one list
    rs1 = bymonth[[1]]
    for (j in 2:12){rs1 = rbind(rs1, bymonth[[j]])}
    # remove non-existant days
    rs2 = rs1[!is.na(rs1[,3]),]
    # generate the date
    # aarg, make the months and days 2 digits
    for (j in 1:nrow(rs2)){
      if(nchar(rs2[,1][j]) == 1){rs2[,1][j]=paste("0",rs2[,1][j], sep="")}
      if(nchar(rs2[,2][j]) == 1){rs2[,2][j]=paste("0",rs2[,2][j], sep="")}
      if(nchar(rs2[,3][j]) == 3){rs2[,3][j]=paste("0",rs2[,3][j], sep="")}
      if(nchar(rs2[,4][j]) == 3){rs2[,4][j]=paste("0",rs2[,4][j], sep="")}  
    }
    # # create dati, the times are always EST, no EDT. aarg again
    year = substr(filenames[i],1,4)
    rise1 = paste(year, "/", rs2[,1], "/", rs2[,2], " ", rs2[,3], sep="")
    set1 = paste(year, "/", rs2[,1], "/", rs2[,2], " ", rs2[,4], sep="")
     
    # convert from chr to utime (GMT)
    datiVec1r = unclass(as.POSIXct(strptime(rise1, "%Y/%m/%d %H%M", tz="EST"), 
      origin="1970-1-1", tz="EST"))
    datiVec1s = unclass(as.POSIXct(strptime(set1, "%Y/%m/%d %H%M", tz="EST"), 
      origin="1970-1-1", tz="EST"))
    # remove the tz attribute from datiVec1
    attributes(datiVec1r) = NULL
    attributes(datiVec1s) = NULL
    # convert to datiG
    datiVec2r = as.POSIXlt(datiVec1r, origin="1970-1-1", tz="GMT")  
    datiVec2s = as.POSIXlt(datiVec1s, origin="1970-1-1", tz="GMT")  
    #convert to datiL                                
    datiVec3r = as.POSIXlt(datiVec1r, origin="1970-1-1", tz="EST5EDT")
    datiVec3s = as.POSIXlt(datiVec1s, origin="1970-1-1", tz="EST5EDT")
    
    # a check
    #datiVec3r[1]; as.POSIXct(datiVec3r[1]); unclass(as.POSIXct(datiVec3r[1]));
    #datiVec1r[1];
    
    # make new df with desired data
    #rs3 = data.frame("dateL"=format(datiVec3r,format="%Y/%m/%d"), 
    #  "rUtime"=datiVec1r, "rDatiG"=datiVec2r, "rDatiL"=datiVec3r,
    #  "sUtime"=datiVec1s, "sDatiG"=datiVec2s, "sDatiL"=datiVec3s)    
    # I'd rather have it like this...
    rs3 = data.frame("dateL"=format(datiVec3r,format="%Y/%m/%d"), 
      "rUtime"=datiVec1r, 
      "rGtime"=format(datiVec2r, format="%H:%M"), # GMT hr:min
      "rLtime"=format(datiVec3r, format="%H:%M"), # EST5EDT hr:min
      "sUtime"=datiVec1s, 
      "sGtime"=format(datiVec2s, format="%H:%M"), # GMT hr:min
      "sLtime"=format(datiVec3s, format="%H:%M")  # EST5EDT hr:min
    )    
    
    # save the result
    df1[[i]] = rs3  
    
  } # end i-loop over all files
  
  # combine all lists into one df
  df2 = rbind(df1[[1]], df1[[2]], df1[[3]])  
  
  
  return(df2) 
} # end importSunData
# sunData = importSunData()








##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
################################################################################# 
# This function imports the text files produced by WHSReader when converting
# individual SDL *.bin files to *.txt files
# These are huge files.  Read else where for a description of their format.
importRawSDLdata = function(){}




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
################################################################################# 
# This function returns a list of 8 items.  1-5 are data.frames for each of
# the SDLs containing the toa data for that SDL.  
# 6 is the tag number, 7 indicates the deployment name, 8 is a tag/beacon/sentinel 
# switch.
importToaData = function(tagName, deployment){    
  # 'tagName' is the tag or beacon number with a leading letter, b1, t13, s79600
  # 'deployment' is the experiment designation, i.e. IF43 
  
  tagID = substr(tagName,2,100)
  tagType = substr(tagName,1,1)
  
  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      tempDir = md[[i]]$homeDir
      startUtime = md[[i]]$startUtime
      stopUtime = md[[i]]$stopUtime
    }  
  }
  setwd(paste(tempDir,"/ALPS 2011Feb14",sep=""))
  cDir = getwd()

  # create a pattern for all *.toa files for this tag
  namePattern = paste("TxId",tagID,".toa",sep="")
  # gather all the file names for the tag  
  fileNames = list.files(pattern = namePattern, recursive=TRUE, ignore.case=TRUE)
  # read in data from each file...
  d1 = lapply( as.list (fileNames), read.table, header=FALSE)
  # ... and row bind them together 
  d2 = do.call("rbind", d1)    # this isn't necessarily in chronological order
  toaData = d2
  
  # close all connections
  closeAllConnections()
    
  # divide into data.frames for each SDL
  d41 = data.frame("utime"=toaData[,1],"fraction"=toaData[,2],"power"=toaData[,3],
    "sType"=toaData[,4],"sValue"=toaData[,5])   
  d42 = data.frame("utime"=toaData[,6],"fraction"=toaData[,7],"power"=toaData[,8],
    "sType"=toaData[,9],"sValue"=toaData[,10])   
  d43 = data.frame("utime"=toaData[,11],"fraction"=toaData[,12],"power"=toaData[,13],
    "sType"=toaData[,14],"sValue"=toaData[,15])   
  d44 = data.frame("utime"=toaData[,16],"fraction"=toaData[,17],"power"=toaData[,18],
    "sType"=toaData[,19],"sValue"=toaData[,20])   
  d45 = data.frame("utime"=toaData[,21],"fraction"=toaData[,22],"power"=toaData[,23],
    "sType"=toaData[,24],"sValue"=toaData[,25])
                             
  # get rid of empty rows
  d41 = d41[d41$utime>0,]    
  d42 = d42[d42$utime>0,]
  d43 = d43[d43$utime>0,]   
  d44 = d44[d44$utime>0,]
  d45 = d45[d45$utime>0,] 
  
  # get rid of points before the time we finished deploying the array, startUtime
  d41 = d41[d41$utime>startUtime,] 
  d42 = d42[d42$utime>startUtime,] 
  d43 = d43[d43$utime>startUtime,] 
  d44 = d44[d44$utime>startUtime,] 
  d45 = d45[d45$utime>startUtime,] 
  
  # get rid of points after the time we began recovering the array, stopUtime
  d41 = d41[d41$utime<stopUtime,] 
  d42 = d42[d42$utime<stopUtime,] 
  d43 = d43[d43$utime<stopUtime,] 
  d44 = d44[d44$utime<stopUtime,] 
  d45 = d45[d45$utime<stopUtime,] 
   
  # return the toa data for 'tag' for each SDL
  list("sdl41"=d41, "sdl42"=d42, "sdl43"=d43, "sdl44"=d44, "sdl45"=d45,
    "tagName"=tagName, "deployment"=deployment)    
} # end importToaData()
# b1toaData = importToaData(tagName="b1", deployment="IF43")
# b2toaData = importToaData(tagName="b2", deployment="IF43")
# b79400toaData = importToaData(tagName="b79400", deployment="IF43")
# b79500toaData = importToaData(tagName="b79500", deployment="IF43")
# t79600toaData = importToaData(tagName="s79600", deployment="IF43")
# t11toaData = importToaData(tagName="t11", deployment="IF43")

# bob = importToaData(tagName="b1", deployment="hb41")



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# calculate toa stats, percent of receptions for 'tagID' at each SDL
#
# This function takes the results of 'importToaData()' and calculates the
# fraction of receptions of a particular tag at all 5 SDLs every 60 min.
#
# It produces a picture and returns a list of 8 items. 1-5 are the fraction
# of receptions at each SDL over time.  6 is the tagID, 7 is the deployment, 
# 8 is a switch for beacon or tag.
toaStats1 = function(toaData, mLines=TRUE){
  # 'toaData' is the output list from 'importToaData()' #toaData=b1toaData
  # toaData = b1toaData
  # 'mLines' a switch for drawing mean lines
  
  tagID = substr(toaData$tagName,2,100)
  tagType = substr(toaData$tagName,1,1)
  
  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (toaData$deployment == md[[i]]$deployment){
      startUtime = md[[i]]$startUtime
      stopUtime = md[[i]]$stopUtime
    }  
  }
  
  # some 'time' book keeping
  totalSec = stopUtime - startUtime
  thirtyMinBins = seq(from=startUtime, to=stopUtime, by=1800) # 30min * 60sec
  sixtyMinBins  = seq(from=startUtime, to=stopUtime, by=3600)
  bins = sixtyMinBins; mins=60; # if you choose a different bin size, fix the sentinel tpi in the lines below
  bins2 = as.POSIXct(bins, origin="1970-1-1",tz="GMT")  
  
  if (tagType == "f"){
    tpm = 30 # transmissions per minute for a fish tag
    tpi = tpm * mins # transmission per bin
  } else if (tagType == "b"){
    tpm = 3 # transmissions per minute for a beacon
    tpi = tpm * mins # transmission per bin
  } else if (tagType == "s"){
    tpm = 30 # transmissions per minute for the sentinel
    tpi = tpm * 5 * 2 # transmissions ber bin of 60 minutes
  } else if (tagType == "c"){ # this is for using codes not symbols in hb2008
    tpm = 30 # transmissions per minute for the sentinel
    tpi = tpm * 5 * 2 # transmissions ber bin of 60 minutes
  } else { 
    print("Please pick a tag type")
  }
  
  # count receptions per time intervals for 'tag' and each 'sdl'
  freqList = list() 
  for (i in 1:5){ # i counts SDLs
    temp1 = data.frame("bin"=bins2[1], "frequency"=0) # for storing frequencies in each bin 
    for (j in 2:length(bins2)){ # j counts time bins
      temp2 = toaData[[i]][ # keep only rows within bin[j] 
        ((toaData[[i]]$utime>bins[j-1]) & (toaData[[i]]$utime<bins[j])),
      ]
      temp1 = rbind(temp1, 
        data.frame("bin"=bins2[j-1], "frequency"=nrow(temp2)/tpi))  
    } # end j loop
    freqList[[i]] = temp1
  } # end i loop
  
  # calculate the average of all 5 SDLs over time
  freqListMean = list()
  for (i in 1:nrow(freqList[[1]])){
    freqListMean[i] = mean(c(freqList[[1]]$frequency[i], 
    freqList[[2]]$frequency[i], freqList[[3]]$frequency[i], 
    freqList[[4]]$frequency[i], freqList[[5]]$frequency[i]))
  }
  
  # plot the results
  plotColors = c("brown", "red", "blue", "green", "yellow")
  plot(freqList[[1]]$bin, freqList[[1]]$frequency, type="b", cex=1, 
    xlim=c(startUtime, stopUtime), ylim=c(0, 1.1), 
    main=paste("Tag", tagID, "Detection Frequencies", sep=" "),
    sub=paste("Bin size:", mins, "min", sep=" "),
    xlab="Time", ylab="Frequency", col=plotColors[1]
  )
  ## to compare bin sizes
  # points(freqList[[1]]$bin, freqList[[1]]$frequency, type="l", col="red")
  
  points(freqList[[2]]$bin, freqList[[2]]$frequency, type="b", col=plotColors[2])
  points(freqList[[3]]$bin, freqList[[3]]$frequency, type="b", col=plotColors[3])
  points(freqList[[4]]$bin, freqList[[4]]$frequency, type="b", col=plotColors[4])
  points(freqList[[5]]$bin, freqList[[5]]$frequency, type="b", col=plotColors[5])
  lines(freqList[[1]]$bin, freqListMean, type="l", col="black", lwd=2)
  
  # show the means
  if (mLines){  
    abline(h=mean(freqList[[1]]$frequency), col=plotColors[1]) 
    abline(h=mean(freqList[[2]]$frequency), col=plotColors[2]) 
    abline(h=mean(freqList[[3]]$frequency), col=plotColors[3]) 
    abline(h=mean(freqList[[4]]$frequency), col=plotColors[4]) 
    abline(h=mean(freqList[[5]]$frequency), col=plotColors[5]) 
  }
  
  # return the detection frequencies for 'tag' for each SDL
  list("sdl41"=freqList[[1]], "sdl42"=freqList[[2]], "sdl43"=freqList[[3]],
    "sdl44"=freqList[[4]], "sdl45"=freqList[[5]], "tagName"=toaData$tagName,
    "deployment"=toaData$deployment)
} # end of toaStats1

# b1toaStats1 = toaStats1(b1toaData)
# b2toaStats = toaStats1(b2toaData)
# b79400toaStats = toaStats1(b79400toaData)
# b79500toaStats = toaStats1(b79500toaData)
# 79600toaStats = toaStats1(s79600toaData)
# t11toaStats = toaStats1(t11toaData)

# sam = toaStats1(bob)



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
### this code imports SDL battery data from all five SDLs at once
# These files have 6 header lines, then 4 columns: 
# date, time, power battery, coin battery
importBatteryData = function(deployment){
  oDir = getwd() # original directory
  
  # fetch deployment specific informaiton
  for (i in 1:length(md)){
    if(deployment == md[[i]]$deployment){
      # set the directory     
      cDir = paste(md[[i]]$homeDir,"/SDL data",sep="") # current directory
      year = md[[i]]$year
      # set the file name
      fileNameTemp = paste("_",md[[i]]$sdlDownloadDate,"_bat.txt",sep="")
  } } # end the 'for' and 'if' loop
  
  
  setwd(cDir)
                                            
  # names of columns in battery log files
  columnNames = c("date","time","powerBat","coinBat")
  
  # read in data from all five SDLs
  batLog1=list(); batLog2=list();
  sdlNames = 41:45
  for (i in 1:5){ # 5 SDLs
    fileName = paste("SN2650",sdlNames[i],fileNameTemp,sep="")
    batLog1[[i]] = read.table(fileName, skip=7, col.names=columnNames,
      colClasses = c('character', 'character', 'numeric', 'numeric')   
    )
    # convert to unix time
    temp1 = paste(batLog1[[i]]$date, batLog1[[i]]$time)
    batLog2[[i]] = list(
      dati = strptime(temp1, "%m/%d/%y %H:%M:%S", tz="GMT"),
      utime = unclass(as.numeric((strptime(temp1, "%m/%d/%y %H:%M:%S", tz="GMT")))),
      powerBat = batLog1[[i]]$powerBat,
      coinBat = batLog1[[i]]$coinBat  
    )  
  } # end 'for' loop over 5 SDLs
  
  # some plotting things
  minV = min(batLog2[[1]]$powerBat, batLog2[[2]]$powerBat, batLog2[[3]]$powerBat,
    batLog2[[4]]$powerBat, batLog2[[5]]$powerBat)
  maxV = max(batLog2[[1]]$powerBat, batLog2[[2]]$powerBat, batLog2[[3]]$powerBat,
    batLog2[[4]]$powerBat, batLog2[[5]]$powerBat)
  minTime = min(batLog2[[1]]$utime, batLog2[[2]]$utime, batLog2[[3]]$utime,
    batLog2[[4]]$utime, batLog2[[5]]$utime)
  maxTime = max(batLog2[[1]]$utime, batLog2[[2]]$utime, batLog2[[3]]$utime,
    batLog2[[4]]$utime, batLog2[[5]]$utime)

  # draw the plot
  plot(x=batLog2[[1]]$dati, y=batLog2[[1]]$powerBat, 
    type="l", col=plotColors[1],
    xlab="Date (GMT)", ylab="Voltage (V)",
    xlim=c(minTime, maxTime), ylim=c(minV-0.1, maxV+0.1),
    main=paste(year,deployment,"SDL Battery Voltages", sep=" ") 
  )  
  for (i in 2:5){
    points(x=batLog2[[i]]$dati, y=batLog2[[i]]$powerBat, type="l", 
    col=plotColors[i])
  }
  leg.txt = c("SDL41", "SDL42", "SDL43", "SDL44", "SDL45")
  legend("topright", leg.txt, text.col=plotColors)

  # reset the directory
  setwd(oDir)
} # end importBatteryData()
# importBatteryData(deployment="oh41", year=2009)




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
### Read in all the biometric data about the fish.  This data was collected
### during tagging, during collections, from otoliths, and Deb's otolith 
### results.
       
importBiometricData = function(){
 
  # locate the right file 
  #fileName = file.choose()
  #fileName = "C:/zy/school closet/Telemetry/R summary files/all fish tagging data 2011June08.csv"
  fileName = paste("C://ZyC//R Data Processing for gag NOT WORKING COPY//",
    "R summary files//all fish tagging data 2013Jul01.csv",sep="")
   
  # column names
  columnNames = c("year1", "month1", "day1", "reefID1", "HBSB", "replicate", 
    "collectionMethod", "weight1", "girth1", "TL1", "FL1", "sizeRange", "tagged",
    "tagID", "knockoutStart", "taggingStart", "recoveryStart", "release", 
    "leftColor", "rightColor", "notes", "mmNumber", "recoveredTagID",
    "year2", "month2", "day2", "reefID2", "depth", "gear", "TL2", "FL2",
    "weight2", "girth2", 
    
    "lOto", "lOtoWeightRecorded", "lOtoWeightUseable",
    "lOtoLengthA", "lOtoLengthACalip", "lOtoLengthAUseable", 
    "lOtoLengthB1", "lOtoLengthB2", "lOtoLengthBCalip", "lOtoLengthBUseable",    
    "lOtoLengthC1", "lOtoLengthC2", "lOtoLengthC3", "lOtoLengthC4", 
    "lOtoLengthCCalip", "lOtoLengthCUseable",
    
    "rOto", "rOtoWeightRecorded", "rOtoWeightUseable",
    "rOtoLengthA", "rOtoLengthACalip", "rOtoLengthAUseable", 
    "rOtoLengthB1", "rOtoLengthB2", "rOtoLengthBCalip", "rOtoLengthBUseable",    
    "rOtoLengthC1", "rOtoLengthC2", "rOtoLengthC3", "rOtoLengthC4", 
    "rOtoLengthCCalip", "rOtoLengthCUseable",
    
    "comments",
    
    # from Deb's otolith work
    "mmNumberD", "monthD", 
    "debAgeclassCorrected", # this is the one to use, the other "resolvedAgeclass" is wrong
    "debAnnuli", "debGrowth", "debAgeclass", 
    "geoffAnnuli", "geoffGrowth", "geoffAgeClass", "difference", 
    "resolvedAgeclass", "otoRadius", "ultimateAnnulus", "penultimateAnnulus",
    "growthIncrement", "fractionalAgeD", "notes2",  
    
    # from Deb's gut content work
    "mmNumberGut","gutPresent","gutCount","gutWeight","gutID","gutNotes"  
  )
  # column units
  columnUnits = c("NA", "NA", "NA", "NA", "NA", "NA", 
    "NA", "kg", "mm", "mm", "mm", "10cm", "Y/N",
    "tag", "time", "time", "time", "time", 
    "NA", "NA", "NA", "NA", "NA",
    "NA", "NA", "NA", "NA", "ft", "NA", "mm", "mm",
    "g", "mm", 
    
    "YBPN", "mg", "mg",
    "um", "mm", "um", 
    "um", "um", "mm", "um",    
    "um", "um", "um", "um", 
    "mm", "um",
    
    "YBPN", "mg", "mg",
    "um", "mm", "um", 
    "um", "um", "mm", "um",    
    "um", "um", "um", "um", 
    "mm", "um",
    
    "NA",
    
    # from Deb's otolith work
    "NA", "NA", "NA", "NA", "NA", "NA", 
    "NA", "NA", "NA", "NA", 
    "NA", "um", "um", "um",
    "um", "yrs", "NA",
    
    # from Deb's gut content work
    "NA", "NA", "numbers", "grams", "NA", "NA"
    
  ) # column classes
  columnClasses = c("factor", "factor", "factor", "factor", "factor", "factor", 
    "factor", "numeric", "numeric", "numeric", "numeric", "factor", "factor",
    "factor", "character", "character", "character", "character", 
    "factor", "factor", "character", "character", "factor",
    "factor", "factor", "factor", "factor", "numeric", "factor", "numeric", "numeric",
    "numeric", "numeric", 
  
    "factor", "character", "numeric",
    "character", "character", "numeric", 
    "character", "character", "character", "numeric",    
    "character", "character", "character", "character", 
    "character", "numeric",
    
    "factor", "character", "numeric",
    "character", "character", "numeric", 
    "character", "character", "character", "numeric",    
    "character", "character", "character", "character", 
    "character", "numeric",
    
    "character",
    
    # from Deb's otolith work
    "factor", "factor", "factor", "numeric", "factor", "factor", 
    "numeric", "factor", "factor", "numeric", 
    "factor", "numeric", "numeric", "numeric",
    "numeric", "numeric", "character",    
    
    # from Deb's gut content work
    "factor", "numeric", "numeric", "numeric", "character", "character"
  )
  
  # before reading in this fileName, you have to get rid of the extra 
  # commas Excel puts in.  You only have to do this once when you change 
  # the *.csv file.  I'll use the find and replace in Word.
  
  biometrics = read.table(fileName, sep=",", col.names=columnNames, 
    colClasses=columnClasses)

  return(biometrics)
}                            




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
### this code draws a 3d compass
circles3d <- function(x, y, z, r, ...){
  # draw the circle
  ang = seq(0, 2*pi, length=512)
  xx = x + r * cos(ang)
  yy = y + r * sin(ang)
  zz = z
  points3d(xx, yy, zz, size=5, ...)
  
  # draw the compass notches
  segments3d(c(0.8*r*cos(0),r*cos(0))+x,c(0.8*r*sin(0),r*sin(0))+y,c(0,0)+z,size=5)   
  segments3d(c(0.8*r*cos(pi/2),r*cos(pi/2))+x,c(0.8*r*sin(pi/2),r*sin(pi/2))+y,c(0,0)+z,size=5)  
  segments3d(c(0.8*r*cos(pi),r*cos(pi))+x,c(0.8*r*sin(pi),r*sin(pi))+y,c(0,0)+z,size=5)  
  segments3d(c(0.8*r*cos(3*pi/2),r*cos(3*pi/2))+x,c(0.8*r*sin(3*pi/2),r*sin(3*pi/2))+y,c(0,0)+z,size=5)  
            
  # draw the labels
  t2 = 1.3
  distancesX = c(r*cos(0),r*cos(pi/2),r*cos(pi),r*cos(3*pi/2))*t2+x
  distancesY = c(r*sin(0),r*sin(pi/2),r*sin(pi),r*sin(3*pi/2))*t2+y
  distancesZ = c(zz, zz, zz)
  labs = c("E",  "N", "W", "S")
  text3d(distancesX,distancesY,distancesZ,labs)                     
} # end circles3d()


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
################################################################################
### this code draws a 3d clock
clocks3d <- function(x, y, z, r, ...){
  # draw the circle
  ang = seq(0, 2*pi, length=512)
  t1 = pi/12 # offset because the clock is slanted onto the xy plane
  hour = head(seq(0, 2*pi, length=25), 24)+t1
  xx = x + r * cos(ang)
  yy = y + r * sin(ang)
  zz = z
  points3d(xx, yy, zz, size=2)#, ...)
  
#################################################################### instead of using hour 5, 11, 17, etc, get the offset right  
  # draw the hour notches
#  segments3d(c(0.8*cos(hour[1]),cos(hour[1]))*r+x,c(0.8*sin(hour[1]),sin(hour[1]))*r+y,c(0,0)+z,size=5, col="red")   
#  segments3d(c(0.8*cos(hour[2]),cos(hour[2]))*r+x,c(0.8*sin(hour[2]),sin(hour[2]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[3]),cos(hour[3]))*r+x,c(0.8*sin(hour[3]),sin(hour[3]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[4]),cos(hour[4]))*r+x,c(0.8*sin(hour[4]),sin(hour[4]))*r+y,c(0,0)+z,size=5)   
  segments3d(c(0.8*cos(hour[5]),cos(hour[5]))*r+x,c(0.8*sin(hour[5]),sin(hour[5]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[6]),cos(hour[6]))*r+x,c(0.8*sin(hour[6]),sin(hour[6]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[7]),cos(hour[7]))*r+x,c(0.8*sin(hour[7]),sin(hour[7]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[8]),cos(hour[8]))*r+x,c(0.8*sin(hour[8]),sin(hour[8]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[9]),cos(hour[9]))*r+x,c(0.8*sin(hour[9]),sin(hour[9]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[10]),cos(hour[10]))*r+x,c(0.8*sin(hour[10]),sin(hour[10]))*r+y,c(0,0)+z,size=5)   
#  segments3d(c(0.8*cos(hour[11]),cos(hour[11]))*r+x,c(0.8*sin(hour[11]),sin(hour[11]))*r+y,c(0,0)+z,size=5)   
  segments3d(c(0.8*cos(hour[11]),cos(hour[11]))*r+x,c(0.8*sin(hour[11]),sin(hour[11]))*r+y,c(0,0)+z,size=5)   
  segments3d(c(0.8*cos(hour[17]),cos(hour[17]))*r+x,c(0.8*sin(hour[17]),sin(hour[17]))*r+y,c(0,0)+z,size=5)   
  segments3d(c(0.8*cos(hour[23]),cos(hour[23]))*r+x,c(0.8*sin(hour[23]),sin(hour[23]))*r+y,c(0,0)+z,size=5)   
  
  # draw the labels
  t2 = 1.39
  hourTicks=c(hour[6],hour[12],hour[18],hour[24])
  distancesX = cos(hourTicks)*r*t2+x
  distancesY = sin(hourTicks)*r*t2+y
  distancesZ = rep(zz,12)
  labs = c("24:00","18:00","12:00","6:00")
  text3d(distancesX,distancesY,distancesZ,labs)             
} # end clocks3d()




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
# These two functions calculate turning angle in the right way...
### I got this from here:
### http://quantitative-ecology.blogspot.com/2007/05/anglefun-function-xxyy-bearing-true-as.html
### and here:
### http://quantitative-ecology.blogspot.com/
anglefun <- function(xx, yy, bearing=TRUE, as.deg=FALSE){
  ## calculates the compass bearing of the line between two points
  ## xx and yy are the differences in x and y coordinates between two points
  ## Options:  ## bearing = FALSE returns +/- pi instead of 0:2*pi
  ## as.deg = TRUE returns degrees instead of radians
  c = 1
  if (as.deg){c = 180/pi}
  b<-sign(xx)
  b[b==0]<-1  #corrects for the fact that sign(0) == 0
  tempangle = b*(yy<0)*pi+atan(xx/yy)
  if(bearing){
    #return a compass bearing 0 to 2pi
    #if bearing==FALSE then a heading (+/- pi) is returned
    tempangle[tempangle<0]<-tempangle[tempangle<0]+2*pi
  }
  return(tempangle*c)
}
##############################################################################
bearing.ta <- function(loc1, loc2, loc3, as.deg=TRUE, replaceNaN=FALSE){
  # loc1 = p1[[i]]; loc2 = p2[[i]]; loc3 = p3[[i]]; 
  ## calculates the bearing and length of the two lines
  ##    formed by three points
  ## the turning angle from the first bearing to the
  ##    second bearing is also calculated
  ## locations are assumed to be in (X,Y) format.
  ## Options:
  ## as.deg = TRUE returns degrees instead of radians
  if (length(loc1) != 2 | length(loc2) != 2 | length(loc3) !=2){
    print("Locations must consist of either three vectors, length == 2,or three two-column dataframes")
    return(NaN)
  }
  c = 1
  if (as.deg){c = 180/pi}
  locdiff1<-loc2-loc1
  locdiff2<-loc3-loc2
  bearing1<-anglefun(locdiff1[1],locdiff1[2],bearing=FALSE)
  bearing2<-anglefun(locdiff2[1],locdiff2[2],bearing=FALSE)
    
  if(is.data.frame(locdiff1)){
    dist1<-sqrt(rowSums(locdiff1^2))
    dist2<-sqrt(rowSums(locdiff2^2))
  }else{
    dist1<-sqrt(sum(locdiff1^2))
    dist2<-sqrt(sum(locdiff2^2))
  }
  
  ta=(bearing2-bearing1)
  
  # bearing1 or bearing2 will be NaN if there is a point that doesn't move
  # from one time to the next.  If this happens then ta will have NaN in it.
  # Do you want to replace the NaN in ta with zero?
  if(replaceNaN){ ta$nextEasting[ is.nan(ta$nextEasting) ] = 0 }
  
  ta[ta < -pi] = ta[ta < -pi] + 2*pi
  ta[ta > pi] = ta[ta > pi] - 2*pi       
  return(list(bearing1=unlist(bearing1*c), bearing2=unlist(bearing2*c),
    ta=unlist(ta*c), dist1=unlist(dist1), dist2=unlist(dist2)))
}



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
###############################################################################
# This calculates the mean angle from a vector of angles in degrees.
# Read Wikipedia for the equation
meanAngle = function(x){ # x is a vector of angles in degrees
  (180/pi)*atan2(sum(sin(x*pi/180))/length(x), sum(cos(x*pi/180))/length(x))
}
      

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
###############################################################################
### I got the following code from Ben's 'HPDregionplot()', I modified it here
### I worked out this function in 'kde2d.r'
###   This function takes fish positions and calculates a KDE then draws a 
###   picture and returns the home range size in units of meters^2.
homeRange = function (easting, northing, tagName, lims, reefEN, sdlEN,   
  n = 100,  prob = 0.5, h = c(bandwidth.nrd(easting),bandwidth.nrd(northing)), 
  pts=TRUE, drawplot=TRUE, ...) 
{
  # calculate the kde
  post1 = kde2d(easting, northing, n=n, h=h, lims=lims)
  # find the size of the boxes in our '2D histogram'
  # each box has an x and y position with a z (density) value  
  dx = diff(post1$x[1:2])
  dy = diff(post1$y[1:2])
  # sort the z (density) values then count the number greater than 'prob' 
  sz = sort(post1$z)
  c1 = cumsum(sz) * dx * dy
  levels = approx(c1, sz, xout = 1 - prob)$y   # this gives an error...
     # "In approx(c1, sz, xout = 1 - prob) : collapsing to unique 'x' values"
     # I (and Ben I believe)think it's okay to ignore this error.

  # how many are above the 'levels' value corresponding to 'prob'...
  hrSize = sum(sz > levels) * dx * dy # when you 'sum' TRUEs = 1 and FALSEs=0

  # draw the plot 
  if (drawplot){ 
    #par(mfrow=c(1,1))
    plot(x=reefEN$easting, y=reefEN$northing, type="n", col="blue", 
      xlab = "Easting (m)", xlim=c(lims[1],lims[2]), 
      ylab = "Northing (m)", ylim=c(lims[3], lims[4]), 
      main=paste(tagName, ": ", prob*100, "% HR = ", round(hrSize,1), "m^2", sep="")
    )
    if(pts){points(easting,northing,pch=19,cex=0.1)}
    points(x=reefEN$easting, y=reefEN$northing,pch=19, col="red")
    points(x=sdlEN$easting, y=sdlEN$northing, pch=19, col="blue")
    contour(post1$x, post1$y, post1$z, level = levels, add=T, 
      col="green", lwd=2, drawlabels = FALSE, ...)
    invisible(contourLines(post1$x, post1$y, post1$z, level = levels))
  }
  
 
  return(hrSize) # because of the dx and dy above, this number is m^2
}



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
### I got the following code from the R graph gallery, I modified it here



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
### I got the following code from the R graph gallery, I modified it here





##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
### I got the following code from the R graph gallery, I modified it here
kde2dplot <- function(d,                # a 2d density computed by kde2d()
                      reefEN, sdlEN,    #
                      ncol=50,          # the number of colors to use 
                      zlim=c(0,max(z)), # limits in z coordinates 
                      nlevels=20,       # see option nlevels in contour 
		                  theta=30,         # see option theta in persp
		                  phi=30,           # see option phi in persp
		                  ...)
{
  z   <- d$z
  nrz <- nrow(z) 
  ncz <- ncol(z) 

  couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol) 
  fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1] 
  dim(fcol) <- c(nrz,ncz) 
  fcol      <- fcol[-nrz,-ncz]

  par(mfrow=c(1,2),mar=c(0.5,0.5,0.5,0.5)) 
  persp(d, col=fcol, zlim=zlim, theta=theta, phi=phi, 
    xlab="Easting (m)", ylab="Northing (m)",
    zlab="Density")#         , main=tagName)  ### why can't I use this?

  par(mar=c(2,2,2,2)) 
  image(d,col=couleurs) 
  contour(d,add=T,nlevels=nlevels)

  points(x=reefEN$easting, y=reefEN$northing,pch=17, col="red")
  points(x=sdlEN$easting, y=sdlEN$northing, pch=19, col="red")

   
  box()
  
  # return the plot area to normal
  par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.)  
} # end kde2dplot

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
###############################################################################
### This is what I did...
### I worked out this function in 'kde2d.r'
kde2dplot2 <- function(easting, northing, # the original data
                      tagName,          # tag being drawn
                      d,                # a 2d density computed by kde2d()
                      prob = 0.5,       # where to draw the contour line
                      ncol=60,          # the number of colors to use 
                      zlim=c(0,max(z)), # limits in z coordinates 
                      nlevels=20,       # see option nlevels in contour 
		                  theta=30,         # see option theta in persp
	                    phi=30,           # see option phi in persp
                      pts=TRUE,         # plot the original points?
                      ... )
{ # easting=d9$data$easting; northing=d9$data$northing; d=kde;
  # Code from homeRange()
  
  # find the size of the boxes in our '2D histogram'
  # each box has an x and y position with a z (density) value  
  dx = diff(d$x[1:2])
  dy = diff(d$y[1:2])
  # sort the z (density) values then count the number greater than 'prob' 
  sz = sort(d$z)
  c1 = cumsum(sz) * dx * dy
  levels = approx(c1, sz, xout = 1 - prob)$y
  # get output from from kde
  z   <- d$z
  nrz <- nrow(z) 
  ncz <- ncol(z) 
  couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol) 
  fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1] 
  dim(fcol) <- c(nrz,ncz) 
  fcol      <- fcol[-nrz,-ncz]
  
  # draw the UD
  image(d,col=couleurs, xlab = "Easting", ylab="Northing", 
    main=paste(tagName, ": ", prob*100, "% HR ", sep="")
  ) 
  # draw the contour line                                       
  contour(d,add=T,level=levels)#nlevels=nlevels) 
  
  # add the points if desired
  if(pts){points(easting,northing,pch=19,cex=0.1)}
  box() 
} # end kde2dplot2






























##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
## hacked version of "movie3d".  "movie3d" sets up a movie,
##   but all it can do is change the perspective (not plot different
##   things for each frame)

## to run this you need the 'ImageMagick' program, and you need
## your path set so R can find the 'convert' program

mymovie <- function (f, duration, dev = rgl.cur(), ..., fps = 10, movie = "movie", 
    frames = movie, dir = tempdir(), convert = TRUE, clean = TRUE, 
    verbose = TRUE, top = TRUE) 
{
    olddir <- setwd(dir)
    on.exit(setwd(olddir))
    for (i in 0:(duration * fps)) {
        time <- i/fps
        if (rgl.cur() != dev) 
            rgl.set(dev)
        f(time,...)
        filename <- sprintf("%s%03d.png", frames, i)
        if (verbose) {
            cat("Writing", filename, "\r")
            flush.console()
        }
       rgl.snapshot(filename = filename, fmt = "png", top = top)
    }
    cat("\n")
    if (.Platform$OS.type == "windows") 
        system <- shell
    if (is.logical(convert) && convert) {
        version <- system("convert --version", intern = TRUE)
        if (!length(grep("ImageMagick", version))) 
            stop("ImageMagick not found")
        movie.filename <- paste(movie, ".gif", sep = "")
        if (verbose) 
            cat("Will create: ", file.path(dir, movie.filename), "\n")
        wildcard <- paste(frames, "*.png", sep = "")
        convert <- paste("convert -delay 1x", fps, " ", wildcard, 
            " ", movie.filename, sep = "")
    }
    if (is.character(convert)) {
        if (verbose) {
            cat("Executing: ", convert, "\n")
            flush.console()
        }
        system(convert)
        if (clean) {
            if (verbose) 
                cat("Deleting frames.\n")
            for (i in 0:(duration * fps)) {
                filename <- sprintf("%s%03d.png", frames, i)
                unlink(filename)
            }
        }
    }
    return(file.path(dir, movie.filename))
} ### end mymovie ##############################################################
### end code from Ben to make a movie
##############################################################################


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
###############################################################################
## This imports all data relating to fish capture, tagging, collection.
## Look in "all fish tagging data.xlsx" for the origin of these numbers.

#columns and units are:
#1. year of contact during tagging
#2. month of contact during tagging
#3. day of contact during tagging
#4. reef where fish was tagged
#5. landscape designation of reef
#6. experimental replicate
#7. collection method during tagging
#8. fish weight during tagging (kg)
#9. fish girth during tagging (mm)
#10. fish total length during tagging (mm)
#11. fish fork length during tagging (mm)
#12. size range in 10cm increments, either from diver or from 10. total length (cm)
#13. was this fish tagged
#14. tag ID used during tagging
#15. start time in knock-out tank
#16. tagging start time 
#17. start time in recovery
#18. release time (end of dive releasing fish to reef)
#19. color of left external PIT tag
#20. color of right external PIT tag
#21. notes of tagging operations
#22. MM number assigned during collections
#23. tag ID found in collected fish
#24. year of contact during collection
#25. month of contact during collection
#26. day of contact during collection
#27. reef where fish was collected
#28. estimated depth of reef where fish was collected (ft), not reliable
#29. collection method during collection
#30. fish total length after collection (mm)
#31. fish fork length after collection (mm)
#32. fish weight after collection (g)
#33. fish girth after collection (mm)
#34. status of left otolith (Yes, Broken, Partial, No) i.e. (present, broken but all present, piece missing, no otolith)
#35. recorded weight of left otolith, has two measurements for broken otoliths (mg)
#36. useable weight of total left otolith, i.e. from 35. 2+3 gives 5 (mg)
#37. length of left otolith measurement A made digitally (um)
#38. length of left otolith measurement A made with calipers (mm)
#39. usable length of left otolith measurement A. The number I pick from 37-38 (um)
#40. length of left otolith measurement B1 made digitally (um)
#41. length of left otolith measurement B2 made digitally (um)
#42. length of left otolith measurement B made with calipers (mm)
#43. usable length of left otolith measurement B. The number I pick from 40-42 (um)
#44. length of left otolith measurement C1 made digitally (um)
#45. length of left otolith measurement C2 made digitally (um)
#46. length of left otolith measurement C3 made digitally (um)
#47. length of left otolith measurement C4 made digitally (um)
#48. length of left otolith measurement C made with calipers (mm)
#49. useable length of left otolith measurement C. The number I pick from 44-48 (um)
#50. status of right otolith (Yes, Broken, Partial, No) i.e. (present, broken but all present, piece missing, no otolith)
#51. recorded weight of right otolith, has two measurements for broken otoliths (mg)
#52. useable weight of total right otolith, i.e. from 35. 2+3 gives 5 (mg)
#53. length of right otolith measurement A made digitally (um)
#54. length of right otolith measurement A made with calipers (mm)
#55. usable length of right otolith measurement A. The number I pick from 53-54 (um)
#56. length of right otolith measurement B1 made digitally (um)
#57. length of right otolith measurement B2 made digitally (um)
#58. length of right otolith measurement B made with calipers (mm)
#59. usable length of right otolith measurement B. The number I pick from 56-58 (um)
#60. length of right otolith measurement C1 made digitally (um)
#61. length of right otolith measurement C2 made digitally (um)
#62. length of right otolith measurement C3 made digitally (um)
#43. length of right otolith measurement C4 made digitally (um)
#64. length of right otolith measurement C made with calipers (mm)
#65. useable length of right otolith measurement C. The number I pick from 60-64 (um)
#66. notes on otoliths



importFishData = function(
  fn="all fish tagging data.csv"  # filename
  

){
  
  setwd("C:/zy/USB working folder/Telemetry/Archive/fish data files")  
  
  columnNames = c(
    # tagging activities 
    "year", "month", "day", "reefID", "ttmt", "rep", "gear", 
    "fishWeight", "girth", "TL", "FL", "sizeRange", "tagged", "tagID", 
    "timeA", "timeB", "timeC", "timeD", "pitL", "pitR", "taggingNotes", 
    # collection activities
    "MM", "tagID2", "year2", "month2", "day2", "reefID2", "depth", "gear2", 
    "TL2", "FL2", "fishWeight2", "girth2", 
    "otoLs", # status
    "otoLwr", "otoLw", # weight
    "otoLa1", "otoLac", "otoLa",  # measurement A 
    "otoLb1", "otoLb2", "otoLbc", "otoLb", # measurement B
    "otoLc1", "otoLc2", "otoLc3", "otoLc4", "otoLcc", "otoLc", # measurement C 
    "otoRs", # status
    "otoRwr", "otoRw", # weight
    "otoRa1", "otoRac", "otoRa", # measurement A
    "otoRb1", "otoRb2", "otoRbc", "otoRb", # measurement B
    "otoRc1", "oroRc2", "otoRc3", "otoRc4", "otoRcc", "otoRc", 
    "collectionNotes")
    
  columnClasses = c(
    'character','character','character','factor','factor', #year-ttmt
    'factor','character','numeric','numeric','numeric','numeric', #rep-FL
    'integer', 'factor','integer','character', #sizeRange-timeA
    'character','character', 'character',#timeB-timeD
    'character','character','character','integer', 'integer', #pitL-tagID2
    'character', 'character', 'character', 'factor', 'integer', #year2-depth
    'character', 'numeric', 'numeric', 'numeric', 'numeric', #gear2-girth2
    'factor', 'character', 'numeric', 'character', 'numeric', #otoLs-otoLac
    'numeric', 'character', 'character', 'numeric', 'numeric', #otoLa-otoLb
    'character', 'character', 'character', 'character', #otoLc1-otoLc4
    'numeric', 'numeric', 'factor', 'character', 'numeric',# otoLcc-otoRw
    'character', 'numeric', 'numeric', 'character', #otoRa1-otoRb1
    'character', 'numeric', 'numeric', 'character', #otoRb2-otoRc1
    'character', 'character', 'character', 'numeric', #otoRc2-otoRcc
    'numeric', 'character' #otoRc-collectionNotes
  )   
    
    
  # read in file    
  d1 = read.table(fn, header=FALSE, sep=",", col.names=columnNames,
    colClasses = columnClasses
  )
  
  # create dates from text columns and put them into d1
  temp1 = strptime(paste(d1$year, d1$month, d1$day, sep="/"), format="%Y/%m/%d")
  temp2 = strptime(paste(d1$year2, d1$month2, d1$day2, sep="/"), format="%Y/%m/%d")
  d2=d1
  d2$year = temp1
  d2$year2 = temp2
  names(d2)[1] = "date"
  names(d2)[24] = "date2"
  
  # convert fishWeight from units of kg to g
  d2$fishWeight = d2$fishWeight*1000
  
  # stuff I want to look at
  # NOTES:  all tagID=tagID2; reefID=reefID2
  d3 = d2[,c(1,4:6,8:12,22,24,30:33,36,39,43,49,52,55,59,65)]
  
  # look at weights
  d4 = d3[,c(1,5:8,10,12:15)]
  
  plot(d4$fishWeight, d4$fishWeight2)
  abline(0,1)
  d4[!is.na(d4$fishWeight2),]
  
}

 

##################################################################################
### For plotting categorical habitat maps
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
