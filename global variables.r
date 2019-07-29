##############################################################################
### This file sets common variable values which are used in multiple other
### files.  This way I only have to change them in one place.
##############################################################################
### Several of these have been moved to 'metadata.r'

### libraries I use    when possible put these to only open when needed  ?require
#library(g]s)
#library(gpclib)
#library(adehabitat)
#library(sp)
#library(plotrix)
#library(rgl)
#library(RandomFields)
#library("PBSmodelling") # for the 'promptOpenFile' dialog box
library("MASS") 

rootDir = "C:/zy/school closet/Telemetry" # this needs to be more widely incorporated
dataDir = "C:/zy/data closet"
sdlDir = "/Telemetry"
adcpDir = "/ADCP Data 2010Sep-Final"


#### A switch to import all days or some subset
#allDays=TRUE
#
#### constants
#secPhour = 60 * 60
#secPday = 24 * secPhour
#secPyear365 = 365 * secPday
#secPyear366 = 366 * secPday
#
##############################################################################
### UTM offsets...
### These are offsets meant to be used with all array deployment locations.
### They're chosen to be south and west of all reefs
### See 'SFMA Reefball scanning waypoints.xls'  
eastingOffset = 236600
northingOffset = 3261700

###############################################################################
# Home range estimates depend on the number and size of grid cells you
# choose.  These are set by the limits and n you choose.
# 'homeRange()' expects lims to come as c(minEasting, maxEasting, minNorthing, maxNorthing)
# For 2007 and 2008...this is big enough to include both array deployment spacings
hrlims = c(8440, 8720, 550, 850)
# for 2009, because each deployment is in a different location, just pick 
# the range to put around the reef location...pick a range to match 2007/2008
#8720-8440=280; 850-550=300; Too bad I wasn't smart enough to make it square
hrRange = c(140,150) # c(add/subtract from reefEN$easting, ditto reefEN$northing)


##############################################################################
### Random stuff

plotColors = c(
  "black", "red", "blue", "green", "yellow", "pink", "brown", "orange", 
  "skyblue", "violet", "grey", "salmon", "black", "red", "blue", "brown", 
  "yellow", "pink", "green", "orange", "skyblue", "violet", "grey", "salmon",
  "black", "red", "blue", "brown", "yellow", "pink", "green", "orange", 
  "skyblue", "violet", "grey", "salmon", "black", "red", "blue", "brown", 
  "yellow", "pink", "green", "orange", "skyblue", "violet", "grey", "salmon",
  "black", "red", "blue", "brown", "yellow", "pink", "green", "orange", 
  "skyblue", "violet", "grey", "salmon", "black", "red", "blue", "brown", 
  "yellow", "pink", "green", "orange", "skyblue", "violet", "grey", "salmon",
  "black", "red", "blue", "brown", "yellow", "pink", "green", "orange", 
  "skyblue", "violet", "grey", "salmon", "black", "red", "blue", "brown", 
  "yellow", "pink", "green", "orange", "skyblue", "violet", "grey", "salmon",
  "black", "red", "blue", "brown", "yellow", "pink", "green", "orange", 
  "skyblue", "violet", "grey", "salmon", "black", "red", "blue", "brown", 
  "yellow", "pink", "green", "orange", "skyblue", "violet", "grey", "salmon"
  )
plotBuffer = 25 # this is for drawing plots 25m bigger than the array


################################################################################
#### 2007 Dec common starting in unix time
## Experiment dates - 2007 Dec 09 - 2008 Jan 15
#
## startTime's and stopTimes's for each fish are:
## T60200 = (1197226947, 1200407671)
## T60400 = (1197227586, 1200414927)
## T60500 = (1197225302, 1200412037)
## T60700 = (1197228303, 1200397705)
## T60900 = (1197224495, 1200414388)
#
## set common start time to 2007 Dec 09 00:00:00 = 1197158400 GMT
## Florida is GMT - 5 hours (18000 sec) and Dec - Jan are not affected by daylight savings
#startTime2007 = 1197158400 # GMT
#janStart2007 = 1167609600
#janStart2008 = 1199145600
#
################################################################################
#### 2008 Oct common starting in unix time
## Experiment dates - 2008 Oct 17 - 2008 Dec 07
#
## set common start time to 2008 Oct 17 00:00:00 = 1197158400 GMT
## Florida is GMT - 5 hours (18000 sec) and reverted to standard time at
## 2am local time on Nov 2.  So the local clock changed from 1:59am to 1:00am.
## However, this did not affect either the SDL or ADCP clocks, as their clocks
## were set to local satellite time at the start of the experiment and did not
## change with the end of daylight savings.  It does affect other times that
## I later relate to the SDL and ADCP clocks.
#startTime2008 = 1224201600 # GMT
#
################################################################################
#### To get multiple fish within a loop...
#### here are lists of tag ID for each year
#tags2007 = c(60200, 60400, 60500, 60700, 60900)
#tags2008 = c(60100, 60300, 60600, 60800, 61100, 61200, 61300)    
#
#tagFolders2007 = paste("C:/zy/Telemetry/2007 Dec/T",tags2007,"B79500", sep="")
#tagFolders2008 = paste("C:/zy/Telemetry/2008 Oct/T",tags2008,"B79500", sep="")
#
#numFish2007 = length(tags2007)
#numFish2008 = length(tags2008)
#
## this gives a list of the days (in unix time) involved in each year
#days2007 = 13856:13893
#days2008 = 14169:14219
#numDays2007 = 38
#numDays2008 = 51
#
#
#
#################################################################################
#### Array Spacing Trials - start and stop times for each spacing
#
### 150m variables
##startTime_150m = 1241708820; # 1241708820 = 11:07:00 EDT 7 May 2009
##stopTime_150m = 1241715240; #  1241715240 = 12:54:00 EDT 7 May 2009
##eTime_150m = stopTime_150m - startTime_150m # number of seconds
##emTime_150m = eTime_150m / 60 # number of minutes
##
### 125m variables
##startTime_125m = 1240504980; # 1240504980 = 12:43:00 EDT 23 April 2009
##stopTime_125m = 1240510800; #  1240510800 = 14:20:00 EDT 23 April 2009
##eTime_125m = stopTime_125m - startTime_125m # number of seconds
##emTime_125m = eTime_125m / 60 # number of minutes
##
### 100m variables
##startTime_100m = 1241719660; # 1241719660 = 14:07:40 EDT 7 May 2009
##stopTime_100m = 1241726040; # 1241726040 = 15:54:00 EDT 7 May 2009
##eTime_100m = stopTime_100m - startTime_100m # number of seconds
##emTime_100m = eTime_100m / 60 # number of minutes
##
##### Notes on when EST and EDT are in effect:
## 2007: EDT started on 11 March and ended on 4 November
## 2008: 9 March - 2 November
## 2009: 8 March - 1 November
## 2010: 14 March - 7 November
## The second Sunday in March and the first Sunday in November
## EDT = GMT-4 hours
## EST = GMT-5 hours
#
#
#
