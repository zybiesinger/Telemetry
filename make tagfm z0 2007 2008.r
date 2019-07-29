# In this file I import SDL and ADCP data for 2007 and 2008.  
# I filter the SDL data and merge
# it with ADCP data.  The code for this was originally worked out 
# in 'analysis single deployment.r' under the section
# 'Working with ALPS position output' AND 'chapter 3 part 1.r' also.
#
# For the 2008 data, the multiple codes for individuals tags are combined
# to gather all info about individuals tags.
#
# This file also has work to manually cut some data from fish position solutions
# at times when there were PSs that I don't think represent true fish movement,
# like the fish seems to have died because the tag doesn't move at all.
# 
# Next, z0 is constructed.  It mostly rearranges tagfm and preps it for use
# with GAM model fitting.

###############################################################################
# if everything below here is to your liking, simply read in the stored 
# tagfm, z0, and results. 
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")



# These lines give you data for all fish, good and bad...   ????zzzzz zzzz      

load("C:/zy/school closet/Telemetry/R summary files/tag 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/tagf 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/tagfm 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/z0 2011May02.rdata")
load("C:/zy/school closet/Telemetry/R summary files/results 2011Mar16.rdata")

## ...if you load the full dataset (i.e. including the bad fish)...then
## ...now take these and remove the bad fish...
## ...pick out only the 5 good fish from tagfm, z0, results
## ... order of tags in tagfm: 
## ... f60200, f60400, f60500, f60700, f60900, f60100, f60300, f61100, f61200, f61300 
## ... good fish are:
## ... f60200, f60400,                 f60900,         f60300, f61100
#library(gdata)
#tag[[3]] = tag[[5]]; tag[[4]] = tag[[7]]; tag[[5]] = tag[[8]]; tag = head(tag,5)
#tagf[[3]] = tagf[[5]]; tagf[[4]] = tagf[[7]]; tagf[[5]] = tagf[[8]]; tagf = head(tagf,5)
#tagfm[[3]] = tagfm[[5]]; tagfm[[4]] = tagfm[[7]]; tagfm[[5]] = tagfm[[8]]; tagfm = head(tagfm,5)
#
#z0 = z0[z0$tagName=="f60200" | z0$tagName=="f60400" | z0$tagName=="f60900" | 
#  z0$tagName=="f60300" |z0$tagName=="f61100", ]
#z0$tagName=drop.levels(z0$tagName)
#results = results[results$numHits>10000 ,]
#
### Now save the 'good' tagfm, z0, and results
#save("tagfm", file="C:/zy/Telemetry/R summary files/tagfm 2011Mar16.rdata")
#save("z0", file="C:/zy/Telemetry/R summary files/z0 2011Mar16.rdata")
#save("results", file="C:/zy/Telemetry/R summary files/results 2011Mar16.rdata")




# If all you want to do is read in the good fish...
# These lines give you data only for the 5 good fish.
load("C:/zy/Telemetry/R summary files/tagfm 2011Mar16.rdata")
load("C:/zy/Telemetry/R summary files/z0 2011May02.rdata")
load("C:/zy/Telemetry/R summary files/results 2011Mar16.rdata")
    












################################################################################
################################################################################
################################################################################
################################################################################
# if you'd like to re-do it or see my decisions then continue on below...
# choose the tags to work with
# read in and execute variables, functions, metadata



c2007TagNames = md[[1]]$fishNames
c2008TagNames = c("f60100", "f60300", "f61100", "f61200", "f61300")
num2007Tags = length(c2007TagNames)
num2008Tags = length(c2008TagNames)
cTagNames = c(c2007TagNames, c2008TagNames)
cDeploymentNames = c(rep("hb2007",length(c2007TagNames)),
    rep("hb2008",length(c2008TagNames)))
# but I really only want the good fish f60200, f60400, f60900, f61100
cTagNames = c("f60200", "f60400", "f60900", "f60300", "f61100")
cDeploymentNames = c(rep("hb2007",3),rep("hb2008",2))


# get the sonar image to use
library(rimage)
rfile = paste("C:/zy/school closet/Telemetry/R summary files/",
  "IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep="")

i1 = round(read.jpeg(rfile))
#par(mar=c(0.2,0.2,0.2,0.2)); plot(i1); box("plot", col="red") 

################################################################################
### MAKE tagfm 
################################################################################

## import, filter, and merge ALPS data
tag = list() # raw tag data
tagf = list() # filtered tag data
tagfm = list() # filtered tag data merged with ADCP/tide data
for (i in 1:length(cTagNames)){
  print(cTagNames[i])
  tag[[i]] = importALPSdata(deployment=cDeploymentNames[i],tagName=cTagNames[i])
  tagf[[i]] = filterALPSdata(df1=tag[[i]], cnF=1.5, speedF=0.8, minuteMean=TRUE)
  tagfm[[i]] = mergeAlpsAdcpData(alpsData=tagf[[i]])
  tagfm[[i]] = mergeSonarData(alpsData=tagfm[[i]],habmap=i1,reference="if41")    
}


# Now that the data are in, look at each fish individually and maybe
# manually chop some data because...maybe the fish looks like it died or, there
# appear to be detections but no position solutions.
#
# T60200 #######################################################################
cTag = 1
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$utime, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
# This fish appears to have been active the entire time.  No action needed.


# T60400 ######################################################################
cTag = 2
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$utime, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
# This fish appears to have been active the entire time.  No action needed.


# T60500 ######################################################################
###############################    this is a bad tag
cTag=3
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")

## it looks like the last real point comes before about 1197400000,
## I'll chop everything after that
#
#cutoffTime = 1197400000
#tag[[cTag]]$data = tag[[cTag]]$data[tag[[cTag]]$data$utime < cutoffTime, ]
#tagf[[cTag]]$data = tagf[[cTag]]$data[tagf[[cTag]]$data$utime < cutoffTime, ]
#tagfm[[cTag]]$data = tagfm[[cTag]]$data[tagfm[[cTag]]$data$utime < cutoffTime, ]



# T60700 #######################################################################
###############################    this is a bad tag
cTag=4
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(2,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$easting, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(8400,8800)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$easting, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$easting, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$easting, col="red")

#
## It looks like it sat for a long time at the eastern SDL.  There are a couple
## times of big movement in the easting and northing.  I want to see the EN plots
## for those times.
## CONCLUSION: during those times of big movement in easting and northing, the
## fish was not simply traversing the array, it was moving within the entire
## array area.  Look at time windows c(1199200000,1199400000) and c(1198220000,1198250000)
#
#
## This one looks funky but appears to be true behavior
## the fish sits still at the east SDL for long periods, but makes a couple
## excursions where it visits the whole area within the array.  Don't cut anything.


# T60900 ######################################################################
cTag = 3
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$utime, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
# This fish appears to have been active the entire time.  No action needed.





# T60100  ####################################################################
###############################    this is a bad tag
cTag=6
#
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(2,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$easting, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(8400,8800)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$easting, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$easting, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$easting, col="red")

## T60100 cTag=6... This one cuts off all by itself.  No need for action




# T60300 #######################################################################
cTag=4
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$utime, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
# This fish appears to have been active the entire time.  No action needed.


# T61100 #######################################################################
cTag=5
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(1,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

plot(tag[[cTag]]$data$utime, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
# This fish appears to have been active the entire time.  No action needed.


# T61200 #######################################################################
###############################    this is a bad tag
cTag=9
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(2,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$easting, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(8400,8800)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$easting, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$easting, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$easting, col="red")


# T61300 #######################################################################
###############################    this is a bad tag
cTag=10
if(cDeploymentNames[cTag] == "hb2007"){cmd = md[[1]]} else {cmd = md[[2]]}
par(mfrow=c(2,2))

plot(tagfm[[cTag]]$data$easting, tagfm[[cTag]]$data$northing, pch=19, cex=0.2,
  main = tagfm[[cTag]]$tagName,
  xlim = cmd$plotLimits$easting, ylim = cmd$plotLimits$northing,
)
points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$northing, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(650,750)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$northing, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$northing, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$northing, col="red")
#
plot(tag[[cTag]]$data$datiL, tag[[cTag]]$data$easting, cex=0.7, pch=19,
  main = tagfm[[cTag]]$tagName, col="green",
  xlim=c(tagfm[[cTag]]$data$utime[1], tagfm[[cTag]]$data$utime[length(tagfm[[cTag]]$data$utime)]),
  ylim=c(8400,8700)
)
points(tagf[[cTag]]$data$utime, tagf[[cTag]]$data$easting, cex=0.4, pch=19,
  col="yellow"
)
points(tagfm[[cTag]]$data$utime, tagfm[[cTag]]$data$easting, cex=0.2, pch=19,
  col="black"
)
abline(h=cmd$reefEN$easting, col="red")


## T61300 cTag=10
#cutoffTime = 1224670000
#tag[[cTag]]$data = tag[[cTag]]$data[tag[[cTag]]$data$utime < cutoffTime, ]
#tagf[[cTag]]$data = tagf[[cTag]]$data[tagf[[cTag]]$data$utime < cutoffTime, ]
#tagfm[[cTag]]$data = tagfm[[cTag]]$data[tagfm[[cTag]]$data$utime < cutoffTime, ]


# IF YOU'RE HAPPY WITH THESE RESULTS, SAVE THEM NOW.
### save these results
save("tag", file="C:/zy/Telemetry/R summary files/tag 2011Mar16.rdata")
save("tagf", file="C:/zy/Telemetry/R summary files/tagf 2011Mar16.rdata")
save("tagfm", file="C:/zy/Telemetry/R summary files/tagfm 2011Mar16.rdata")

# load("C:/zy/Telemetry/R summary files/tag 2010Nov11.rdata")
# load("C:/zy/Telemetry/R summary files/tagf 2010Nov11.rdata")
# load("C:/zy/Telemetry/R summary files/tagfm 2011Feb07.rdata")





















################################################################################
### MAKE results 
################################################################################
# z0 combines tagfm into a single long list with data for all 2007/2008 fish.
# It also adds a couple other columns for use in GAM fitting stuff.


library(circular)
library(rimage)
library(MASS)

 
# a dataframe to hold data about fish movement
results1 = data.frame(
  "tagName" = cTagNames,
  "deployment" = cDeploymentNames,
  "weight" = rep(NA, length(cTagNames)),
  "TL" = rep(NA, length(cTagNames)),
  "FL" = rep(NA, length(cTagNames)),
  "relWeight" = rep(NA, length(cTagNames)),
  "numDays" = rep(NA, length(cTagNames)),
  "numHits" = rep(NA, length(cTagNames)),
  "fracHitsPerDay" = rep(NA, length(cTagNames)),
  "medianInterval" = rep(NA, length(cTagNames)),
  "medianDtr" = rep(NA, length(cTagNames)),
  "medianSpeed" = rep(NA, length(cTagNames)),
#  "meanTurning" = rep(NA, length(cTagNames)),
  "kde50" = rep(NA, length(cTagNames)),
  "kde95" = rep(NA, length(cTagNames)),
  stringsAsFactors=FALSE
)
                       
# get fish biometric data.  This contains data recorded in the field on tagging
# day and any recaptures.  It also contains the otolith data.  
biometrics = importBiometricData()


# cycle through each tag/year, calculate things, create table, create plots
par(mfrow=c(3,3))
for (i in 1:length(cTagNames)){
  # fetch deployment specific informaiton
  for (j in 1:length(md)){
    if(results1$deployment[i] == md[[j]]$deployment){
      cmd = md[[j]]  
    } # end if statement 
  } # end for j loop
 
  # for some things I'll want to know things before calculating the minuteMean
  temptag = filterALPSdata(df1=tag[[i]], cnF=1.5, speedF=0.8, minuteMean=F)
 
  # count number of days with tag receptions
  results1$numDays[i] = length(unique(temptag$data$datiL$yday))

  # count total number of position solutions without doing minuteMean
  results1$numHits[i] = nrow(temptag$data)

  # calculate the fraction (position solutions)/(transmissions) each day
  results1$fracHitsPerDay[i] = round(results1$numHits[i] / 
    (results1$numDays[i] * 30*60*24),3)
                              
  # calculate the median interval between position solutions
  # (seconds per day) / (fracHitsPerDay * pings per day) = mean interval
  #results1$meanInterval[i]=round((60*60*24)/(results1$fracHitsPerDay[i]*30*60*24),0)
  thistime = head(temptag$data$utime,-1)
  nexttime = tail(temptag$data$utime,-1)  
  ints = nexttime-thistime
  results1$medianInterval[i] = median(ints)

  # calculate the median distance to reef using minute-averaged data
  results1$medianDtr[i] = round(median(tagfm[[i]]$data$dtr),1)
    
  # calculate the median travel speed using minute-averaged data
  results1$medianSpeed[i] = round(median(tagfm[[i]]$data$speed),3)
  
#  # calculate the mean turning angle
#  # ... for every set of three consecutive points
#
#  # some lists
#  p1 = list() # all but the last two points
#  p2 = list() # all but the first and last points
#  p3 = list() # all but the first two points
#  turns = list() # the list of all turns, this will be one shorter than p1
#  turns1 = list() # this is turns as the class 'circular'
#  difference=list(); uniques=list(); # these are for removing duplicates
#
#  # check for duplicate positions next to each other, since 'bearing.ta()'
#  # ... doesn't allow zero-length moves
#  
#  # find every row that's the same as the one before it.  To do this, look at the 
#  # ... easting/northing columns, take the whole list but the first one (tail())
#  # ... then take the whole list but the last one (head()) and subtact them 
#  # ... this gives the 'difference[[ ]]' list which has east and north columns), 
#  difference[[i]] = tail(tagfm[[i]]$data[,4:5], -1) - head(tagfm[[i]]$data[,4:5], -1)
#  # ... then any column that == 0 in both the east and north columns is dropped
#  uniques[[i]] = 
#    tagfm[[i]]$data[ !((difference[[i]][1] == 0) & (difference[[i]][2] == 0)), ]
#  # now take just the 'easting' and 'northing' columns
#  uniques[[i]] = uniques[[i]][,4:5]  
#  
#
#  # ... take the entire list (of east/north pairs) but the last two
#  p1[[i]] = head(uniques[[i]], -2) 
#  # ... take the entire list but the first and last
#  p2[[i]] = head( tail( uniques[[i]],-1), -1) 
#  # ... take the entire list but the first two
#  p3[[i]] = tail(uniques[[i]], -2) 
#  
#  # now calculate the bearing for each set of 3 pts
#  # ... bearing.ta can accept three 2-column data.frames instead of three length-2 vectors
#  turns[[i]] = bearing.ta(p1[[i]],p2[[i]],p3[[i]],as.deg=TRUE)$ta
#  attr(turns[[i]], "names") = NULL
# 
#  # to make a rose diagram of turning angle
#  turnRadians = turns[[i]] * pi / 180
#  rose.diag(turnRadians, bins=18, pts=F, prop=2,    # there is a rose.diag in both packages 'circular' and 'circStats'
#    main="Distribution of angles turned from forward travel")
#  text(0.8,0.8,"Left Turn")
#  text(0.8,-0.8,"Right Turn")
#  ############## end turning angle
#    
#  # change the class to 'circular'
#  turns[[i]] = as.circular(turns[[i]], units="degrees")
#  results1$meanTurning[i] = mean(turns[[i]])
# 
  # get the biometric data for the fish with this tag
  oneTag = paste("f",biometrics$tagID, sep="")
  oneFish = biometrics[oneTag == results1$tagName[i] ,]
  results1$weight[i] = oneFish$weight1
  results1$TL[i] = oneFish$TL1
  results1$FL[i] = oneFish$FL1
  
  # calculate the relative weight.  I got this equation from Doug.
  #   a = 9.21744 x 10-6; b = 3.04;
  #   (standard weight,g) = a (length, mm)^b
  #   relative weight = (actual weight / standard weight) * 100
  stdWt = 9.21744e-6 * results1$TL[i]^3.04
  results1$relWeight[i] = round(100 * results1$weight[i]*1000 / stdWt,1)
                                             
  # calculate the home ranges
  # THE LIMITS YOU USE WHEN CALCULATING THE KDE AFFECT THE ANSWER, SO FOR ALL 
  # FISH MAKE SURE TO USE THE SAME LIMITS ON EASTING AND NORTHING.
  # There's more in 'chapter 3 part 1.r' on looking at home ranges.
  #
  # Also, I don't want to use days 1 or 2
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
  
  cProb = 0.50
  results1$kde50[i] = round(
    homeRange(easting = hrEasting, 
      northing = hrNorthing,  
      tagName = cTagNames[i], lims = hrlims, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
     prob=cProb, drawplot=FALSE
    ),0
  ) 
  
  cProb = 0.95
  results1$kde95[i] = round(
    homeRange(easting = hrEasting, 
      northing = hrNorthing,  
      tagName = cTagNames[i], lims = hrlims, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
     prob=cProb, drawplot=FALSE
    ),0
  ) 
  
#  # another kind of plot
#  kde = kde2d(tagfm[[i]]$data$easting, tagfm[[i]]$data$northing, n=50,
#    lims=c(md[[2]]$plotLimits$easting, md[[2]]$plotLimits$northing)) 
#
#  kde2dplot2(tagfm[[i]]$data$easting, tagfm[[i]]$data$northing, d=kde, 
#    prob=cProb, pts=FALSE, tagName=cTagNames[i])
#  points(x=cmd$sdlEN$easting, y=cmd$sdlEN$northing, pch=19, col="blue")
#  points(x=cmd$reefEN$easting, y=cmd$reefEN$northing, pch=17, col="red")  
#
  
#  # plot EN
#  plot(tagfm[[i]]$data$easting, tagfm[[i]]$data$northing, 
#    pch=19, cex=0.1, 
#    xlim = md[[2]]$plotLimits$easting, ylim = md[[2]]$plotLimits$northing,
#    main = cTagNames[i]
#  )
#  points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
#  points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)

#  # hexbin EN
#  plot(hexbin(tagfm[[i]]$data$easting, tagfm[[i]]$data$northing, 
#    xbnds = md[[2]]$plotLimits$easting, ybnds = md[[2]]$plotLimits$northing),
#    main = cTagNames[i]
#  )
#  points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19, col="blue", cex=1)
#  points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red", cex=1)
#  
  
#  # time v. northing
#  plot(tagfm[[i]]$data$datiL, tagfm[[i]]$data$northing, 
#    pch=19, cex=0.1, 
#    ylim = md[[2]]$plotLimits$northing,
#    main = cTagNames[i]
#  )
#  abline(h=cmd$sdlEN$northing, col="blue", cex=1)
#  abline(h= cmd$reefEN$northing, col="red", cex=1)
  
  
} # end for i-loop over all tag names

results = results1
resultsByWeight = results[order(results$weight),]
results = resultsByWeight

# if you're happy, save it
save("results", file="C:/zy/Telemetry/R summary files/results 2011Mar16.rdata")




                                                                                      
### Now that 'results' is full, add it to tagfm and rearrange into z0

# functions I'll use later
zoom <- function(...) coord_cartesian(...) # for easier plot limits in ggplot
ss <- function(...) drop.levels(subset(...),reorder=FALSE)

## rearrange data for a single fish
tmpf <- function(x,ssize=1.0) {
  n <- nrow(x$data)
  dat <- x$data
  # pick out only some columns
  dat <- subset(x$data,select=c(utime,datiG,datiL,dod,tod,hod,lunarIndex,
    easting,northing,depth,altitude,dtr,btr,interval,speed,npos,#turnAngle,
    temperature,waterDepth,
    eaaL,magL,dirL,  #eaaM,eaaU,magM,magU,dirM,dirU,tidalHeight
    habType
  ))
  dat$yr = as.factor(substr(x$deployment,3,7)) #ifelse(dat$utime>1.21e9,2008,2007)
  
  # change class                          
  dat$datiG <- as.POSIXct(dat$datiG)
  dat$datiL <- as.POSIXct(dat$datiL)
  # should we be sampling randomly or regularly?
  # could do: x$data[seq(1,n,by=10),] for regular sampling
  if (ssize<1){dat <- dat[sort(sample(1:n,size=round(ssize*n),replace=FALSE)),]}
  # add a column with the fish ID
  data.frame(dat,tagName=x$tagName) 
} # end combineFish()


## subsample the data lists in the FULL data set down to 10% (ssize=10) of original
##  combine the data lists into a single data frame with a factor indicating
##  which fish it's associated with
z0 = do.call(rbind,lapply(tagfm, tmpf, ssize=1))

# add some things
z0$tl = NA
z0$weight = NA
z0$relWeight = NA
for (i in 1:nrow(results)){
  z0$tl[z0$tagName == results$tagName[i]] = results$TL[i]
  z0$weight[z0$tagName == results$tagName[i]] = results$weight[i]
  z0$relWeight[z0$tagName == results$tagName[i]] = results$relWeight[i]
}

z0$day = NA
z0[(z0$tod<=6 | z0$tod>19), ]$day = "night"
z0[(z0$tod>6 & z0$tod<=8), ]$day = "dawn"
z0[(z0$tod>8 & z0$tod<=17), ]$day = "day"
z0[(z0$tod>17 & z0$tod<=19), ]$day = "dusk"
z0$day = as.factor(z0$day)




# IF YOU'RE HAPPY WITH THESE RESULTS, SAVE THEM NOW.
### save these results
save("z0", file="C:/zy/Telemetry/R summary files/z0 2011May02.rdata")

# load("C:/zy/Telemetry/R summary files/z0 2010Nov11.rdata")


