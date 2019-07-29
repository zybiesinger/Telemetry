################################################################################
################################################################################
################################################################################
################################################################################
### Testing Array Performance
################################################################################
################################################################################
################################################################################
################################################################################
# This file creates a list of tags to be used by the file:
# 'chapter 2 array validation.r'

### FIRST MAKE A LIST OF ALL THE TAGS I'LL USE...then in a separate file I'll 
###   actually do the cool calculations.


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



# if you like what I already have, just load the saved file

save("tagList", file="C:/zy/The closets/school closet/Telemetry/R summary files/tagList 2011Apr09.rdata")



load("C:/zy/The closets/school closet/Telemetry/R summary files/tagList 2011Apr09.rdata")


source("C:/zy/The closets/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/The closets/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/The closets/school closet/Telemetry/R Data Processing/global metadata.r")


################################################################################
# create the empty list structure
totalNumTags = 112
tagList = rep(list(
  list(tagName="", deployment="", tagLocation=c(), startUtime="", stopUtime="")
),totalNumTags)
  



################################################################################
# All fish study deployments
################################################################################

# I can fill the table automatically for things in md[[ ]] except for spacing
# trials 100 and 150m because they had two beacons at the same locations.  They
# will have to be done by hand.  So far I do the 9 fish study deployments. 


numTagsDone = 0
for (z in 1:9){  # so far I can do this for the first 9 lists in md[[]]
  # which deployment, which list in md[[z]] 
  cmd = z
  # gather the beacon/sentinel tag names
  cTags = c(md[[cmd]]$beaconNames, md[[cmd]]$sentinelNames)

  for (i in 1:length(cTags)){
    # which element of tagList are we on?
    listIndex = i+numTagsDone
  
    # set the tagName, deployment, startUtime, and stopUtime
    tagList[[listIndex]]$tagName = cTags[i] 
    tagList[[listIndex]]$deployment = md[[cmd]]$deployment
    tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
    tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
  
    # figure out where this beacon/sentinel was...
    # ... for all 6 possible locations...
    location = FALSE
    for (j in 1:length(md[[cmd]]$beaconEN$beaconID)){
      # ...figure out which SDL/reef the current beacon/sentinel was on 
      if(cTags[i] == md[[cmd]]$beaconEN$beaconID[j]){
        location = as.character(md[[cmd]]$beaconEN$location[j])
      } 
    } # end j for loop finding the tag location name
    # figure out the easting, northing if it was on the reef
    if(location == "reef"){ 
      tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)    
    }  
    # figure out the easting, northing if it was on an SDL
    for (j in 1:length(md[[cmd]]$sdlEN$ID)){
      if (location == md[[cmd]]$sdlEN$ID[j]){
        tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[j], md[[cmd]]$sdlEN$northing[j])
      }
    } # end j for loop finding the easting, northing
  } # end i for loop over all the tags in this deployment  
  numTagsDone = numTagsDone + length(cTags)  
} # end z loop over md[[]] elements

### Now because hb2008 is funky, fill in the tagLocations by hand...
# b80 = b79200
listIndex = 6  # tagList[[listIndex]]
tagList[[listIndex]]$tagLocation = c(8684.45, 686.0)
# b81 = b79200
listIndex = 7  # tagList[[listIndex]]
tagList[[listIndex]]$tagLocation = c(8684.45, 686.0)
# b85 = b79400
listIndex = 8  # tagList[[listIndex]]
tagList[[listIndex]]$tagLocation = c(8471.48, 699.5)
# b86 = b79400
listIndex = 9  # tagList[[listIndex]]
tagList[[listIndex]]$tagLocation = c(8471.48, 699.5)
# b130 = b79500
listIndex = 10  # tagList[[listIndex]]
tagList[[listIndex]]$tagLocation = c(8582.56, 699.2)

       

################################################################################
# All Spacing Trials
################################################################################
  
# Now for the spacing trials...when there were stationary fish tags
numTagsDone # which element of tagList are we on?  = 43
# 125m spacing trial:   These tags in these places
#   b79400 - north 41  
#   b79500 - center 45
#   f2 - reef
#   f61000 - reef
#   f61500 - outer

cmd = 10

# b79400 on north 41
listIndex = 44
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[1], md[[cmd]]$sdlEN$northing[1])  

# b79500 on center 45
listIndex = 45
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[5], md[[cmd]]$sdlEN$northing[5])  

# f2 at reef
listIndex = 46
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61000 at reef
listIndex = 47
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61500 at outer margin...I get this position estimate 
# from 'GPS position estimates 2010Nov12.r'
listIndex = 48
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[3] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(8529.822,643.8664)  


# 150m spacing trial:   These tags in these places
#   b1 - center 45
#   b2 - north 41
#   b79400 - north 41 
#   b79500 - center 45
#   s79600 - reef
#   f61000 - reef
#   f61500 - outer margin

cmd = 11

# b1 at center 45
listIndex = 49
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[5], md[[cmd]]$sdlEN$northing[5])  

# b2 at north 41
listIndex = 50
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[1], md[[cmd]]$sdlEN$northing[1])  

# b79400 at north 41
listIndex = 51
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[3] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[1], md[[cmd]]$sdlEN$northing[1])  

# b79500 at center 45
listIndex = 52
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[4] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[5], md[[cmd]]$sdlEN$northing[5])   

# s79600 at reef
listIndex = 53
tagList[[listIndex]]$tagName = md[[cmd]]$sentinelNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61000 at reef
listIndex = 54
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61500 at outer margin...I get this position estimate 
# from 'GPS position estimates 2010Nov12.r'
listIndex = 55 
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(8520.005,642.2417)
                                   

# 100m spacing trial:   These tags in these places
#   b1 - center 45
#   b2 - north 41
#   b79400 - north 41 
#   b79500 - center 45
#   s79600 - reef
#   f61000 - reef
#   f61500 - outer margin

cmd = 12

# b1 at center 45
listIndex = 56
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[5], md[[cmd]]$sdlEN$northing[5])  

# b2 at north 41
listIndex = 57
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[1], md[[cmd]]$sdlEN$northing[1])  

# b79400 at north 41
listIndex = 58
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[3] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[1], md[[cmd]]$sdlEN$northing[1])  

# b79500 at center 45
listIndex = 59
tagList[[listIndex]]$tagName = md[[cmd]]$beaconNames[4] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$sdlEN$easting[5], md[[cmd]]$sdlEN$northing[5])   

# s79600 at reef
listIndex = 60
tagList[[listIndex]]$tagName = md[[cmd]]$sentinelNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61000 at reef
listIndex = 61
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[1] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(md[[cmd]]$reefEN$easting, md[[cmd]]$reefEN$northing)  

# f61500 at outer margin...We didn't get any GPS data for this one, use the
# target location...
listIndex = 62 
tagList[[listIndex]]$tagName = md[[cmd]]$fishNames[2] 
tagList[[listIndex]]$deployment = md[[cmd]]$deployment
tagList[[listIndex]]$startUtime = md[[cmd]]$startUtime
tagList[[listIndex]]$stopUtime = md[[cmd]]$stopUtime
tagList[[listIndex]]$tagLocation = c(245128.6-eastingOffset, 3262353-northingOffset)




################################################################################
# All Internal Performance Trials
################################################################################
#
# These trials took place over two days at if4.3 at the beginning of hb1.
# Because each of these tags has it's own start and stop time do them all by hand.
# They don't have an md[[cmd]].  Also note we didn't record their positions
# so here we just use their target locations.
#
# Instead of re-typing all the locations, I'll read them in and convert to UTM...

df1 = read.table("C:/zy/The closets/school closet/Telemetry/R summary files/internal performance locations used 2011Mar25.txt", header = TRUE)
library("PBSmapping") # for converting LL to UTM
targetLL = data.frame(X=-df1$Longitude, Y=df1$Latitude) 
attr(targetLL, "zone") <- 17
attr(targetLL, "projection") <- "LL" 
targetUTM = convUL(targetLL,km=FALSE)  #(longitude, latitude)
df1$easting = targetUTM$X
df1$northing = targetUTM$Y

# now convert date, time to utime
df1$startUtime = unclass(as.POSIXct(strptime(paste(df1$Date, df1$startTime, 
  sep=" "), "%Y%B%d %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))           
df1$stopUtime =  unclass(as.POSIXct(strptime(paste(df1$Date, df1$stopTime, 
  sep=" "), "%Y%B%d %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))  

# lastly, I don't want to use f49 so remove them
df1 = df1[df1$tag != 49, ]

listIndex = 62 # where in the tagList did I stop

for (i in 1:nrow(df1)){
  tagList[[listIndex+i]]$tagName = paste("f",df1$tag[i], sep="")
  tagList[[listIndex+i]]$deployment = "hb1"
  tagList[[listIndex+i]]$startUtime = df1$startUtime[i]
  tagList[[listIndex+i]]$stopUtime = df1$stopUtime[i]
  if(df1$Location[i]=="reef"){
    tagList[[listIndex+i]]$tagLocation = c(md[[3]]$reefEN$easting, 
    md[[3]]$reefEN$northing)
  } else {
    tagList[[listIndex+i]]$tagLocation = c(df1$easting[i]-eastingOffset,
    df1$northing[i]-northingOffset)
  }
}


###############################################################################
# Some plots to check that all is well...YES, ALL IS WELL.
###############################################################################

# sp150
plot(md[[11]]$sdlEN$easting, md[[11]]$sdlEN$northing, pch=19, col="red")
points(md[[11]]$reefEN$easting, md[[11]]$reefEN$northing, pch=19, col="red")
points(tagList[[49]]$tagLocation[1], tagList[[49]]$tagLocation[2], pch=17, col="green")
points(tagList[[50]]$tagLocation[1], tagList[[50]]$tagLocation[2], pch=17, col="green")
points(tagList[[51]]$tagLocation[1], tagList[[51]]$tagLocation[2], pch=17, col="blue")
points(tagList[[52]]$tagLocation[1], tagList[[52]]$tagLocation[2], pch=17, col="blue")
points(tagList[[53]]$tagLocation[1], tagList[[53]]$tagLocation[2], pch=17, col="green")
points(tagList[[54]]$tagLocation[1], tagList[[54]]$tagLocation[2], pch=17, col="green")
points(tagList[[55]]$tagLocation[1], tagList[[55]]$tagLocation[2], pch=17, col="green")

# sp125
points(md[[10]]$sdlEN$easting, md[[10]]$sdlEN$northing, pch=19, col="black")
points(md[[10]]$reefEN$easting, md[[10]]$reefEN$northing, pch=19, col="black")
points(tagList[[44]]$tagLocation[1], tagList[[44]]$tagLocation[2], pch=17, col="brown")
points(tagList[[45]]$tagLocation[1], tagList[[45]]$tagLocation[2], pch=17, col="brown")
points(tagList[[46]]$tagLocation[1], tagList[[46]]$tagLocation[2], pch=17, col="brown")
points(tagList[[47]]$tagLocation[1], tagList[[47]]$tagLocation[2], pch=17, col="yellow")
points(tagList[[48]]$tagLocation[1], tagList[[48]]$tagLocation[2], pch=17, col="brown")

# sp100
points(md[[12]]$sdlEN$easting, md[[12]]$sdlEN$northing, pch=19, col="black")
points(md[[12]]$reefEN$easting, md[[12]]$reefEN$northing, pch=19, col="black")
points(tagList[[56]]$tagLocation[1], tagList[[56]]$tagLocation[2], pch=17, col="grey")
points(tagList[[57]]$tagLocation[1], tagList[[57]]$tagLocation[2], pch=17, col="grey")
points(tagList[[57]]$tagLocation[1], tagList[[58]]$tagLocation[2], pch=17, col="yellow")
points(tagList[[58]]$tagLocation[1], tagList[[59]]$tagLocation[2], pch=17, col="yellow")
points(tagList[[60]]$tagLocation[1], tagList[[60]]$tagLocation[2], pch=17, col="grey")
points(tagList[[61]]$tagLocation[1], tagList[[61]]$tagLocation[2], pch=17, col="yellow")
points(tagList[[62]]$tagLocation[1], tagList[[62]]$tagLocation[2], pch=17, col="grey")

# hb2008
points(md[[2]]$sdlEN$easting, md[[2]]$sdlEN$northing, pch=19, col="red", cex=2)
points(md[[2]]$reefEN$easting, md[[2]]$reefEN$northing, pch=19, col="red", cex=2)
points(tagList[[6]]$tagLocation[1], tagList[[6]]$tagLocation[2], pch=17, col="green")
points(tagList[[7]]$tagLocation[1], tagList[[7]]$tagLocation[2], pch=17, col="green")
points(tagList[[8]]$tagLocation[1], tagList[[8]]$tagLocation[2], pch=17, col="green")
points(tagList[[9]]$tagLocation[1], tagList[[9]]$tagLocation[2], pch=17, col="green")
points(tagList[[10]]$tagLocation[1], tagList[[10]]$tagLocation[2], pch=17, col="green")

# hb2007
points(md[[1]]$sdlEN$easting, md[[1]]$sdlEN$northing, pch=19)
points(md[[1]]$reefEN$easting, md[[1]]$reefEN$northing, pch=19)
points(tagList[[1]]$tagLocation[1], tagList[[1]]$tagLocation[2], pch=17)
points(tagList[[2]]$tagLocation[1], tagList[[2]]$tagLocation[2], pch=17)
points(tagList[[3]]$tagLocation[1], tagList[[3]]$tagLocation[2], pch=17)
points(tagList[[4]]$tagLocation[1], tagList[[4]]$tagLocation[2], pch=17)
points(tagList[[5]]$tagLocation[1], tagList[[5]]$tagLocation[2], pch=17)

# sp125

# hb1
plot(md[[3]]$sdlEN$easting, md[[3]]$sdlEN$northing, pch=19)
points(md[[3]]$reefEN$easting, md[[3]]$reefEN$northing, pch=19)
points(tagList[[11]]$tagLocation[1], tagList[[11]]$tagLocation[2], pch=17, col="red")
points(tagList[[12]]$tagLocation[1], tagList[[12]]$tagLocation[2], pch=17, col="red")
points(tagList[[13]]$tagLocation[1], tagList[[13]]$tagLocation[2], pch=17, col="red")
points(tagList[[14]]$tagLocation[1], tagList[[14]]$tagLocation[2], pch=17, col="red")
points(tagList[[15]]$tagLocation[1], tagList[[15]]$tagLocation[2], pch=17, col="blue")
for(i in 63:112){
  points(tagList[[i]]$tagLocation[1], tagList[[i]]$tagLocation[2], pch=17, col="green")
}

# sb1
plot(md[[4]]$sdlEN$easting, md[[4]]$sdlEN$northing, pch=19)
points(md[[4]]$reefEN$easting, md[[4]]$reefEN$northing, pch=19)
points(tagList[[16]]$tagLocation[1], tagList[[16]]$tagLocation[2], pch=17, col="red")
points(tagList[[17]]$tagLocation[1], tagList[[17]]$tagLocation[2], pch=17, col="red")
points(tagList[[18]]$tagLocation[1], tagList[[18]]$tagLocation[2], pch=17, col="red")
points(tagList[[19]]$tagLocation[1], tagList[[19]]$tagLocation[2], pch=17, col="red")
points(tagList[[20]]$tagLocation[1], tagList[[20]]$tagLocation[2], pch=17, col="red")

# sb2
plot(md[[5]]$sdlEN$easting, md[[5]]$sdlEN$northing, pch=19)
points(md[[5]]$reefEN$easting, md[[5]]$reefEN$northing, pch=19)
points(tagList[[21]]$tagLocation[1], tagList[[21]]$tagLocation[2], pch=17, col="red")
points(tagList[[22]]$tagLocation[1], tagList[[22]]$tagLocation[2], pch=17, col="red")
points(tagList[[23]]$tagLocation[1], tagList[[23]]$tagLocation[2], pch=17, col="red")
points(tagList[[24]]$tagLocation[1], tagList[[24]]$tagLocation[2], pch=17, col="red")
points(tagList[[25]]$tagLocation[1], tagList[[25]]$tagLocation[2], pch=17, col="red")

# hb2
plot(md[[6]]$sdlEN$easting, md[[6]]$sdlEN$northing, pch=19)
points(md[[6]]$reefEN$easting, md[[6]]$reefEN$northing, pch=19)
points(tagList[[26]]$tagLocation[1], tagList[[26]]$tagLocation[2], pch=17, col="red")
points(tagList[[27]]$tagLocation[1], tagList[[27]]$tagLocation[2], pch=17, col="red")
points(tagList[[28]]$tagLocation[1], tagList[[28]]$tagLocation[2], pch=17, col="red")
points(tagList[[29]]$tagLocation[1], tagList[[29]]$tagLocation[2], pch=17, col="red")
points(tagList[[30]]$tagLocation[1], tagList[[30]]$tagLocation[2], pch=17, col="red")

# sb3
plot(md[[7]]$sdlEN$easting, md[[7]]$sdlEN$northing, pch=19)
points(md[[7]]$reefEN$easting, md[[7]]$reefEN$northing, pch=19)
points(tagList[[31]]$tagLocation[1], tagList[[31]]$tagLocation[2], pch=17, col="red")
points(tagList[[32]]$tagLocation[1], tagList[[32]]$tagLocation[2], pch=17, col="red")
points(tagList[[33]]$tagLocation[1], tagList[[33]]$tagLocation[2], pch=17, col="red")
points(tagList[[34]]$tagLocation[1], tagList[[34]]$tagLocation[2], pch=17, col="red")
points(tagList[[35]]$tagLocation[1], tagList[[35]]$tagLocation[2], pch=17, col="red")

# hb3
plot(md[[8]]$sdlEN$easting, md[[8]]$sdlEN$northing, pch=19)
points(md[[8]]$reefEN$easting, md[[8]]$reefEN$northing, pch=19)
points(tagList[[36]]$tagLocation[1], tagList[[36]]$tagLocation[2], pch=17, col="red")
points(tagList[[37]]$tagLocation[1], tagList[[37]]$tagLocation[2], pch=17, col="red")
points(tagList[[38]]$tagLocation[1], tagList[[38]]$tagLocation[2], pch=17, col="red")
points(tagList[[39]]$tagLocation[1], tagList[[39]]$tagLocation[2], pch=17, col="red")

# sb4
plot(md[[9]]$sdlEN$easting, md[[9]]$sdlEN$northing, pch=19)
points(md[[9]]$reefEN$easting, md[[9]]$reefEN$northing, pch=19)
points(tagList[[40]]$tagLocation[1], tagList[[40]]$tagLocation[2], pch=17, col="red")
points(tagList[[41]]$tagLocation[1], tagList[[41]]$tagLocation[2], pch=17, col="red")
points(tagList[[42]]$tagLocation[1], tagList[[42]]$tagLocation[2], pch=17, col="red")
points(tagList[[43]]$tagLocation[1], tagList[[43]]$tagLocation[2], pch=17, col="red")







###############################################################################
# ONCE THE LIST IS FULLY POPULATED GO TO THE NEXT FILE AND DO THE CALCULATIONS...
# NAMED SOMETHING LIKE "chapter 2 array validation.r"
###############################################################################








