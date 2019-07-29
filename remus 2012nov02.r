###########################################################################
###########################################################################
###########################################################################
# This file is created after my dissertation is done and I'm graduated. for
# a publication, want to include the REMUS info. I went back to ALPS, ran it
# using code 120 as a symbol for T61000 and code 130 as a symbol for B79500. 
# These are unique codes for the time REMUS was in the water. This 2008 deployment
# was the time of incorrect SDL settings, so that ALPS is fooled into thinking
# codes are symbols. That means I ran ALPS using these codes as if they were
# complete symbols.

# I ran ALPS and produced the position file for REMUS: T120B130.txt
# I want to import it here and filter as usual. Then I'll compare it with 
# REMUS' own internal position estimate.

# I put all the discovery and figuring it out code at the bottom and put the
# pretty version at the top.

# On 2 Nov 2012, I switched to just using data from 10 Oct 2008 and not from
# 9 Oct 2008. There's a marked difference in the accuracy. I believe it's 
# caused in poorly placed REMUS self positioning beacons. I'm trying to describe
# ALPS error, not REMUS error, so I'll just use the day with (seemingly) good
# REMUS accuracy.


library(PBSmapping)

# get the global code
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


##############################################################################
# import and filter telemetry position estimates of remus
# From md[[2]]
cmd=md[[2]]
alpsDir = paste(cmd$homeDir,
  "/ALPS REMUS 2012Oct02 - copy to hard drive backup",sep="")
# note the 'copy to hard drive back will be removed when backedup 
remus = importALPSdata(deployment=cmd$deployment,tagName="f120",
  beaconName="b130",altDir=alpsDir,offset=TRUE)
# Drop all data not on Friday 10 Oct 2008 EDT. 
# 2008 Oct 09 23:59:59 EDT = 2008 Oct 10 03:59:59 GMT = 1223611199
# 2008 Oct 10 23:59:59 EDT = 2008 Oct 11 03:59:59 GMT = 1223697599
remus$data = remus$data[remus$data$utime>1223611199, ]                                                                      
remus$data = remus$data[remus$data$utime<1223697599, ]                                                                      
remusf = filterALPSdata(df1=remus, cnF=1.5, minuteMean=FALSE)
r = remusf$data[,c(1,8,9)]
# r$utime[121]-r$utime[1] = 9144 = 152.4 min

 
               
##############################################################################
# get REMUS' own position estimates, there are several places...I picked one
cDir = paste("C:/zy/data closet/Telemetry/2008/",
  "2008Oct07 REMUS data and documentation/","AUV 10Oct2008/",sep="")
fn = "ADCP.txt"

# read in and pick desired data from files
setwd(cDir)
adcp = read.csv(fn,header=TRUE)[,c(1,2,17)]
adcp$utime = unclass(as.POSIXct(strptime(paste("10/10/2008", adcp$mission_time), 
  "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))
# remove the attribute
attributes(adcp$utime) <- NULL

# convert from lat/long to utm
temp1 = data.frame(X=adcp$longitude,Y=adcp$latitude)
attr(temp1,"zone") = 17
attr(temp1,"projection") = "LL"
temp1 = convUL(temp1,km=FALSE)
temp1$X = temp1$X - eastingOffset
temp1$Y = temp1$Y - northingOffset
adcp = data.frame(utime=adcp$utime,easting=temp1$X,northing=temp1$Y) 

# 
par(mfrow=c(1,1))
plot(adcp$easting,adcp$northing,type="l")

par(mfrow=c(2,1))
plot(adcp$utime,adcp$easting,type="l")
plot(adcp$utime,adcp$northing,type="l")

#############################################################################
# merge both datasets
# Now I want to put them into one dataframe
d1 = merge(r, adcp, by="utime", all=TRUE)   # r is the Lotek array position estimate of REMUS
names(d1) = c("utime","rE","rN","aE","aN")  # a is REMUS's own position estimate from the adcp

# trim lines without adcp position data. I don't know why REMUS' own positions
# aren't longer than the telemetry data. Four extra lines at the head and tail
# when there are telemetry positions and no adcp positions
d1 = tail(head(d1,-4),-4) 

# interpolate missing adcp positions
d1$aE = na.approx(object=d1$aE, x=d1$utime)  
d1$aN = na.approx(object=d1$aN, x=d1$utime)  

# trim to only the space within the array
d1 = d1[d1$aE > min(cmd$sdlEN$easting) & d1$aE < max(cmd$sdlEN$easting)  ,]
d1 = d1[d1$aN > min(cmd$sdlEN$northing) & d1$aN < max(cmd$sdlEN$northing)  ,]
# trim on top right
d1 = d1[!(d1$aN > 763 & d1$aE > 8621), ]
d1 = d1[!(d1$aN > 733 & d1$aE > 8645), ]
d1 = d1[!(d1$aN > 713 & d1$aE > 8661), ]
# trim on bottom left
d1 = d1[!(d1$aN < 692 & d1$aE < 8478), ]
d1 = d1[!(d1$aN < 688 & d1$aE < 8481), ]
d1 = d1[!(d1$aN < 677 & d1$aE < 8490), ]

# a quick look
plot(d1$aE,d1$aN,pch=19,xlim=c(cmd$plotLimits$easting),
  ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(d1$rE,d1$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")




# make a nice figure for the paper
# NOTE THAT TO GET THE X- AND Y-AXES SCALES TO MATCH I DO A MANUALLY RESIZE 
# THE WINDOW AFTER DRAWING THE PLOT
#
# Make this match figure 6 as much as possible
# the raw plot
par(mar=c(4,5,1,1)+0.1)
# shift easting and northing to start at (0,0)
tempEshift = 8470
tempNshift = 580

plot(cmd$sdlEN$easting-tempEshift, cmd$sdlEN$northing-tempNshift, 
  type="n", bty="l", las=1,
  xlab="", ylab="", cex.lab=1.5, cex.axis=1.5,
  xlim=c(0,220), ylim=c(0,240)
)

# a quick look
points(d1$aE-tempEshift,d1$aN-tempNshift,type="l")
points(d1$rE-tempEshift,d1$rN-tempNshift,pch=19,col="blue",cex=0.7)

# the sdls
points(cmd$sdlEN$easting-tempEshift, cmd$sdlEN$northing-tempNshift, 
  pch=17, col="black", cex=1.5)
mtext(text="Easting (m)",side=1,line=2.5,cex=1.5)
mtext(text="Northing (m)",side=2,line=3.5,cex=1.7)

# add legend  
legend(132,45,
  legend=c("Hydrophone"), 
  pch=c(17), pt.cex=c(1.5), cex=1.0) 






#
#d2 = d1[d1$utime < cuttime,]
#d3 = d1[d1$utime > cuttime,]
#par(mfrow=c(2,1))
#plot(d2$utime,d2$aE,type="b")
#plot(d2$utime,d2$aN,type="b")

#### find the times remus entered and left on each pass
# find time gaps > 1 min
timeStep = c(1,tail(d1$utime,-1) - head(d1$utime,-1))
d1$timeStep = timeStep
timeBreaks = which(d1$timeStep > 60)
d1$pass = rep(1,nrow(d1))
# calcualte time since last break
passNumber = 2
for(i in 1:nrow(d1)){
  if(d1$timeStep[i] > 60){ # if remus has been outside for > 1min...
    # ...then give it a new pass number
    d1$pass[i:nrow(d1)] = passNumber
    passNumber = passNumber + 1
  } # if
} # i
# REMUS passed through the array 9 times = passNumber -1 
ls1 = split(d1,d1$pass)


# look
par(mfrow=c(3,3))
for(i in 1:9){
  plot(ls1[[i]]$aE,ls1[[i]]$aN,type="l")
  points(ls1[[i]]$rE,ls1[[i]]$rN,pch=19,col="blue")
}

# a different type of figure for the paper
# NOTICE THAT YOU HAVE TO MANYALLY STRETCH THE WINDOW TO MAKE THE X AND Y AXES
# HAVE THE SAME SCALE
tempEshift = 8450
tempNshift = 580

par(mfrow=c(4,2))
par(oma=c(3,3,0.5,0.5))
par(mar=rep(0.25,4))
for(i in 1:8){
  plot(cmd$sdlEN$easting-tempEshift, cmd$sdlEN$northing-tempNshift, 
    pch=17, cex=1.5, las=1, xaxt="n", yaxt="n",
    xlab="", ylab="", cex.lab=1.5, cex.axis=1,
    xlim=c(0,250), ylim=c(0,250)
  )

  points(ls1[[i]]$aE-tempEshift,ls1[[i]]$aN-tempNshift,type="l")
  points(ls1[[i]]$rE-tempEshift,ls1[[i]]$rN-tempNshift,pch=19,cex=1.5)
  
  # put ticks on the left axis of figures in the left column
  if(i==1 | i==3 | i==5 | i==7){
    axis(2,labels=FALSE,at=seq(0,250,50),outer=TRUE,line=-0.3)
  }
  # put ticks on the bottom axis of figures in the bottom row
  if (i==7 | i==8){
    axis(1,labels=FALSE,at=seq(0,250,50),outer=TRUE,line=-0.3)
  }
  
}
mtext("Easting", side = 1, line = 1.3, outer = TRUE, adj = 0.5, cex=1.5)
mtext("Northing", side = 2, line = 1.3, outer = TRUE, adj = 0.5, cex=1.5)
      
      
      
      
      


# what's the length of REMUS' course in the array and how long did it take to 
# traverse it?
pathDistance = startE = startN = stopE = stopN = midE = 
    midN = vector(mode="numeric",length=length(ls1))

for(i in c(1,3,5,7)){ # for straight paths
  startE[i] = head(ls1[[i]]$aE,1);startN[i] = head(ls1[[i]]$aN,1)
  stopE[i] = tail(ls1[[i]]$aE,1);  stopN[i] = tail(ls1[[i]]$aN,1)
  midE[i] = ls1[[i]]$aE[nrow(ls1[[i]])/2]; midN[i] = ls1[[i]]$aN[nrow(ls1[[i]])/2]
  pathDistance[i] = sqrt((stopE[i]-startE[i])^2+(stopN[i]-startN[i])^2)
}

trialanderror = c(48,49,54,49)
for(i in c(2,4,6,8)){ # for turning paths
  startE[i] = head(ls1[[i]]$aE,1); startN[i] = head(ls1[[i]]$aN,1)
  stopE[i] = tail(ls1[[i]]$aE,1); stopN[i] = tail(ls1[[i]]$aN,1)
  midE[i] = ls1[[i]]$aE[trialanderror[i/2]]; midN[i] = ls1[[i]]$aN[trialanderror[i/2]]
  pathDistance[i] = sqrt((midE[i]-startE[i])^2+(midN[i]-startN[i])^2) + 
    sqrt((stopE[i]-midE[i])^2+(stopN[i]-midN[i])^2)
}
# so on the straight paths, REMUS traveled ~160 within the array
# on turning paths, it traveled ~228m within the array
# at 1.8 m/s that equals, 89 s and 127 s passage times
# That's pretty close to the times of ~97 s and ~ 145 s calcualted below
#
# a check
sum(pathDistance) / 1.8 # = 828 sec, comapred to 1003 sec from below, using the
                        # larger number estimates a lower  psFrac and is more 
                        #conservative



par(mfrow=c(3,3))
for(i in 1:9){
  plot(ls1[[i]]$aE,ls1[[i]]$aN,pch=19)
  points(c(startE[i],midE[i],stopE[i]),c(startN[i],midN[i],stopN[i]),
    type="b",pch=19,col="red")
}

# how sharp is the turn
pts = cbind(startE,midE,stopE,startN,midN,stopN)[1,]
i=2
p1 = c(startE[i],startN[i]); p2=c(midE[i],midN[i]); p3=c(stopE[i],stopN[i]);
d12 = sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
d13 = sqrt((p1[1]-p3[1])^2+(p1[2]-p3[2])^2)
d23 = sqrt((p2[1]-p3[1])^2+(p2[2]-p3[2])^2)
turnAngle = acos((d12^2 + d23^2 - d13^2)/(2*d12*d23))*180/pi # = 125 degree turn        


# how long is each pass...and...
# how many pings sounded while REMUS was within the array
passTime = vector(mode="numeric",length=length(ls1))
for(i in 1:length(ls1)){
  passTime[i] = ls1[[i]]$utime[nrow(ls1[[i]])] - ls1[[i]]$utime[1]
}
totalTime = sum(passTime) # 1003 seconds = 16.72 min within the array
totalPings = totalTime / 2 # 2 sec burst rate  = 501 pings during 1003 sec
psFrac = sum(!is.na(d1$rE))/totalPings  # 0.2093719

         
# how accurate are position solutions?
d1$error = sqrt( (d1$rE - d1$aE)^2 + (d1$rN - d1$aN)^2 )
d1hist = hist(d1$error,breaks=seq(0,8,0.5))
# how many have error <= 3
sum(d1$error<=3,na.rm=TRUE) / sum(!is.na(d1$error)) # = 0.8761905 = 87.6% within 3m
sum(d1$error<=2,na.rm=TRUE) / sum(!is.na(d1$error)) # = 0.647619 = 64.8% within 2m
min(d1$error, na.rm=TRUE) #=0.3861973m
max(d1$error, na.rm=TRUE) #=7.153768m


# is there spatial pattern in the error
plot(d1$aE,d1$aN,pch=".",col="blue",cex=0.7,
  xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")
points(d1$rE,d1$rN,pch=19,cex=d1$error/4)

par(mfrow=c(2,1))
plot(d1$utime,d1$rN,pch=19)
plot(d1$utime,d1$error,pch=19,col="red")
# greatest errors occur at the beginning, when REMUS was first descending to 
# crusing depth. 



# IN SUMMARY: RESULTS FOR THE PAPER ARE:  CONFIRM THESE BEFORE WRITING INTO PAPER
# 1. Positon Solution Fraction, psFrac -> 
#    1003 sec = 16.72 min, 501 total pings, 105 position solutions, 0.2093719 psFrac
# 2. Position Solution Accuracy
#    0.8761905 of the errors are <3m, min=0.3861973m, max=7.153768m
#    0.647619 of the errors are <2m
#














##############################################################################
##############################################################################
# How do errors or changes in water temperature / sound speed affect psFrac 
# and accuracy?



 

###############################################################################
###############################################################################
### How does the chosen sound speed affect accuracy, PS requency?
# The 2008 deployment had a temperature range of almost 10 degC, but you can
# only pick one sound speed.  The question is how much does the wrong sound 
# speed/water temperature affect PS accuaracy and frequency?

### I'll use the 2008 REMUS 2008Oct09 IF41 deployment.
# I'll increase and decrease the temp by +/- 5 and +/- 10 degC.
# I'll run ALPS five times total for the T120B130. 
# I'll compare the location of estimated positions over time. I'll also
# compare the fraction of PS.
#
# 10 degC = 1490 m/s - m10
# 15 degC = 1507 m/s - m5
# 20 degC = 1521 m/s - center
# 25 degC = 1535 m/s - p5
# 30 degC = 1546 m/s - p10

cmd=md[[2]]
theDirs = c("/ALPS REMUS minus 10 degC","/ALPS REMUS minus 5 degC",
  "/ALPS REMUS plus 5 degC","/ALPS REMUS plus 10 degC") 

#
alpsDir = paste(cmd$homeDir,theDirs[1],sep="")
m10 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[2],sep="")
m5 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[3],sep="")
p5 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[4],sep="")
p10 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
  
# drop all data not on 10 Oct 2008 EDT. 
m10$data = m10$data[(m10$data$utime>1223611199) & (m10$data$utime<1223697599) ,]  
m5$data = m5$data[(m5$data$utime>1223611199) & (m5$data$utime<1223697599) ,] 
p5$data = p5$data[(p5$data$utime>1223611199) & (p5$data$utime<1223697599) ,]  
p10$data = p10$data[(p10$data$utime>1223611199) & (p10$data$utime<1223697599) ,]  

# filter                                                  
rm10 = filterALPSdata(df1=m10, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
rm5 = filterALPSdata(df1=m5, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
rp5 = filterALPSdata(df1=p5, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
rp10 = filterALPSdata(df1=p10, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]

#############################################################################
# merge each dataset with adcp data
# Now I want to put them into one dataframe, and trim the right number of head
# and tail lines.
dm10 = tail(head(merge(rm10, adcp, by="utime", all=TRUE),-4),-4)
names(dm10) = c("utime","rE","rN","aE","aN")
dm5 = tail(head(merge(rm5, adcp, by="utime", all=TRUE),-4),-4)
names(dm5) = c("utime","rE","rN","aE","aN")
dp5 = tail(head(merge(rp5, adcp, by="utime", all=TRUE),-4),-4)
names(dp5) = c("utime","rE","rN","aE","aN")
dp10 = tail(head(merge(rp10, adcp, by="utime", all=TRUE),-4),-4)
names(dp10) = c("utime","rE","rN","aE","aN")

# interpolate missing adcp positions
dm10$aE = na.approx(object=dm10$aE, x=dm10$utime); dm10$aN = na.approx(object=dm10$aN, x=dm10$utime)  
dm5$aE = na.approx(object=dm5$aE, x=dm5$utime); dm5$aN = na.approx(object=dm5$aN, x=dm5$utime)  
dp5$aE = na.approx(object=dp5$aE, x=dp5$utime); dp5$aN = na.approx(object=dp5$aN, x=dp5$utime)  
dp10$aE = na.approx(object=dp10$aE, x=dp10$utime); dp10$aN = na.approx(object=dp10$aN, x=dp10$utime)  

# trim to only the space within the array
dm10 = dm10[dm10$aE > min(cmd$sdlEN$easting) & dm10$aE < max(cmd$sdlEN$easting)  ,]
dm10 = dm10[dm10$aN > min(cmd$sdlEN$northing) & dm10$aN < max(cmd$sdlEN$northing)  ,]
dm5 = dm5[dm5$aE > min(cmd$sdlEN$easting) & dm5$aE < max(cmd$sdlEN$easting)  ,]
dm5 = dm5[dm5$aN > min(cmd$sdlEN$northing) & dm5$aN < max(cmd$sdlEN$northing)  ,]
dp5 = dp5[dp5$aE > min(cmd$sdlEN$easting) & dp5$aE < max(cmd$sdlEN$easting)  ,]
dp5 = dp5[dp5$aN > min(cmd$sdlEN$northing) & dp5$aN < max(cmd$sdlEN$northing)  ,]
dp10 = dp10[dp10$aE > min(cmd$sdlEN$easting) & dp10$aE < max(cmd$sdlEN$easting)  ,]
dp10 = dp10[dp10$aN > min(cmd$sdlEN$northing) & dp10$aN < max(cmd$sdlEN$northing)  ,]

# trim on top right and then on bottom left
dm10=dm10[!(dm10$aN>763&dm10$aE>8621),];dm10=dm10[!(dm10$aN>733&dm10$aE>8645),];dm10=dm10[!(dm10$aN>713&dm10$aE>8661),];
dm10=dm10[!(dm10$aN<692&dm10$aE<8478),];dm10=dm10[!(dm10$aN<688&dm10$aE<8481),];dm10=dm10[!(dm10$aN<677&dm10$aE<8490),];
dm5=dm5[!(dm5$aN>763&dm5$aE>8621),];dm5=dm5[!(dm5$aN>733&dm5$aE>8645),];dm5=dm5[!(dm5$aN>713&dm5$aE>8661),];
dm5=dm5[!(dm5$aN<692&dm5$aE<8478),];dm5=dm5[!(dm5$aN<688&dm5$aE<8481),];dm5=dm5[!(dm5$aN<677&dm5$aE<8490),];
dp5=dp5[!(dp5$aN>763&dp5$aE>8621),];dp5=dp5[!(dp5$aN>733&dp5$aE>8645),];dp5=dp5[!(dp5$aN>713&dp5$aE>8661),];
dp5=dp5[!(dp5$aN<692&dp5$aE<8478),];dp5=dp5[!(dp5$aN<688&dp5$aE<8481),];dp5=dp5[!(dp5$aN<677&dp5$aE<8490),];
dp10=dp10[!(dp10$aN>763&dp10$aE>8621),];dp10=dp10[!(dp10$aN>733&dp10$aE>8645),];dp10=dp10[!(dp10$aN>713&dp10$aE>8661),];
dp10=dp10[!(dp10$aN<692&dp10$aE<8478),];dp10=dp10[!(dp10$aN<688&dp10$aE<8481),];dp10=dp10[!(dp10$aN<677&dp10$aE<8490),];

# look
# minus 10
plot(dm10$aE,dm10$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(dm10$rE,dm10$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# minus 5
plot(dm5$aE,dm5$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(dm5$rE,dm5$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# plus 5
plot(dp5$aE,dp5$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(dp5$rE,dp5$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# plus 10
plot(dp10$aE,dp10$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(dp10$rE,dp10$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")

# okay, I have all the data, now I want to know:
# 1. what are the changes in psFrac?
# 2. what are the changes in accuracy?

#### find the times remus entered and left on each pass
# find time gaps > 1 min
dm10$timeStep = c(1,tail(dm10$utime,-1) - head(dm10$utime,-1))
dm5$timeStep = c(1,tail(dm5$utime,-1) - head(dm5$utime,-1))
dp5$timeStep = c(1,tail(dp5$utime,-1) - head(dp5$utime,-1))
dp10$timeStep = c(1,tail(dp10$utime,-1) - head(dp10$utime,-1))

dm10Breaks = which(dm10$timeStep > 60)
dm5Breaks = which(dm5$timeStep > 60)
dp5Breaks = which(dp5$timeStep > 60)
dp10Breaks = which(dp10$timeStep > 60)

dm10$pass = rep(1,nrow(dm10))
dm5$pass = rep(1,nrow(dm5))
dp5$pass = rep(1,nrow(dp5))
dp10$pass = rep(1,nrow(dp10))

# calcualte time since last break, see above for explanation
passNumber = 2
for(i in 1:nrow(dm10)){if(dm10$timeStep[i]>60){dm10$pass[i:nrow(dm10)]=passNumber;passNumber=passNumber+1}} 
dm10ls = split(dm10,dm10$pass)
passNumber = 2
for(i in 1:nrow(dm5)){if(dm5$timeStep[i]>60){dm5$pass[i:nrow(dm5)]=passNumber;passNumber=passNumber+1}} 
dm5ls = split(dm5,dm5$pass)
passNumber = 2
for(i in 1:nrow(dp5)){if(dp5$timeStep[i]>60){dp5$pass[i:nrow(dp5)]=passNumber;passNumber=passNumber+1}} 
dp5ls = split(dp5,dp5$pass)
passNumber = 2
for(i in 1:nrow(dp10)){if(dp10$timeStep[i]>60){dp10$pass[i:nrow(dp10)]=passNumber;passNumber=passNumber+1}} 
dp10ls = split(dp10,dp10$pass)

# how long is each pass and how many pings sounded while REMUS was within the array
# dm10
passTime = vector(mode="numeric",length=length(dm10ls))
for(i in 1:length(dm10ls)){passTime[i]=dm10ls[[i]]$utime[nrow(dm10ls[[i]])]-dm10ls[[i]]$utime[1]}
dm10TotalTime = sum(passTime); 
dm10TotalPings = floor(dm10TotalTime / 2) 
dm10psFrac = sum(!is.na(dm10$rE))/dm10TotalPings  # 111 ps = 0.2215569 psFrac
# dm5
passTime = vector(mode="numeric",length=length(dm5ls))
for(i in 1:length(dm5ls)){passTime[i]=dm5ls[[i]]$utime[nrow(dm5ls[[i]])]-dm5ls[[i]]$utime[1]}
dm5TotalTime = sum(passTime); 
dm5TotalPings = floor(dm5TotalTime / 2) 
dm5psFrac = sum(!is.na(dm5$rE))/dm5TotalPings  # 108 ps = 0.2155689 psFrac
# dp5
passTime = vector(mode="numeric",length=length(dp5ls))
for(i in 1:length(dp5ls)){passTime[i]=dp5ls[[i]]$utime[nrow(dp5ls[[i]])]-dp5ls[[i]]$utime[1]}
dp5TotalTime = sum(passTime); 
dp5TotalPings = floor(dp5TotalTime / 2) 
dp5psFrac = sum(!is.na(dp5$rE))/dp5TotalPings  # 103 ps = 0.2055888 psFrac
# dp10
passTime = vector(mode="numeric",length=length(dp10ls))
for(i in 1:length(dp10ls)){passTime[i]=dp10ls[[i]]$utime[nrow(dp10ls[[i]])]-dp10ls[[i]]$utime[1]}
dp10TotalTime = sum(passTime); 
dp10TotalPings = floor(dp10TotalTime / 2) 
dp10psFrac = sum(!is.na(dp10$rE))/dp10TotalPings  # 102 ps = 0.2035928 psFrac

# compare 
c(dm10psFrac,dm5psFrac,psFrac,dp5psFrac,dp10psFrac)

# how accurate are position solutions?
dm10$error = sqrt( (dm10$rE - dm10$aE)^2 + (dm10$rN - dm10$aN)^2 )
dm5$error = sqrt( (dm5$rE - dm5$aE)^2 + (dm5$rN - dm5$aN)^2 )
dp5$error = sqrt( (dp5$rE - dp5$aE)^2 + (dp5$rN - dp5$aN)^2 )
dp10$error = sqrt( (dp10$rE - dp10$aE)^2 + (dp10$rN - dp10$aN)^2 )

# look
par(mfrow=c(1,5))
dm10hist = hist(dm10$error,breaks=seq(0,8,0.5))
dm5hist = hist(dm5$error,breaks=seq(0,8,0.5))
hist(d1$error,breaks=seq(0,8,0.5))
dp5hist = hist(dp5$error,breaks=seq(0,8,0.5))
dp10hist = hist(dp10$error,breaks=seq(0,8,0.5))

# how many have error < 3
(dm10.3 = sum(dm10$error<3,na.rm=TRUE) / sum(!is.na(dm10$error))) # = 0.7927928 within 3m
(dm5.3 = sum(dm5$error<3,na.rm=TRUE) / sum(!is.na(dm5$error))) # = 0.8333333 within 3m
(dp5.3 = sum(dp5$error<3,na.rm=TRUE) / sum(!is.na(dp5$error))) # = 0.8640777 within 3m
(dp10.3 = sum(dp10$error<3,na.rm=TRUE) / sum(!is.na(dp10$error))) # = 0.8529412 within 3m
# how many have error < 2
(dm10.2 = sum(dm10$error<2,na.rm=TRUE) / sum(!is.na(dm10$error))) # = 0.5405405 within 3m
(dm5.2 = sum(dm5$error<2,na.rm=TRUE) / sum(!is.na(dm5$error))) # = 0.6111111 within 3m
(dp5.2 = sum(dp5$error<2,na.rm=TRUE) / sum(!is.na(dp5$error))) # = 0.6407767 within 3m
(dp10.2 = sum(dp10$error<2,na.rm=TRUE) / sum(!is.na(dp10$error))) # = 0.5686275 within 3m

# compare
c(dm10.3,dm5.3,dp5.3,dp10.3)
c(dm10.2,dm5.2,dp5.2,dp10.2)

# look 
plot(d1$rN,type="b",pch=19)
points(dm10$rN,type="b",pch=19,col="red")
points(dm5$rN,type="b",pch=19,col="blue")
points(dp5$rN,type="b",pch=19,col="green")
points(dp10$rN,type="b",pch=19,col="yellow")
points(d1$rN,type="b",pch=19,col="black",cex=0.5)

plot(d1$rE,d1$rN,pch=19)
points(dm10$rE,dm10$rN,pch=19,col="red")
points(dm5$rE,dm5$rN,pch=19,col="blue")
points(dp5$rE,dp5$rN,pch=19,col="green")
points(dp10$rE,dp10$rN,pch=19,col="yellow")
points(d1$rE,d1$rN,pch=19,col="black",cex=0.5)



###############################################################################
###############################################################################
### How does the chosen sound speed affect accuracy, PS requency?
# The 2008 deployment had a temperature range of almost 10 degC, but you can
# only pick one sound speed.  The question is how much does the wrong sound 
# speed/water temperature affect PS accuaracy and frequency?

### I'll use the 2008 REMUS 2008Oct09 IF41 deployment.
# I'll increase and decrease the temp by +/- 5 and +/- 10 degC.
# I'll run ALPS five times total for the T120B130. 
# I'll compare the location of estimated positions over time. I'll also
# compare the fraction of PS.
#
# 10 degC = 1490 m/s - m10
# 15 degC = 1507 m/s - m5
# 20 degC = 1521 m/s - center
# 25 degC = 1535 m/s - p5
# 30 degC = 1546 m/s - p10

cmd=md[[2]]
theDirs = c("/ALPS REMUS c45 2m east","/ALPS REMUS c45 4m east",
  "/ALPS REMUS c45 6m east","/ALPS REMUS c45 8m east","/ALPS REMUS c45 10m east") 

#
alpsDir = paste(cmd$homeDir,theDirs[1],sep="")
e2 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[2],sep="")
e4 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[3],sep="")
e6 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[4],sep="")
e8 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
#
alpsDir = paste(cmd$homeDir,theDirs[5],sep="")
e10 = importALPSdata(deployment=cmd$deployment,tagName="f120",beaconName="b130",altDir=alpsDir,offset=TRUE)
  
# drop all data not on 10 Oct 2008 EDT. 
e2$data = e2$data[(e2$data$utime>1223611199) & (e2$data$utime<1223697599) ,]  
e4$data = e4$data[(e4$data$utime>1223611199) & (e4$data$utime<1223697599) ,] 
e6$data = e6$data[(e6$data$utime>1223611199) & (e6$data$utime<1223697599) ,]  
e8$data = e8$data[(e8$data$utime>1223611199) & (e8$data$utime<1223697599) ,]  
e10$data = e10$data[(e10$data$utime>1223611199) & (e10$data$utime<1223697599) ,]  

# filter                                                  
re2 = filterALPSdata(df1=e2, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
re4 = filterALPSdata(df1=e4, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
re6 = filterALPSdata(df1=e6, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
re8 = filterALPSdata(df1=e8, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]
re10 = filterALPSdata(df1=e10, cnF=1.5, minuteMean=FALSE)$data[,c(1,8,9)]

#############################################################################
# merge each dataset with adcp data
# Now I want to put them into one dataframe, and trim the right number of head
# and tail lines.                                                      
de2 = tail(head(merge(re2, adcp, by="utime", all=TRUE),-4),-4)            
names(de2) = c("utime","rE","rN","aE","aN")
de4 = tail(head(merge(re4, adcp, by="utime", all=TRUE),-3),-4)
names(de4) = c("utime","rE","rN","aE","aN")
de6 = tail(head(merge(re6, adcp, by="utime", all=TRUE),-3),-4)
names(de6) = c("utime","rE","rN","aE","aN")
de8 = tail(head(merge(re8, adcp, by="utime", all=TRUE),-3),-4)
names(de8) = c("utime","rE","rN","aE","aN")
de10 = tail(head(merge(re10, adcp, by="utime", all=TRUE),-3),-4)
names(de10) = c("utime","rE","rN","aE","aN")

# interpolate missing adcp positions
de2$aE = na.approx(object=de2$aE, x=de2$utime); de2$aN = na.approx(object=de2$aN, x=de2$utime)  
de4$aE = na.approx(object=de4$aE, x=de4$utime); de4$aN = na.approx(object=de4$aN, x=de4$utime)  
de6$aE = na.approx(object=de6$aE, x=de6$utime); de6$aN = na.approx(object=de6$aN, x=de6$utime)  
de8$aE = na.approx(object=de8$aE, x=de8$utime); de8$aN = na.approx(object=de8$aN, x=de8$utime)  
de10$aE = na.approx(object=de10$aE, x=de10$utime); de10$aN = na.approx(object=de10$aN, x=de10$utime)  

# trim to only the space within the array
de2 = de2[de2$aE > min(cmd$sdlEN$easting) & de2$aE < max(cmd$sdlEN$easting)  ,]
de2 = de2[de2$aN > min(cmd$sdlEN$northing) & de2$aN < max(cmd$sdlEN$northing)  ,]
de4 = de4[de4$aE > min(cmd$sdlEN$easting) & de4$aE < max(cmd$sdlEN$easting)  ,]
de4 = de4[de4$aN > min(cmd$sdlEN$northing) & de4$aN < max(cmd$sdlEN$northing)  ,]
de6 = de6[de6$aE > min(cmd$sdlEN$easting) & de6$aE < max(cmd$sdlEN$easting)  ,]
de6 = de6[de6$aN > min(cmd$sdlEN$northing) & de6$aN < max(cmd$sdlEN$northing)  ,]
de8 = de8[de8$aE > min(cmd$sdlEN$easting) & de8$aE < max(cmd$sdlEN$easting)  ,]
de8 = de8[de8$aN > min(cmd$sdlEN$northing) & de8$aN < max(cmd$sdlEN$northing)  ,]
de10 = de10[de10$aE > min(cmd$sdlEN$easting) & de10$aE < max(cmd$sdlEN$easting)  ,]
de10 = de10[de10$aN > min(cmd$sdlEN$northing) & de10$aN < max(cmd$sdlEN$northing)  ,]

# trim on top right and then on bottom left
de2=de2[!(de2$aN>763&de2$aE>8621),];de2=de2[!(de2$aN>733&de2$aE>8645),];de2=de2[!(de2$aN>713&de2$aE>8661),];
de2=de2[!(de2$aN<692&de2$aE<8478),];de2=de2[!(de2$aN<688&de2$aE<8481),];de2=de2[!(de2$aN<677&de2$aE<8490),];
de4=de4[!(de4$aN>763&de4$aE>8621),];de4=de4[!(de4$aN>733&de4$aE>8645),];de4=de4[!(de4$aN>713&de4$aE>8661),];
de4=de4[!(de4$aN<692&de4$aE<8478),];de4=de4[!(de4$aN<688&de4$aE<8481),];de4=de4[!(de4$aN<677&de4$aE<8490),];
de6=de6[!(de6$aN>763&de6$aE>8621),];de6=de6[!(de6$aN>733&de6$aE>8645),];de6=de6[!(de6$aN>713&de6$aE>8661),];
de6=de6[!(de6$aN<692&de6$aE<8478),];de6=de6[!(de6$aN<688&de6$aE<8481),];de6=de6[!(de6$aN<677&de6$aE<8490),];
de8=de8[!(de8$aN>763&de8$aE>8621),];de8=de8[!(de8$aN>733&de8$aE>8645),];de8=de8[!(de8$aN>713&de8$aE>8661),];
de8=de8[!(de8$aN<692&de8$aE<8478),];de8=de8[!(de8$aN<688&de8$aE<8481),];de8=de8[!(de8$aN<677&de8$aE<8490),];
de10=de10[!(de10$aN>763&de10$aE>8621),];de10=de10[!(de10$aN>733&de10$aE>8645),];de10=de10[!(de10$aN>713&de10$aE>8661),];
de10=de10[!(de10$aN<692&de10$aE<8478),];de10=de10[!(de10$aN<688&de10$aE<8481),];de10=de10[!(de10$aN<677&de10$aE<8490),];

# look
# east 2m
plot(de2$aE,de2$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(de2$rE,de2$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# east 4m
plot(de4$aE,de4$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(de4$rE,de4$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# east 6m
plot(de6$aE,de6$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(de6$rE,de6$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# east 8m
plot(de8$aE,de8$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(de8$rE,de8$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")
# east 10m
plot(de10$aE,de10$aN,pch=19,xlim=c(cmd$plotLimits$easting),ylim=c(cmd$plotLimits$northing))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="green")
points(de10$rE,de10$rN,pch=19,col="blue",cex=0.7)
points(cmd$sdlEN$easting,cmd$sdlEN$northing,type="l")

# okay, I have all the data, now I want to know:
# 1. what are the changes in psFrac?
# 2. what are the changes in accuracy?

#### find the times remus entered and left on each pass
# find time gaps > 1 min
de2$timeStep = c(1,tail(de2$utime,-1) - head(de2$utime,-1))
de4$timeStep = c(1,tail(de4$utime,-1) - head(de4$utime,-1))
de6$timeStep = c(1,tail(de6$utime,-1) - head(de6$utime,-1))
de8$timeStep = c(1,tail(de8$utime,-1) - head(de8$utime,-1))
de10$timeStep = c(1,tail(de10$utime,-1) - head(de10$utime,-1))

de2Breaks = which(de2$timeStep > 60)
de4Breaks = which(de4$timeStep > 60)
de6Breaks = which(de6$timeStep > 60)
de8Breaks = which(de8$timeStep > 60)
de10Breaks = which(de10$timeStep > 60)

de2$pass = rep(1,nrow(de2))
de4$pass = rep(1,nrow(de4))
de6$pass = rep(1,nrow(de6))
de8$pass = rep(1,nrow(de8))
de10$pass = rep(1,nrow(de10))

# calcualte time since last break, see above for explanation
passNumber = 2
for(i in 1:nrow(de2)){if(de2$timeStep[i]>60){de2$pass[i:nrow(de2)]=passNumber;passNumber=passNumber+1}} 
de2ls = split(de2,de2$pass)
passNumber = 2
for(i in 1:nrow(de4)){if(de4$timeStep[i]>60){de4$pass[i:nrow(de4)]=passNumber;passNumber=passNumber+1}} 
de4ls = split(de4,de4$pass)
passNumber = 2
for(i in 1:nrow(de6)){if(de6$timeStep[i]>60){de6$pass[i:nrow(de6)]=passNumber;passNumber=passNumber+1}} 
de6ls = split(de6,de6$pass)
passNumber = 2
for(i in 1:nrow(de8)){if(de8$timeStep[i]>60){de8$pass[i:nrow(de8)]=passNumber;passNumber=passNumber+1}} 
de8ls = split(de8,de8$pass)
passNumber = 2
for(i in 1:nrow(de10)){if(de10$timeStep[i]>60){de10$pass[i:nrow(de10)]=passNumber;passNumber=passNumber+1}} 
de10ls = split(de10,de10$pass)

# how long is each pass and how many pings sounded while REMUS was within the array
# de2
passTime = vector(mode="numeric",length=length(de2ls))
for(i in 1:length(de2ls)){passTime[i]=de2ls[[i]]$utime[nrow(de2ls[[i]])]-de2ls[[i]]$utime[1]}
de2TotalTime = sum(passTime); 
de2TotalPings = floor(de2TotalTime / 2) 
de2psFrac = sum(!is.na(de2$rE))/de2TotalPings  # 101 ps = 0.2015968 psFrac
# de4
passTime = vector(mode="numeric",length=length(de4ls))
for(i in 1:length(de4ls)){passTime[i]=de4ls[[i]]$utime[nrow(de4ls[[i]])]-de4ls[[i]]$utime[1]}
de4TotalTime = sum(passTime); 
de4TotalPings = floor(de4TotalTime / 2) 
de4psFrac = sum(!is.na(de4$rE))/de4TotalPings  # 99 ps = 0.1976048 psFrac
# de6
passTime = vector(mode="numeric",length=length(de6ls))
for(i in 1:length(de6ls)){passTime[i]=de6ls[[i]]$utime[nrow(de6ls[[i]])]-de6ls[[i]]$utime[1]}
de6TotalTime = sum(passTime); 
de6TotalPings = floor(de6TotalTime / 2) 
de6psFrac = sum(!is.na(de6$rE))/de6TotalPings  # 97 ps = 0.1936128 psFrac
# de8
passTime = vector(mode="numeric",length=length(de8ls))
for(i in 1:length(de8ls)){passTime[i]=de8ls[[i]]$utime[nrow(de8ls[[i]])]-de8ls[[i]]$utime[1]}
de8TotalTime = sum(passTime); 
de8TotalPings = floor(de8TotalTime / 2) 
de8psFrac = sum(!is.na(de8$rE))/de8TotalPings  # 94 ps = 0.1876248 psFrac
# de10
passTime = vector(mode="numeric",length=length(de10ls))
for(i in 1:length(de10ls)){passTime[i]=de10ls[[i]]$utime[nrow(de10ls[[i]])]-de10ls[[i]]$utime[1]}
de10TotalTime = sum(passTime); 
de10TotalPings = floor(de10TotalTime / 2) 
de10psFrac = sum(!is.na(de10$rE))/de10TotalPings  # 89 ps = 0.1776447 psFrac

# compare 
c(de2psFrac,de4psFrac,psFrac,de6psFrac,de8psFrac)

# how accurate are position solutions?
de2$error = sqrt( (de2$rE - de2$aE)^2 + (de2$rN - de2$aN)^2 )
de4$error = sqrt( (de4$rE - de4$aE)^2 + (de4$rN - de4$aN)^2 )
de6$error = sqrt( (de6$rE - de6$aE)^2 + (de6$rN - de6$aN)^2 )
de8$error = sqrt( (de8$rE - de8$aE)^2 + (de8$rN - de8$aN)^2 )
de10$error = sqrt( (de10$rE - de10$aE)^2 + (de10$rN - de10$aN)^2 )

# look
par(mfrow=c(1,5))
de2hist = hist(de2$error,breaks=seq(0,10,0.5))
de4hist = hist(de4$error,breaks=seq(0,12,0.5))
de6hist = hist(de6$error,breaks=seq(0,14,0.5))
de8hist = hist(de8$error,breaks=seq(0,16,0.5))
de10hist = hist(de10$error,breaks=seq(0,18,0.5))

# how many have error < 3
(de2.3 = sum(de2$error<3,na.rm=TRUE) / sum(!is.na(de2$error))) # = 0.4158416 within 3m
(de4.3 = sum(de4$error<3,na.rm=TRUE) / sum(!is.na(de4$error))) # = 0.06060606 within 3m
(de6.3 = sum(de6$error<3,na.rm=TRUE) / sum(!is.na(de6$error))) # = 0.01030928 within 3m
(de8.3 = sum(de8$error<3,na.rm=TRUE) / sum(!is.na(de8$error))) # = 0.0106383 within 3m
(de10.3 = sum(de10$error<3,na.rm=TRUE) / sum(!is.na(de10$error))) # = 0 within 3m
# how many have error < 2
(de2.2 = sum(de2$error<2,na.rm=TRUE) / sum(!is.na(de2$error))) # = 0.06930693 within 3m
(de4.2 = sum(de4$error<2,na.rm=TRUE) / sum(!is.na(de4$error))) # = 0.01010101 within 3m
(de6.2 = sum(de6$error<2,na.rm=TRUE) / sum(!is.na(de6$error))) # = 0.01030928 within 3m
(de8.2 = sum(de8$error<2,na.rm=TRUE) / sum(!is.na(de8$error))) # = 0 within 3m
(de10.2 = sum(de10$error<2,na.rm=TRUE) / sum(!is.na(de10$error))) # = 0 within 3m

# compare
c(de2.3,de4.3,de6.3,de6.3,de8.3,de10.3)
c(de2.2,de4.2,de6.2,de6.2,de8.2,de10.2)

# ranges
range(de2$error,na.rm=TRUE)
range(de4$error,na.rm=TRUE)
range(de6$error,na.rm=TRUE)
range(de8$error,na.rm=TRUE)
range(de10$error,na.rm=TRUE)

# look 
plot(d1$rN,type="b",pch=19)
points(de2$rN,type="b",pch=19,col="red")
points(de4$rN,type="b",pch=19,col="blue")
points(de6$rN,type="b",pch=19,col="green")
points(de8$rN,type="b",pch=19,col="yellow")
points(de10$rN,type="b",pch=19,col="brown")
points(d1$rN,type="b",pch=19,col="black",cex=0.5)

plot(d1$rE,d1$rN,pch=19)
points(de2$rE,de2$rN,pch=19,col="red")
points(de4$rE,de4$rN,pch=19,col="blue")
points(de6$rE,de6$rN,pch=19,col="green")
points(de8$rE,de8$rN,pch=19,col="yellow")
points(de10$rE,de10$rN,pch=19,col="brown")
points(d1$rE,d1$rN,pch=19,col="black",cex=0.5)













































##################################################################################
##################################################################################
##################################################################################
##################################################################################
# Discovery and Figuring-it-out code
library(PBSmapping)

# get the global code
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


# now try and do the same things as I do for all fish tags. This is largely 
# follows 'make tagfm z0 2007 2008.r'. But much easier because there's only 
# one file for one tag.

# From md[[2]]
cmd=md[[2]]
alpsDir = paste(cmd$homeDir,
  "/ALPS REMUS 2012Oct02 - copy to hard drive backup",sep="")
# note the 'copy to hard drive back will be removed when backedup 


remus = importALPSdata(deployment=cmd$deployment,tagName="f120",
  beaconName="b130",altDir=alpsDir,offset=TRUE)
# CAREFUL...IS THIS GMT OR EST?...it came from my core function and should be GMT...is that right
# remember that we only want data from 9 and 10 Oct 2008. Drop all 
# data after that. 2008 Oct 10 midnight EST = 2008 Oct 11 0400 GMT =  1223697600
remusStopTime = 1223697600
remus$data = remus$data[remus$data$utime<remusStopTime ,]                                                                      
                                                                      
                                                                      
remusf = filterALPSdata(df1=remus, cnF=1.5, minuteMean=FALSE)
r = remusf$data[,c(1,8,9)]
r$datiG = as.POSIXlt(r$utime, origin="1970-1-1", tz="GMT") 
r$datiL = as.POSIXlt(r$utime, origin="1970-1-1", tz="EST5EDT") 
r = r[,c(1,5,4,2,3)]
# split this by day for better plotting
cuttime = 1223596799 #= 9 Oct2008 23:59:59 GMT

rd1 = r[r$datiG < cuttime,]
rd2 = r[r$datiG > cuttime,]

 
plot(r$easting,r$northing,type="p",pch=19,
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing
)
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")


plot(rd1$easting,rd1$northing,type="p",pch=19,,cex=0.7,
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing
)
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")


plot(r$datiG, r$northing,type="b",ylim=cmd$plotLimits$northing)
abline(h=cmd$reefEN$northing,col="red")
abline(h=cmd$sdlEN$northing,col="blue")

                                  
plot(r$utime, r$easting,type="b",ylim=cmd$plotLimits$easting)
abline(h=cmd$reefEN$easting,col="red")
abline(h=cmd$sdlEN$easting,col="blue")

plot(rd1$datiG, rd1$northing,type="b",ylim=cmd$plotLimits$northing)
abline(h=cmd$reefEN$northing,col="red")
abline(h=cmd$sdlEN$northing,col="blue")


plot(rd2$datiG, rd2$northing,type="b",ylim=cmd$plotLimits$northing)
abline(h=cmd$reefEN$northing,col="red")
abline(h=cmd$sdlEN$northing,col="blue")




# Now to get REMUS' own position estimates
# There appear to be several gps/time files in several locations:
# root = C:\zy\data closet\Telemetry\2008\2008Oct07 REMUS data and documentation\
# 1. ~\AUV 09Oct2008\081009-1st\Sonar: has several survey files...looks like 18-27 are 9 Oct 2008
# 2. ~\AUV 09Oct2008\081009-1st\: ADCP.txt, Bathy.txt, CTD.txt, O2.txt
# 3. ~\AUV 09Oct2008\081009-2nd\Sonar: has several survey files...looks like 18-27 are 9 Oct 2008
# 4. ~\AUV 09Oct2008\081009-2nd\: ADCP.txt, Bathy.txt, CTD.txt, O2.txt
# 5. ~\AUV 09Oct2008\ADCP100908\: 081009.GPS
# 6. ~\AUV 09Oct2008\from Joe email 09Oct2008\: MSN013.GPS

# 7. ~\AUV 10Oct2008\ADCP101008\: MSN013.GPS
# 8. ~\AUV 10Oct2008\from Joe email 09Oct2008\: MSN013.GPS
# 9. ~\AUV 10Oct2008\Sonar: has several survey files...
#        looks like 18-27 are 9 Oct 2008
#        looks like 30-33 are 10 Oct 2008
# 10. ~\AUV 10Oct2008\: ADCP.txt, Bathy.txt, CTD.txt, O2.txt

# everything on the same day should be the same...there was only one GPS signal 
# feeding all the instruments...but I'll do a quick check and use the best one

cDir = "C:/zy/data closet/Telemetry/2008/2008Oct07 REMUS data and documentation/"

# numbers 4. and 10.
cDir1 = paste(cDir,"AUV 09Oct2008/081009-2nd/",sep="")
cDir2 = paste(cDir,"AUV 10Oct2008/",sep="")
fn1 = c("ADCP.txt", "Bathy.txt", "CTD.txt", "O2.txt")
fn2 = c("ADCP.txt", "Bathy.txt", "CTD.txt", "O2.txt")

# read in and pick desired data
setwd(cDir1)
adcp1 = read.csv(fn1[1],header=TRUE)[,c(1,2,17)]
##put all times in time format
adcp1$mission_time = as.POSIXct(strptime(paste("10/9/2008", adcp1$mission_time), 
  "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")




setwd(cDir2)
adcp2 = read.csv(fn2[1],header=TRUE)[,c(1,2,17)]
##put all times in time format
adcp2$mission_time = as.POSIXct(strptime(paste("10/10/2008", adcp2$mission_time), 
  "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")

## on REMUS speed...before dropping it I see this
#mean(adcp1$forward_velocity)
#plot(adcp1$forward_velocity,type="l")
#abline(h=mean(adcp1$forward_velocity))
#abline(h=1.7,col="red")
#
#mean(adcp2$forward_velocity)
#plot(adcp2$forward_velocity,type="l")
#abline(h=mean(adcp2$forward_velocity))
#abline(h=1.7)
## I assume this is m/s but...I'll calculate it later



#combine both days into one data.frame
str(adcp1)
adcp = rbind(adcp1,adcp2)


#ctd$mission_time = as.POSIXct(strptime(paste("8/9/2008", ctd$mission_time), 
#  "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
#o2$mission_time = as.POSIXct(strptime(paste("8/9/2008", o2$Mission.Time), 
#  "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
#
# compare
#nrow(adcp); nrow(bathy); nrow(ctd); nrow(o2);    # not all the same
#sum(adcp$longitude != bathy$longitude) # these are the same
# ctd matches great, just less frequently
# o2 matches very closely
# 
#
# make adcp match 'r' data.frame from above
adcp$utime=unclass(adcp$mission_time)
attributes(adcp$utime) <- NULL
temp1 = data.frame(X=adcp$longitude,Y=adcp$latitude)
attr(temp1,"zone") = 17
attr(temp1,"projection") = "LL"
temp2 = convUL(temp1,km=FALSE)
# add easting and northing offsets
temp3 = temp2
temp3$X = temp2$X - eastingOffset; temp3$Y = temp2$Y - northingOffset


adcp = data.frame(utime=adcp$utime,easting=temp3$X,northing=temp3$Y) 

# add datiG and datiL
adcp$datiG = as.POSIXlt(adcp$utime, origin="1970-1-1", tz="GMT") 
adcp$datiL = as.POSIXlt(adcp$utime, origin="1970-1-1", tz="EST5EDT") 
adcp = adcp[,c(1,5,4,2,3)]


ad1 = adcp[adcp$datiG < cuttime,]
ad2 = adcp[adcp$datiG > cuttime,]

# now look at this and figure out how remus got from the end of the chevron path
# back to the beginning and if is did a 180 degree turn, and how


plot(adcp$easting,adcp$northing,type="l",pch=".",xlim=c(8950,9000),ylim=c(920,960))
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")
# looks like it did come back to here and make a left or a right hand turn to 
# reverse direction. It looks like it came here 6 times.



# calcualte travel speed
top = head(adcp,nrow(adcp)-1)      
bottom = tail(adcp,nrow(adcp)-1)

speed = sqrt( 
  (top$easting - bottom$easting)^2 + (top$northing - bottom$northing)^2 ) /
  (bottom$utime - top$utime)  

plot(speed,type="l")
abline(h=mean(speed),col="red")
remusSpeed = 1.852051  # = mean(speed)   # =~ 1.8 m/s
# so at 1.8 m/s REMUS would cross 100m in 18 s. With a 2s burst rate for the 
# telemetry tag, REMUS would burst about 9 times while within the array


# Below, in the Chevron part I find that one 3988.75m round trip should take
# 7387 seconds (=123 min). I want to look at the adcp position data and 
# confirm it. 

ad1 = adcp[adcp$datiG < cuttime,]
ad2 = adcp[adcp$datiG > cuttime,]

plot(ad1$easting,ad1$northing,pch=19,cex=0.3) # NOTE THAT THESE n= NUMBERS ONLY WORK WITH CHOPPED ADCP DATA
# find repeated positions by trial and error
n=80; points(ad1$easting[n],ad1$northing[n],pch=19,cex=1.5,col="red")
n=281; points(ad1$easting[n],ad1$northing[n],pch=19,cex=1.3,col="blue")
(ad1$utime[281]-ad1$utime[80])/60  # = 2342 sec = 39 min
# hmmm...am I picking the right two points...try the second day with more data


plot(ad2$easting,ad2$northing,pch=19,cex=0.3)
# find repeated positions by trial and error
n=130; points(ad2$easting[n],ad2$northing[n],pch=19,cex=1.5,col="red")
n=331; points(ad2$easting[n],ad2$northing[n],pch=19,cex=1.3,col="blue")
n=532; points(ad2$easting[n],ad2$northing[n],pch=19,cex=1.1,col="yellow")
n=733; points(ad2$easting[n],ad2$northing[n],pch=19,cex=0.7,col="brown")
(ad2$utime[331]-ad2$utime[130])/60  #  = 40.1 min
(ad2$utime[532]-ad2$utime[331])/60  # = 40.4 min
(ad2$utime[733]-ad2$utime[532])/60  # = 40.6 min
# that's longer than the 123 min I expected...one third as long actually...why


#SOMETHING'S NOT RIGHT...GO BACK AND DON'T CHOP ANY DATA OUT AND START AGAIN...

plot(adcp$easting,adcp$northing,type="l",pch=".")
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")

# let's look at just one bottom point
plot(adcp$easting,adcp$northing,type="l",pch=".",
  #xlim=c(8378,8385),ylim=c(436,438))  #left bottom point
  xlim=c(8760,8790),ylim=c(434,437)) # right bottom point

n1 = which(adcp$northing == min(adcp$northing))
points(adcp$easting[n1],adcp$northing[n1],pch=19,col="red")
bob = adcp[adcp$northing < (min(adcp$northing)+1),]
points(bob$easting,bob$northing)  # these are all in this area, not in the left low point
# pick one per pass, by trial and error
n2=c(1218:1221); points(adcp$easting[n2],adcp$northing[n2],type="b",pch=19,col="red")
n3=c(2887:2889); points(adcp$easting[n3],adcp$northing[n3],type="b",pch=19,col="blue")
n4=c(4576:4578); points(adcp$easting[n4],adcp$northing[n4],type="b",pch=19,col="green")
n5=c(6245:6248); points(adcp$easting[n5],adcp$northing[n5],type="b",pch=19,col="yellow")
n6=c(7913:7917); points(adcp$easting[n6],adcp$northing[n6],type="b",pch=19,col="brown")

# great what are the time differences 
(adcp$utime[2887]-adcp$utime[1218])/60  #  = 39 min
(adcp$utime[4576]-adcp$utime[2887])/60  # = 1270 min = overnight
(adcp$utime[6245]-adcp$utime[4576])/60  # = 40 min
(adcp$utime[7913]-adcp$utime[6245])/60  # = 40 min

# OKAY, I'M CONVINCED IT ONLY TOOK 40 MIN TO DO THE WHOLE ROUND TRIP, SO WHY DID 
# I CALCULATE FROM THE chevron DISTANCE AND REMUS SPEED THAT IT SHOULD TAKE 
# 123 MIN? IT MUST BE THAT REMUS SPEED WAS FASTER THAN I THOUGHT OR THE DISTANCE
# IS SHORTER OR SOME CALCULATION ERROR. 
#
# I CALCULATED REMUS SPEED FROM THE ADCP DATA AND IN THIS PICTURE/FILE 
# (07Oct08 Mission statistics.jpg) IT SAYS REMUS SPEED WAS 1.7 m/s. 





# looks like 5 passes on both days
abline(h=min(adcp$northing))


# now pick more points
?so




 



# trim the adcp data to only the times it's within the array
# drop ADCP positions when they are outside the array, use the SDL locations
adcp = adcp[adcp$easting > min(cmd$sdlEN$easting) & adcp$easting < max(cmd$sdlEN$easting)  ,]
adcp = adcp[adcp$northing > min(cmd$sdlEN$northing) & adcp$northing < max(cmd$sdlEN$northing)  ,]

ad1 = adcp[adcp$datiG < cuttime,]
ad2 = adcp[adcp$datiG > cuttime,]

plot(adcp$easting,adcp$northing,pch=19,
  xlim=cmd$plotLimits$easting,ylim=cmd$plotLimits$northing)
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")

# now determine the times when remus was within the array
plot(adcp$utime,adcp$northing,pch=19,ylim=cmd$plotLimits$northing)
plot(ad1$utime,ad1$northing,pch=19,ylim=cmd$plotLimits$northing)
abline(h=cmd$reefEN$northing,col="red")
abline(h=cmd$sdlEN$northing,col="blue")

plot(adcp$utime,adcp$easting,pch=19,ylim=cmd$plotLimits$easting)
plot(ad1$utime,ad1$easting,pch=19,ylim=cmd$plotLimits$easting)
abline(h=cmd$reefEN$easting,col="red")
abline(h=cmd$sdlEN$easting,col="blue")









### OKAY, I'M NOT GOING TO FIGHT MY WAY THROUGH ALL THESE FILES. I'LL JUST 
### USE THE ADCP FILE...EASY TO USE, HOPEFULLY ALREADY IN UTM, MOST COMPREHENSIVE

# One additional check I will do is to compare the 'turn points' Joe sent me
# look in a file named: "Chevron Pattern-coordinates.doc"
# NOTE: THERE APPEARS TO BE A TYPE IN THE LATITUDE OF POINT 9, maybeit should be
# 29N939. The path recorded by REMUS appears to be right.
chevron = data.frame(
  points = c("launch","start",1:14,"pickup"),
  lat = c("29N27.907","29N28.045","29N27.984","29N28.041","29N27.980",
    "29N28.038","29N27.929","29N27.872","29N27.993","29N27.876","29N27.937",
    "29N27.828","29N27.768","29N27.825","29N27.764","29N27.821","29N27.984"),
  long = c("83W37.656","83W37.413","83W37.534","83W37.656","83W37.777",
    "83W37.900","83W37.900","83W37.777","83W37.656","83W37.534","83W37.413",
    "83W37.413","83W37.534","83W37.656","83W37.777","83W37.900","83W37.534"),
    stringsAsFactors=FALSE
)    
# CORRECT AN APPARENT TYPO BY USING THE FOLLOWING LINE CHANGE
chevron$lat[9] = "29N27.930"     # this looks about right
# and...the last point is the pickup point and shouldn't be included in the
# the adcp paths
chevron = chevron[-nrow(chevron),]


# now, what did remus do between the end of the chevron and restarting again?
# where did it go and how did it execute a 180 degree turn?
# to find out, look at the adcp position data before stuff gets chopped off
# ...then come back here
# IT LOOKS LIKE REMUS DID GO BACK TO THE START AND MADE A U-TURNOVER ABOUT 15M
# GREAT.


chevron$latitude = as.numeric(substr(chevron$lat,1,2)) + 
  as.numeric(substr(chevron$lat,4,9))/60
chevron$longitude = -(as.numeric(substr(chevron$long,1,2)) + 
  as.numeric(substr(chevron$long,4,9))/60)
  
plot(chevron$longitude,chevron$latitude,type="b",pch=19,
  col=c("black",rep("red",4),rep("blue",11)))  
#convert to utm
temp1 = data.frame(X=chevron$longitude,Y=chevron$latitude)
attr(temp1,"zone") = 17
attr(temp1,"projection") = "LL"
temp2 = convUL(temp1,km=FALSE)
# add easting and northing offsets
temp3 = temp2
temp3$X = temp2$X - eastingOffset; temp3$Y = temp2$Y - northingOffset
chevron = data.frame(easting=temp3$X,northing=temp3$Y) 

# how long is one complete round trip on the chevron?
# I got some of this info from "Chevron Pattern-Coordinates.doc" in the folder:
# C:\zy\data closet\Telemetry\2008\2008Oct07 REMUS data and documentation 
# One complete trip would be from the second point in 'chevron'. The first point
# is the deployment position. 

plot(chevron$easting,chevron$northing,type="b",pch=19)

# calcualte the distance between two points 
oneDist = sqrt( 
  (chevron$easting[3]-chevron$easting[2])^2 + 
  (chevron$northing[3]-chevron$northing[2])^2)
twoDist = sqrt( 
  (chevron$easting[7]-chevron$easting[6])^2 + 
  (chevron$northing[7]-chevron$northing[6])^2)
lastDist = sqrt( 
  (chevron$easting[16]-chevron$easting[2])^2 + 
  (chevron$northing[16]-chevron$northing[2])^2)  
# say 12 diagonal legs and 2 vertical legs
12*oneDist + 2*twoDist + lastDist # 4002.779 m about the same as below

# calculate the distance between all points, then between the second point and
# the last point
top = head(tail(chevron,-1),-1)
bottom = tail(chevron,-2)
distance = sqrt( 
  (top$easting - bottom$easting)^2 + (top$northing - bottom$northing)^2 ) 
sum(distance)
cbind(top,bottom,distance)

# now add the distance from the end back to the start, from the last point to the
# second point
lastDist = sqrt( 
  (chevron$easting[16]-chevron$easting[2])^2 + 
  (chevron$northing[16]-chevron$northing[2])^2
)
totalDist = sum(distance)+lastDist  # = 3988.75 m
totalTime = totalDist / remusSpeed # this is the time it should take to 
                                   # complete one 3988.75m round trip
                                   # confirm this in the actual data
totalTime/60 # = 2153.693 seconds = 35.89489 minutes 
             # about equal to the empirical 40 min trip time found from adcp data
# NOW I'M HAPPY THAT I KNOW THE TRUE REMUS TRAVEL SPEED...1.7 OR 1.8 m/s



plot(adcp$easting,adcp$northing,type="l",col="red")
points(chevron$easting,chevron$northing,type="l")
points(r$easting,r$northing,,pch=19,col="blue")
points(cmd$reefEN$easting,cmd$reefEN$northing)
points(cmd$sdlEN$easting,cmd$sdlEN$northing)


 
plot(r$easting,r$northing,type="l",
  xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing
)
points(cmd$reefEN$easting,cmd$reefEN$northing)
points(cmd$sdlEN$easting,cmd$sdlEN$northing)
points(adcp$easting,adcp$northing,type="l",col="red")

plot(r$utime, r$northing,type="b",ylim=cmd$plotLimits$northing)
abline(h=c(cmd$reefEN$northing),col="red")
abline(h=c(cmd$sdlEN$northing),col="blue")
points(adcp$utime,adcp$northing,type="b",col="green")
                                  
                                  
plot(adcp$utime, adcp$easting,pch=".")#,ylim=cmd$plotLimits$easting)
points(r$utime, r$easting,pch=19,col="green")
                                               

range(adcp$utime)
range(r$utime)
range(r$easting)
range(adcp$easting)              
                                                   

###################################################################33
# There are three location and location/time data sets
# 1. Telemetry data  = head(r)       = rd1,rd2
# 2. Chevron data  = head(chevron)     # don't really need this
# 3. REMUS'own position estimate...ADCP data     = head(adcp) = ad1, ad2
# 

#do a little work to trim the ADCP position data to the general space and time
range(adcp$datiL)
range(r$datiL)

range(rd1$datiL)
range(ad1$datiL)
range(rd2$datiL)
range(ad2$datiL)
# time overlaps pretty well...they look suspiciously similar, but I checked
# and they appear to be right...now what about space...




plot(adcp$easting,adcp$northing,pch=".",xlim=cmd$plotLimits$easting,
  ylim=cmd$plotLimits$northing)
points(r$easting,r$northing,pch=19,col="red")
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")



# these look good...
par(mfrow=c(2,1))
one = ad1  # ad2
two = rd1   # rd2
plot(one$datiL,one$northing,type="p",pch=".",ylim=cmd$plotLimits$northing,xlab="")
points(two$datiL,two$northing,type="b",pch=19,col="red")
abline(h=c(cmd$reefEN$northing),col="red")
abline(h=c(cmd$sdlEN$northing),col="blue")

plot(one$datiL,one$easting,type="p",pch=".",ylim=cmd$plotLimits$easting,xlab="")
points(two$datiL,two$easting,type="b",pch=19,col="red")
abline(h=c(cmd$reefEN$easting),col="red")
abline(h=c(cmd$sdlEN$easting),col="blue")

par(mfrow=c(1,1))
plot(one$easting,one$northing,type="p",pch=".")  #, xlim=cmd$plotLimit$easting, ylim=cmd$plotLimits$northing)
points(two$easting,two$northing,type="b",pch=19,col="red")
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")




# Now I want to put them into one dataframe
# oh look, I don't really want all these lines yet
d1 = merge(r, adcp, by="utime", all=TRUE)[,c(1,4,5,8,9)] 
names(d1) = c("utime","rEasting","rNorthing","aEasting","aNorthing") 



# this gives a list and mostly there's an adcp a-position for every 
# telemetry r-position. But for the missing positions, I'll interpolate
d1$aEasting = na.approx(object=d1$aEasting, x=d1$utime)  
d1$aNorthing = na.approx(object=d1$aNorthing, x=d1$utime)  

# add datiG and datiL back
d1$datiG = as.POSIXlt(d1$utime, origin="1970-1-1", tz="GMT") 
d1$datiL = as.POSIXlt(d1$utime, origin="1970-1-1", tz="EST5EDT") 
d1 = d1[,c(1,6,7,2:5)]


par(mfrow=c(1,1))
plot(d1$aEasting,d1$aNorthing,type="p",pch=".")  #, xlim=cmd$plotLimit$easting, ylim=cmd$plotLimits$northing)
points(d1$rEasting,d1$rNorthing,type="b",pch=19,col="red")
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")



# drop lines without telemetry data
d2 = d1[!is.na(d1$rEasting),]


par(mfrow=c(1,1))
plot(d2$aEasting,d2$aNorthing,type="p",pch=19,xlim=cmd$plotLimit$easting, ylim=cmd$plotLimits$northing)
points(d2$rEasting,d2$rNorthing,type="p",pch=19,col="red")
points(cmd$reefEN$easting,cmd$reefEN$northing,pch=19,col="red")
points(cmd$sdlEN$easting,cmd$sdlEN$northing,pch=19,col="blue")
points(adcp$easting,adcp$northing,pch=19,cex=0.3,col="green")

... something is wrong here...


error = sqrt( 
  (d2$rEasting - d2$aEasting)^2 + (d2$rNorthing - d2$aNorthing)^2 ) 

plot(error,type="l")




    