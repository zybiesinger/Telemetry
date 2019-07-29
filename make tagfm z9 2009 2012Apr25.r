# In this file I import SDL and ADCP data for 2009.  
# I filter the SDL data and merge
# it with ADCP data.  The code for this was originally worked out 
# in 'make tagfm z0 2007 2008.r'
#
# This file creates 'tag9', 'tagf9', 'tagfm9', and 'z9'.
# 
# This file also has work to manually cut some data from fish position solutions
# at times when there were PSs that I don't think represent true fish movement,
# like the fish seems to have died because the tag doesn't move at all.
# 
# Next, z9 is constructed.  It mostly rearranges tagfm9 and preps it for use
# with GAM model fitting.


library(rimage)
library(ggplot2)
library(mgcv)

###############################################################################
# if everything below here is to your liking, simply read in the stored 
# tagfm9, z9, and results9. 
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


################################################################################
################################################################################
################################################################################
################################################################################
# gather all the data 
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



### if you're happy with what I've done, simply read in the good tagfm9 data
### here, otherwise recreate it with the following lines
load("C:/zy/school closet/Telemetry/R summary files/depList 2011June25.rData")
load("C:/zy/school closet/Telemetry/R summary files/results9 2011Jun25.rdata")   
load("C:/zy/school closet/Telemetry/R summary files/z9 2011Jun25.rdata")









## import, filter, and merge ALPS data
# my computer can't do all deployments at the same time...so save them as they finish

# for each deployment...
for(i in 1:length(numexpt)){      
  # pick the right metadata
  cmd = md[[ numexpt[i] ]]
  cTagNames = cmd$fishNames
  # read in the correct habitat map  
  rfile = cmd$habmapFileName
  i1 = round(read.jpeg(rfile))
  # plot(i1)
    
  # create some empty lists
  tag9 = list() # raw tag data
  tagf9 = list() # filtered tag data
  tagfm9 = list() # filtered tag data merged with ADCP/tide data
  # for each fishName in the current cDep...import, filter, merge data
  for (j in 1:length(cTagNames)){ 
    print(paste("start",i,j))
    print(Sys.time())
    tag9[[j]] = importALPSdata(deployment=cmd$deployment,tagName=cTagNames[j])
    tagf9[[j]] = filterALPSdata(df1=tag9[[j]], cnF=1.5, speedF=0.8, minuteMean=TRUE)
    tagfm9[[j]] = mergeAlpsAdcpData(alpsData=tagf9[[j]])
    tagfm9[[j]] = mergeSonarData(alpsData=tagfm9[[j]],habmap=i1,reference=cmd$site) 
    print(paste("stop",i,j))
    print(Sys.time())
  }

  # my computer can't do all deployments at the same time...so save them as they finish
  #dep[[i]] = list(tag9=tag9, tagf9=tagf9, tagfm9=tagfm9, cmd=numexpt[i])
  depData = list(tag9=tag9, tagf9=tagf9, tagfm9=tagfm9, dn=numexpt[i])
  
  save("depData", file=paste("C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/dep_", 
    cmd$deployment, " 2013Aug08.rdata", sep=""))
    
  rm(i1)
  rm(depData)  
}

# to look at these saved depData...
load("C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/dep_hb1 2011June23.rdata")

# to compare june 2011 with Aug 2013. I'll just look at tagfm = depData[[3]]
### great these two years match
load(paste("C:/zy/school closet/Telemetry/R summary files/",
  "Experiment tagfm9 and figs/dep_sb4 2011June23.rdata",sep=""))
jun = depData[[3]]  
load(paste("C:/zy/school closet/Telemetry/R summary files/",
  "Experiment tagfm9 and figs/dep_sb4 2013Aug08.rdata",sep=""))
aug =  depData[[3]]  
rm(depData)

par(mfrow=c(4,2))

for(i in 1:length(jun)){
  plot(aug[[i]]$data$utime,aug[[i]]$data$northing,type="l",xlab="",ylab="")
  points(jun[[i]]$data$utime,jun[[i]]$data$northing,type="l",xlab="",ylab="",col="red")
}

bob=vector("numeric",8)
for(i in 1:length(jun)){
  bob[i] = sum(jun[[i]]$data$dtr - aug[[i]]$data$dtr)
}
bob

  


# After this has been finished and files saved for all 7 depolyments
# look at individual deployments, drop the bad fish, clean up data, and 
# save all the tagfm9 data into a single data.frame, something like z0 


# All these files names are...

depNamesList = c("hb1","sb1","sb2","hb2","sb3","hb3","sb4") 
fn9 = paste("C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/dep_", 
  depNamesList, " 2011June23.rdata",sep="")


# This is a summary...after looking at all deployments, these are the tags 
# with consistent, continuous data
# None of them look like they need to have data chopped off the end, one has a 
# big gap.

# cDep = 1.  deployment = hb1. 
# f11:bad. f12:bad. f13:good. f14:good. f15:bad. f16:good. f17:bad. f61000:bad.
hb1 = c(F,F,T,T,F,T,F,F)

# cDep = 2.   deploymetn = sb1           
# all tags : bad
sb1 = rep(F,8)

# cDep = 3.  deployment = sb2. 
# f25:bad. f26:good. f27:bad. f28:good. f29:good. f30:good. f31:good with gap. f61600:bad.
sb2 = c(F,T,F,T,T,T,T,F)

# cDep = 4.  deployment = hb2. 
# f32:bad. f33:good. f34:good. f35:good. f36:good. f37:good. f38:good. f61700:bad.
hb2 = c(F,T,T,T,T,T,T,F)

# cDep = 5.  deployment = sb3. 
# f39:good. f40:good. f41:bad. f42:good. f43:good. f44:bad. f45:bad. f61800:bad.
sb3 = c(T,T,F,T,T,F,F,F)

# cDep = 6.  deployment = hb3. 
# f46:bad. f47:good. f48:good. f50:bad. f51:good. f52:good. f61900:bad. f62000:bad.
#                              f52 might be considered bad, I'll chose good
hb3 = c(F,T,T,F,T,T,F,F)

# cDep = 7.  deployment = sb4. 
# f53:bad. f54:good. f55:bad. f56:good. f57:good. f58:bad. f59:good. f62100:good.
sb4 = c(F,T,F,T,T,F,T,T)
keepers = list(hb1, sb1, sb2, hb2, sb3, hb3, sb4)

# pick one deployment, load and rename it, get metadata for it
cDep = 6  # this should be 1-7 for the number of experimental deployments
load(fn9[cDep])  
cmd = md[[ numexpt[cDep] ]]

# take a look
par(mfrow=c(2,4))
for(i in 1:length(depData$tag9)){
  plot(depData$tagfm9[[i]]$data$datiL, depData$tagfm9[[i]]$data$northing, 
    pch=".", 
    xlim=c(cmd$taggingUtime,cmd$stopUtime), ylim=c(cmd$plotLimits$northing),
    main=paste(depData$tag9[[i]]$deployment,"-",depData$tag9[[i]]$tagName))
  abline(h=cmd$reefEN$northing)
}

# take another look
par(mfrow=c(2,4))
for(i in 1:length(depData$tag9)){
  plot(depData$tag9[[i]]$data$easting, depData$tag9[[i]]$data$northing, pch=".", 
    xlim=c(cmd$plotLimits$easting), ylim=c(cmd$plotLimits$northing),
    main=paste(depData$tag9[[i]]$deployment,"-",depData$tag9[[i]]$tagName))
  points(depData$tagfm9[[i]]$data$easting, depData$tagfm9[[i]]$data$northing, 
    pch=".", col="blue")  
  points(cmd$reefEN$easting, cmd$reefEN$northing, pch=19,col="red")
  points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=19,col="red")
}

# and another look
par(mfrow=c(2,4))
for(i in 1:length(depData$tag9)){
  plot(depData$tag9[[i]]$data$cn, depData$tag9[[i]]$data$northing, pch=".", 
    ylim=c(cmd$plotLimits$northing),
    main=paste(depData$tag9[[i]]$deployment,"-",depData$tag9[[i]]$tagName))
}



# Now that I've looked at them all, just gather the good tagfm9 files into one 
# structure
depList = list()  


for(cDep in 1:length(numexpt)){
  load(fn9[cDep])  
  cmd = md[[ numexpt[cDep] ]]
  # get only the tagfm9 data
  temp1 = depData$tagfm9
  # drop the bad fish
  for (i in length(temp1):1){ # count backwards 
    if (!keepers[[cDep]][i]){temp1[[i]] = NULL} 
  }
  # save only good tagfm9 into depList
  depList[[cDep]] = temp1
  rm(temp1)
}
# now make sure you drop sb2 because there were no fish in it
depList[[2]] = NULL

aug = depList




###############################################################################
# Now save depList as my working structure holding all tagfm9 data
z save("depList", file="C:/zy/school closet/Telemetry/R summary files/depList 2011June25.rData")
z load("C:/zy/school closet/Telemetry/R summary files/depList 2011June25.rData")
# depList is a list of 6 elements, one for each good deployment.
# each top element is another list holding tagfm9 data for each good tag in that deployment





################################################################################
### MAKE results 
################################################################################
# z9 combines tagfm9 into a single long list with data for all 2007/2008 fish.
# It also adds a couple other columns for use in GAM fitting stuff.

cTagNames = c()
cDeploymentNames = c()


for(i in 1:length(depList)){
  for(j in 1:length(depList[[i]])){
    cTagNames = c(cTagNames, depList[[i]][[j]]$tagName)
    cDeploymentNames = c(cDeploymentNames, depList[[i]][[j]]$deployment)
  }
}

 
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
for(i in 1:length(depList)){
  for(j in 1:length(depList[[i]])){
   # figure out which row of tag1 is for this tag
   cRow = which(depList[[i]][[j]]$tagName == results1$tagName)
  
  # fetch deployment specific informaiton
  for (k in 1:length(md)){
    if(results1$deployment[cRow] == md[[k]]$deployment){
      cmd = md[[k]]  
    } # end if statement 
  } # end for k loop
  
  cTagfm9 = depList[[i]][[j]] # this is good, filtered, minute-averaged tagfm9 data
 
 
 
  # for some things I'll want to know things before calculating the minuteMean
  # ... this is a pain for 2009 data...
  # read in the right deployment, extract the unfiltered data for the right tag
  aDir = "C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/"
  aFile = paste("dep_",depList[[i]][[1]]$deployment," 2011June23.rData",sep="")
  fn = paste(aDir, aFile, sep="")
  load(fn)
  # get only tag9 data and drop the full dataset
  temp1 = depData$tag9
  rm(depData)
  for(k in 1:length(temp1)){
    if(depList[[i]][[j]]$tagName == temp1[[k]]$tagName){temp2 = temp1[[k]]}
  }
                                
  temptag = filterALPSdata(df1=temp2, cnF=1.5, speedF=0.8, minuteMean=F)
 
  # count number of days with tag receptions
  results1$numDays[cRow] = length(unique(temptag$data$datiL$yday))

  # count total number of position solutions without doing minuteMean
  results1$numHits[cRow] = nrow(temptag$data)

  # calculate the fraction (position solutions)/(transmissions) each day
  results1$fracHitsPerDay[cRow] = round(results1$numHits[cRow] / 
    (results1$numDays[cRow] * 30*60*24),3)
                              
  # calculate the median interval between position solutions
  # (seconds per day) / (fracHitsPerDay * pings per day) = mean interval
  results1$meanInterval[cRow]=round((60*60*24)/(results1$fracHitsPerDay[cRow]*30*60*24),0)
  thistime = head(temptag$data$utime,-1)
  nexttime = tail(temptag$data$utime,-1)  
  ints = nexttime-thistime
  results1$medianInterval[cRow] = median(ints)

  # calculate the median distance to reef with minuteMean
  results1$medianDtr[cRow] = round(median(cTagfm9$data$dtr),1)
    
  # calculate the median travel speed with minuteMean
  results1$medianSpeed[cRow] = round(median(cTagfm9$data$speed),3)
  
    # get the biometric data for the fish with this tag
    oneTag = paste("f",biometrics$tagID, sep="")
    oneFish = biometrics[oneTag == results1$tagName[cRow] ,]
    results1$weight[cRow] = oneFish$weight1
    results1$TL[cRow] = oneFish$TL1
    results1$FL[cRow] = oneFish$FL1
  
    # calculate the relative weight.  I got this equation from Doug.
    #   a = 9.21744 x 10-6; b = 3.04;
    #   (standard weight,g) = a (length, mm)^b
    #   relative weight = (actual weight / standard weight) * 100
    stdWt = 9.21744e-6 * results1$TL[cRow]^3.04
    results1$relWeight[cRow] = round(100 * results1$weight[cRow]*1000 / stdWt,1)
                                             
    # calculate the home ranges
    # THE LIMITS YOU USE WHEN CALCULATING THE KDE AFFECT THE ANSWER, SO FOR ALL 
    # FISH MAKE SURE TO USE THE SAME LIMITS ON EASTING AND NORTHING.
    # There's more in 'chapter 3 part 1.r' on looking at home ranges.
    #
    # Also, don't use the first two days
    hrEasting = cTagfm9$data$easting[cTagfm9$data$dod > 2]
    hrNorthing = cTagfm9$data$northing[cTagfm9$data$dod > 2]
    
    cProb = 0.50                               
    results1$kde50[cRow] = round(
      homeRange(easting =  hrEasting, 
        northing = hrNorthing,
        n=250,  
        tagName = results1$tagName[cRow], 
        lims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
                 cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2]),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=FALSE
      ),0
    ) 
  
    cProb = 0.95
    results1$kde95[cRow] = round(
      homeRange(easting = hrEasting, 
        northing = hrNorthing, 
        n=250, 
        tagName = results1$tagName[cRow],   
        lims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
                 cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2]), 
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb, drawplot=TRUE
      ),0
    )   
  
  } # end for j-loop over all tag names in the current depList[[i]]
} # end for i-loop over all deployments in depList

results9 = results1
resultsByWeight = results9[order(results9$weight),]
results = resultsByWeight

# if you're happy, save it
z save("results9", file="C:/zy/Telemetry/R summary files/results9 2011Jun25.rdata")
z load("C:/zy/school closet/Telemetry/R summary files/results9 2011Jun25.rdata")

plot(as.factor(results9$deployment),results9$kde95)


# get some summary numbers for the abstract


resultsHB = results9[grepl("hb",results9$deployment),]   
resultsSB = results9[grepl("sb",results9$deployment),]

range(resultsHB$kde50)
range(resultsSB$kde50)

rhb1 = results9[results9$deployment == "hb1",]
rhb2 = results9[results9$deployment == "hb2",]
rhb3 = results9[results9$deployment == "hb3",]
rsb2 = results9[results9$deployment == "sb2",]
rsb3 = results9[results9$deployment == "sb3",]
rsb4 = results9[results9$deployment == "sb4",]


plot(as.factor(results9$deployment),results9$kde50)
points(rep(1,nrow(rhb1)),rhb1$kde50,pch=19)
points(rep(2,nrow(rhb2)),rhb2$kde50,pch=19)
points(rep(3,nrow(rhb3)),rhb3$kde50,pch=19)
points(rep(4,nrow(rsb2)),rsb2$kde50,pch=19)
points(rep(5,nrow(rsb3)),rsb3$kde50,pch=19)
points(rep(6,nrow(rsb4)),rsb4$kde50,pch=19)


meanhb1 = mean(rhb1$kde50)
meanhb2 = mean(rhb2$kde50)
meanhb3 = mean(rhb3$kde50)

meansb2 = mean(rsb2$kde50)
meansb3 = mean(rsb3$kde50)
meansb4 = mean(rsb4$kde50)

hbMeans = c(meanhb1,meanhb2,meanhb3)
sbMeans = c(meansb2,meansb3,meansb4)

mean(c(meanhb1,meanhb2,meanhb3))
mean(c(meansb2,meansb3,meansb4))

# t-test
t.test(hbMeans, y=sbMeans, alternative="g", var.equal=TRUE)
  

















# z9 combines tagfm9 into a single long list with data for all 2009 fish.
# It also adds a couple other columns for use in GAM fitting stuff.
### Now that 'results9' is full, add it to tagfm9 and rearrange into z9

# functions I'll use later
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
  dat$deployment = as.factor(x$deployment) #ifelse(dat$utime>1.21e9,2008,2007)
  
  # change class                          
  dat$datiG <- as.POSIXct(dat$datiG)
  dat$datiL <- as.POSIXct(dat$datiL)
  # should we be sampling randomly or regularly?
  # could do: x$data[seq(1,n,by=10),] for regular sampling
  if (ssize<1){dat <- dat[sort(sample(1:n,size=round(ssize*n),replace=FALSE)),]}
  # add a column with the fish ID
  data.frame(dat,tagName=x$tagName) 
} # end combineFish()


##  combine the data lists into a single data frame with a factor indicating
##  which fish it's associated with
## ...first gather all tagfm9 in each deployment
zdep1 = do.call(rbind,lapply(depList[[1]], tmpf, ssize=1))
zdep2 = do.call(rbind,lapply(depList[[2]], tmpf, ssize=1))
zdep3 = do.call(rbind,lapply(depList[[3]], tmpf, ssize=1))
zdep4 = do.call(rbind,lapply(depList[[4]], tmpf, ssize=1))
zdep5 = do.call(rbind,lapply(depList[[5]], tmpf, ssize=1))
zdep6 = do.call(rbind,lapply(depList[[6]], tmpf, ssize=1))
# ...now put these all together
z9 = rbind(zdep1, zdep2, zdep3, zdep4, zdep5, zdep6)     


# add some things
z9$tl = NA
z9$weight = NA
z9$relWeight = NA
for (i in 1:nrow(results9)){
  z9$tl[z9$tagName == results9$tagName[i]] = results9$TL[i]
  z9$weight[z9$tagName == results9$tagName[i]] = results9$weight[i]
  z9$relWeight[z9$tagName == results9$tagName[i]] = results9$relWeight[i]
}

z9$day = NA
z9[(z9$tod<=6 | z9$tod>19), ]$day = "night"
z9[(z9$tod>6 & z9$tod<=8), ]$day = "dawn"
z9[(z9$tod>8 & z9$tod<=17), ]$day = "day"
z9[(z9$tod>17 & z9$tod<=19), ]$day = "dusk"
z9$day = as.factor(z9$day)
z9$treatment = as.factor(substr(z9$deployment,1,2))




# IF YOU'RE HAPPY WITH THESE RESULTS, SAVE THEM NOW.
### save these results                                         
z save("z9", file="C:/zy/Telemetry/R summary files/z9 2011Jun25.rdata")
z load("C:/zy/school closet/Telemetry/R summary files/z9 2011Jun25.rdata")



# some diagnostic looks at z9
plot(z9$tl, z9$weight, pch=19)
plot(z9$datiL, pch=19)
plot(z9$datiL, z9$easting, pch=".", ylim=c(0,1000))


ggplot(z9, aes(x=easting, y=northing, colour=tagName)) +     #      
  geom_point(alpha=0.05) + 
  #geom_path(data=dC3,aes(group=f,col="red"))      
  theme_bw()
  

# look at daily EN plots of individual fish
ggplot(zdep6, aes(x=easting, y=northing, colour=tagName))+
  geom_point(alpha=0.2) + 
  #coord_cartesian(xlim=c(0,60), ylim=c(0,10)) +
  theme_bw() + 
  facet_wrap(~deployment) +
  scale_x_continuous("Easting (m)")+#, breaks = seq(0,60,by=20)) +
  scale_y_continuous("Northing (m)")#,breaks=c(0,1,2,3,4,6,8,10))
  
  
  
ggplot(z9, aes(x=tod, y=dtr, group=tagName, colour=tagName, fill=tagName)) + 
  geom_point(alpha=0.05) + 
  geom_smooth(method="gam",formula=y~s(x,bs="cc"),lwd=1.3) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +  #,bs="cc"
  #coord_cartesian(xlim=c(1,30), ylim=c(0,10)) +    # range(z0$temperature,na.rm=T)    range(z0$magL,na.rm=T)
  facet_wrap(~deployment) +
  theme_bw() #+ 
  #scale_x_continuous("Lunar Index",breaks = c(1,7,15,23,30)) +
  #scale_y_continuous("Altitude (m)", breaks=c(0,1,2,3,seq(4,10,by=2))) +
  #opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  #opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))
        