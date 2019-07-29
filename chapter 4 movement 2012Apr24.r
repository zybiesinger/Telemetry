#############################################################################
# In this chapter I use 2009 experimental data

library(ggplot2)
library(mgcv)
library(plotrix) # for multhist
library(gdata)  # for drop.levels

source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")

source("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\global variables.r")
source("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\global functions.r")
source("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\global metadata.r")

# read in tagfm, z0, results, and 
#       all depData files which hold tag9, tagf9, tagfm9
# depList holds the good tagfm9 files

load("C:/zy/school closet/Telemetry/R summary files/z9 2011Jun25.rdata")
load("C:/zy/school closet/Telemetry/R summary files/results9 2011Jun25.rdata")
load("C:/zy/school closet/Telemetry/R summary files/depList 2011June25.rdata") # all tagfm data

load("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\R summary files\\z9 2011Jun25.rdata")
load("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\R summary files\\results9 2011Jun25.rdata")
load("C:\\ZyC\\R Data Processing for gag NOT WORKING COPY\\R summary files\\depList 2011June25.rdata") # all tagfm



# some Ben code for prettier ggplots
zmargin = opts(panel.margin=unit(0,"lines"))



## add some things to z9 and results9
################################################################################
# paperID   tagName   deployment 
#1	f26	sb2
#2	f28	sb2
#3	f29	sb2
#4	f30	sb2
#5	f31	sb2
#6	f39	sb3
#7	f40	sb3
#8	f42	sb3
#9	f43	sb3
#10	f54	sb4
#11	f56	sb4
#12	f57	sb4
#13	f59	sb4
#14	f62100	sb4
#15	f13	hb1
#16	f14	hb1
#17	f16	hb1
#18	f33	hb2
#19	f34	hb2
#20	f35	hb2
#21	f36	hb2
#22	f37	hb2
#23	f38	hb2
#24	f47	hb3
#25	f48	hb3
#26	f51	hb3
#27	f52	hb3

# add a column to z9 for paper ID so they're labeled correctly in figures
tempID = 1:27
tempTagName = c("f26","f28","f29","f30","f31","f39","f40","f42","f43","f54",
  "f56","f57","f59","f62100","f13","f14","f16","f33","f34","f35","f36","f37",
  "f38","f47","f48","f51","f52")

z9$ID = 7777777
for (i in 1:length(tempID)){
  z9$ID[ z9$tagName == tempTagName[i] ] = tempID[i] 
} 
z9$ID = as.factor(z9$ID)

# add a column for a pretty treatment name
z9$ttmt = 7777777
z9$ttmt[z9$treatment == "sb"] = "Sand-bottom Landscapes"
z9$ttmt[z9$treatment == "hb"] = "Hard-bottom Landscapes"
z9$ttmt = as.factor(z9$ttmt)


z9$Date = as.Date(z9$datiL)

results9$ttmt = 7777777
results9$ttmt[grepl("sb", results9$deployment)] = "Sand-bottom Landscapes"
results9$ttmt[grepl("hb", results9$deployment)] = "Hard-bottom Landscapes"
results9$ttmt = as.factor(results9$ttmt)

results9$ID = c(15:17, 1:5, 18:23, 6:9, 24:27, 10:14)



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


################################################################################
# some summaries about tagged fish
results9

sbr2 = results9[results9$deployment == "sb2",]
sbr3 = results9[results9$deployment == "sb3",]
sbr4 = results9[results9$deployment == "sb4",]
hbr1 = results9[results9$deployment == "hb1",]
hbr2 = results9[results9$deployment == "hb2",]
hbr3 = results9[results9$deployment == "hb3",]



###############################################################################
## look at distributions of dtr, speed, interval
# pick one and this gives you depData, then make those tag9, tagf9, and tagfm9 normal
load("C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/dep_hb1 2011June23.rdata")
tag = depData$tag9
tagf = depData$tagf9
tagfm = depData$tagfm9 







################################################################################
# look at tagged gag
#
sbres9 = results9[grepl("sb",results9$deployment),]
hbres9 = results9[grepl("hb",results9$deployment),]

sb2res9 = results9[results9$deployment == "sb2",]
sb3res9 = results9[results9$deployment == "sb3",]
sb4res9 = results9[results9$deployment == "sb4",]
hb1res9 = results9[results9$deployment == "hb1",]
hb2res9 = results9[results9$deployment == "hb2",]
hb3res9 = results9[results9$deployment == "hb3",]

# kde ranges
range(sbres9$kde50)
range(sbres9$kde95)
range(hbres9$kde50)
range(hbres9$kde95)

# mean 50% kde on each reef and t.tests
# size of all fish observed on reefs
sb2kde50 = mean(sb2res9$kde50)
sb3kde50 = mean(sb3res9$kde50)
sb4kde50 = mean(sb4res9$kde50)
hb1kde50 = mean(hb1res9$kde50)
hb2kde50 = mean(hb2res9$kde50)
hb3kde50 = mean(hb3res9$kde50)

sbmeans = c(sb2kde50,sb3kde50,sb4kde50)
hbmeans = c(hb1kde50,hb2kde50,hb3kde50)
sbmean = mean(sbmeans)
hbmean = mean(hbmeans)
sbsd = sd(sbmeans)
hbsd = sd(hbmeans)


# test for equal variance
var.test(sbmeans, hbmeans)  # variances are unequal
t.test(sbmeans, y=hbmeans, alternative="l", var.equal=F)
# ...not equal           


# mean kde on each reef and t.tests
# size of all fish observed on reefs
sb2kde95 = mean(sb2res9$kde95)
sb3kde95 = mean(sb3res9$kde95)
sb4kde95 = mean(sb4res9$kde95)
hb1kde95 = mean(hb1res9$kde95)
hb2kde95 = mean(hb2res9$kde95)
hb3kde95 = mean(hb3res9$kde95)

sbmeans = c(sb2kde95,sb3kde95,sb4kde95)
hbmeans = c(hb1kde95,hb2kde95,hb3kde95)

# test for equal variance
var.test(sbmeans, hbmeans)  # variances are equal
t.test(sbmeans, y=hbmeans, alternative="l", var.equal=T)
# ... not equal 
 
# mean median speed on each reef and t.tests
sb2spd = mean(sb2res9$medianSpeed)
sb3spd = mean(sb3res9$medianSpeed)
sb4spd = mean(sb4res9$medianSpeed)
hb1spd = mean(hb1res9$medianSpeed)
hb2spd = mean(hb2res9$medianSpeed)
hb3spd = mean(hb3res9$medianSpeed)

sbmeans = c(sb2spd,sb3spd,sb4spd)
hbmeans = c(hb1spd,hb2spd,hb3spd)

# test for equal variance
var.test(sbmeans, hbmeans)  # variances are equal
t.test(sbmeans, y=hbmeans, alternative="t", var.equal=T)
# ...were not different 



# mean median dtr on each reef and t.tests
sb2dtr = mean(sb2res9$medianDtr)
sb3dtr = mean(sb3res9$medianDtr)
sb4dtr = mean(sb4res9$medianDtr)
hb1dtr = mean(hb1res9$medianDtr)
hb2dtr = mean(hb2res9$medianDtr)
hb3dtr = mean(hb3res9$medianDtr)


sbmeans = c(sb2dtr,sb3dtr,sb4dtr)
hbmeans = c(hb1dtr,hb2dtr,hb3dtr)

# test for equal variance
var.test(sbmeans, hbmeans)  # variances are unequal
t.test(sbmeans, y=hbmeans, alternative="l", var.equal=F)
# ...were different 



###########################################################################
# a pairs plot of tagged individuals
pairs(results9[c(3:5,11:14)], pch=21, 
  bg=c("red","blue")[unclass(results9$ttmt)], #cex.labels=1,
  #labels=c("Weight","Total Length", "Fork Length", "Median DFR","Median Speed", "50% KDE", "95% KDE")
  labels=c("W","LT", "LF", "DFR","SPD", "50%", "95%")

)                     

# a diagnostic pairs plot   ...LOOKING OKAY
pairs(results9[c(6,7,8,9,10,15,16,17)], pch=21,
  bg = c("red","blue")[unclass(results9$ttmt)]
)


###############################################################################
# I want to calculate the number of days at large for recaptured individuals
# ...look in biometrics for dates captured
b1 = importBiometricData()

# pick some data for tagged and recaptured fish, there should be 6
atlarge = b1[(b1$tagged=="yes") & (!is.na(b1$tagged) & !is.na(b1$mmNumber)),
  c(1:4,8,10,22,24:26,30,32)]
atlarge$catch = as.Date(paste(atlarge$year1,"-",atlarge$month1,"-",atlarge$day1,sep=""))
atlarge$recatch = as.Date(paste(atlarge$year2,"-",atlarge$month2,"-",atlarge$day2,sep=""))
atlarge$days = atlarge$recatch - atlarge$catch

al=atlarge[,c(7,5,6,11:15)]

al$deltaLT = al$TL2 - al$TL1
al$relDeltaLT = 100*al$deltaLT/al$TL1
al$relDeltaLTperDay = al$relDeltaLT/as.numeric(al$days)
al$deltaLTperDay = al$deltaLT/as.numeric(al$days)
plot(al$TL1,al$relDeltaLTperDay)

al$deltaW = al$weight2 - (al$weight1*1000) # convert kg to g
al$relDeltaW = 100*al$deltaW/(al$weight1*1000)
al$relDeltaWperDay = al$relDeltaW/as.numeric(al$days)
al$deltaWperDay = al$deltaW/as.numeric(al$days)
plot(al$weight1,al$relDeltaWperDay)





############################################################################
# plot to display 50% and 95% KDE ... like the poster figure

# x-axis positions for each reef
xpps = c(2,3,4,6,7,8)
#MAKE SURE TO STRETCH THE WINDOW OUT TO GET ALL THE X-AXIS WORDS


# first plot the individual fish HR50 estimates...
par(mar=c(5, 6, 1, 2) + 0.1)
plot(1,1, type="n", las=1, bty="l", cex.lab=1.5, cex.axis=1.5,
  xlim=c(1.5,8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
  ylim=c(0,1000)) 
  
axis(1, at = c(xpps), tick = TRUE, cex.axis=1.1,
  labels=c("Deployment 1","Deployment 2","Deployment 3",
    "Deployment 2","Deployment 3","Deployment 4")
)
axis(2,at=seq(0,1000,by=100),las=1,cex.axis=1.5)

# add the x-axis category labels
mtext(text="Hard-bottom Landscapes", side=1, line=3,  at = 3, cex=1.5)
mtext(text="Sand-bottom Landscapes", side=1, line=3,  at = 7, cex=1.5)
mtext("50% KDE Area (    )", side=2, line=4.4, cex=1.7)

mtext(expression(m^2), side=2, line=4.5, adj=0.685, cex=1.7)

# add the points
points(rep(xpps[1], nrow(hb1res9)), hb1res9$kde50, pch=19)
points(rep(xpps[2], nrow(hb2res9)), hb2res9$kde50, pch=19)
points(rep(xpps[3], nrow(hb3res9)), hb3res9$kde50, pch=19)
points(rep(xpps[4], nrow(sb2res9)), sb2res9$kde50, pch=19)
points(rep(xpps[5], nrow(sb3res9)), sb3res9$kde50, pch=19)
points(rep(xpps[6], nrow(sb4res9)), sb4res9$kde50, pch=19)
# now add the mean values of all fish on a reef
points(x=c(xpps[1]-0.2,xpps[1]+0.2), rep(hb1kde50,2), col="red", type="l", lwd=3)
points(x=c(xpps[2]-0.2,xpps[2]+0.2), rep(hb2kde50,2), col="red", type="l", lwd=3)
points(x=c(xpps[3]-0.2,xpps[3]+0.2), rep(hb3kde50,2), col="red", type="l", lwd=3)
points(x=c(xpps[4]-0.2,xpps[4]+0.2), rep(sb2kde50,2), col="red", type="l", lwd=3)
points(x=c(xpps[5]-0.2,xpps[5]+0.2), rep(sb3kde50,2), col="red", type="l", lwd=3)
points(x=c(xpps[6]-0.2,xpps[6]+0.2), rep(sb4kde50,2), col="red", type="l", lwd=3)
# now add the mean value of the reef means (n=3) for each treatment
hbmean = mean(c(hb1kde50,hb2kde50,hb3kde50))
sbmean = mean(c(sb2kde50,sb3kde50,sb4kde50))
hbsd = sd(c(hb1kde50,hb2kde50,hb3kde50))
sbsd = sd(c(sb2kde50,sb3kde50,sb4kde50))
points(x=c(xpps[1]-0.5,xpps[3]+0.5), rep(hbmean,2), col="blue", type="l", lwd=3)
points(x=c(xpps[4]-0.5,xpps[6]+0.5), rep(sbmean,2), col="blue", type="l", lwd=3)
# now boxes showing standard deviations
points(x=c(xpps[1]-0.3,xpps[3]+0.3,xpps[3]+0.3,xpps[1]-0.3,xpps[1]-0.3), 
  y=c(hbmean+hbsd,hbmean+hbsd,hbmean-hbsd,hbmean-hbsd,hbmean+hbsd),
  type="l", col="black")
points(x=c(xpps[4]-0.3,xpps[6]+0.3,xpps[6]+0.3,xpps[4]-0.3,xpps[4]-0.3), 
  y=c(sbmean+sbsd,sbmean+sbsd,sbmean-sbsd,sbmean-sbsd,sbmean+sbsd),
  type="l", col="black")
# now some text reporting means
# hb label
text(4.5,hbmean, expression(paste("412 m" ^2)), cex=1.2, pos=4, col="blue")
text(4.5,hbmean-50, "(n=3)", cex=1.2, pos=4, col="blue")
# sb label
text(4.9,sbmean, expression(paste("54 m" ^2)), cex=1.2, pos=4, col="blue")
text(4.9,sbmean-50, "(n=3)", cex=1.2, pos=4, col="blue")
# add n's for each column
text(xpps[1],100, paste("n=",nrow(hb1res9),sep=""),cex=1.2,col="red")
text(xpps[2],100, paste("n=",nrow(hb2res9),sep=""),cex=1.2,col="red")
text(xpps[3],100, paste("n=",nrow(hb3res9),sep=""),cex=1.2,col="red")
text(xpps[4],120, paste("n=",nrow(sb2res9),sep=""),cex=1.2,col="red")
text(xpps[5],120, paste("n=",nrow(sb3res9),sep=""),cex=1.2,col="red")
text(xpps[6],260, paste("n=",nrow(sb4res9),sep=""),cex=1.2,col="red")



# a legend
xref = 5.5; yref=150; boxWidth=3;

points(xref+0.8, yref+800, pch=19)
text(xref+1, yref+800, "Single fish estimate", pos=4)
points(c(xref+0.6,xref+1), c(yref+740,yref+740), col="red", type="l", lwd=3)
text(xref+1, yref+740, "Mean of all fish on one reef", pos=4)
points(c(xref+0.6,xref+1), c(yref+680,yref+680), col="blue", type="l", lwd=3)
text(xref+1, yref+680, "Mean of 3 reefs", pos=4)
points(c(xref+0.6,xref+1,xref+1,xref+0.6,xref+0.6), 
  c(yref+620,yref+620,yref+620-80,yref+620-80,yref+620), col="black", type="l", lwd=1)
text(xref+1, yref+620-40, "Standard deviation of 3 reefs", pos=4)
points(c(xref+0.4, xref+boxWidth, xref+boxWidth, xref+0.4, xref+0.4), 
  c(yref+840,yref+840,yref+840-340,yref+840-340,yref+840), col="black", type="l", lwd=2)
















############################################################################
# plot to display 50% KDE ... like the poster figure
# NOW DO IT AGAIN WITH DIFFERENT FIGURE SPACING-LAYOUt AND NO COLOR
# x-axis positions for each reef
xpps = c(2,3,4,6,7,8)


# first plot the individual fish HR50 estimates...
par(mar=c(5, 6, 1, 1) + 0.1)
plot(1,1, type="n", las=1, bty="l", cex.lab=1.5, cex.axis=1.5,
  xlim=c(1.5,8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
  ylim=c(0,1000)) 
  
axis(1, at = c(xpps), tick = TRUE, cex.axis=1.3,
  labels=c("HB1","HB2","HB3","SB2","SB3","SB4")
)
axis(2,at=seq(0,1000,by=100),las=1,cex.axis=1.5)

# add the x-axis category labels
mtext(text="Deployment", side=1, line=3, cex=1.5)
mtext("50% KDE Area (    )", side=2, line=3.7, cex=1.7)
mtext(expression(m^2), side=2, line=3.7, adj=0.685, cex=1.7)

# add the points
points(rep(xpps[1], nrow(hb1res9)), hb1res9$kde50, pch=19)
points(rep(xpps[2], nrow(hb2res9)), hb2res9$kde50, pch=19)
points(rep(xpps[3], nrow(hb3res9)), hb3res9$kde50, pch=19)
points(rep(xpps[4], nrow(sb2res9)), sb2res9$kde50, pch=19)
points(rep(xpps[5], nrow(sb3res9)), sb3res9$kde50, pch=19)
points(rep(xpps[6], nrow(sb4res9)), sb4res9$kde50, pch=19)
# now add the mean values of all fish on a reef
points(x=c(xpps[1]-0.4,xpps[1]+0.4), rep(hb1kde50,2), col="black", type="l", lwd=2)
points(x=c(xpps[2]-0.4,xpps[2]+0.4), rep(hb2kde50,2), col="black", type="l", lwd=2)
points(x=c(xpps[3]-0.4,xpps[3]+0.4), rep(hb3kde50,2), col="black", type="l", lwd=2)
points(x=c(xpps[4]-0.4,xpps[4]+0.4), rep(sb2kde50,2), col="black", type="l", lwd=2)
points(x=c(xpps[5]-0.4,xpps[5]+0.4), rep(sb3kde50,2), col="black", type="l", lwd=2)
points(x=c(xpps[6]-0.4,xpps[6]+0.4), rep(sb4kde50,2), col="black", type="l", lwd=2)
# now add the mean value of the reef means (n=3) for each treatment
hbmean = mean(c(hb1kde50,hb2kde50,hb3kde50))
sbmean = mean(c(sb2kde50,sb3kde50,sb4kde50))
hbsd = sd(c(hb1kde50,hb2kde50,hb3kde50))
sbsd = sd(c(sb2kde50,sb3kde50,sb4kde50))
points(x=c(xpps[1]-0.5,xpps[3]+1), rep(hbmean,2), col="black", type="l", lty=2, lwd=3)
points(x=c(xpps[4]-1,xpps[6]+0.5), rep(sbmean,2), col="black", type="l", lty=2, lwd=3)
# now boxes showing standard deviations
points(x=c(xpps[1]-0.3,xpps[3]+0.3,xpps[3]+0.3,xpps[1]-0.3,xpps[1]-0.3), 
  y=c(hbmean+hbsd,hbmean+hbsd,hbmean-hbsd,hbmean-hbsd,hbmean+hbsd),
  type="l", col="black")
points(x=c(xpps[4]-0.3,xpps[6]+0.3,xpps[6]+0.3,xpps[4]-0.3,xpps[4]-0.3), 
  y=c(sbmean+sbsd,sbmean+sbsd,sbmean-sbsd,sbmean-sbsd,sbmean+sbsd),
  type="l", col="black")
# now some text reporting means
# hb label
text(5,hbmean+10, expression(paste("412 m" ^2)), cex=1.7, pos=4, col="black")
#text(5,hbmean-20, "(n=3)", cex=1.2, pos=4, col="black")
# sb label
text(3.7,sbmean+10, expression(paste("54 m" ^2)), cex=1.7, pos=4, col="black")
#text(4,sbmean-20, "(n=3)", cex=1.2, pos=4, col="black")
# add n's for each column
text(xpps[1],610, paste("n=",nrow(hb1res9),sep=""),cex=1.7,col="black")
text(xpps[2],1020, paste("n=",nrow(hb2res9),sep=""),cex=1.7,col="black")
text(xpps[3],740, paste("n=",nrow(hb3res9),sep=""),cex=1.7,col="black")
text(xpps[4],120, paste("n=",nrow(sb2res9),sep=""),cex=1.7,col="black")
text(xpps[5],140, paste("n=",nrow(sb3res9),sep=""),cex=1.7,col="black")
text(xpps[6],260, paste("n=",nrow(sb4res9),sep=""),cex=1.7,col="black")

# a legend
xref = 4.5; yref=150; boxWidth=4.25;

points(xref+0.9, yref+800, pch=19)
text(xref+1.3, yref+800, "Single fish estimate", pos=4, cex=1.3)
points(c(xref+0.6,xref+1.3), c(yref+740,yref+740), type="l", lwd=2, cex=1.3)
text(xref+1.3, yref+740, "Mean of all fish on ", pos=4, cex=1.3)
text(xref+1.5, yref+740-40, "one reef", pos=4, cex=1.3)
points(c(xref+0.6,xref+1.3), c(yref+640,yref+640), col="black", type="l", lty=2, lwd=3)
text(xref+1.3, yref+640, "Mean of 3 reefs", pos=4, cex=1.3)
points(c(xref+0.6,xref+1.3,xref+1.3,xref+0.6,xref+0.6), 
  c(yref+580,yref+580,yref+580-80,yref+580-80,yref+580), col="black", type="l", lwd=1)
text(xref+1.3, yref+580-20, "Standard deviation", pos=4, cex=1.3)
text(xref+1.5, yref+580-60, "of 3 reefs", pos=4, cex=1.3)
points(c(xref+0.4, xref+boxWidth, xref+boxWidth, xref+0.4, xref+0.4), 
  c(yref+840,yref+840,yref+840-380,yref+840-380,yref+840), col="black", type="l", lwd=2)




#############################################################################
## plot to display 95% KDE ... like the poster figure
## NOW DO IT AGAIN WITH DIFFERENT FIGURE SPACING-LAYOUt AND NO COLOR
## x-axis positions for each reef
#xpps = c(2,3,4,6,7,8)
#
## first plot the individual fish HR50 estimates...
#par(mar=c(5, 6, 1, 2) + 0.1)
#plot(1,1, type="n", las=1, bty="l", cex.lab=1.5, cex.axis=1.5,
#  xlim=c(1.5,8.5), xaxt="n", yaxt="n", xlab="", ylab="", 
#  ylim=c(0,6500)) 
#  
#axis(1, at = c(xpps), tick = TRUE, cex.axis=1.3,
#  labels=c("HB1","HB2","HB3","SB2","SB3","SB4")
#)
#axis(2,at=seq(0,6500,by=500),las=1,cex.axis=1.5)
#
## add the x-axis category labels
#mtext(text="Deployment", side=1, line=3, cex=1.5)
#mtext("95% KDE Area (    )", side=2, line=4.3, cex=1.7)
#mtext(expression(m^2), side=2, line=4.3, adj=0.685, cex=1.7)
#
## add the points
#points(rep(xpps[1], nrow(hb1res9)), hb1res9$kde95, pch=19)
#points(rep(xpps[2], nrow(hb2res9)), hb2res9$kde95, pch=19)
#points(rep(xpps[3], nrow(hb3res9)), hb3res9$kde95, pch=19)
#points(rep(xpps[4], nrow(sb2res9)), sb2res9$kde95, pch=19)
#points(rep(xpps[5], nrow(sb3res9)), sb3res9$kde95, pch=19)
#points(rep(xpps[6], nrow(sb4res9)), sb4res9$kde95, pch=19)
## now add the mean values of all fish on a reef
#points(x=c(xpps[1]-0.4,xpps[1]+0.4), rep(hb1kde95,2), col="black", type="l", lwd=2)
#points(x=c(xpps[2]-0.4,xpps[2]+0.4), rep(hb2kde95,2), col="black", type="l", lwd=2)
#points(x=c(xpps[3]-0.4,xpps[3]+0.4), rep(hb3kde95,2), col="black", type="l", lwd=2)
#points(x=c(xpps[4]-0.4,xpps[4]+0.4), rep(sb2kde95,2), col="black", type="l", lwd=2)
#points(x=c(xpps[5]-0.4,xpps[5]+0.4), rep(sb3kde95,2), col="black", type="l", lwd=2)
#points(x=c(xpps[6]-0.4,xpps[6]+0.4), rep(sb4kde95,2), col="black", type="l", lwd=2)
## now add the mean value of the reef means (n=3) for each treatment              
#hbmean = mean(c(hb1kde95,hb2kde95,hb3kde95))
#sbmean = mean(c(sb2kde95,sb3kde95,sb4kde95))
#hbsd = sd(c(hb1kde95,hb2kde95,hb3kde95))
#sbsd = sd(c(sb2kde95,sb3kde95,sb4kde95))
#points(x=c(xpps[1]-0.5,xpps[3]+1), rep(hbmean,2), col="black", type="l", lty=2, lwd=3)
#points(x=c(xpps[4]-1,xpps[6]+0.5), rep(sbmean,2), col="black", type="l", lty=2, lwd=3)
## now boxes showing standard deviations
#points(x=c(xpps[1]-0.3,xpps[3]+0.3,xpps[3]+0.3,xpps[1]-0.3,xpps[1]-0.3), 
#  y=c(hbmean+hbsd,hbmean+hbsd,hbmean-hbsd,hbmean-hbsd,hbmean+hbsd),
#  type="l", col="black")
#points(x=c(xpps[4]-0.3,xpps[6]+0.3,xpps[6]+0.3,xpps[4]-0.3,xpps[4]-0.3), 
#  y=c(sbmean+sbsd,sbmean+sbsd,sbmean-sbsd,sbmean-sbsd,sbmean+sbsd),
#  type="l", col="black")
## now some text reporting means
## hb label
#text(5,hbmean+30, expression(paste("412 m" ^2)), cex=1.2, pos=4, col="black")
#text(5,hbmean-20, "(n=3)", cex=1.2, pos=4, col="black")
## sb label
#text(4,sbmean+30, expression(paste("54 m" ^2)), cex=1.2, pos=4, col="black")
#text(4,sbmean-20, "(n=3)", cex=1.2, pos=4, col="black")
## add n's for each column         
#zyzyzy - move the text to the right place...
#text(xpps[1],610, paste("n=",nrow(hb1res9),sep=""),cex=1.2,col="black")
#text(xpps[2],1020, paste("n=",nrow(hb2res9),sep=""),cex=1.2,col="black")
#text(xpps[3],740, paste("n=",nrow(hb3res9),sep=""),cex=1.2,col="black")
#text(xpps[4],120, paste("n=",nrow(sb2res9),sep=""),cex=1.2,col="black")
#text(xpps[5],140, paste("n=",nrow(sb3res9),sep=""),cex=1.2,col="black")
#text(xpps[6],260, paste("n=",nrow(sb4res9),sep=""),cex=1.2,col="black")
#
## a legend
#xref = 5; yref=150; boxWidth=3.6;
#
#points(xref+0.9, yref+800, pch=19)
#text(xref+1.3, yref+800, "Single fish estimate", pos=4)
#points(c(xref+0.6,xref+1.3), c(yref+740,yref+740), type="l", lwd=2)
#text(xref+1.3, yref+740, "Mean of all fish on ", pos=4)
#text(xref+1.5, yref+740-40, "one reef", pos=4)
#points(c(xref+0.6,xref+1.3), c(yref+640,yref+640), col="black", type="l", lty=2, lwd=3)
#text(xref+1.3, yref+640, "Mean of 3 reefs", pos=4)
#points(c(xref+0.6,xref+1.3,xref+1.3,xref+0.6,xref+0.6), 
#  c(yref+580,yref+580,yref+580-80,yref+580-80,yref+580), col="black", type="l", lwd=1)
#text(xref+1.3, yref+580-20, "Standard deviation", pos=4)
#text(xref+1.5, yref+580-60, "of 3 reefs", pos=4)
#points(c(xref+0.4, xref+boxWidth, xref+boxWidth, xref+0.4, xref+0.4), 
#  c(yref+840,yref+840,yref+840-380,yref+840-380,yref+840), col="black", type="l", lwd=2)
#



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

    
system.time(boottest <- bootHR(dat=z9, by="day", #by = "2 days" or "week"
  prob=0.95, lims=hrlims,nboot=5, progressbar=TRUE, bootplot=FALSE))

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
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA),
  list(tagName = NA, originalHR = NA, bootResults = NA) # this one for all fish combined
)




depList[[1]] is same as tagfm, each depList holds a list of all tags in one deployment

tagfm=depList[[1]]

#####################################################################

cProb = 0.95   
howManyDeps = 6
howManyTags = length(unique(z9$tagName))
tagCount = 1
#hrboots= replicate(howManyTags,list(tagName = NA, originalHR = NA, bootResults = NA))  


for (depLoop in 1:length(depList)){
  tagfm = depList[[depLoop]]        
  for (i in 1:length(tagfm)){
    # grab things pertinent to this tag
    for (j in 1:length(md)){
      if(tagfm[[i]]$deployment == md[[j]]$deployment){ cmd = md[[j]] } 
    } # end for j loop
  
    # name
    hrboots[[tagCount]]$tagName = tagfm[[i]]$tagName
  
    # find HR estimate will all data
    # ...except I don't want to use days 1 or 2
    hrUtime = tagfm[[i]]$data$utime
    hrEasting = tagfm[[i]]$data$easting
    hrNorthing = tagfm[[i]]$data$northing
 
    #hrlims are formed like c(minEasting, maxEasting, minNorthing, maxNorthing)
    cLims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
      cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2])
 
    hrboots[[tagCount]]$originalHR = homeRange(hrEasting, hrNorthing, 
      prob=cProb, lims=cLims, tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, 
      sdlEN=cmd$sdlEN, pts=TRUE, drawplot=FALSE)  

    # now the boot strapping...first put the hrUtime, hrEasting, hrNorthing into a dataframe
    hrDF = data.frame(easting=hrEasting, northing=hrNorthing)
    hrboots[[tagCount]]$bootResults = bootHR(tagfm[[i]]$data, #by = "day" or "2 days" or "week"
      prob=cProb, lims=cLims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
      tagName=tagfm[[i]]$tagName, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
      drawplot=FALSE
    )

    tagCount = tagCount + 1
  } # end i
} # end depLoop
 
## now calculate the 95% KDE for all fish combined...z0 is required
#i=length(tagfm)+1
#
#everyfish =     the answer I get with z0 seems funky...do it again with this
#  = 
#hrboots[[i]]$originalHR = homeRange(z0$easting, z0$northing, 
#  prob=cProb, lims=hrlims, tagName="All", reefEN=cmd$reefEN, 
#  sdlEN=cmd$sdlEN, pts=FALSE, drawplot=FALSE)  
#
#hrboots[[i]]$bootResults =bootHR(z0, #by = "day" or "2 days" or "week"
#  prob=cProb, lims=hrlims, nboot=1000, progressbar=TRUE, bootplot=FALSE,
#  tagName="All", reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, pts=FALSE, 
#  drawplot=FALSE
#)

cTag = 1
plot(rep(cTag,length(hrboots[[cTag]]$bootResults)), hrboots[[cTag]]$bootResults,
  pch=19, xlim=c(0,length(hrboots)), ylim=c(10,10000))
  
for(i in 1:length(hrboots)){
  points(rep(i,length(hrboots[[i]]$bootResults)), hrboots[[i]]$bootResults,
    pch=19)
}
points(1:27,c(hrboots[[1]]$originalHR, hrboots[[2]]$originalHR,hrboots[[3]]$originalHR,
  hrboots[[4]]$originalHR, hrboots[[5]]$originalHR, hrboots[[6]]$originalHR,
  hrboots[[7]]$originalHR, hrboots[[8]]$originalHR, hrboots[[9]]$originalHR,
  hrboots[[10]]$originalHR, hrboots[[11]]$originalHR, hrboots[[12]]$originalHR,
  hrboots[[13]]$originalHR, hrboots[[14]]$originalHR, hrboots[[15]]$originalHR,
  hrboots[[16]]$originalHR, hrboots[[17]]$originalHR, hrboots[[18]]$originalHR,
  hrboots[[19]]$originalHR, hrboots[[20]]$originalHR, hrboots[[21]]$originalHR,
  hrboots[[22]]$originalHR, hrboots[[23]]$originalHR, hrboots[[24]]$originalHR,
  hrboots[[25]]$originalHR, hrboots[[26]]$originalHR, hrboots[[27]]$originalHR
  ),
  pch=19,col="red")

# now pick out the 95% confidence interval (2.5% and 97.5%  points), that is 
# 25, 975 our of 1000 different runs
cTag=4
#plot(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)])
#abline(v=c(25,975))
#abline(h=hrboots[[cTag]]$originalHR)
cTag=27
results9[cTag,c(1,14,17)]   # results9[,c(1,14,17)]
print(hrboots[[cTag]]$tagName)
hrboots[[cTag]]$originalHR
c(hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][25],
  hrboots[[cTag]]$bootResults[order(hrboots[[cTag]]$bootResults)][975]
)

                    
save("hrboots", file="C:/zy/school closet/Telemetry/R summary files/hrboots_z9 2013July13.rdata")
load("C:/zy/school closet/Telemetry/R summary files/hrboots_z9 2013July13.rdata")
 
 
 





################################################################################
#  histograms of DTR, SPDG, and time interval/detections per day        
                                                       
# dtr histogram
par(mfrow=c(2,4))
bks = c(20,20,20,30,30)
for (i in 1:length(tagfm)){                                 
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$dtr,  freq=T, main=tagfm[[cTag]]$tagName, #breaks=bks[i],
    xlim=c(0,100))
  abline(v=median(tagfm[[cTag]]$data$dtr), col="red")
}

hist(z9$dtr, breaks=90, freq=F, #xlim=c(0,100), ylim=c(0,0.05),
  col="grey", las=1,
  main="",
  xlab="Distance to the Reef (m)")
abline(v=median(z9$dtr),lwd=4)



# speed histogram
par(mfrow=c(2,4))
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$speed, breaks=30, freq=F, main=tagfm[[cTag]]$tagName)
  abline(v=mean(tagfm[[cTag]]$data$speed), col="red")
}


hist(z9$speed, breaks=30, freq=F, col="grey", las=1, 
  main="",
  xlab="Gag Speed (m/s)")
abline(v=median(z9$speed),col="black",lwd=4)



# I want histograms of all good fish dtr and spd
# gather dtr of all good fish in all deployments

alldtrsb = c(); alldtrhb = c();
allspeedsb = c(); allspeedhb = c()
for (i in 1:length(depList)){
  for (j in 1:length(depList[[i]])){
    if(grepl("hb",depList[[i]][[j]]$deployment)){ 
      alldtrhb = c(alldtrhb,depList[[i]][[j]]$data$dtr)  
      allspeedhb = c(allspeedhb,depList[[i]][[j]]$data$speed)  
    } else if (grepl("sb",depList[[i]][[j]]$deployment)){ 
      alldtrsb = c(alldtrsb,depList[[i]][[j]]$data$dtr)  
      allspeedsb = c(allspeedsb,depList[[i]][[j]]$data$speed)  
    } else { print("The deployment isn't HB or SB") }
  }
}

# histograms
par(mfrow=c(1,2))
par(mar=c(5,5.5,2,0))
bks = seq(0,130,by=2)
bob = hist(alldtrsb,freq=F,col="grey",las=1,main="Sand-bottom Landscapes",
  cex.lab=1.5, cex.axis=1.5, cex.main=2, breaks=bks, yaxt="n",
  xlim=c(0,130), ylim=c(0,0.16),
  xlab="Distance From the Reef (m)", ylab="")
abline(v=median(alldtrsb),col="black",lwd=4) # median = 4.23m
mtext("Density",side=2,line=4,cex=1.7)
text(10,0.155,labels="a)",cex=1.5)
axis(1,seq(0,120,by=10), cex.axis=1.5)
axis(2,seq(0,0.16,by=0.02), las=1, cex.axis=1.5)

par(mar=c(5,4.5,2,1))
hist(alldtrhb,freq=F,col="grey",las=1,main="Hard-bottom Landscapes",
  cex.lab=1.5, cex.axis=1.5, cex.main=2, breaks=bks, yaxt="n",
  xlim=c(0,130), ylim=c(0,0.05),
  xlab="Distance From the Reef (m)", ylab="")
abline(v=median(alldtrhb),col="black",lwd=4) # median = 17.69 # all fish combined =7.56m
mtext("Density",side=2,line=4,cex=1.7)
text(1,0.049,labels="b)",cex=1.5)   
axis(1,seq(0,120,by=10), cex.axis=1.5)
axis(2,seq(0,0.05,by=0.01), las=1, cex.axis=1.5)



### speed
par(mfrow=c(1,2))
par(mar=c(5,5.5,2,0))
bks = seq(0,0.8,by=0.01)

hist(allspeedsb,freq=F,col="grey",las=1,main="Sand-bottom Landscapes",
  cex.lab=1.5, cex.axis=1.5, , cex.main=2,
  #xlim=c(0,130), 
  breaks=bks,
  xlab="Travel Speed (m/s)", ylab="")
abline(v=median(allspeedsb),col="black",lwd=4) # median = 0.146475
mtext("Density",side=2,line=3,cex=1.7)
text(0.04,3.9,labels="a)",cex=1.5)   
axis(1,seq(0,0.8,by=0.1), cex.axis=1.5)

par(mar=c(5,4.5,2,1))
hist(allspeedhb,freq=F,col="grey",las=1,main="Hard-bottom Landscapes",
  cex.lab=1.5, cex.axis=1.5, cex.main=2,
  #xlim=c(0,130), ylim=c(0,0.04),
  breaks=bks,
  xlab="Travel Speed (m/s)", ylab="")
abline(v=median(allspeedhb),col="black",lwd=4) # median = 0.14846 # all fish combined =0.14727m
mtext("Density",side=2,line=3,cex=1.7)
text(0.04,4.9,labels="b)",cex=1.5)   
axis(1,seq(0,0.8,by=0.1), cex.axis=1.5)


### detections per day and detection interval
### For each fish I want to know how many detections per day and the mean
### detection interval.
# interval histogram




results9$numHits/results9$numDays

par(mfrow=c(2,4))
for (i in 1:length(tagfm)){
  cTag=i; #cmd=md[[2]]
  hist(tagfm[[cTag]]$data$interval, breaks=30, freq=F, main=tagfm[[cTag]]$tagName)
  abline(v=mean(tagfm[[cTag]]$data$interval), col="red")
}


temp1 = z9$interval[z9$interval < 60]
hist(temp1, breaks=200, freq=F, col="grey", las=1, 
  main="",
  xlab="Interval (s)")
abline(v=c(mean(temp1),median(temp1)),col=c("red","black"),lwd=1)

          

##############################################################################
# Now I want EN plots...I've looked at all of them and picked a few 
# to go in the paper
#hb1 f16   depList[[1]][[3]]   md[[3]]    good hb centered on the reef  paper ID 17
#sb2 f26   depList[[2]][[1]]   md[[5]]    good sb centered on the reef     ID 1
#hb2 f38   depList[[3]][[6]]   md[[6]]    good hb wider off the reef       ID 23
#sb4 f57   depList[[6]][[3]]   md[[9]]    good sb wider off reef           ID 12

# BUT in ggplot I can't show the reef location, so instead I'll shift the E N 
# numbers so that the reef is at (0,0) and make sure each one shows the same scale
#
# select things that go together for one tag

cTag=c(); cReef=list(); prettyNames = list();
cTag[1] = "f16"; cReef[[1]] = md[[3]]$reefEN; prettyNames[[1]]=c("Fish ID 17","Hard-bottom 1");
cTag[2] = "f26"; cReef[[2]] = md[[5]]$reefEN; prettyNames[[2]]=c("Fish ID 1","Sand-bottom 2");
cTag[3] = "f38"; cReef[[3]] = md[[6]]$reefEN; prettyNames[[3]]=c("Fish ID 23","Hard-bottom 2");
cTag[4] = "f57"; cReef[[4]] = md[[9]]$reefEN; prettyNames[[4]]=c("Fish ID 12","Sand-bottom 4");

# pick these fish from z9 and shift EN to center on the right reef
z1 = z9[(z9$tagName==cTag[1] | z9$tagName==cTag[2] | 
  z9$tagName==cTag[3] | z9$tagName==cTag[4]),]

z2=z1
z2$title = "z"
# center each one on it's reef and make the names pretty
for(i in 1:length(cTag)){
  z2[z2$tagName == cTag[i],]$easting = 
    z2[z2$tagName == cTag[i],]$easting - cReef[[i]]$easting
  z2[z2$tagName == cTag[i],]$northing = 
    z2[z2$tagName == cTag[i],]$northing - cReef[[i]]$northing
  z2[z2$tagName == cTag[i],]$title = prettyNames[i]  
}  
             
             
                     
                                                                          
# dissertation stuff...I use these in 'plots for dissertation talk.r'
z38 = z1[z1$tagName==cTag[3],]    # I see that sometimes I want z1 and sometimes z2
z26 = z1[z1$tagName==cTag[2],]    

# look at daily EN plots of individual fish
ggplot(z2, aes(x=easting, y=northing)) +     #      
  geom_point(alpha=0.03) + 
  #geom_path(data=dC3,aes(group=f,col="red"))      
  theme_bw() +
  facet_wrap(~title)+
  coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
  scale_y_continuous("Northing (m)", breaks = seq(-50,100,by=50)) +
  scale_x_continuous("Easting (m)", breaks = seq(-50,100,by=50)) +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15)) +
  zmargin
  
# add a) etc
grid.text("a)", x = unit(0.15, "npc"), y = unit(0.93, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 

grid.text("b)", x = unit(0.56, "npc"), y = unit(0.93, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 
  
grid.text("c)", x = unit(0.15, "npc"), y = unit(0.5, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 

grid.text("d)", x = unit(0.56, "npc"), y = unit(0.5, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 









###############################################################################
# I want to redo this plot one fish at a time

# pick ID as one of (17,1,23,12)
whichFish = 23
z4 = drop.levels(subset(z2,ID==whichFish),reorder=FALSE)


#hb1 f16   depList[[1]][[3]]   md[[3]]    good hb centered on the reef  paper ID 17
#sb2 f26   depList[[2]][[1]]   md[[5]]    good sb centered on the reef     ID 1
#hb2 f38   depList[[3]][[6]]   md[[6]]    good hb wider off the reef       ID 23
#sb4 f57   depList[[6]][[3]]   md[[9]]    good sb wider off reef           ID 12

cmd=NA
if(whichFish==17){cmd = md[[3]]; pn = prettyNames[[1]]}
if(whichFish==1){cmd = md[[5]]; pn = prettyNames[[2]]}
if(whichFish==23){cmd = md[[6]]; pn = prettyNames[[3]]}
if(whichFish==12){cmd = md[[9]]; pn = prettyNames[[4]]}

# calculate the contour lines...look in 'ggcont.R' from Ben...but this doesn't quite work now
# ... remember that the easting and northing have been shifted to show (0,0) in plot
# ... so shift the hrlims also...


# because the EN data are already shifted so the reef is
# centered on (0,0) don't do like : cmd$reefEN$easting-hrRange[1]
clims = c(-hrRange[1], hrRange[1], -hrRange[2],hrRange[2])

# calculate the KDE contour lines                                        
prob <- c(0.5,0.95)         ## utilization regions to plot
dens <- kde2d(z4$easting,z4$northing,n=250,lims=clims)
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


# now plot
ggplot(z4, aes(x=easting, y=northing)) +           
  geom_point(alpha=0.05) +     
  geom_path(aes(x=dC3_50$x, y=dC3_50$y, group=dC3_50$f), size=1, colour="gray40") +   #Change to gray40 for fish 23 and gray70 for all others
  geom_path(aes(x=dC3_95$x, y=dC3_95$y, group=dC3_95$f), size=1, colour="black") +
  theme_bw() +
  coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
  scale_y_continuous("Northing (m)", breaks =  seq(-100,100,by=50)) +
  scale_x_continuous("Easting (m)", breaks =  seq(-100,100,by=50)) +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))

# add panel label
grid.text(pn[1], x = unit(0.16, "npc"), y = unit(0.96, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 
grid.text(pn[2], x = unit(0.16, "npc"), y = unit(0.91, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 

# add sdl locations...
# ...with grid.text the entire box is 1 unit by 1 unit. 
# For whichFish = 17, by trial and error I 
# see that the EN plotting range goes from bottom-left corner (0.13E,0.11N)
# to top-right corner (0.96E,0.97N). So the 
# easting conversion is (0.96 - 0.13) = 0.83 units = 160 m. 
# northing conversion is (0.97 - 0.11) = 0.86 unist = 160 m. 
# And the reef at (0,0) is at (0.83/2)+0.13E=0.545 and (0.86/2)+0.11N = 0.54
# 
# Turns out if the plotting range (-100,100) (-100,100) is same, corners are same
bb=0.11; tt=0.97; ll=0.145; rr=0.96; 

eastings = (cmd$sdlEN$easting - cmd$reefEN$easting) * ((rr-ll)/200) + ((rr-ll)/2)+ll
northings = (cmd$sdlEN$northing - cmd$reefEN$northing) * ((tt-bb)/200) + ((tt-bb)/2)+bb
# fudge just a little to make it pretty
if(whichFish==17){}
if(whichFish==1){northings[[1]]=0.955; northings[[3]]=0.125}
if(whichFish==23){northings[[1]]=0.955}
if(whichFish==12){northings[[1]]=0.955}

grid.text("X", x = unit(eastings, "npc"), y = unit(northings, "npc"),
          just="center", gp=gpar(fontsize=20)) ## right- and top- justified 





###############################################################################
# START AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
# I want to redo this plot one fish at a time but for all fish...these will 
# go in the appendix.


# draw and save plots for all fish
oDir = getwd()
cDir = paste("C:/zy/school closet/Dissertation/Individual chapters/",
  "Ch 5 Experiment/work for publication after defense/figures/EN",
  sep="")
setwd(cDir)

# loop through all fish ...A MANUAL LOOP SO I CAN SEE THE FIGURES AS THEY COME
# pick one Fish ID (1-27), use results9 to get data for that fish, then plot
cID = 1  # choose 1 then run the manual loop and cID will increment up



# pretend manual loop starts here

  # prep to save the image to a file...
  filename = paste(cID,"EN.pdf",sep=" ")  
  pdf(filename, width=7, height=6.5)
 
  # figure out which fish this is using results9
  temp1 = results9[results9$ID == cID,]
  # pick the right md
  for(i in 1:length(md)){
    if(md[[i]]$deployment == temp1$deployment){
      for(j in 1:length(md[[i]]$fishNames)){
        if(md[[i]]$fishNames[[j]] == temp1$tagName){
          cmd = md[[i]]
          cTag = cmd$fishNames[[j]]
          cReef = cmd$reefEN
          if(substr(cmd$deployment,1,1) == "s"){
            temp2 = "Sand-Bottom"
          } else if(substr(cmd$deployment,1,1) == "h") {
            temp2 = "Hard-Bottom"
          }
          prettyNames = c(paste("Fish ID",cID), paste(temp2,substr(cmd$deployment,3,3)))
        } # end if
      } # end j
    } # end if
  } # end i

  # pick out data for just this one fish
  zTemp = z9[as.character(z9$tagName) == cTag,]

  z9$title = "z"
  # center each one on it's reef and make the names pretty
  zTemp$easting = zTemp$easting - cReef$easting
  zTemp$northing = zTemp$northing - cReef$northing
       
  # calculate the contour lines...look in 'ggcont.R' from Ben...but this doesn't quite work now
  # ... remember that the easting and northing have been shifted to show (0,0) in plot
  # ... so shift the hrlims also...


  # because the EN data are already shifted so the reef is
  # centered on (0,0) don't do like : cmd$reefEN$easting-hrRange[1]
  clims = c(-hrRange[1], hrRange[1], -hrRange[2], hrRange[2])

  # calculate the KDE contour lines                                        
  prob <- c(0.5,0.95)         ## utilization regions to plot
  dens <- kde2d(zTemp$easting,zTemp$northing,n=250,lims=clims)
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

   
  # now plot
  ggplot(zTemp, aes(x=easting, y=northing)) +           
    geom_point(alpha=0.05) +     
    geom_path(aes(x=dC3_50$x, y=dC3_50$y, group=dC3_50$f), size=1, colour="gray40") +   #Change to gray40 for fish 23 and gray70 for all others
    geom_path(aes(x=dC3_95$x, y=dC3_95$y, group=dC3_95$f), size=1, colour="black") +
    theme_bw() +
    coord_cartesian(xlim=c(-110,110), ylim=c(-110,110)) +
    scale_y_continuous("Northing (m)", breaks =  seq(-100,100,by=50)) +
    scale_x_continuous("Easting (m)", breaks =  seq(-100,100,by=50)) +
    opts(axis.title.x = theme_text(size = 20)) +
    opts(axis.title.y = theme_text(size=20, angle=90)) +  
    opts(axis.text.x = theme_text(size = 15)) +
    opts(axis.text.y = theme_text(size = 15))

  # add panel label
  grid.text(prettyNames[1], x = unit(0.16, "npc"), y = unit(0.96, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 
  grid.text(prettyNames[2], x = unit(0.16, "npc"), y = unit(0.91, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 

  # add sdl locations...they're not all exactly 100m out...what's the maximum
  # ...repeat this for all deployments... 110m out will cover all six deployments

  # ...with grid.text the entire box is 1 unit by 1 unit. 
  # 
  # find the corners of the plot by trial and error...these are good when plotting 
  #     out to 110...also the size of the window matters...I'll use 6.5 width 
  #     by 7 height in the 'pdf()' command
  #
  # bottom left
#  grid.text("X", x = unit(0.145, "npc"), y = unit(0.11, "npc"),just="center", gp=gpar(fontsize=20)) 
  # top right
#  grid.text("X", x = unit(0.955, "npc"), y = unit(0.97, "npc"),just="center", gp=gpar(fontsize=20)) 

  # I see that the EN plotting range goes from bottom-left corner (0.145E,0.11N)
  # to top-right corner (0.955E,0.97N). So the 
  # easting conversion is (0.955 - 0.145) = 0.81 units = 220 m. 
  # northing conversion is (0.97 - 0.11) = 0.86 unist = 220 m. 
  # And the reef at (0,0) is at (0.81/2)+0.145E=0.55 and (0.86/2)+0.11N = 0.54
  # 
  # center
#  grid.text("X", x = unit(0.55, "npc"), y = unit(0.54, "npc"),just="center", gp=gpar(fontsize=20)) 

  # Turns out if the plotting range (-110,110) (-110,110) is same, corners are same
  bb=0.11; tt=0.97; ll=0.145; rr=0.955; 

  eastings = (cmd$sdlEN$easting - cmd$reefEN$easting) * ((rr-ll)/220) + ((rr-ll)/2)+ll
  northings = (cmd$sdlEN$northing - cmd$reefEN$northing) * ((tt-bb)/220) + ((tt-bb)/2)+bb

  grid.text("X", x = unit(eastings, "npc"), y = unit(northings, "npc"),
          just="center", gp=gpar(fontsize=20)) ## right- and top- justified 
          
  dev.off() # close fig file?
  cID = cID+1
# pretend manual loop ends here
# restore directory
setwd(oDir)



# END AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA











#############################################################################
#############################################################################
# some time series ...I'll make this for all fish and maybe put in appendix
# NOTES: for i=3, f16, manually delete the two points out past 100m



i=1     

            
results9$tagName[i]                                        
z3 = z9[z9$tagName==results9$tagName[i],] 
if(i==3){z3=z3[z3$dtr<100,]}                 

# full time series of individuals dtr and alt 
ggplot(z3, aes(x=tod, y=dtr))+#, group=tagName, colour=tagName, fill=tagName)) +
  geom_point(size=1, alpha=0.9) + 
  #geom_smooth(method="gam",formula=y~s(x)) +
  #geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
  #coord_cartesian(xlim=c(0,24), ylim=c(0,20)) +
  theme_bw() + 
  facet_wrap(~Date) +
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,130,by=20)) +
  scale_x_continuous("Time of Day", breaks = seq(0,18,by=6))  +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))



############################################################################
### BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB


# draw and save plots for all fish
oDir = getwd()
cDir = paste("C:/zy/school closet/Dissertation/Individual chapters/",
  "Ch 5 Experiment/work for publication after defense/figures/dfr time",
  sep="")
setwd(cDir)

# loop through all fish ...I CAN'T GET A REAL LOOP TO WORK, SO A MANUAL LOOP
cID = 1



# pretend manual loop starts here
  cID
  
  # figure out which fish this is using results9
  temp1 = results9[results9$ID == cID,]
  # pick the right md
  for(i in 1:length(md)){
    if(md[[i]]$deployment == temp1$deployment){
      for(j in 1:length(md[[i]]$fishNames)){
        if(md[[i]]$fishNames[[j]] == temp1$tagName){
          cmd = md[[i]]
          cTag = cmd$fishNames[[j]]
          cReef = cmd$reefEN
          if(substr(cmd$deployment,1,1) == "s"){
            temp2 = "Sand-Bottom"
          } else if(substr(cmd$deployment,1,1) == "h") {
            temp2 = "Hard-Bottom"
          }
          prettyNames = c(paste("Fish ID",cID), paste(temp2,substr(cmd$deployment,3,3)))
        } # end if
      } # end j
    } # end if
  } # end i

  
  tempZ = z9[z9$tagName==cTag,] 
  # some need manual manipulation
#  if(cID==3){z3=z3[z3$dtr<100,]}    

  # prep to save the image to a file...
  filename = paste(cID,"dfr v time main.pdf",sep=" ")  
  pdf(filename, width=20, height=10)
 
  # full time series of individuals dtr and alt 
  ggplot(tempZ, aes(x=tod, y=dtr))+#, group=tagName, colour=tagName, fill=tagName)) +
    geom_point(size=1, alpha=0.9) + 
    #geom_smooth(method="gam",formula=y~s(x)) +
    #geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +
    coord_cartesian(xlim=c(0,24), ylim=c(0,60)) +
    theme_bw() + 
    facet_wrap(~Date) +
    scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,60,by=10)) +
#    scale_y_continuous("Travel Speed (m/s)", breaks=seq(0,8,by=0.2)) +
    scale_x_continuous("Time of Day", breaks = seq(0,18,by=6))  +
    opts(axis.text.x = theme_text(size=20), axis.text.y = theme_text(size = 20)) +               
    opts(axis.title.x = theme_text(size=20), axis.title.y = theme_text(size=20, angle=90, vjust=0.4))

    # add a label
    grid.text(prettyNames, x = unit(c(0.825,0.825), "npc"), y = unit(c(0.27,0.22), "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=20)) ## right- and top- justified 
 
 
 

  dev.off() # close fig file?
  cID = cID+1
# pretend manual loop ends here



# restore directory
setwd(oDir)


### BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
##############################################################################








# look at daily EN plots of individual fish
ggplot(z3, aes(x=easting, y=northing))+
  geom_point(alpha=0.05) + 
  #coord_cartesian(xlim=c(0,60), ylim=c(0,10)) +
  theme_bw() + 
  facet_wrap(~dod) +
  scale_x_continuous("Easting (m)")+#, breaks = seq(0,60,by=20)) +
  scale_y_continuous("Northing (m)")#,breaks=c(0,1,2,3,4,6,8,10))



plot(z9$dod)

#############################################################################
#############################################################################
# GAMS                         
 
z9hb = z9[z9$treatment == "hb",]
z9sb = z9[z9$treatment == "sb",]
                                                        
ggplot(z9hb, aes(x=lunarIndex, y=dtr, group=ID, colour=deployment, fill=deployment)) + 
  geom_point(alpha=0.1) + 
  #geom_smooth(method="gam",formula=y~s(x,bs="cc"),lwd=1.3) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +  #,bs="cc"
  coord_cartesian(xlim=c(1,30), ylim=c(0,130)) +    # range(z0$temperature,na.rm=T)    range(z0$magL,na.rm=T)
  theme_bw() +
  facet_wrap(~tl) +
  #scale_x_continuous("Time of Day",breaks = c(6,12,18)) +
  scale_x_continuous("Lunar Index",breaks = c(7,15,23,30)) +
  scale_y_continuous("Distance From the Reef (m)", breaks=c(0,50,100)) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))
       

ggplot(z9, aes(x=tod, y=dtr, group=ttmt, colour=deployment, fill=deployment)) + 
  geom_point(alpha=0.1) + 
  #geom_smooth(method="gam",formula=y~s(x,bs="cc"),lwd=1.3) +   #,bs="cc"
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x,bs="cc")) +  #,bs="cc"
  #coord_cartesian(xlim=c(1,30), ylim=c(0,130)) +    # range(z0$temperature,na.rm=T)    range(z0$magL,na.rm=T)
  coord_cartesian(xlim=c(0,24), ylim=c(0,130)) + 
  theme_bw() +
  #facet_wrap(~tl) +
  scale_x_continuous("Time of Day",breaks = c(6,12,18)) +
  #scale_x_continuous("Lunar Index",breaks = c(7,15,23,30)) +
  scale_y_continuous("Distance From the Reef (m)", breaks=c(0,50,100)) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))



# use this code to produce gam figs for the final publication of the experiment
# Showing: dtr v (tod, lunar, temp, spdw, dir) and 
#          spdg v (tod, lunar, temp, spdw, dir) and 
#          dtr v spdg 

ggplot(z9, aes(x=magL, y=dtr, colour=ID, fill=ID)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method="gam",formula=y~s(x),lwd=1.3) +   #formula=y~s(x,bs="cc")  is used for cyclic fits
  geom_smooth(aes(group=1),colour="black",lwd=1.3,method="gam",formula=y~s(x)) +  #,bs="cc"
  theme_bw() +
  facet_wrap(~ttmt) +
  
  ## Set axis limits
  #xlim=c(0,24)                                   - for tod
  #xlim=c(0,30)                                   - for lunarIndex
  #xlim=range(z9$temperature,na.rm=T)             - for temperature
  #xlim=c(range(z9$magL,na.rm=T)[1],0.3)                    - for water speed
  #xlim=c(0,360)                         - for water flow direction
  #
  #ylim=c(0,100)                                  - for dtr
  #ylim=c(0,0.4)                                  - for spd
  coord_cartesian(xlim=c(range(z9$magL,na.rm=T)[1],0.3), ylim=c(0,100)) + 
  
  ## set x asix labels 
  #scale_x_continuous("Time of Day",breaks = c(6,12,18,24)) + # for tod
  #scale_x_continuous("Lunar Index",breaks = c(7,15,23,30)) + # for lunarIndex
  #scale_x_continuous("Water Temperature (Celcius)",breaks = seq(20,32,by=2)) + # water temp
  scale_x_continuous("Water Speed (m/s)",breaks = seq(0,0.3,by=0.05)) + # water speed
  #scale_x_continuous("Water Flow Direction",breaks = seq(60,360,by=60)) + # water flow direction
  #scale_x_continuous("Distance From the Reef (m)", breaks=seq(10,100,by=10)) + # for dtr
  
  ## set y axis labels
  scale_y_continuous("Distance From the Reef (m)", breaks=seq(0,100,by=10)) + # for dtr
  #scale_y_continuous("Travel Speed (m/s)", breaks=seq(0,0.4,by=0.04)) + # for speed
  
  opts(axis.text.x = theme_text(size=15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))

 

 
     
     
# keep this for a publication plot dtr v tod, with points for one fish, curves for each, one curve for all
ggplot(z4, aes(x=tod, y=dtr)) +
  geom_point(size=1, alpha=0.2, colour="black") + 
  geom_smooth(        linetype=1,lwd=1.3,method="gam",formula=y~s(x,bs="cc"),colour="black",fill="black") +   
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

#############################################################################
#############################################################################
# GAMS but I want to look at EN plots with the reef at the center of each 
# individual fish's plot, 

# shift EN to center on the right reef
z9centered = z9
cTag = unique(z9centered$tagName) # make a list of individual fish
cReef=NA 


for(i in 1:length(cTag)){ # pick one fish at a time
  for(j in 1:length(md)){ # go through each deployment
    for(k in 1:length(md[[j]]$fishNames)){ # go through each fishName
      #... and pick the reef that cTag == fishNames was on
      if(cTag[i] == md[[j]]$fishNames[[k]]){cReef = md[[j]]$reefEN}
      
    }
  }     
  # now that I've picked the reef this cTag fish was on...
  #   center this fish on it's reef to make the reef be at (0 E,0 N)
      z9centered[z9centered$tagName == cTag[i],]$easting = 
        z9centered[z9centered$tagName == cTag[i],]$easting - cReef$easting
      z9centered[z9centered$tagName == cTag[i],]$northing = 
        z9centered[z9centered$tagName == cTag[i],]$northing - cReef$northing
}

z9hbc = z9centered[z9centered$treatment == "hb",]
z9sbc = z9centered[z9centered$treatment == "sb",]
                                                        
ggplot(z9sbc, aes(x=easting, y=northing, group=ID, colour=deployment, fill=deployment)) + 
  geom_point(alpha=0.1) + 
  coord_cartesian(xlim=c(-70,70), ylim=c(-70,70)) + 
  scale_x_continuous("Easting (m)", breaks=c(-30,0,30)) +
  scale_y_continuous("Northing (m)", breaks=c(-60,-30,0,30,60)) +
  theme_bw() +
  facet_wrap(~tl) +
  opts(axis.text.x = theme_text(size = 15), axis.text.y = theme_text(size = 15)) +               
  opts(axis.title.x = theme_text(size=15), axis.title.y = theme_text(size=15, angle=90))







################################################################################
# KDE stabilization curves
  
results9

# something to hold the changing HR answers for all fish
allFishKDE = list()                                          

cProb50 = 0.5
cProb95 = 0.95               

for (cTag in 1:nrow(results9)){  
  # which tag and pick out that data for just that tag
  whichTag = results9$tagName[cTag]
  d1 = z9[z9$tagName == whichTag,c("utime","datiL","easting","northing","dod")]
  # drop the first two days
  d1 = d1[d1$dod > 2,]
  
    
  # fetch deployment specific informaiton
  for (k in 1:length(md)){
    if(results9$deployment[cTag] == md[[k]]$deployment){
      cmd = md[[k]]  
    } # end if statement 
  } # end for k loop
  
    
  # something to hold the changing HR for one fish
  hrVSdays = data.frame("numDays" = NA, "hr50Size" = NA, "hr95Size" = NA) 

  for (i in 3:results9$numDays[cTag]){  # start with 3 because we dropped the first two days                         
    # grab only position solutions during the first i days
    d2 = d1[d1$dod <= i,]                  
    # ...but drop data from the first two days
    #d2 = d2[d2$utime > endOfAllDays[2] ,]
    print(i)  
    if(nrow(d2) > 0){
      # calculate the 50% kde for these PS
      hr50Size = homeRange(
        easting = d2$easting, northing = d2$northing, 
        n=250, 
        tagName = paste(results9$tagName[cTag], ", ", i, " days", sep=""),
        lims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
                 cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2]),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb50, drawplot=FALSE
      )
      # calculate the 95% kde for these PS
      hr95Size = homeRange(
        easting = d2$easting, northing = d2$northing, 
        n=250, 
        tagName = paste(results9$tagName[cTag], ", ", i, " days", sep=""),
        lims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
                 cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2]),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb95, drawplot=FALSE
      )
      
      
    } else {hr50Size = 0; hr95Size = 0;}
    # save the answer
    hrVSdays[i,] = c(i, hr50Size, hr95Size) 
  } # end for-loop over all days for one tag



  # a plot
  plot(hrVSdays$numDays, hrVSdays$hr50Size, pch=19, 
    main=paste(results9$tagName[cTag], ": ", cProb50*100, "% HR", sep="")
  )
  points(hrVSdays$numDays, hrVSdays$hr50Size, pch=19, col="red")
  
  # save the answer for this fish
  allFishKDE[[cTag]] = hrVSdays
  
} # end cTag for-loop     


# divide by treatment
hbs = allFishKDE[grepl("hb",results9$deployment)]
sbs = allFishKDE[grepl("sb",results9$deployment)]





# a four panel plot for HB reefs
par(mfrow=c(2,2))
# top left pane, SB 50% KDE
par(mar=c(3,6,2,1)+0.1)
plot(1,1,type="n", las=1, cex.axis=1.5, cex.lab=1.5, cex.main=2, bty="l",
  xlim=c(3,17),ylim=c(20,220), xaxt="n",
  xlab="", ylab="", main="Sand-bottom Landscapes")
axis(1,at=seq(3,17,by=2), cex.axis=1.5)
mtext("50% KDE (    )", side=2, line=3.9, cex=1.7)
mtext(expression(m^2), side=2, line=4.0, adj=0.79, cex=1.7)
text(3,220,"a)",cex=1.5)
# add the lines
for(i in 1:length(hbs)){
  points(tail(sbs[[i]]$numDays,-2), 
    tail(sbs[[i]]$hr50Size,-2), type="l",  lwd=2)
}

# top-right pane, HB 50% KDE
par(mar=c(3,5,2,1)+0.1)
plot(1,1,type="n", las=1, cex.axis=1.5, cex.lab=1.5, cex.main=2, bty="l",
  xlim=c(3,17),ylim=c(0,1100), xaxt="n",
  xlab="", ylab="", main="Hard-bottom Landscapes")
axis(1,at=seq(3,17,by=2), cex.axis=1.5)
mtext("50% KDE (    )", side=2, line=4.0, cex=1.7)
mtext(expression(m^2),side=2, line=4.1, adj=0.79, cex=1.7)
text(3,1100,"b)",cex=1.5)
# add the lines
for(i in 1:length(hbs)){
  points(tail(hbs[[i]]$numDays,-2), 
    tail(hbs[[i]]$hr50Size,-2), type="l",  lwd=2)
}

# bottom left pane, SB 95% KDE  
par(mar=c(4,6,0,1)+0.1)
plot(1,1,type="n", las=1, cex.axis=1.5, cex.lab=1.5, bty="l",
  xlim=c(3,17),ylim=c(0,2000), xaxt="n",
  xlab="", ylab="")
axis(1,at=seq(3,17,by=2), cex.axis=1.5)
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("95% KDE (    )",  side=2, line=3.9, cex=1.7)
mtext(expression(m^2),  side=2, line=4.0, adj=0.78, cex=1.7)
text(3,2000,"c)",cex=1.5)
# add the lines  
for(i in 1:length(hbs)){
  points(tail(sbs[[i]]$numDays,-2), 
    tail(sbs[[i]]$hr95Size,-2), type="l", lwd=2)
}

# bottom-right pane, HB 95% KDE
par(mar=c(4,5,0,1)+0.1)
plot(1,1,type="n", las=1, cex.axis=1.5, cex.lab=1.5, bty="l",
  xlim=c(3,17),ylim=c(0,6300), xaxt="n",
  xlab="", ylab="")
axis(1,at=seq(3,17,by=2), cex.axis=1.5)
mtext("Number of Days", side=1, line=2.5, cex=1.5)
mtext("95% KDE (    )", side=2, line=4.5, cex=1.7)
mtext(expression(m^2),side=2, line=4.6, adj=0.78, cex=1.7)
text(3,6300,"d)",cex=1.5)
# add the lines  
for(i in 1:length(hbs)){
  points(tail(hbs[[i]]$numDays,-2), 
    tail(hbs[[i]]$hr95Size,-2), type="l", lwd=2)
}



# compare these results with results9
for (i in 1:nrow(results9)){
  print(paste(results9$tagName[i],"-",results9$deployment[i],": ",
    "results9 50% =", results9$kde50[i], " ?= allFishKDE = ", 
    round(tail(allFishKDE[[i]]$hr50Size,1),0),sep=""))
}

















































################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
AS OF 2013 FEB 20 ALL THIS BIOMETRIC STUFF IS NOW DONE IN 'chapter 4 otoliths.r'  

                     
# get fish biometric data.  This contains data recorded in the field on tagging
# day and any recaptures.  It also contains the otolith data.  
biometrics = importBiometricData()



#################################################################################
# some plots Deb suggested for the 2009 collected fish
# pick only relevant data
b1 = biometrics

# only data with MM numbers...those caught and kept and otoliths extracted
# ...and from one of the experimental reefs
b1 = b1[(!is.na(b1$mmNumberD) & !is.na(b1$HBSB)),]
# only keep some information about these
b1 = b1[,c('reefID1','HBSB','replicate','weight1','girth1','TL1','FL1','tagged',
  'tagID','mmNumber',
  'recoveredTagID','year2','reefID2','TL2','FL2','weight2','girth2',
  #'lOto', 'lOtoWeightUseable','lOtoLengthAUseable','lOtoLengthBUseable',
  #'lOtoLengthCUseable',
  #'rOto','rOtoWeightUseable','rOtoLengthAUseable','rOtoLengthBUseable',
  #'rOtoLengthCUseable',
  'monthD','resolvedAgeclass','otoRadius', 'ultimateAnnulus',
  'penultimateAnnulus','growthIncrement')
]

# for learning aov and lm keep even fewer data
b1 = b1[,c('HBSB','TL2','weight2',
  'otoRadius', 'ultimateAnnulus','penultimateAnnulus','growthIncrement')
]

names(b1) = c("treatment","tl","weight","otoRadius", "ultimateAnnulus",
  "penultimateAnnulus","growthIncrement")



# back calculate length using the two equations in Deb and Daryl's paper
# ...each treatment should be done separately
#
# la = [ (a + b * ra)/(a + b * rc) ] lc
# lc = a + b * rc
#
# la = back-calculated length to opaque zone 'a'
# a = intercept from the linear regression of total length as a function of 
#     otolith radius
# b = slope from same regression
# ra = otolith radius to opaque zone 'a'
# rc = total otolith radius at time of capture
# lc = total length at time of capture

#  Sand-bottom treatment
sbreg1 = lm(TL2 ~ otoRadius, sb)
plot(sb$otoRadius, sb$TL2, pch=19)
abline(a=sbreg1$coef[1], b=sbreg1$coef[2])

# hard-bottom treatment
hbreg1 = lm(TL2 ~ otoRadius, hb)
plot(hb$otoRadius, hb$TL2, pch=19)
abline(a=hbreg1$coef[1], b=hbreg1$coef[2])

# plot together
plot(sb$otoRadius, sb$TL2, pch=2, xlim=c(0.5,1.5), ylim=c(300,800))
abline(a=sbreg1$coef[1], b=sbreg1$coef[2], lty=2)
points(hb$otoRadius, hb$TL2, pch=19)
abline(a=hbreg1$coef[1], b=hbreg1$coef[2])


# compare regression lines using
# ...following root/fruit/grazing example in http://www.scribd.com/doc/843947/ANCOVA-in-R
#attach(b1)

# using lm() 
ancova = lm(otoRadius ~ HBSB * TL2)
summary(ancova)

# this shows that TL2 has an effect on otoRadius, but there is no indication of
# difference in the slope of this relationship between the two treatments

anova(ancova)


################################################################################
# some summaries about tagged fish
results9

sbr2 = results9[results9$deployment == "sb2",]
sbr3 = results9[results9$deployment == "sb3",]
sbr4 = results9[results9$deployment == "sb4",]
hbr1 = results9[results9$deployment == "hb1",]
hbr2 = results9[results9$deployment == "hb2",]
hbr3 = results9[results9$deployment == "hb3",]





###############################################################################
## look at distributions of dtr, speed, interval
# pick one and this gives you depData, then make those tag9, tagf9, and tagfm9 normal
load("C:/zy/school closet/Telemetry/R summary files/Experiment tagfm9 and figs/dep_hb1 2011June23.rdata")
tag = depData$tag9
tagf = depData$tagf9
tagfm = depData$tagfm9 



#############################################################################
# histograms of tagged and non-tagged fish observed at time of tagging
     
# get fish biometric data.  This contains data recorded in the field on tagging
# day and any recaptures.  It also contains the otolith data.  
b1 = importBiometricData()

# pick only relevant data, 2009 fish on the reef at time of tagging...
# ...b1 has those fish plus 2007/8 and otolith collection fish at end...
# ...to separate...luckily sizeRange is not had for otolith collection fish
b2 = b1[((b1$year1 == "2009")|(b1$year1 == "2010"))& !is.na(b1$sizeRange), 1:14]
# make b1$sizeRange numeric
b2$sizeRange = as.numeric(levels(b2$sizeRange))[b2$sizeRange]


# now split this by treatment
b2sb = b2[b2$HBSB == "SB",]
b2hb = b2[b2$HBSB == "HB",]

# pick out only the tagged fish on 6 good reefs and split by treatment
b3 = b2[b2$tagged == "yes",]
b3 = b3[!((b3$HBSB == "SB") & (b3$replicate == 1)),]
b3sb = b3[b3$HBSB == "SB",]
b3hb = b3[b3$HBSB == "HB",]


# histogram of all fish observed on reefs at time of tagging
par(mfrow=c(1,2))
par(mar=c(4.5,4,1,1)+0.1)
bob = multhist(list(b2sb$sizeRange,b2hb$sizeRange),freq=T,breaks=seq(10,70,by=10),
  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
  ylim=c(0,80),
  #legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
  names.arg=c("20-30","30-40","40-50","50-60","60-70","70-80")
)
axis(2,seq(0,80,by=5), las=1, cex.axis=1.5)
mtext("Total Length Category (cm)    ",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
text(x=0.5,y=78,labels="a)",cex=1.5)

# histogram of all fish tagged on 6 used reefs at time of tagging
par(mar=c(4.5,4,1,1)+0.1)
sam = multhist(list(b3sb$TL1,b3hb$TL1),freq=T,breaks=seq(300,800,by=20),
  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
  ylim=c(0,4),
  legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
  names.arg=tail(sam$breaks,-1)
)
axis(2,seq(0,8,by=1), las=1, cex.axis=1.5)
mtext("Total Length (mm)    ",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
text(x=0.5,y=3.9,labels="b)",cex=1.5)

#############################################################################
# t-tests of tagged and non-tagged fish observed at time of tagging
#
# Is there a difference in the number of total fish seen on reefs?
sbr1 = b2[((b2$HBSB == "SB") & (b2$replicate == 1)),]
sbr2 = b2[((b2$HBSB == "SB") & (b2$replicate == 2)),]
sbr3 = b2[((b2$HBSB == "SB") & (b2$replicate == 3)),]
sbr4 = b2[((b2$HBSB == "SB") & (b2$replicate == 4)),]
hbr1 = b2[((b2$HBSB == "HB") & (b2$replicate == 1)),]
hbr2= b2[((b2$HBSB == "HB") & (b2$replicate == 2)),]
hbr3 = b2[((b2$HBSB == "HB") & (b2$replicate == 3)),]

sbcounts = c(nrow(sbr1),nrow(sbr2),nrow(sbr3),nrow(sbr4))
hbcounts = c(nrow(hbr1),nrow(hbr2),nrow(hbr3))

# test for equal variance
var.test(sbcounts, hbcounts)  # variances are equal
# t-test
t.test(sbcounts, y=hbcounts, alternative="t", var.equal=TRUE)
...so were not different total numbers

# using the lower bound of each size class, are there differences in the mean 
# size of all fish observed on reefs
sb1m = mean(sbr1$sizeRange)
sb2m = mean(sbr2$sizeRange)
sb3m = mean(sbr3$sizeRange)
sb4m = mean(sbr4$sizeRange)
hb1m = mean(hbr1$sizeRange)
hb2m = mean(hbr2$sizeRange)
hb3m = mean(hbr3$sizeRange)

sbmeans = c(sb1m,sb2m, sb3m, sb4m)
hbmeans = c(hb1m, hb2m, hb3m)

# test for equal variance
var.test(sbmeans, hbmeans)  # variances are equal
# t-test
t.test(sbmeans, y=hbmeans, alternative="t", var.equal=TRUE)
...so were not different total lengths

# is there a difference in the total length of tagged individuals
# ... pick only tagged
s1 = sbr1[sbr1$tagged == "yes",]
s2 = sbr2[sbr2$tagged == "yes",]
s3 = sbr3[sbr3$tagged == "yes",]
s4 = sbr4[sbr4$tagged == "yes",]
h1 = hbr1[hbr1$tagged == "yes",]
h2 = hbr2[hbr2$tagged == "yes",]
h3 = hbr3[hbr3$tagged == "yes",]

s1m = mean(s1$TL1)
s2m = mean(s2$TL1)
s3m = mean(s3$TL1)
s4m = mean(s4$TL1)
h1m = mean(h1$TL1)
h2m = mean(h2$TL1)
h3m = mean(h3$TL1)

sbmeans = c(s1m,s2m, s3m, s4m)
hbmeans = c(h1m, h2m, h3m)


# test for equal variance
var.test(sbmeans, hbmeans)  # variances are equal
# t-test
t.test(sbmeans, y=hbmeans, alternative="t", var.equal=TRUE)
...so were not different total lengths


aaa
# is there a difference in the weight of tagged individuals
# ... pick only tagged

s1mw = mean(s1$weight1)
s2mw = mean(s2$weight1)
s3mw = mean(s3$weight1)
s4mw = mean(s4$weight1)
h1mw = mean(h1$weight1)   # the scale broke and there are no weights for this deployment
h2mw = mean(h2$weight1)
h3mw = mean(h3$weight1)

sbmeans = c(s1mw,s2mw, s3mw, s4mw)
hbmeans = c(h2mw, h3mw)


# test for equal variance
var.test(sbmeans, hbmeans)  # variances are barely equal, p = 0.06663
# t-test
t.test(sbmeans, y=hbmeans, alternative="t", var.equal=TRUE)
...so were not different weights
bbb


# what are the relationships between LT, LF, and weight of all tagged individuals

aaa an example from chapter 3
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

bbb
