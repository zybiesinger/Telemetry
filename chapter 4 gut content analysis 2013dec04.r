
source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")

library(ggplot2)
library(date)
library(plotrix) # for multhist
library(gdata) # for drop.levels

################################################################################
### Gut Content Analysis of Diet Analysis
################################################################################

According to Baker et al 2013, Fish and Fisheries online 25 Feb 2013
"Fish gut content analysis..."
the best thing to do is simple presence/absence or frequecy of occurance.
Frequency of occurance is "the number of stomachs containing 1 or more
individuals of each food item. Expressed as percent of all stomachs
or all stomachs containing food."
See also Hynes 1950 J. Animal Ecology 19:35-
and Hyslop 1980 J of fish biology 17:411-429




biometrics = importBiometricData()
g1 = biometrics

# ...keep only gag collected in 2011 for diet analysis
# ...I use mmNumberGut as a marker...drop anything with NA  
g1 = g1[!is.na(g1$mmNumberGut),]


# ...drop many unused columns
g1 = g1[,c(
  'mmNumber','reefID1','HBSB',
  'year2','month2','day2','TL2','FL2','weight2',
  'fractionalAgeD','gutID','gutPresent','gutCount'
)]

names(g1) = c(
  "mmNumber","reefID","HBSB",
  "year2","month2","day2","tl2","fl2","weight2",
  "ageFrac","gutID","gutPresent","gutCount")

g1sb = g1[g1$HBSB == "SB",]
g1hb = g1[g1$HBSB == "HB",]

# do some looking
plot(g1$HBSB, g1$tl2)
sbtl = g1$tl2[g1$HBSB == "SB"]
hbtl = g1$tl2[g1$HBSB == "HB"]
abline(h=mean(sbtl),col="red")
abline(h=mean(hbtl),col="blue")

# is there a difference in the size of fish collected from HB and SB
var.test(sbtl, hbtl)  # variances are equal
t.test(sbtl, y=hbtl, alternative="two.sided", var.equal=TRUE) 
# ... yes there is a difference, SB fish are larger...

# calculate the frequency of occurance in SB and HB
# ...what percentage of SB/HB stomachs had something in them?

freqOccurSB = sum(g1sb$gutPresent)/nrow(g1sb)
freqOccurHB = sum(g1hb$gutPresent)/nrow(g1hb)





































































































































##############################################################################
##############################################################################
### ALL OLD WORK BELOW HERE, LOOK BELOW TO FIND THE TRIAL AND ERROR OF THE ABOVE
##############################################################################
##############################################################################
##############################################################################
##############################################################################
### ALL OLD WORK BELOW HERE, LOOK BELOW TO FIND THE TRIAL AND ERROR OF THE ABOVE
##############################################################################
##############################################################################

#
#
#
#                    
## get fish biometric data.  This contains data recorded in the field on tagging
## day and any recaptures.  It also contains the otolith data.  
#biometrics = importBiometricData()
#
## pick only relevant data
#b1 = biometrics
## only data with MM numbers...those caught and kept and otoliths extracted
## ...and from one of the experimental reefs used in the 2009 experiment
#b1 = b1[(!is.na(b1$mmNumberD) & !is.na(b1$replicate)),]
## only keep some information about these
#b1 = b1[,c('reefID1','HBSB','replicate','weight1','girth1','TL1','FL1','tagged',
#  'tagID','mmNumber',
#  'recoveredTagID','year2','month2','day2','reefID2','TL2','FL2','weight2','girth2',
#  #'lOto', 'lOtoWeightUseable','lOtoLengthAUseable','lOtoLengthBUseable',
#  #'lOtoLengthCUseable',
#  #'rOto','rOtoWeightUseable','rOtoLengthAUseable','rOtoLengthBUseable',
#  #'rOtoLengthCUseable',
#  'monthD','debAgeclassCorrected','debAnnuli',
#  #'resolvedAgeclass',
#  'otoRadius', 'ultimateAnnulus','penultimateAnnulus','growthIncrement',
#  'fractionalAgeD'
#  )
#]
## again
#b1 = b1[,c('TL1','tagID','mmNumber','HBSB','replicate','year2','month2','day2',
#  'TL2','FL2','weight2','debAgeclassCorrected','debAnnuli',
#  'otoRadius','ultimateAnnulus','penultimateAnnulus','growthIncrement',
#  "fractionalAgeD")
#]
#names(b1) = c("tl1","tagID","number","treatment","replicate","year","month","day",
#  "tl","fl","weight","ageClass","numAnnuli",
#  "otoRadius", "ultAnnul",
#  "penultAnnul","growthInc","debAgeFrac")
#  
## now calculate the fractional age using 1 April as everyones b-day
## ... I'll get fractional age as: numAnnuli + (num days since 1 April)/(365)
#temp1 = paste(b1$year,b1$month,b1$day)
#temp2 = as.POSIXlt(strptime(temp1, "%Y %m %d"), origin="1970-1-1")$yday
#birthday = as.POSIXlt(strptime("2009 4 1", "%Y %m %d"), origin="1970-1-1")$yday
## now we have to treat differently fish caught in early 2010 than late 2009
## ...because $yday starts over again and I need a correct count of days since b-day
## first get the ageFrac for fish caught late in year
#b1$ageFrac = b1$numAnnuli + (temp2 - birthday)/365
## now fix ageFrac for fish caught early in year
#b1$ageFrac[temp2<birthday] = b1$numAnnuli[temp2<birthday] + (temp2[temp2<birthday] + 365 - birthday)/365
## I see that my fractional age calculation matches Deb's. Yeah. Except I said
## birthday was day 90 and she said it was 91. 
#
#  
#  
## for pretty labeling in ggplot
#b1$Treatment = "unknown"
#b1$Treatment[b1$treatment == "HB"] = "Hard-bottom"
#b1$Treatment[b1$treatment == "SB"] = "Sand-bottom" 
#  
#  
#                           
#ptType = as.numeric(b1$treatment)
#ptCol = ptType 
#ptType[ ptType == 1 ] = 17
#ptType[ ptType == 2 ] = 19
#ptCol[ ptCol == 1 ] = "red"
#ptCol[ ptCol == 2 ] = "black"
#
## at this point use b1 to get data for a table in the paper...
## ...paste the R output into a notepad file, open with Excel, rearragne to 
## ...the desired order, paste into Word
#b1
#
#
## calculate the relative weight.  I got this equation from Doug.
##   a = 9.21744 x 10-6; b = 3.04;
##   (standard weight,g) = a (length, mm)^b
##   relative weight = (actual weight / standard weight) * 100
#stdwt = function(tl){9.21744e-6 * tl^3.04}
#b1$stdwt = stdwt(b1$tl)
#b1$relwt = (b1$weight / stdwt(b1$tl)) * 100
#
#
#
## separate by treatment
#sb = b1[b1$treatment=="SB",]
#hb = b1[b1$treatment=="HB",]
#
## do a little checking
#with(b1,plot(tl~fl,  col=treatment, pch=ptType))
#abline(0,1)
#
#
###### size and age distributions of fish caught for otolith work
#par(mfrow=c(1,3))
#par(mar=c(4.5,4,1,1)+0.1)
#bob=multhist(list(sb$tl,hb$tl), freq=T, breaks=28,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  #legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
#  names.arg=tail(bob$breaks,-1)
#)
#axis(2,0:10, las=1, cex.axis=1.5)
#mtext("Total Length (mm)    ",1,3, cex=1.5)
#mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=7.8,labels="a)",cex=1.5)
#
## age histogram
#ben=multhist(list(sb$ageFrac,hb$ageFrac), freq=T, breaks=15,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  #legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
#  names.arg=tail(ben$breaks,-1)
#)
#axis(2,0:120, las=1, cex.axis=1.5)
#mtext("Fractional Age (years)    ",1,3, cex=1.5)
#mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=9.8,labels="b)", cex=1.5)
#
#
## weight histogram
#sam=multhist(list(sb$weight/1000,hb$weight/1000), freq=T, breaks=14,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
#  names.arg=tail(sam$breaks,-1)
#)
#axis(2,seq(0,20,by=2), las=1, cex.axis=1.5)
#mtext("Weight (kg)    ",1,3, cex=1.5)
#mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)
#
#
#
## a simple t-test to see of there's a difference in total length, weight, age
## total length ###################
## test for equal variance
#var.test(sb$tl,hb$tl)  # variances are equal
## t-test
#t.test(sb$tl, y=hb$tl, alternative="t", var.equal=TRUE)
#...so were not different total lengths
#
## weight ###################
## test for equal variance
#var.test(sb$weight,hb$weight)  # variances are equal
## t-test
#t.test(sb$weight, y=hb$weight, alternative="t", var.equal=TRUE)
#...so were not different total weights
#
## fractional age ###################
## test for equal variance
#var.test(sb$ageFrac,hb$ageFrac)  # variances are equal
## t-test
#t.test(sb$ageFrac, y=hb$ageFrac, alternative="t", var.equal=TRUE)
#...so were not different total lengths
#
#
#
#
#
#
#
#
#
## now look at the difference
#b1$diffL = b1$tl - b1$fl
#plot(b1$diffL, pch=19)
##---or---
#ggplot(b1,aes(x=tl,y=diffL,colour=treatment)) +
#  geom_text(aes(label=b1$number),size=3) + ##geom_point() + 
#  scale_x_continuous("TL")+
#  scale_y_continuous("TL - FL") +
#  theme_bw()
#
## look 
#b1[,c('numAnnuli','ageFrac')]                                             
#
## now look at them
#with(b1,plot(tl~ageFrac,  col=treatment, pch=ptType))
##---or---
#ggplot(b1,aes(x=ageFrac,y=tl,colour=treatment)) +
#  geom_text(aes(label=b1$number),size=3) + ##geom_point() + 
#  scale_x_continuous("Fractional Age")+
#  scale_y_continuous("Total Length at Capture") +
#  theme_bw()
#
#
#
## There are two things I want to do:
## 1. Ask if there are there differences between the age-total length curves between treatments
## 2. Ask if there are differences between the length-weight curves of two treatments.
##
#
## Now I want to compare the linear tl~ageFrac relationships 
##   between treatments
#
#
## first get the regression parameters
## regression of all data
#b1reg4 = lm(tl ~ ageFrac, b1)
#
##  regression of Sand-bottom treatment
#sbreg4 = lm(tl ~ ageFrac, sb)
#
## regression of hard-bottom treatment
#hbreg4 = lm(tl ~ ageFrac, hb)
#
#
#
#                                                   
#                                                      
#                                                      
#                                                      
## plot together with fractional age              
## THIS WILL BE A FIGURE PLOT        
#par(mar=c(5,5,1,1))                                          
#with(b1,plot(tl~ageFrac,  pch=ptType, las=1, col=ptCol, 
#  cex.lab=1.5, cex.axis=1.5, bty="l", xaxt="n", yaxt="n",
#  xlab="Fractional Age at Capture", ylab="",
#  xlim=c(1.5,5)
#))
##abline(b1reg4)
#abline(sbreg4, lty=1, lwd=2)
#abline(hbreg4, lty=2, lwd=2, col="red")
#mtext(text="Total Length (mm)", side=2, line=3.5, cex=1.7)
#axis(1,seq(1.5,5,by=0.5),cex.axis=1.5)
#axis(2,seq(350,750,by=50),las=1,cex.axis=1.5)
#
#legend(x=1.5,y=750,legend=c("Sand-bottom","Sand-bottom regression","Hard-bottom",
#  "Hard-bottom regression"), pch=c(19,NA,17,NA), lty=c(NA,1,NA,2), 
#  col=c("black","black","red","red"), lwd=2)
#
## or do this with ggplot
## this is panel a) of paper figure 5-12
#ggplot(b1,aes(x=ageFrac,y=tl,colour=Treatment, shape=Treatment, fill=Treatment))+
#  geom_point()+
#  geom_smooth(method="lm") +
#  theme_bw() +
#  #coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
#  scale_y_continuous("Total Length (mm)") +
#  scale_x_continuous("Fractional Age (yr)") +
#  opts(axis.title.x = theme_text(size = 20)) +
#  opts(axis.title.y = theme_text(size=20, angle=90, vjust=0.3)) +  
#  opts(axis.text.x = theme_text(size = 15)) +
#  opts(axis.text.y = theme_text(size = 15))
#
## add a) etc
#grid.text("a)", x = unit(0.15, "npc"), y = unit(0.95, "npc"),
#          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 
#
#
#
#
#
#
#
#
#
## Now compare regression lines using lm() and interpret as before
#
#ancova = lm(tl ~ treatment * ageFrac,data=b1)
#summary(ancova)
#
#
#  
#
###############################################################################
## 2. ARE THERE DIFFERENCES BETWEEN THE LENGTH-WEIGHT CURVEs OF TWO TREATMENTS
## Now I want to do the same thing to the length-weight curves. 
## After log transforming I'll follow steps similar to above...
#
## START WITH:  weight = c * tl^d
## log(weight) = log(c) + d * log(tl)
## ...so y = a + d * x
## ... and 'a' = log(c), 'b' = 'd', y = log(weight), x = log(tl) 
#
#
## plot together
#plot(sb$tl, sb$weight, pch=2, xlim=c(300,800), ylim=c(500,7000))
#points(hb$tl, hb$weight, pch=19)
## ---or---
#with(b1, plot(weight~tl, col=treatment, pch=ptType))
#
## all data
#b1reg2 = lm(log(weight) ~ log(tl), b1)
#
##  Sand-bottom treatment
#sbreg2 = lm(log(weight) ~ log(tl), sb)
#
## hard-bottom treatment
#hbreg2 = lm(log(weight) ~ log(tl), hb)
#
## look at the log-transformed data
#plot(log(b1$tl), log(b1$weight), xlim=c(0,7), ylim=c(-15,10))
#abline(b1reg2)
#abline(h=-11.27,v=0)                    
#
#
## look at the un-transformed data
#plot(sb$tl, sb$weight, pch=2, xlim=c(300,800), ylim=c(500,7000))
#weightsb = function(tl){exp(sbreg2$coef[1]) * tl^(sbreg2$coef[2])} 
#curve(weightsb, add=TRUE, lty=2)
#
#points(hb$tl, hb$weight, pch=19)
#weighthb = function(tl){exp(hbreg2$coef[1]) * tl^(hbreg2$coef[2])} 
#curve(weighthb, add=TRUE)
#
#weightb1 = function(tl){exp(b1reg2$coef[1]) * tl^(b1reg2$coef[2])} 
#curve(weightb1, add=TRUE, lwd=2)
##---or---
#
#
#
#
#
## THIS PLOT FOR THE PAPER
#par(mar=c(5,5,1,1))                     
#with(b1, plot(weight/1000~tl, pch=ptType, las=1, col=ptCol, 
#  cex.lab = 1.5, cex.axis=1.5, bty="l", xaxt="n",
#  xlab="Total Length (mm)", ylab="Weight (kg)",
#  xlim=c(300,750)
#))
#axis(1,seq(300,750,by=50),cex.axis=1.5)  
#
## change units form g to kg
#weightsbkg = function(tl){exp(sbreg2$coef[1]) * tl^(sbreg2$coef[2])/1000} 
#weighthbkg = function(tl){exp(hbreg2$coef[1]) * tl^(hbreg2$coef[2])/1000} 
#  
#curve(weightsbkg, add=TRUE, lty=1, lwd=2)
#curve(weighthbkg, add=TRUE, lty=2, lwd=2, col="red")
#legend(x=300,y=6.2,legend=c("Sand-bottom","Sand-bottom regression","Hard-bottom",
#  "Hard-bottom regression"), pch=c(19,NA,17,NA), lty=c(NA,1,NA,2), 
#  col=c("black","black","red","red"), lwd=2)
#
#
## try it with ggplot 
## this is panel b) of paper figure 5-12
#ggplot(b1,aes(x=log(tl),y=log(weight/1000),colour=Treatment, shape=Treatment, fill=Treatment))+
#  geom_point()+
#  geom_smooth(method="lm") +
#  theme_bw() +
#  #coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
#  scale_y_continuous("Log Weight (kg)") +
#  scale_x_continuous("Log Total Length (mm)") +
#  opts(axis.title.x = theme_text(size = 20)) +
#  opts(axis.title.y = theme_text(size=20, angle=90)) +  
#  opts(axis.text.x = theme_text(size = 15)) +
#  opts(axis.text.y = theme_text(size = 15))
#
#
#grid.text("b)", x = unit(0.15, "npc"), y = unit(0.95, "npc"),
#          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 
#  
#
#
#
#wtancova <- lm(log(weight)~log(tl)*treatment,data=b1)
#rwtancova <- MASS::rlm(log(weight)~log(tl)*treatment,data=b1)  ## robust version -- makes little difference
#
#
### provided that logging the response variable (tl) doesn't mess up the variance
### structure (i.e. variance is more or less independent of mean for logged data)
### then everything proceeds as before
#
#summary(wtancova)
#summary(rwtancova)
#
#par(mfrow=c(2,2))
#plot(wtancova,col=b1$treatment)                         
#MASS::boxcox(wtancova)  ## BMB> technically this says we should further transform the data,
### but I don't really believe it ...  may also be driven by outliers
####### The preceeding comment applies to data without the fish collected in 2011.
#
### also note that log(tl) coefficient is very close to 3
### (2.97 +/- 0.14 SE) -- allometry makes perfect sense (i.e.
###  fish grow isometrically in length/width/depth)
#
#b1[as.character(c(301:302,320)),]
#
#ggplot(b1,aes(x=tl,y=weight,colour=treatment)) +
#  geom_text(aes(label=number),size=3) + ##geom_point()+
#  scale_x_log10()+scale_y_log10() +
#  geom_smooth(method="lm")
#
#ggplot(b1,aes(x=tl,y=weight)) +#,colour=treatment)) +
#  geom_text(aes(label=number),size=3) + ##geom_point()+
#  #scale_x_log10()+scale_y_log10() +
#  #geom_smooth(method="lm")
#  scale_x_continuous("Total Length at Capture")+
#  scale_y_continuous("Weight at Capture") +
#  theme_bw()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#################################################################################
#################################################################################
#################################################################################
#### tagged fish biometrics
#################################################################################
#################################################################################
#
#
#
## pick only relevant data
#b1
#
#b2 = b1[,1:14]
#b3 = b2[b2$year1 == 2009,]
#b4 = b3[b3$tagged == "yes",]
#b5 = b4[!is.na(b4$tagged),]
#names(b5) = c("year","month","day","reef","treatment","rep","method","weight",
#  "girth","tl","fl","sizeclass","tagged","ID")
#
#plot(b5$girth, b5$weight, pch=19)
#plot(b5$tl, b5$weight, pch=as.numeric(b5$treatment))
#plot(b5$tl, b5$girth, pch=as.numeric(b5$treatment))
#ptType = as.numeric(b5$treatment)
#
#
#
## there are some missing weights... I want to use TL to estimate weights
## First I have to fit curves to hb and sb separately to say they're the same,
##   then pool them to get fitting parameters for the equation to predice
##   weight from tl
#
## After log transforming I'll follow steps similar to above...
#
## START WITH:  weight = c * tl^d
## log(weight) = log(c) + d * log(tl)
## ...so y = a + d * x
## ... and 'a' = log(c), 'b' = 'd', y = log(weight), x = log(tl) 
#
#with(b5, plot(weight~tl, col=treatment, pch=ptType))
#b1reg2 = lm(log(weight) ~ log(tl), b1)
#
## look at the log-transformed data
#plot(log(b5$tl), log(b5$weight))
#
## ANOVA to see if treatment has an effect
#wtancova <- lm(log(weight)~log(tl)*treatment,data=b5)
#rwtancova <- MASS::rlm(log(weight)~log(tl)*treatment,data=b1)  ## robust version -- makes little difference
#
#
### provided that logging the response variable (tl) doesn't mess up the variance
### structure (i.e. variance is more or less independent of mean for logged data)
### then everything proceeds as before
#
#summary(wtancova)   # CONCLUSION: treatment doesn't make a difference
#summary(rwtancova)
#
#par(mfrow=c(2,2))
#plot(wtancova,col=b5$treatment)                         
#MASS::boxcox(wtancova)  ## BMB> technically this says we should further transform the data,
### but I don't really believe it ...  may also be driven by outliers
#
### also note that log(tl) coefficient is very close to 3
### (2.97 +/- 0.14 SE) -- allometry makes perfect sense (i.e.
###  fish grow isometrically in length/width/depth)
#
## use this equation to predict weight from tl
#weightb5 = function(tl){exp(b5reg2$coef[1]) * tl^(b5reg2$coef[2])} 
#b5$weightPredicted = weightb5(b5$tl)
## now make a column holding the usable weights: measured ones plus filled in missing ones
#b5$weightUseable = b5$weight 
#b5$weightUseable[is.na(b5$weight)] = b5$weightPredicted[is.na(b5$weight)]
#
#
#plot(b5$weight,b5$weightPredicted)
#abline(0,1)
#
#
#
#
#
#
#
#ggplot(b1,aes(x=tl,y=weight,colour=treatment)) +
#  geom_text(aes(label=number),size=3) + ##geom_point()+
#  scale_x_log10()+scale_y_log10() +
#  geom_smooth(method="lm")
#
#ggplot(b1,aes(x=tl,y=weight)) +#,colour=treatment)) +
#  geom_text(aes(label=number),size=3) + ##geom_point()+
#  #scale_x_log10()+scale_y_log10() +
#  #geom_smooth(method="lm")
#  scale_x_continuous("Total Length at Capture")+
#  scale_y_continuous("Weight at Capture") +
#  theme_bw()
#
#
#
#
## total length histogram
#hbfish = b5$tl[ b5$treatment=="HB" ]
#sbfish = b5$tl[ b5$treatment=="SB" ]
#
#
#par(mar=c(4.5,4,1,1)+0.1)
#bob=multhist(list(sbfish,hbfish), freq=T, breaks=28,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
#  names.arg=tail(bob$breaks,-1)
#)
#axis(2,seq(0,20,by=2), las=1, cex.axis=1.5)
#mtext("Total Length at Capture (mm)    ",1,3, cex=1.5)
#mtext("Frequency",side=2,line=2.7,cex=1.7)
#
## weight histogram
#hbfish = b5$weightUseable[ b5$treatment=="HB" ]
#sbfish = b5$weightUseable[ b5$treatment=="SB" ]
#
#bob=multhist(list(sbfish,hbfish), freq=T, breaks=14,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  legend.text=c("Gag in sand-bottom landscapes","Gag in hard-bottom landscapes"),
#  names.arg=tail(bob$breaks,-1)
#)
#axis(2,seq(0,20,by=2), las=1, cex.axis=1.5)
#mtext("Weight (kg)    ",1,3, cex=1.5)
#mtext("Frequency",side=2,line=2.7,cex=1.7)
#
#
#
## a simple t-test to see of there's a difference in sizes
## test for equal variance
#var.test(sbfish,hbfish)
## t-test
#t.test(sbfish, y=hbfish, alternative="t", var.equal=FALSE)
#...so fish on hb are bigger than fish on sb a 
#
#
#
#
#
##################################################################################
#### compare my fish relative weights to the gag standard relative weight curve ##  
##################################################################################
#
#plot(b1$tl, b1$relwt, pch=19)
#abline(h=100)
#
#plot(b1$tl, b1$weight, pch=19)
#curve(stdWt,add=TRUE)
#
#
## a plot for the paper
#par(mar=c(5,5,1,1))
#with(b1,plot(relwt~tl,  pch=ptType, las=1, col=ptCol, 
#  cex.lab=1.5, cex.axis=1.5, bty="l", xaxt="n", yaxt="n",
#  xlim=c(350,750), ylim=c(80,135),
#  xlab="Total Length (mm)", ylab=""
#))
#abline(h=100, lwd=2)
#
## add horizontal or linear regression lines
#abline(h=mean(b1[b1$treatment=="SB",]$relwt, na.rm=T),lty=2,lwd=2)
#abline(h=mean(b1[b1$treatment=="HB",]$relwt, na.rm=T),lty=2,col="red",lwd=2)
#
## for writing 
#mean(b1$relwt,na.rm=T)
#
## abline(lm(relwt~tl, sb), lty=2, col="red", lwd=2)
## abline(lm(relwt~tl, hb), lty=1, lwd=2)
#
#axis(1,seq(350,750,by=50), cex.axis=1.5)
#axis(2,seq(80,135,by=5), las=1, cex.axis=1.5)
#mtext("Relative Weight (percent)",side=2,line=3.5,cex=1.7)
#
#legend(x=575,y=94,legend=c("Sand-bottom","Sand-bottom mean","Hard-bottom",
#  "Hard-bottom mean","Population relative weight"), 
#  pch=c(19,NA,17,NA,NA), lty=c(NA,2,NA,2,1), lwd=c(NA,2,NA,2,2),
#  col=c("black","black","red","red","black"))
#
## Is there a difference in the relative weight of fish between treatments?
#
#sb$relwt
#hb$relwt
#
## test for equal variance
#var.test(sb$relwt, hb$relwt)  # variances are not equal
## t-test
#t.test(sb$relwt, y=hb$relwt, alternative="t", var.equal=FALSE)
#...so were not different total numbers
#
#
#
#
#
#
#
#
##################################################################################
##################################################################################
#### an example of how to interpret anova ancova results ##############################  
##################################################################################
##################################################################################
## In these my questions generally group as:
## 1a. Am I correctly using lm() to compare two regression lines?
## 1b.  Am I correctly interpreting lm() output?
## 2a. Am I correctly log-transforming length-weight data to be linear so I can
##   fit curves to the data
## 2b. How do you test if two curved lines are equal?
#
#
## 1. BACK-CALCULATE LENGTHS
## back calculate total length using the two equations in Deb and Daryl's paper
##
## la = [ (a + b * ra)/(a + b * rc) ] * lc
## lc = a + b * rc
##
## la = back-calculated length to opaque zone 'a'
## a = intercept from the linear regression of total length as a function of 
##     otolith radius
## b = slope from same regression
## ra = otolith radius to opaque zone 'a' = b1$ultimateAnnulus
## rc = total otolith radius at time of capture = b1$otoRadius
## lc = total length at time of capture = b1$tl
## 
## first determine if the regressions of lc = a + b * rc are the same for both 
## treatments...using ANCOVA...
#
#sb = b1[b1$treatment=="SB",]
#hb = b1[b1$treatment=="HB",]
#
#
## look at all data
#with(b1,plot(tl~otoRadius, col=treatment, pch=19))
#
## regression of all data
#b1reg1 = lm(tl ~ otoRadius, b1)
#
##  regression of Sand-bottom treatment
#sbreg1 = lm(tl ~ otoRadius, sb)
#
## regression of hard-bottom treatment
#hbreg1 = lm(tl ~ otoRadius, hb)
#
## plot together
#with(b1,plot(tl~otoRadius,  col=treatment, pch=ptType, 
#  xlim=c(0.5,1.5), ylim=c(300,800), 
#  xlab="Otolith Radius at Capture", ylab="Total Length at Capture"))
## ---or---  
#plot(sb$otoRadius, sb$tl, pch=2, xlim=c(0.5,1.5), ylim=c(300,800))
#points(hb$otoRadius, hb$tl, pch=19)
### BMB> equivalent to above:
#abline(b1reg1,lwd=2)
#abline(sbreg1)
#abline(hbreg1, lty=2)
## ---or---
#ggplot(b1,aes(x=otoRadius,y=tl)) +#,colour=treatment)) +
#  geom_text(aes(label=b1$number),size=3) + ##geom_point() + 
#  #geom_smooth(method="lm")+
#  geom_smooth(method="lm",aes(group=NA)) +
#  scale_x_continuous("Otolith Radius at Capture")+
#  scale_y_continuous("Total Length at Capture") +
#  theme_bw()
#              
#
#
#
#
## compare regression lines using
## ...following root/fruit/grazing example in http://www.scribd.com/doc/50843947/ANCOVA-in-R
#
## using lm() 
#ancova = lm(tl ~ treatment * otoRadius,data=b1)
#summary(ancova)
#
### BMB> 'intercept' is for the first treatment (alphabetically) -- the expected average
### BMB> total length of a HB fish with otolith radius zero (not really a sensible number)
#
#
## - there is no effect of 'treatment' (p=0.7663) ...BUT WHY IS 'SB' ADDED TO THE NAME?
### BMB> because this is the effect of level "SB" relative to the baseline level "HB"
### BMB> if there were a third treatment (say "MB" for medium bottom) its difference
### BMB>  from the baseline level would be listed as treatmentMB
#
## - there is an effect of otoRadius on tl, p=0.0416 ... the slope is greater than zero
### BMB> yes
#
## - there is no interaction between treatment and otoRadius ... the slopes are equal
### BMB> yes, or not significantly different ...
#
#
## The next step is to remove the non-significant terms, manually or automatically
## using step().
#step(ancova)
#
## ...gives the following results with my interpretations
#
## first the full model and its AIC value...
#Start:  AIC=303.08
#tl ~ treatment * otoRadius
#
#                      Df Sum of Sq    RSS    AIC
#- treatment:otoRadius  1    1445.2 324915 301.22
#<none>                             323470 303.08
#
#
## remove the most complicated term...             
#Step:  AIC=301.22
#tl ~ treatment + otoRadius
#
#            Df Sum of Sq    RSS    AIC
#- treatment  1       534 325450 299.27
#<none>                   324915 301.22
#- otoRadius  1    122911 447826 309.49
## ...the AIC goes down so the removal is justified, there is no interaction
## between treatment and otoRadius, or the slopes are equal
#
### BMB yes.
#
## remove another term...
#Step:  AIC=299.27
#tl ~ otoRadius
#
#            Df Sum of Sq    RSS    AIC
#<none>                   325450 299.27
#- otoRadius  1    123936 449385 307.60
## ...the AIC goes down so the removal is justified, meaning that there is no
## difference between treatments, so the slopes and the intercepts are the same
#
## now do the regression using best model, which is all data together...
#Call:
#lm(formula = tl ~ otoRadius)
#
#Coefficients:
#(Intercept)    otoRadius  
#      196.6        316.4  
## this matches b1reg1      
#
### BMB> I don't think it's a particularly big deal in this case, but there
### BMB> are certainly situations in which the stepwise approach is bad -- it almost
### BMB> certainly underestimates the uncertainty on the slope ... but it is convenient
#
## Now use these fitted parameters in the back-calculation equation
#
#a <- coef(b1reg1)[1]
#b <- coef(b1reg1)[2]
#
#
#
#
###############################################################################
###############################################################################
#### I want to compare the size of the tagged fish and the size of the rest of
#### the fish on each reef at the beginning of each deployment. Perhaps later
#### I'll also compare the size of fish collected at the end and in subsequent
#### years.
###############################################################################
###############################################################################
#biometrics = importBiometricData()
#
## pick only relevant data
## I want
#bio1 = biometrics
#bio1 = bio1[bio1$year1 == "2009",]  # only fish from the 2009 experiment
#bio1 = bio1[!is.na(bio1$tagged),]      # fish collected at the end of the experiment
#                                    # have NA in this column. It would be better
#                                    # if the dataset had a column indicating 
#                                    # pre or post experiment.
#bio1$sizeRange = as.factor(bio1$sizeRange) # I think I need this for the barplot
#
#
## create a unique deployment name
#bio1$deployment = paste(bio1$HBSB,bio1$replicate,sep="")
#
## only keep some information about these fish
#bio1 = bio1[,c(
#  #'year1','month1','day1',
#  'deployment','reefID1',
#  'HBSB',#'replicate',
#  #'collectionMethod','weight1','girth1',
#  'TL1',
#  #'FL1',
#  'sizeRange','tagged'
#  #'tagID',
#  #'knockoutStart','taggingStart','recoveryStart','release','leftColor','rightcolor',
#  #'notes','mmNumber','recoveredTagID','year2','month2','day2','reefID2',
#  #'depth','gear',
#  #'TL2','FL2','weight2'
#  #'girth2',
#  #'lOto', 'lOtoWeightUseable','lOtoLengthAUseable','lOtoLengthBUseable',
#  #'lOtoLengthCUseable',
#  #'rOto','rOtoWeightUseable','rOtoLengthAUseable','rOtoLengthBUseable',
#  #'rOtoLengthCUseable',
#  #'monthD','debAgeclassCorrected','debAnnuli',
#  #'resolvedAgeclass',
#  #'otoRadius', 'ultimateAnnulus','penultimateAnnulus','growthIncrement'
#  )
#]
#
#names(bio1) = c("deployment","reef","treatment","tl","sizeRange","tagged")
#
## split by deployment
#bio1split = split(bio1,bio1$deployment)
#
#
#hist(as.numeric(as.character(bio1split[[1]]$sizeRange)),breaks=seq(15,75,by=10))
#
## I want a stacked histogram so you can see which of the fish were tagged
## ...so I need to get data into the right format
## ...I need to count the number of tagged and non-tagged fish in each size rang
#
#
#
#
## There must be an easier way
#
#totalCounts = array(
#  dim=c(length(bio1split),length(levels(bio1split[[1]]$sizeRange))),
#  dimnames=c("deployment","sizeRange")
#)  
#tagCounts = totalCounts
#notagCounts = totalCounts
#checkCounts = totalCounts
#
#
#for(i in 1:nrow(tagCounts)){ # for each deployment (row)
#  for(j in 1:ncol(tagCounts)){ # for each sizeRange (columns)
#    # count the total number of fish in this deployment, in this sizeRange
#    totalCounts[i,j] = sum(
#       bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j] 
#    )
#    # count the number of tagged fish in this deployment, in this sizeRange                            
#    tagCounts[i,j] = sum(
#      (bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j]) &
#      (bio1split[[i]]$tagged == "yes") 
#    )
#      
#    # count the number of not-tagged fish in this deployment, in this sizeRange                            
#    notagCounts[i,j] = sum(  
#      (bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j]) &
#      (bio1split[[i]]$tagged != "yes") 
#    )
#    
#   # do a check
#   checkCounts[i,j] = tagCounts[i,j] + notagCounts[i,j] - totalCounts[i,j]
# 
#  } # end j loop
#} # end i loop
#
#
#
###############################################################################
## I want the plot to be sb first.
#sbhbOrder = c(4:7,1:3)
#plotnames = c("Hard-bottom 1", "Hard-bottom 2", "Hard-bottom 3",
#  "Sand-bottom 1", "Sand-bottom 2", "Sand-bottom 3", "Sand-bottom 4") 
#
#windows(9,5)
#par(mfrow=c(2,4), oma=c(3,5,0,1), mar=c(2,1,2,0), xpd=TRUE)
#
## now place each histogram with it's own margins and axis labels
#i=4
#barplot(rbind(tagCounts[i,],notagCounts[i,]), 
#  las=1, col=c("grey","white"),
#  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
#  ylim=c(0,30), xaxt="n"
#)                                                                     
#
#for(i in 5:6){
#  barplot(rbind(tagCounts[i,],notagCounts[i,]), 
#    col=c("grey","white"),
#    space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, ylim=c(0,30),
#    xaxt="n", yaxt="n"
#  )
#  axis(2,at=seq(0,30,by=5),labels=rep("",7))
#}
#
#i=7
#barplot(rbind(tagCounts[i,],notagCounts[i,]), 
#  col=c("grey","white"),
#  #names.arg=c(levels(bio1split[[1]]$sizeRange)), 
#  names.arg=c("20-30","","40-50","","60-70",""),
#  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
#  ylim=c(0,30), yaxt="n"
#)
#axis(2,at=seq(0,30,by=5),labels=rep("",7))
#
#i=1
#barplot(rbind(tagCounts[i,],notagCounts[i,]), col=c("grey","white"), 
#  #names.arg=c(levels(bio1split[[1]]$sizeRange)), 
#  las=1,
#  names.arg=c("20-30","","40-50","","60-70",""),
#  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
#  ylim=c(0,30)
#)
#
#for(i in 2:3){
#  barplot(rbind(tagCounts[i,],notagCounts[i,]), col=c("grey","white"), 
#    #names.arg=c(levels(bio1split[[1]]$sizeRange)),
#    names.arg=c("20-30","","40-50","","60-70",""),
#    space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
#    ylim=c(0,30), yaxt="n"
#  )
#  axis(2,at=seq(0,30,by=5),labels=rep("",7))
#}
# 
## add a blank plot as a legend
#plot(c(0.4,0.4),c(2,2.7), xaxt="n", yaxt="n", bty="n", 
#  xlim=c(0,5), ylim=c(0,4),
#  pch=22, cex=4, bg=c("grey","white")
#)
#text(c(0.6,0.6),c(2,2.7),labels=c("Tagged Gag","Non-tagged Gag"), cex=1.5, pos=4)
##put a box around the "legend"
#points(c(0,5,5,0,0),c(1.7,1.7,3,3,1.7), type="l", lwd=1.5)
#
#mtext("Total Length Category (mm)", side=1, line=1.2, outer=T, cex=2)
#mtext("Frequency", side=2, line=2.5, outer=T, cex=2.2)
##mtext("Size Distributions", side=3, line=0.5, outer=T, cex=2)
#
#
#
#
#
#
#################################################################################
#################################################################################
## create histograms comparing SB and HB fish:
## 1. weight of tagged fish
## 2. tl of all fish on reef at time of tagging
## 3. tl of tagged fish
#################################################################################
#
#biometrics = importBiometricData()
#bio1 = biometrics
#bio1 = bio1[bio1$year1 == "2009",]  # only fish from the 2009 experiment
#bio1 = bio1[,1:14]                  # to make it manageable
#bio1 = bio1[!is.na(bio1$tagged),]   # fish collected at the end of the experiment
#                                    # have NA in this column. It would be better
#                                    # if the dataset had a column indicating 
#                                    # pre or post experiment.
#bio1 = bio1[!(bio1$HBSB=="SB" & bio1$replicate==1),] # don't use SB1 
#bio1$sizeRange = as.numeric(as.character(bio1$sizeRange)) # I think I need this for the multihist
#
#
#
#
#
#
## only keep some information about these fish
#bio1 = bio1[,c(
#  #'year1','month1','day1','reefID1',
#  'HBSB',#'replicate',
#  #'collectionMethod',
#  'weight1',#'girth1',
#  'TL1',
#  #'FL1',
#  'sizeRange','tagged'
#  #'tagID',
#  #'knockoutStart','taggingStart','recoveryStart','release','leftColor','rightcolor',
#  #'notes','mmNumber','recoveredTagID','year2','month2','day2','reefID2',
#  #'depth','gear',
#  #'TL2','FL2','weight2'
#  #'girth2',
#  #'lOto', 'lOtoWeightUseable','lOtoLengthAUseable','lOtoLengthBUseable',
#  #'lOtoLengthCUseable',
#  #'rOto','rOtoWeightUseable','rOtoLengthAUseable','rOtoLengthBUseable',
#  #'rOtoLengthCUseable',
#  #'monthD','debAgeclassCorrected','debAnnuli',
#  #'resolvedAgeclass',
#  #'otoRadius', 'ultimateAnnulus','penultimateAnnulus','growthIncrement'
#  )
#]
#
#names(bio1) = c("treatment","weight","tl","sizeRange","tagged")
#
#
#
#
#
#
## separate by treatment
#sbTotal = bio1[bio1$treatment=="SB",]                         
#hbTotal = bio1[bio1$treatment=="HB",]
#sbTagged = bio1[bio1$treatment=="SB" & bio1$tagged=="yes",]
#hbTagged = bio1[bio1$treatment=="HB" & bio1$tagged=="yes",]
#
#
###### I want a three panel with histograms
#
###### total length distributions of tagged fish 
#windows(5,10)
#par(mfrow=c(3,1))
#par(mar=c(4,5,2,1))           
#bob=multhist(list(sbTagged$tl,hbTagged$tl), 
#  freq=T, breaks=28, space=c(0,0.5), axes=F,
#  cex.axis=1, cex.names=1.5,
#  legend.text=c("Sand-bottom","Hard-bottom"),
#  names.arg=tail(bob$breaks,-1)
#)
#axis(2,0:10, las=1, cex.axis=1.5)
#mtext("Total Length (mm)",1,2.7, cex=1.2)
#mtext("Frequency",side=2,line=3,cex=1.5)
#text(x=1,y=3.8,labels="a)",cex=1.7)
#
#
##### total length category distributions of all fish on reef at time of tagging
#ben=multhist(list(sbTotal$sizeRange,hbTotal$sizeRange), freq=T, 
#  breaks=seq(15,75,by=10),
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,             
#  names.arg=c("20-30","","40-50","","60-70","")
#)
#axis(2,seq(0,50,by=10), las=1, cex.axis=1.5)
#mtext("Total Length Category (cm)",1,2.7, cex=1.2)
#mtext("Frequency",side=2,line=3,cex=1.5)
#text(x=0.7,y=50,labels="b)", cex=1.7)
#
## weight histogram of all fish captured at time of tagging
#sam=multhist(list(sbTotal$weight,hbTotal$weight), freq=T, breaks=14,
#  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
#  names.arg=tail(sam$breaks,-1)
#)
#axis(2,seq(0,30,by=5), las=1, cex.axis=1.5)
#mtext("Weight (kg)",1,2.7, cex=1.2)
#mtext("Frequency",side=2,line=3,cex=1.5)
#text(x=3,y=29,labels="c)", cex=1.7)
#
#
#
#
#
#
#
#
#
#
#
#
#