source("C://ZyC//R Data Processing for gag NOT WORKING COPY//global variables.r")
source("C://ZyC//R Data Processing for gag NOT WORKING COPY//global functions.r")
source("C://ZyC//R Data Processing for gag NOT WORKING COPY//global metadata.r")



library(ggplot2)
library(date)
library(plotrix) # for multhist
library(gdata) # for drop.levels
library(compute.es) # for ANCOVA effect size
library(effects) # for ANCOVA group means


### These are the things I want to do in this file...
#   ...for gag at reef at time of tagging, reef-as-replicate
1. total gag at reef at time of tagging
  a. gag not caught: counted, estimated 10cm size-class
  b. gag caught and not tagged: length, weight
  c. gag caught and tagged: length, weight
2. reef mean abundance and t-test 
3. reef mean 10cm size-class of all (caught + non-caught) gag and t-test
4. reef mean weight and mean length of tagged and t-test
5. reef mean weight and mean length comparing stay and leave tagged gag...did little ones leave?
#  ...for gag collected for growth estimates after experiment, pooled by treatment
6. weight of collected, compare HBSB
7. length of collected, compare HBSB
8. fractional age, compare HBSB 
9. relate length to ageFrac, ANCOVA
10. relate weight, length, log-linear ANCOVA
11. gut contents analysis, compare HBSB..unsure


# get fish biometric data.  This contains data recorded in the field on tagging
# day and any recaptures.  It also contains the otolith data.  
biometrics = importBiometricData()
b1 = biometrics

# ...keep gag for 2009 experiment
b1 = b1[as.character(b1$year1)>2008,]

# for pretty labeling in ggplot
b1$treatment[b1$HBSB == "HB"] = "Hard-bottom"
b1$treatment[b1$HBSB == "SB"] = "Sand-bottom"   
#
ptType = as.numeric(b1$HBSB)
ptCol = ptSize = ptType 
ptSize[ ptType == 1 ] = 0.7  # HB smaller
ptSize[ ptType == 2 ] = 1  # SB arger
ptType[ ptType == 1 ] = 17  # HB - triangle
ptType[ ptType == 2 ] = 19  # SB - circle
ptCol[ ptCol == 1 ] = "black"  # HB
ptCol[ ptCol == 2 ] = "grey60"   # SB
#
b1$repName = paste(b1$HBSB,b1$replicate)


# ...drop many unused columns
b1 = b1[,c(
  'tagID','recoveredTagID','tagged','mmNumber','reefID1','HBSB','treatment',
  'replicate','repName',
  'year1','month1','day1','TL1','FL1','sizeRange','weight1',
  'year2','month2','day2','TL2','FL2','weight2',
  'debAgeclassCorrected','fractionalAgeD',
  'gutPresent','gutCount','gutWeight')
]

names(b1) = c(
  "id","recoveredID","tagged","mmNumber","reefID","HBSB","treatment",
  "replicate","repName",
  "year1","month1","day1","tl1","fl1","sizeRange1","weight1",
  "year2","month2","day2","tl2","fl2","weight2",
  "ageClass","ageFrac",
  "gutPresent","gutCount","gutWeight")
b1$sizeRange1 = as.numeric(as.character(b1$sizeRange1))




####################################################################
# gag counted, caught, and tagged during tagging at reefs during 2009,
#      excluding the failed replicate
b2 = b1
b2 = b2[(!is.na(b2$tagged) & b2$repName != "SB 1"),
  c("id","tagged","mmNumber","reefID","HBSB","treatment",
  "replicate","repName","tl1","fl1","sizeRange1","weight1")]

sb2 = b2[b2$HBSB == "SB",]
hb2 = b2[b2$HBSB == "HB",]
sb2$replicate = factor(sb2$replicate)
hb2$replicate = factor(hb2$replicate)

# number of gag counted at each reef  
table(b2$repName)         

# Is there a difference in the number of total fish seen on reefs?
sb2counts = table(sb2$repName)
hb2counts = table(hb2$repName)
var.test(sb2counts, hb2counts)  # variances are equal
t.test(sb2counts, y=hb2counts, alternative="two.sided", var.equal=TRUE)
#...so were not different total numbers

# Is there a difference in the 10 cm size-classes of all fish on reefs
sbmeans = tapply(sb2$sizeRange1, sb2$replicate, mean)
hbmeans = tapply(hb2$sizeRange1, hb2$replicate, mean)
var.test(sbmeans,hbmeans) # variances equal
t.test(sbmeans,y=hbmeans, alternative="two.sided", var.equal=T)
#...sizes equal

# histogram of 10 cm size-classes of reefs
par(mar=c(4.5,4,1,1)+0.1)
hist1 = multhist(list(sb2$sizeRange1,hb2$sizeRange1),freq=T,col=c("grey","black"),
  breaks=c(15,25,35,45,55,65,75),ylim=c(0,60),
  cex.axis=1, cex.names=1.5, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=c("20-30","30-40","40-50","50-60","60-70","70-80")
)
axis(2,seq(0,60,by=10), las=1, cex.axis=1.5)
mtext("Total Length Category (cm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
# does this equal the original fish counts of all fish counted on all reefs
sum(hist1$out) == sum(table(b2$repName))


# Is there a difference between tagged fish
b3 = b2
b3 = b3[b2$tagged == "yes" ,]
sb3 = b3[b3$HBSB == "SB",]
hb3 = b3[b3$HBSB == "HB",]


# Is there a difference in total length of tagged fish 
sbmeans = tapply(sb3$tl1, sb3$replicate, mean)
hbmeans = tapply(hb3$tl1, hb3$replicate, mean)
var.test(sbmeans,hbmeans) # variances equal
t.test(sbmeans,y=hbmeans, alternative="two.sided", var.equal=T)
#...sizes equal
  
# histogram of tagged fish total length
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb3$tl1,hb3$tl1),freq=T,plot=F,breaks=seq(300,800,by=50))
hist2 = multhist(list(sb3$tl1,hb3$tl1),freq=T,col=c("grey","black"),
  ylim=c(0,10),breaks=seq(300,800,by=50),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,10,by=1), las=1, cex.axis=1.5)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
# does this equal the original fish counts of all fish counted on all reefs
sum(hist2$out) == 48 # 8 tagged fish on 6 good reefs = 48


# Is there a difference in fork length of tagged fish 
sbmeans = tapply(sb3$fl1, sb3$replicate, mean)
hbmeans = tapply(hb3$fl1, hb3$replicate, mean)
var.test(sbmeans,hbmeans) # variances equal
t.test(sbmeans,y=hbmeans, alternative="two.sided", var.equal=T)
#...sizes equal
  
# histogram of tagged fish fork length
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb3$fl1,hb3$fl1),freq=T,plot=F,breaks=seq(300,800,by=50))
multhist(list(sb3$fl1,hb3$fl1),freq=T,col=c("grey","black"),
  ylim=c(0,8),breaks=seq(300,800,by=50),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,8,by=1), las=1, cex.axis=1.5)
mtext("Fork Length (mm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)


# Is there a difference in weight of tagged fish 
sbmeans = tapply(sb3$weight1, sb3$replicate, mean)
hbmeans = tapply(hb3$weight1, hb3$replicate, mean)
var.test(sbmeans,hbmeans) # variances equal
t.test(sbmeans,y=hbmeans, alternative="two.sided", var.equal=T)
#...sizes equal
  
# histogram of tagged fish weight
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb3$weight1,hb3$weight1),freq=T,plot=F,breaks=0:7)
hist3 = multhist(list(sb3$weight1,hb3$weight1),freq=T,col=c("grey","black"),
  ylim=c(0,16),breaks=0:7,
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,16,by=1), las=1, cex.axis=1.5)
mtext("Weight (kg)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
# does this equal the original fish counts of all fish counted on all reefs
sum(hist3$out) == 48 # 8 tagged fish on 6 good reefs = 48


### combine some of these for paper figure ###############
par(mfrow=c(3,1))
# histogram of 10 cm size-classes of reefs
par(mar=c(4.5,5,1,0)+0.1)
bob=multhist(list(sb2$sizeRange1,hb2$sizeRange1),freq=T,col=c("grey","black"),
  breaks=c(15,25,35,45,55,65,75),ylim=c(0,50),
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  #legend.text=c("Sand-bottom","Hard-bottom"), 
  names.arg=c("20-30","30-40","40-50","50-60","60-70","70-80")
)
axis(2,seq(0,50,by=10), las=1, cex.axis=2)
mtext("Total Length Category (cm)",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=0.4,y=45,labels="a)", cex=2)


# histogram of tagged fish total length
par(mar=c(4.5,5,1,0)+0.1)
temp1 = multhist(list(sb3$tl1,hb3$tl1),freq=T,plot=F,breaks=seq(300,800,by=50))
bob=multhist(list(sb3$tl1,hb3$tl1),freq=T,col=c("grey","black"),
  ylim=c(0,10),breaks=seq(300,800,by=50),
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,10,by=2), las=1, cex.axis=2)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=0.4,y=9,labels="b)", cex=2)

# histogram of tagged fish weight
par(mar=c(4.5,5,1,1)+0.1)
temp1 = multhist(list(sb3$weight1,hb3$weight1),freq=T,plot=F,breaks=0:7)
bob=multhist(list(sb3$weight1,hb3$weight1),freq=T,col=c("grey","black"),
  ylim=c(0,16),breaks=0:7,
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,16,by=4), las=1, cex.axis=2)
mtext("Weight (kg)",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=2,y=15,labels="c)", cex=2)
legend(x=10,y=15,legend=c("Sand-bottom","Hard-bottom"),fill=c("grey","black"),
  cex=2)
### end of paper figure ###############


# is there a difference between total length of non-tagged and tagged
# Is there a difference between tagged fish
b6 = b2
b6 = b6[b2$tagged != "not collected" ,] # keep only fish brought onto boat
sb6 = b6[b6$HBSB == "SB",]
hb6 = b6[b6$HBSB == "HB",]


# Is there a difference in total length of non-tagged and tagged fish 
sbmeans = tapply(sb6$tl1, sb6$replicate, mean)
hbmeans = tapply(hb6$tl1, hb6$replicate, mean)
var.test(sbmeans,hbmeans) # variances equal
t.test(sbmeans,y=hbmeans, alternative="two.sided", var.equal=T)
#...sizes equal
  
# histogram of non-tagged and tagged fish total length
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb6$tl1,hb6$tl1),freq=T,plot=F,breaks=seq(250,800,by=50))
multhist(list(sb6$tl1,hb6$tl1),freq=T,col=c("grey","black"),
  ylim=c(0,21),breaks=seq(250,800,by=50),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,22,by=2), las=1, cex.axis=1.5)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)






# Is there a difference in total length between tagged fish that stayed and left
#   ...which fish stayed and which left
md[[3]]$fishNames
md[[3]]$goodFishNames
# pick the right deployments
mdlist = md[c(3,5:9)]
goodfish = c()
for(i in 1:length(mdlist)){
  goodfish = c(goodfish,mdlist[[i]]$goodFishNames)
}

b3$goodfish = paste("f",b3$id,sep="") %in% goodfish
goods = b3[b3$goodfish == TRUE,]
bads = b3[b3$goodfish == FALSE,]

goodsmeans = tapply(goods$tl1, goods$replicate, mean)
badsmeans = tapply(bads$tl1, bads$replicate, mean)
var.test(goodsmeans,badsmeans) # variances equal
t.test(goodsmeans,y=badsmeans, alternative="two.sided", var.equal=T)
#...sizes equal
  
# histogram of stayers and leavers
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(goods$tl1,bads$tl1),freq=T,plot=F,
  breaks=seq(300,800,by=50))
multhist(list(goods$tl1,bads$tl1),freq=T,col=c("grey","black"),
  ylim=c(0,10),breaks=seq(300,800,by=50),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag remaining at the reef",
  "Gag leaving the reef"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,10,by=1), las=1, cex.axis=1.5)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)

# Is there a difference in the number of gag leaving between treatments




####################################################################
# gag caught for growth and diet data for the 2009 experiment
b4 = b1
b4 = b4[!is.na(b4$mmNumber), c("id","mmNumber","HBSB","treatment",
  "tl2","fl2","weight2","ageClass","ageFrac","gutCount","gutWeight")]

# for pretty plot labels
b4$Treatment = b4$treatment


sb4 = b4[b4$HBSB == "SB",]
hb4 = b4[b4$HBSB == "HB",]

# number of gag in each treatment  
table(b4$HBSB)           

# Is there a difference in total length of collected 
var.test(sb4$tl2, hb4$tl2)  # variances are equal
t.test(sb4$tl2, y=hb4$tl2, alternative="two.sided", var.equal=TRUE)
#...so were not different total length

# histogram of collected fish total length
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb4$tl2,hb4$tl2),freq=T,plot=F,breaks=seq(300,800,by=50))
multhist(list(sb4$tl2,hb4$tl2),freq=T,col=c("grey","black"),
  ylim=c(0,10),breaks=seq(300,800,by=50),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,10,by=1), las=1, cex.axis=1.5)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)
legend(x=9,y=55,legend=c("Sand-bottom","Hard-bottom"),fill=c("grey","black"),
  cex=2)
  


# Is there a difference in weight of collected 
var.test(sb4$weight2, hb4$weight2)  # variances are equal
t.test(sb4$weight2, y=hb4$weight2, alternative="two.sided", var.equal=TRUE)
#...no difference

# histogram of collected fish weight
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb4$weight2,hb4$weight2),freq=T,plot=F,breaks=seq(0,7000,by=500))
multhist(list(sb4$weight2,hb4$weight2),freq=T,col=c("grey","black"),
  ylim=c(0,20),breaks=seq(0,7000,by=500),
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,20,by=2), las=1, cex.axis=1.5)
mtext("Weight (g)",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)


# Is there a difference in ageFrac of collected 
var.test(sb4$ageFrac, hb4$ageFrac)  # variances are equal
t.test(sb4$ageFrac, y=hb4$ageFrac, alternative="two.sided", var.equal=TRUE)
#...no difference

# histogram of tagged fish total length
par(mar=c(4.5,4,1,1)+0.1)
temp1 = multhist(list(sb4$ageFrac,hb4$ageFrac),freq=T,plot=F,breaks=1:5)
multhist(list(sb4$ageFrac,hb4$ageFrac),freq=T,col=c("grey","black"),
  ylim=c(0,20),breaks=1:5,
  cex.axis=1, cex.names=1.2, space=c(0,0.5), axes=F,
  legend.text=c("Gag in sand-bottom landscapes",
  "Gag in hard-bottom landscapes"),
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,20,by=2), las=1, cex.axis=1.5)
mtext("Age Class",1,3, cex=1.5)
mtext("Frequency",side=2,line=2.7,cex=1.7)
#text(x=0.3,y=19.8,labels="c)", cex=1.5)






### combine some of these for paper figure ###############
par(mfrow=c(3,1))
# histogram of collected fish total length
par(mar=c(4.5,5,1,0)+0.1)
temp1 = multhist(list(sb4$tl2,hb4$tl2),freq=T,plot=F,breaks=seq(300,750,by=50))
multhist(list(sb4$tl2,hb4$tl2),freq=T,col=c("grey","black"),
  ylim=c(0,10),breaks=seq(300,750,by=50),
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,10,by=2), las=1, cex.axis=2)
mtext("Total Length (mm)",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=0.3,y=9.5,labels="a)", cex=2)
legend(x=15,y=9,legend=c("Sand-bottom","Hard-bottom"),fill=c("grey","black"),
  cex=2)
  
# histogram of collected fish weight
par(mar=c(4.5,5,1,0)+0.1)
temp1 = multhist(list(sb4$weight2,hb4$weight2),freq=T,plot=F,breaks=seq(0,6500,by=500))
multhist(list(sb4$weight2,hb4$weight2),freq=T,col=c("grey","black"),
  ylim=c(0,20),breaks=seq(0,6500,by=500),
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,20,by=5), las=1, cex.axis=2)
mtext("Weight (g)",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=0.3,y=19,labels="b)", cex=2)

# histogram of tagged fish total length
par(mar=c(4.5,5,1,0)+0.1)
temp1 = multhist(list(sb4$ageFrac,hb4$ageFrac),freq=T,plot=F,breaks=1:5)
multhist(list(sb4$ageFrac,hb4$ageFrac),freq=T,col=c("grey","black"),
  ylim=c(0,20),breaks=1:5,
  cex.axis=1, cex.names=2, space=c(0,0.5), axes=F,
  names.arg=tail(temp1$breaks,-1)
)
axis(2,seq(0,20,by=5), las=1, cex.axis=2)
mtext("Age Class",1,3, cex=1.5)
mtext("Count",side=2,line=3.3,cex=1.7)
text(x=0.4,y=19,labels="c)", cex=2)
### end figure for paper #################




# Are there differences between the age-total length curves between treatments
b4reg = lm(tl2 ~ ageFrac, b4) # regression of all data
sb4reg = lm(tl2 ~ ageFrac, sb4)  #  regression of Sand-bottom treatment
hb4reg = lm(tl2 ~ ageFrac, hb4) # regression of hard-bottom treatment
               
# Now compare regression lines using lm() 
ancova = lm(tl2 ~ treatment * ageFrac, data=b4)
summary(ancova)               

library(RVAideMemoire)
multcomp(ancova)
ancova.multcomp(ancova)

glht(ancova)

# plot together with fractional age              
# make black dots smaller
par(mar=c(5,5,1,1)) 
plot(sb4$ageFrac,sb4$tl2,pch=19, bty="l", cex.lab=1.5, cex.axis=1.5, 
  xaxt="n", yaxt="n", 
  col="grey60",cex=1,
  xlim=c(1.5,5), ylim=c(300,750),
  xlab="Fractional Age at Capture", ylab=""
)
points(hb4$ageFrac,hb4$tl2, pch=19, col="black", cex=0.7)
#abline(b1reg4)
abline(sb4reg, lty=1, lwd=2, col="grey60")
abline(hb4reg, lty=1, lwd=2)
mtext(text="Total Length (mm)", side=2, line=3.5, cex=1.7)
axis(1,seq(1.5,5,by=0.5),cex.axis=1.5)
axis(2,seq(300,750,by=50),las=1,cex.axis=1.5)

legend(x=1.5,y=750,legend=c("Sand-bottom","Hard-bottom"),cex=1.5, 
  pch=c(19,19), pt.cex=c(1,1),
  lty=c(1.2,1.3), 
  col=c("grey60","black"), lwd=2)


# Are there differences in the length-weight curves between two treatments

# START WITH:  weight = c * tl^d
# log(weight) = log(c) + d * log(tl)
# ...so y = a + d * x
# ... and 'a' = log(c), 'b' = 'd', y = log(weight), x = log(tl) 

b4reg2 = lm(log(weight2) ~ log(tl2), b4) # regression of all data
sb4reg2 = lm(log(weight2) ~ log(tl2), sb4) #  regression of Sand-bottom treatment
hb4reg2 = lm(log(weight2) ~ log(tl2), hb4) #  regression of hard-bottom treatment

ancova <- lm(log(weight2)~treatment*log(tl2),data=b4)
summary(ancova)


par(mar=c(5,5,1,1)) 
plot(log(sb4$tl2),log(sb4$weight2),pch=19, bty="l", cex.lab=1.5, cex.axis=1.5, 
  xaxt="n", yaxt="n", 
  col="grey60",cex=1,
  xlim=c(5.8,6.6), ylim=c(6,8.5),
  xlab="Log Total Length", ylab=""
)
points(log(hb4$tl2),log(hb4$weight2), pch=19, col="black", cex=0.7)
abline(sb4reg2, lty=1, lwd=2, col="grey60")
abline(hb4reg2, lty=1, lwd=2)
mtext(text="Log Weight", side=2, line=3.5, cex=1.7)
axis(1,seq(5.8,6.6,by=0.2),cex.axis=1.5)
axis(2,seq(6,8.5,by=0.5),las=1,cex.axis=1.5)

legend(x=5.8,y=8.5,legend=c("Sand-bottom","Hard-bottom"),cex=1.5, 
  pch=c(19,19), pt.cex=c(1,1),
  lty=c(1.2,1.3), 
  col=c("grey60","black"), lwd=2)


# PLOTS for the final publication
# this is panel a) of paper figure 5-12
ggplot(b4,aes(x=ageFrac,y=tl2, colour=Treatment, shape=Treatment, fill=Treatment))+
  scale_color_manual(values=c("red", "blue")) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  #coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
  scale_y_continuous("Total Length (mm)") +
  scale_x_continuous("Fractional Age (yr)") +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90, vjust=0.3)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))
grid.text("a)", x = unit(0.15, "npc"), y = unit(0.95, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 

# this is panel b) of paper
ggplot(b4,aes(x=log(tl2),y=log(weight2/1000),colour=Treatment, shape=Treatment, 
  fill=Treatment))+
  scale_color_manual(values=c("red", "blue")) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_y_continuous("Log Weight (kg)") +
  scale_x_continuous("Log Total Length (mm)") +
  #coord_cartesian(xlim=c(-100,100), ylim=c(-100,100)) +
  opts(axis.title.x = theme_text(size = 20)) +
  opts(axis.title.y = theme_text(size=20, angle=90)) +  
  opts(axis.text.x = theme_text(size = 15)) +
  opts(axis.text.y = theme_text(size = 15))
grid.text("b)", x = unit(0.15, "npc"), y = unit(0.95, "npc"),
          hjust=0, vjust=1, gp=gpar(fontsize=15)) ## right- and top- justified 





par(mfrow=c(2,2))
plot(wtancova,col=ptCol)                         
MASS::boxcox(wtancova)  



##############################################################################
##############################################################################
### I want to compare the size of the tagged fish and the size of the rest of
### the fish on each reef at the beginning of each deployment. 
##############################################################################
##############################################################################

# pick only relevant data
bio1 = b1
bio1 = bio1[!is.na(bio1$tagged),]      # fish collected at the end of the experiment
                                    # have NA in this column. It would be better
                                    # if the dataset had a column indicating 
                                    # pre or post experiment.
bio1$sizeRange = as.factor(bio1$sizeRange) # I think I need this for the barplot


# create a unique deployment name
bio1$deployment = paste(bio1$HBSB,bio1$replicate,sep="")

# only keep some information about these fish
bio1 = bio1[,c('repName','reefID','HBSB','tl1','sizeRange1','tagged')]
names(bio1) = c("deployment","reef","treatment","tl","sizeRange","tagged")
bio1$sizeRange = as.factor(bio1$sizeRange)

# split by deployment
bio1split = split(bio1,bio1$deployment)
hist(as.numeric(as.character(bio1split[[1]]$sizeRange)),breaks=seq(15,75,by=10))

# I want a stacked histogram so you can see which of the fish were tagged
# ...so I need to get data into the right format
# ...I need to count the number of tagged and non-tagged fish in each size range
totalCounts = tagCounts = notagCounts = checkCounts = array(
  dim=c(length(bio1split),length(unique(bio1$sizeRange))),
  dimnames=c("deployment","sizeRange")
)  

for(i in 1:nrow(tagCounts)){ # for each deployment (row)
  for(j in 1:ncol(tagCounts)){ # for each sizeRange (columns)
    # count the total number of fish in this deployment, in this sizeRange
    totalCounts[i,j] = sum(
       bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j] 
    )
    # count the number of tagged fish in this deployment, in this sizeRange                            
    tagCounts[i,j] = sum(
      (bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j]) &
      (bio1split[[i]]$tagged == "yes") 
    )
      
    # count the number of not-tagged fish in this deployment, in this sizeRange                            
    notagCounts[i,j] = sum(  
      (bio1split[[i]]$sizeRange == levels(bio1split[[i]]$sizeRange)[j]) &
      (bio1split[[i]]$tagged != "yes") 
    )
    
   # do a check
   checkCounts[i,j] = tagCounts[i,j] + notagCounts[i,j] - totalCounts[i,j]
 
  } # end j loop
} # end i loop



##############################################################################
# I want the plot to be sb first.
sbhbOrder = c(4:7,1:3)
plotnames = c("Hard-bottom 1", "Hard-bottom 2", "Hard-bottom 3",
  "Sand-bottom 1", "Sand-bottom 2", "Sand-bottom 3", "Sand-bottom 4") 

windows(9,5)
par(mfrow=c(2,4), oma=c(3,5,0,1), mar=c(2,1,2,0), xpd=TRUE)

# now place each histogram with it's own margins and axis labels
i=4
barplot(rbind(tagCounts[i,],notagCounts[i,]), 
  las=1, col=c("grey","white"),
  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
  ylim=c(0,30), xaxt="n"
)                                                                     

for(i in 5:6){
  barplot(rbind(tagCounts[i,],notagCounts[i,]), 
    col=c("grey","white"),
    space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, ylim=c(0,30),
    xaxt="n", yaxt="n"
  )
  axis(2,at=seq(0,30,by=5),labels=rep("",7))
}

i=7
barplot(rbind(tagCounts[i,],notagCounts[i,]), 
  col=c("grey","white"),
  #names.arg=c(levels(bio1split[[1]]$sizeRange)), 
  names.arg=c("20-30","","40-50","","60-70",""),
  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
  ylim=c(0,30), yaxt="n"
)
axis(2,at=seq(0,30,by=5),labels=rep("",7))

i=1
barplot(rbind(tagCounts[i,],notagCounts[i,]), col=c("grey","white"), 
  #names.arg=c(levels(bio1split[[1]]$sizeRange)), 
  las=1,
  names.arg=c("20-30","","40-50","","60-70",""),
  space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
  ylim=c(0,30)
)

for(i in 2:3){
  barplot(rbind(tagCounts[i,],notagCounts[i,]), col=c("grey","white"), 
    #names.arg=c(levels(bio1split[[1]]$sizeRange)),
    names.arg=c("20-30","","40-50","","60-70",""),
    space=0, main=plotnames[i], cex.main=1.5, cex.names=1.5, cex.axis=2, 
    ylim=c(0,30), yaxt="n"
  )
  axis(2,at=seq(0,30,by=5),labels=rep("",7))
}
 
# add a blank plot as a legend
plot(c(0.4,0.4),c(2,2.7), xaxt="n", yaxt="n", bty="n", 
  xlim=c(0,5), ylim=c(0,4),
  pch=22, cex=4, bg=c("grey","white")
)
text(c(0.6,0.6),c(2,2.7),labels=c("Tagged Gag","Non-tagged Gag"), cex=1.5, pos=4)
#put a box around the "legend"
points(c(0,5,5,0,0),c(1.7,1.7,3,3,1.7), type="l", lwd=1.5)

mtext("Total Length Category (cm)", side=1, line=1.2, outer=T, cex=2)
mtext("Frequency", side=2, line=2.5, outer=T, cex=2.2)
#mtext("Size Distributions", side=3, line=0.5, outer=T, cex=2)






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

# do some looking
plot(g1$HBSB, g1$tl2)
sbtl = g1$tl2[g1$HBSB == "SB"]
hbtl = g1$tl2[g1$HBSB == "HB"]
abline(h=mean(sbtl),col="red")
abline(h=mean(hbtl),col="blue")

var.test(sbtl, hbtl)  # variances are equal
t.test(sbtl, y=hbtl, alternative="two.sided", var.equal=TRUE)







































































































































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