##########################################################################
# In this chapter I make a map figure of the reef system

library(rimage)
library(PBSmapping)


source("C:/zy/school closet/Telemetry/R Data Processing/global variables.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global functions.r")
source("C:/zy/school closet/Telemetry/R Data Processing/global metadata.r")


# read in tagfm, z0, results
# load("C:/zy/Telemetry/R summary files/tag9 2011Mar16.rdata")
# load("C:/zy/Telemetry/R summary files/tagf9 2011Mar16.rdata")

# load("C:/zy/Telemetry/R summary files/tagfm9 2011Mar16.rdata")
load("C:/zy/school closet/Telemetry/R summary files/z9 2011Jun25.rdata")
# load("C:/zy/Telemetry/R summary files/results9 2011Mar16.rdata")



# read in data
lrs = read.table("C:/zy/school closet/Telemetry/R summary files/LRS locations.txt", header=T)


# add columns for UTM, UTM increases northward and eastward
temp2 = data.frame(X = -lrs$Longitude, Y = lrs$Latitude)
attr(temp2, "zone") <- 17
attr(temp2, "projection") <- "LL"   
temp3 = convUL(temp2,km=FALSE)  #X is easting in m, Y is northing in m
lrs$easting = temp3$X - eastingOffset
lrs$northing = temp3$Y - northingOffset

# specify which reefs I used: if41, if42, if43, oh41, of43, os43
# ...in lrs these are reefs c(10,14,18,30,44,52)

lrs$used = FALSE
lrs$used[c(10,14,18,30,44,52)] = TRUE


# drop the 0 reefball reefs
lrs1 = subset(lrs, Treatment > 0, 
  select = c(Mosaic,Habitat_type,Treatment,easting,northing,used))

#hard or soft bottom for the experimental reefs
lrs1$bottom = NA   # make everything NA
lrs1$bottom[lrs1$used] = "sand"  # change all used sand reefs 
lrs1$bottom[lrs1$used & (lrs1$Mosaic == "Inner")] = "hard" # change all hard-bottom reefs blue

#select symbols, sizes, and colors
lrs1$symbol = 19
#lrs1$symbol[lrs1$used] = 17 
lrs1$symbol[lrs1$bottom == "sand"] = 24 
lrs1$symbol[lrs1$bottom == "hard"] = 25
lrs1$size = 1
lrs1$size[lrs1$Treatment == 4] = 1.5
lrs1$size[!is.na(lrs1$bottom)] = 1.3
lrs1$color = "black"
lrs1$color[lrs1$bottom == "sand"] = "black" 
lrs1$color[lrs1$bottom == "hard"] = "black"




### MAKE SURE TO CORRECTLY SIZE THE WINDOW MANYALLY
# now draw the map
par(mar=c(5,6,1,1)+0.1)
plot(lrs1$easting, lrs1$northing, pch=lrs1$symbol, col="black", #col=lrs1$color, 
  cex=lrs1$size, bg="black",
  las = 1, cex.lab=1.5, cex.axis=1.5, xaxt="n", yaxt="n",
  xlim=c(0,10000), ylim=c(-300,3000),
  xlab="Easting (m)", ylab=""
)
mtext("Northing (m)", side=2, line=4.5, cex=1.7)
axis(1,at=seq(0,10000,by=1000), cex.axis=1.5)
axis(2,at=seq(0,3000,by=1000), las=1, cex.axis=1.5)


# label reefs
arrows(0, 2500, 270, 2200, length=0.1, angle=30, lwd=2)
text(0, 2600,"ID 5",cex=1.5)
#
arrows(1400, 2050, 550, 2050, length=0.1, angle=30, lwd=2)
text(1650, 2090, "ID 4",cex=1.5)
#
arrows(1900, 1200, 1400, 1400, length=0.1, angle=30, lwd=2)
text(2150, 1200,"ID 3",cex=1.5)
#
arrows(8000, 700, 8500, 700, length=0.1, angle=30, lwd=2)
text(7800, 730,"ID 1",cex=1.5)
#
arrows(9500, 410, 8950, 410, length=0.1, angle=30, lwd=2)
text(9750, 440,"ID 2",cex=1.5)
#
arrows(9200, 1100, 8800, 800, length=0.1, angle=30, lwd=2)
text(9450, 1100,"ID 6",cex=1.5)
# Compass arrow
arrows(8000, 1500, 8000, 2500, length=0.2, angle=30, lwd=3)
text(8000, 2700,"N",cex=2)



# legend
legend(500,900,
  legend=c("1-Unit reefs","4-Unit reefs",
    "4-Unit sand-bottom reefs used in this study",
    "4-Unit hard-bottom reefs used in this study"),
  cex=1.3, col="black", 
  pch=c(19,19,24,25), pt.bg="black", pt.cex=c(1,1.5,1.3,1.3)) 


# add ADCP locations: 
#   if4.1 (1 june 2009 - 21 aug)
#   if4.3 (24 aug - )
#   is4.1 (13 oct - )
#   os4.1 (18 nov - )

  

# pick one reef and find the nearest neighbors
# these are the indices
checkers = which(lrs1$used) # 7 10 12 20 30 35
chosenone = list()
for(i in 1:length(checkers)){
  chosenone[[i]]=lrs1[checkers[i],c("easting","northing")]
}

answer = data.frame(chosenone = NA, nn=NA)
for (i in 1:length(chosenone)){
  nn = 777777777777
  for (j in 1:length(lrs1)){
    temp1 = sqrt( (chosenone[[i]]$easting - lrs1[j,"easting"])^2 +
          (chosenone[[i]]$northing - lrs1[j,"northing"])^2
    )
    if(temp1>0){nn = min(nn,temp1)}
  }
  answer[i,] = c(checkers[i],nn)
}
 


# I want to have a locator map as an inset. I can't figure out how to place 
# the inset with R, I 'll do it manually, but make it here.
library(maps)

windows(3.5,3.5)
map('state', 'florida', fill=TRUE, col="grey")
box()
symbols(-83.72, 29.46,
  stars=as.matrix(t(c(0.7,0.3,0.7,0.3,0.7,0.3,0.7,0.3,0.7,0.3)/2)),
  add=T, lwd=2, inches=F, fg="black", bg="black")


















