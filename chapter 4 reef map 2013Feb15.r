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
 
 
# add ADCP locations: 
#   if4.1 (1 june 2009 - 21 aug)
#   if4.3 (24 aug - )
#   is4.1 (13 oct - )
#   os4.1 (18 nov - )

legend(500,900,
  legend=c("1-Unit reefs","4-Unit reefs",
    "4-Unit sand-bottom reefs used in this study",
    "4-Unit hard-bottom reefs used in this study"),
  cex=1.3, col="black", 
  pch=c(19,19,24,25), pt.bg="black", pt.cex=c(1,1.5,1.3,1.3)) 
  

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






















###############################################################################
###########################################################################
# Create a two-panel plot, catagorical habitat map on the left, individual
# habitat preference curves on the right
# habitat plots

# pick one image at a time...

# gather all the data 
# md3 -hb1    -if43 -2009Jun01  
# md4 -sb1    -of43 -2009Jul10  
# md5 -sb2    -oh41 -2009Aug03  
# md6 -hb2    -if41 -2009Aug24  
# md7 -sb3    -os43 -2009Sep14 
# md8 -hb3    -if42 -2009Oct12 
# md9 -sb4    -of43 -2009Nov16

# if41 - IF41_IF42_lines_aligned_HBandSB_bluebox.jpg 
#   bc = c(1590+1, 1805+1)
#   IF41/center is at (245174.8 easting, 3262391 northing)
# if42 - IF41_IF42_lines_aligned_HBandSB_bluebox.jpg 
#   bc = c(1455+1, 3345+1)
#   IF42/center is at (245318.6 easting, 3262407 northing)
# if43 - IF43_lines_aligned_HB_SB_bluebox.jpg 
#   bc = c(1387+1, 2515+1)
#   IF43/center is at (245478 easting, 3262128 northing)
# of43 - OF43_lines_SBonly_bluebox.jpg 
#   bc = c(1473+1, 1966+1)
#   OF43/center is at (237897.6 easting, 3263128 northing)
# oh41 - OH41_OS43_lines_SBonly_bluebox.jpg 
#   bc = c(2288+1, 3232+1)
#   OH41/center is at (237034.9 easting, 3263760 northing)
# os43 - OH41_OS43_lines_SBonly_bluebox.jpg 
#   bc = c(1472+1, 2413+1)
#   OS43/center is at (236946.8 easting, 3263839 northing)

### SEE THAT I'VE ALREADY GOT THE gimpX AND gimpY BUILT INTO 'findHabType()'
 
numexpt = 3:9
# these gimp pixel locations work in the full i1 images, not the subset catmaps...
gimpX = c(2515,1966,3232,1805,2413,3345,1966) # from above, make sure they're in the right order
gimpY = c(1387,1473,2288,1590,1472,1455,1473) # from above, make sure they're in the right order
sites = vector(length=length(numexpt))
fns = vector(length=length(numexpt))
reefE = vector(length=length(numexpt))
reefN = vector(length=length(numexpt))
deployment = vector(length=length(numexpt))
goodFish = list()
cmd = vector(length=length(numexpt))


for (i in 1:length(numexpt)){
  sites[i] = md[[numexpt[i]]]$site
  fns[i] = md[[numexpt[i]]]$habmapFileName
  reefE[i] = md[[numexpt[i]]]$reefEN$easting
  reefN[i] = md[[numexpt[i]]]$reefEN$northing
  deployment[i] = md[[numexpt[i]]]$deployment
  goodFish[[i]] = md[[numexpt[i]]]$goodFishNames
  cmd[i] = numexpt[i]
}




# check a plot that everything looks good...It all looks good
whichSite = 1
# import the image to use
rfile = fns[whichSite]
i1 = round(read.jpeg(rfile))
plot(i1, useRaster=TRUE)   
findHabType(e=reefE[whichSite], n=reefN[whichSite], 
  reference = sites[whichSite], show=T, crosshairs=T, habmap=i1)

rm(i1)
# It all looks good.  






#################################################################################
## make a figure plot...do this once for each hb deployment, the sb deployments
## ... had only sand bottom around them
## ... hb deployments correspond to whichSite = c(1,4,6) = (if43, if41, if42)
whichSite = 6 # 1   4   6
# import the image to use
rfile = fns[whichSite]
i1 = round(read.jpeg(rfile))         
#plot(i1, useRaster=TRUE)   

# pick out just the portion I want
bc = c(gimpY[whichSite]+1, gimpX[whichSite]+1) # bc = box center...location of reef
bw = 2600 # bw = box width, 2*130m = 2600pixels
bh = 2600 # bh = box height
bl = function(){bc[2] - bw/2}
br = function(){bc[2] + bw/2}
bt = function(){bc[1] - bh/2}
bb = function(){bc[1] + bh/2}

catmap = i1[bt():bb(), bl():br(), 3]
catmap1 =  i1[bt():bb(), bl():br(), ] # curse it, plotting needs 2D and findHabType needs 3D
# From this I calculate the eastings and northings of the edged of catmap, using
# 10pix/m.  
#                             
nx=bw+1
ny=bh+1 
r=10 # 10 pixels = 1m   

# look down to "plot fig 1" for the plotting code


  
### figure 1, right panel ##################
habmap = i1
rm(i1)

# location of reef in easting/northing or row,column.  row/column is right?
# i.e. ce = 245174.8-eastingOffset # center (meters) in easting direction
# i.e. cn =  3262391-northingOffset # center (meters) in northing direction
ce = gimpX[whichSite]+1 # gimpX center column    # these only work when using the full i1 image
cn = gimpY[whichSite]+1 # gimpY center row       # these only work when using the full i1 image
ne = dim(habmap)[2] # number of columns        # these only work when using the full i1 image
nn = dim(habmap)[1] # number of rows           # these only work when using the full i1 image
# compute the distance between pixel (ce,cn) and every other pixel
# ...the default is for outer to do the product, but this will add instead
d = sqrt(outer( (cn-(1:nn))^2, (ce-(1:ne))^2, "+")) 

# which tags are we dealing with
# ... just use the tags for the current whichSite / deployment...
# ... and don't do this for sb sites, their maps are all white sand bottom
cTagNames = goodFish[whichSite][[1]]

# list to hold (tagName, radius, # positions, % time over HB)
allfish = list()
# list to hold overall HB cover measure for each site
overallPercentHB = list()
                  
                  
d2 = data.frame(radius=NA, percentHB=NA)
annulusThickness = 1  # that is 1m
rings = seq(annulusThickness,100, by=annulusThickness)


# cycle through each ring and count the fraction of HB pixels
for (i in 1:length(rings)){ 
  pixelsPerMeter = 10
  
  rOuter = rings[i] * pixelsPerMeter # r is in units of pixels, radius is in meters...10 pixels per meter
  if(i==1){rInner=0} else {rInner = rings[i-1] * pixelsPerMeter}
  
  kernel = function(z){(z > rInner) & (z < rOuter)} # if passed a matrix, this returns a T/F matrix 
                                                    # T if within ring, F if not
  
  
  # k is an nx by ny matrix with TRUE(=1) everywhere [within 
  # rOuter of (i,j) and beyond rInner of (i,j)] and FALSE(=0) everywhere else
  k = kernel(d) 
  #k = k/sum(k) # this normalizes k so that it sums to one, probably not necessary but a good habit
  percentHB = 1 - sum(k * habmap[,,2]) / sum(k)
  
  
  # some exploration
  #d[(cn-10):(cn+10),(ce-10):(ce+10)]
  #habmap[(cn-10):(cn+10), (ce-10):(ce+10), 2]
  #k[(cn-10):(cn+10), (ce-10):(ce+10)]
  
  
  d2[i,] = c(rings[i], percentHB)
  #points(c(ce,ce+r,ce,ce-r),c(cn+r,cn,cn-r,cn),pch=19,cex=0.5,col="green")
  #points(ce,cn,pch=10, cex=0.5,col="red")
  
  
  # And finally, what is the percentHB for the entire 100m area?
  if(i == 100){
    # use the maximum outer ring (100m) and the minimum inner ring (0m)
    # rOuter already = 1000
    rInner = 0 
    k = kernel(d) 
    #k = k/sum(k) # this normalizes k so that it sums to one, probably not necessary but a good habit
    overallPercentHB[[whichSite]] = 1 - sum(k * habmap[,,2]) / sum(k)
  
  }
   
} # end for-loop     

# If you want to see it... 
# plot(d2,type="l",main="Landscape composition around reef")
       
# for each fish, loop through all radii and calculate the %HB use
for (i in 1:length(cTagNames)){
  habUse = data.frame(tagName=NA, radius=NA, numPositions=NA, HBuse=NA, HBpreference=NA)
  cFish = z9[z9$tagName == cTagNames[i], ]
  for (j in 1:length(rings)){
    rOuter = rings[j] # rOuter is in units of m
    if(j==1){rInner=0} else {rInner = rings[j-1]}                                    
    # pick all the positions within this ring 
    cPositions = cFish[(cFish$dtr > rInner) & (cFish$dtr < rOuter), ]
    # pick only those points over hard bottom
    cHB = cPositions[cPositions$habType == "black", ]
    # calculate the number of positions, HBuse, HBpreference
    habUse[j,] = c(cTagNames[i], rings[j], 
      nrow(cPositions), 
      nrow(cHB)/nrow(cPositions), 
      (nrow(cHB)/nrow(cPositions))/d2$percentHB[j]
    )
    allfish[[i]] = habUse
  }
}

# for all fish together calculate the %HB use at all radii
i=length(allfish)+1
habUse = data.frame(tagName=NA, radius=NA, numPositions=NA, HBuse=NA, HBpreference=NA)
for (j in 1:length(rings)){
  # for each ring, calculate the mean of the habUse of all fish
  temp1 = vector(length=length(allfish))
  temp2 = vector(length=length(allfish))
  for (k in 1:length(allfish)){
    temp1[k] = as.numeric(allfish[[k]]$HBuse[j])
    temp2[k] = as.numeric(allfish[[k]]$HBpreference[j])
  }
  habUse[j,] = c("all", rings[j], NA, mean(temp1, na.rm=T), mean(temp2, na.rm=T))
}
allfish[[i]] = habUse 

rm(habmap)






###############################################################################
### plot fig 1 ... a double plot for the paper ############################3
# now that I use the catmap/catmap1 subsets be sure to correctly offset values
# like bc, ce, cn, and pay special attention to EN locations used in findHabType.
windows(14,6.8)
par(mar=c(5,6,1,1), mfrow=c(1,2))
  
# draw the plot    
plot.imagematrix.zy(imagematrix(catmap),useRaster=TRUE)
box(which = "plot", lty = "solid")
# add axes labels and numbers so reefs are at (0,0)
mtext(text="Northing (m)", side=2, line=3.4, cex=2)
# remember that catmap1 is offset so the tick marks must be corrected, 
# c(1300,1300) is center 
axis(1, at=1300+c(-1000,-500,0,500,1000), 
  cex.axis=1.7, labels=seq(-100,100,by=50))
axis(2, at=1300+c(-1000,-500,0,500,1000), cex.axis=1.7, las=1, 
  labels=seq(-100,100,by=50))

# check that position references are working
# Because catmap1 is a cropped version of the full i1 image, the easting/northing
# and colmun/row mapping numbers are off. To plot things (SDLs or reef) in the 
# correct place you have to account for the croping. To do that shift E/N
# by the correct number of meters equivalent to the number of columns/rows, 
# bl()/bt()
# Remember that 1 meter = 10 pixel rows or columns, meaning that if 
# bl() = 506 columns that equals 50.6 meters 
#
# AND, the '+1' on the easting is clearly necessary (by visual inspection) 
# when marking the reef. It 
# must be a 'starting-at-0-or-1' issue  

#findHabType(
#  e=reefE[whichSite]-(bl()/10)+1, 
#  n=reefN[whichSite]+(bt()/10), 
#  reference=sites[whichSite], habmap=catmap1, 
#  show=TRUE, pixels=T, dotCol="red", crosshairs=T, dotShape=4, dotSize=2) 



# I want to draw on the reef and sdl locations  

# mark the reef location
bob=findHabType(
  e=md[[cmd[whichSite]]]$reefEN$easting[1]-(bl()/10)+1, 
  n=md[[cmd[whichSite]]]$reefEN$northing[1]+(bt()/10), 
  habmap=catmap1, reference=sites[whichSite],# erange=c(245045,245305), nrange=c(3262261,3262521),
  show=TRUE, crosshairs=FALSE, pixels=TRUE, 
  dotCol="black", dotShape=18, dotSize=1) 
 points(bob$pixelEN[1], bob$pixelEN[2], pch=23,col="black", bg="white",cex=2,lwd=2)  


## mark the sdl locations                                                        
#for (cpoint in 1:5){
#  dotColors = c("black", "white", "white", "white", "black")
#  findHabType(
#    e=md[[cmd[whichSite]]]$sdlEN$easting[cpoint]-(bl()/10)+1, 
#    n=md[[cmd[whichSite]]]$sdlEN$northing[cpoint]+(bt()/10), 
#    habmap=catmap1, reference="if41",
#    show=TRUE, crosshairs=FALSE, pixels=TRUE, 
#    dotCol=dotColors[cpoint], dotShape=17, dotSize=2) 
#}

# outline the eastern, southern, and western sdls to show up better                                                        
for (cpoint in 1:5){
  bob = findHabType(
    e=md[[cmd[whichSite]]]$sdlEN$easting[cpoint]-(bl()/10)+1, 
    n=md[[cmd[whichSite]]]$sdlEN$northing[cpoint]+(bt()/10), 
    habmap=catmap1, reference=sites[whichSite],
    show=FALSE, crosshairs=FALSE, pixels=TRUE, 
    dotCol=dotColors[cpoint], dotShape=17, dotSize=1.7) 
  points(bob$pixelEN[1], bob$pixelEN[2], pch=24,col="black", bg="white",cex=1.7,lwd=2)  
}



# figure improvement, perhaps unnecessary
# Look at the black hard-bottom in the upper left and notice that what was a 
# blue (or was it red) line in i1 is now a white line. I'll color it black in
# a terribly ugly way

# for whichSite=1, if43
if(whichSite == 1){
  text(100,2300,"a)", cex=2)  # place the a)
  points(10,2580,pch=19); points(50,2580,pch=19); points(250,2570,pch=19);  
  points(290,2570,pch=19); points(310,2570,pch=19);  points(330,2570,pch=19) 
  points(360,2570,pch=19);  points(400,2570,pch=19); points(440,2570,pch=19);  
  points(480,2570,pch=19); points(790,2560,pch=19);  points(830,2560,pch=19) 
  points(870,2560,pch=19);  points(910,2560,pch=19); points(950,2560,pch=19);  
  points(1210,2550,pch=19); points(1250,2550,pch=19); points(1290,2550,pch=19) 
  points(1440,2540,pch=19); points(1750,2540,pch=19); points(1790,2540,pch=19); 
  points(1830,2540,pch=19); points(1870,2540,pch=19); points(1910,2540,pch=19)
  points(1950,2540,pch=19); points(1990,2540,pch=19); points(2010,2530,pch=19); 
  points(2050,2530,pch=19); points(2090,2530,pch=19); points(2130,2530,pch=19) 
  points(2170,2530,pch=19); points(2210,2530,pch=19); points(2250,2530,pch=19); 
  points(2290,2530,pch=19); points(2330,2530,pch=19); points(2370,2530,pch=19) 
  points(2410,2530,pch=19); points(2430,2530,pch=19); points(2570,100,pch=19); 
  points(2570,60,pch=19); points(2570,20,pch=19); points(2570,60,pch=19) 
}

# for whichSite=4, if41
if(whichSite == 4){
  text(130,2500,"a)", cex=2)   # place the a)
  points(90,1910,pch=19)   
  points(80,1880,pch=19)   
  points(80,1850,pch=19) 
  points(80,1830,pch=19) 
  points(80,1320,pch=19)   
  points(80,1290,pch=19)   

}

# for whichSite=6
if(whichSite == 6){
  text(1050,2500,"a)", cex=2) # place the a)
  points(10,2400,pch=19); points(10,2440,pch=19); points(2600,2120,pch=19);
  points(2600,2080,pch=19); points(2600,2040,pch=19); points(2600,2000,pch=19);
  points(2590,1860,pch=19); points(2590,1820,pch=19); points(2590,1780,pch=19);
  points(2590,1740,pch=19); points(2550,350,pch=19); points(2550,310,pch=19);
  points(2550,70,pch=19); points(2550,30,pch=19);
}



### figure 1, right panel ################# TO GET THIS RIGHT YOU HAVE TO GO
#                                           ALL THE WAY BACK TO WHERE 
#                                           'whichSite' IS CHOOSEN
#

plot(d2, lwd=4, las=1, bty="l", cex.lab = 2, cex.axis=1.7, type="l", 
  xlim=c(0,100), ylim=c(0,1), 
  xlab="Distance from Reef (m)",
  ylab="") #
mtext(text="Fraction of HB Cover or Use", side=2, line=3.4, cex=2)

# add a horizontal line for the mean HB cover in 100m
abline(h=overallPercentHB[[whichSite]],lty=3, lwd=3)


# add individual fish with dashed lines
#for (i in 1:length(cTagNames)){
#  points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l")
#  text(x=100,y=as.numeric(tail(allfish[[i]]$HBuse,1))+0.01, labels=tail(allfish[[i]]$tagName,1))
#}
lineType = c(rep(1,length(allfish)-1),2)
lineWidth = c(rep(1,length(allfish)-1),4)
for (i in 1:(length(allfish))){  # the last one is all fish together
  points(allfish[[i]]$radius, allfish[[i]]$HBuse, type="l", 
  lty=lineType[i], lwd=lineWidth[i])
}

# place the b)
text(2,1,"b)", cex=2) 
  

# add a legend
if(whichSite==1){xx=45;yy=0.4} else {xx=-10;yy=-10} 
legend(xx, yy, legend=c("Hard-bottom cover", "Overall hard-bottom", "cover within 100 m",
  "3 Individuals'", "hard-bottom use", "All individuals' use", "combined"), seg.len=4,
  lty=c(1,3,NA,1,NA,2,NA), lwd=c(4,3,NA,1,NA,4,NA)) 







































### I THINK THIS IS OLD CODE BELOW HERE
################################################################################
# Calculate the percent HB coverage and habitat preference index for each 
# landscape and each fish
#
# recall that d is a matrix of distances from the reef. the correct reef
#    is specified when d is made

# count all the HB pixels in habmap[] within 100m of the reef ... similar to 
# the rings work above

pixelsPerMeter = 10
rOuter = 100 * pixelsPerMeter
kernel100 = function(z){z < rOuter}  # if passed a matrix, this returns a T/F matrix 
                                     # T if within 100m, F if not
k = kernel100(d)
percentHB100 = 1 - sum(k * habmap[,,2]) / sum(k) # this is fractional HB cover within 100m



# list to hold (tagName, radius, # positions, % time over HB)
allfish100 = list()

# for each fish calculate the fractional HB use within 100m
for (i in 1:length(cTagNames)){
  habUse = data.frame(tagName=NA, radius=NA, numPositions=NA, HBuse=NA, HBpreference=NA)
  cFish = z9[z9$tagName == cTagNames[i], ]
  
  for (j in 100){ # I want everything within 100m
    rOuter = 100 # rOuter is in units of m because it's compared to dFish$dtr which is in mm
   
    cPositions = cFish[(cFish$dtr < rOuter), ]
    cHB = cPositions[cPositions$habType == "black", ]
     
    habUse[1,] = c(cTagNames[i], rOuter, 
      nrow(cPositions), nrow(cHB)/nrow(cPositions), 
      (nrow(cHB)/nrow(cPositions))/percentHB100
    )
    allfish100[[i]] = habUse
  }
}

mean(as.numeric(c(allfish100[[1]]$HBpreference,
  allfish100[[2]]$HBpreference,allfish100[[3]]$HBpreference,
  allfish100[[4]]$HBpreference,allfish100[[5]]$HBpreference,
  allfish100[[6]]$HBpreference)))




 