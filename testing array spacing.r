# In this file I want to see how the HR estimate would have been different
# if the array spacing were smaller.  I'll use the 2008 fish.



# start with tagfm[[7]] = f60300. tagfm[[8]] = f61100

str(tagfm[[7]])
str(tagfm[[8]])

d1 = subset(tagfm[[4]]$data, select=c(utime,easting,northing))


plot(d1$easting, d1$northing, pch=19, cex=0.5, 
  xlim=md[[2]]$plotLimits$easting, ylim=md[[2]]$plotLimits$northing)
points(md[[2]]$sdlEN$easting, md[[2]]$sdlEN$northing, pch=19, col="red")
points(md[[2]]$reefEN$easting, md[[2]]$reefEN$northing, pch=19, col="blue")  






rotate = function(fisht, fishx, fishy, spin=45, show=FALSE,
  reefEN=c(md[[2]]$reefEN$easting,md[[2]]$reefEN$northing) )
{
  # reef location
  xc = reefEN[1]
  yc = reefEN[2]

  # fish location
  x=fishx
  y=fishy

  # shift center of rotation
  x1=x-xc
  y1=y-yc

  # convert to polar coordinates
  r = sqrt(x1^2 + y1^2)
  theta = atan2(y1,x1)*180/pi

  # rotate 
  newtheta = theta + spin

  # convert to cartesian coordinates
  x2 = r * cos(newtheta*pi/180)
  y2 = r * sin(newtheta*pi/180)  

  # shift center back to (0,0)
  x3 = x2+xc
  y3 = y2+yc

  if(show){
    plot(x,y,pch=19,xlim=md[[2]]$plotLimits$easting,
      ylim=md[[2]]$plotLimits$northing)
    points(x3, y3, pch=19, col="red")
    points(md[[2]]$reefEN$easting, md[[2]]$reefEN$northing, pch=19, col="blue")
    points(md[[2]]$sdlEN$easting, md[[2]]$sdlEN$northing, pch=19, col="green")
    points(md[[2]]$reefEN$easting+c(0,125,0,-125),
      md[[2]]$reefEN$northing+c(125,0,-125,0), pch=17, col="green")
  
    abline(h=yc, v=xc, lty="dashed")
    abline(h=0, v=0)
  }
  
  data.frame(fisht=fisht, fishx=x3, fishy=y3)
}

r1 = rotate(d1$utime, d1$easting, d1$northing)




chop = function(fisht, fishx, fishy,  spacing,
  reefEN=c(md[[2]]$reefEN$easting,md[[2]]$reefEN$northing)
)
{
  en = data.frame(fisht, fishx, fishy)
  en = en[abs(en$fishx-reefEN[1]) < spacing,]
  en = en[abs(en$fishy-reefEN[2]) < spacing,]
}



  cmd=md[[2]]     
  
  
  plot(d1$easting, d1$northing, pch=19, 
    xlim=cmd$plotLimits$easting, ylim=cmd$plotLimits$northing)
  points(cmd$reefEN$easting, cmd$reefEN$northing, pch=17, col="red")
  points(cmd$sdlEN$easting, cmd$sdlEN$northing, pch=17, col="green")
  
  r1 = rotate(d1$easting, d1$northing)
  r2 = chop(r1$fishx, r1$fishy, spacing=70)
  r3 = rotate(r2$fishx, r2$fishy, spin=-45)
  
  points(r3$fishx, r3$fishy, pch=19, cex=0.5, col="red")

  d1 = subset(tagfm[[7]]$data, select=c(easting,northing))
  d1 = subset(tagfm[[8]]$data, select=c(easting,northing))

  spacing_hr = data.frame(tagName=NA, spacing=NA, kde50=NA, kde95=NA)
  spacings = seq(10,130, by=10)
  
  for(i in 1:length(spacings)){
    r1 = rotate(d1$easting, d1$northing)
    r2 = chop(r1$fishx, r1$fishy, spacing=spacings[i])
    r3 = rotate(r2$fishx, r2$fishy, spin=-45)
    # calculate the home ranges
    # THE LIMITS YOU USE WHEN CALCULATING THE KDE AFFECT THE ANSWER, SO FOR ALL 
    # FISH MAKE SURE TO USE THE SAME LIMITS ON EASTING AND NORTHING.
    # There's more in 'chapter 3 part 1.r' on looking at home ranges.
    spacing_hr[i,] = c(
      "f60300",
      spacings[i],
      homeRange(easting = r3$fishx, northing = r3$fishy,  
        tagName = "?", lims = hrlims, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=0.5, drawplot=FALSE
      ),
      homeRange(easting = r3$fishx, northing = r3$fishy,  
        tagName = "?", lims = hrlims, reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=0.95, drawplot=FALSE
      )
    )  
  }

bob = spacing_hr # 60300

sam = spacing_hr # 61100


plot(bob$spacing, bob$kde95, type="b", ylim=c(0,4500), col=2, 
  main="f60300, f61100",xlab="Simulated Array Spacing",ylab="Home Range Estimate")
points(bob$spacing, bob$kde50, type="b", col=3)

abline(v=50,h=c(bob[5,3], bob[13,3]))

points(sam$spacing, sam$kde95, type="b", pch=19, col=2)
points(sam$spacing, sam$kde50, type="b", pch=19, col=3)
abline(v=50)

abline(v=50,h=c(spacing_hr[5,3],spacing_hr[13,3]))

(as.numeric(bob[13,4]) - as.numeric(bob[5,4])) / as.numeric(bob[13,4])
(as.numeric(sam[13,4]) - as.numeric(sam[5,4])) / as.numeric(sam[13,4])


head(z0)

