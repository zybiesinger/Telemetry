tagfmI = tagfm
    
    
    
llist = c(7,7,7)
blist = c(8,8,8)


cTag = 1

median(tagfm[[cTag]]$data$altitude, na.rm=T)

bob = hist(tagfm[[cTag]]$data$altitude)




llist[cTag] = sum(head(bob$counts,6))  
blist[cTag] = sum(bob$counts)

llist[cTag] / blist[cTag]





sum(llist) / sum(blist)




bob = z0[ z0$tagName=="f60900",]

median(bob$altitude,na.rm=T)



bob = hist(z0$speed)

aa = sum(head(bob$counts,5))
aa / sum(bob$counts)

bb = sum(head(bob$counts,9))
bb / sum(bob$counts)

cc = sum(head(bob$counts,11))
cc / sum(bob$counts)



# from ...http://intersci.ss.uci.edu/wiki/index.php/HexBin_working_example
library(hexbin)
library(grid)
x <- rnorm(1000)
y <- rnorm(1000)
##-- Hexagon Bins: --
hbin <- hexbin(x,y, xbins = 25)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2)))
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
plot(hbin, style="lattice", legend=0, xlab = "X", ylab = "Y", newpage=FALSE)
popViewport()

##-- Manual "square" binning: --
## grid
rx <- range(x); bx <- seq(rx[1],rx[2], length=29)
ry <- range(y); by <- seq(ry[1],ry[2], length=29)
## midpoints
mx <- (bx[-1]+bx[-29])/2
my <- (by[-1]+by[-29])/2
gg <- as.matrix(expand.grid(mx,my))# dim = (28^2, 2)
zz <- unname(table(cut(x, b = bx), cut(y, b = by)))# 28 x 28
ind <- zz > 0
if(FALSE) ## ASCII image:
    symnum(unname(ind))
sq.size <- zz[ind]^(1/3) / max(zz)
## if we used base graphics:
##	symbols(gg[ind,], squares = sq.size, inches = FALSE, fg = 2, bg = 2)
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
vp <- plot(hbin, style="lattice", legend=0,
           xlab = "X", ylab = "Y", newpage=FALSE, type="n")
pushHexport(vp$plot, clip="on")
grid.rect(x= gg[ind,1], y=gg[ind,2], width = sq.size, height= sq.size,
          default.units = "native", gp = gpar(col="black",fill="black"))
popViewport()




# my try
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))


pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
grid.rect(gp=gpar(col="red", lwd=1))
par(plt=gridPLT(),oma=c(2,2,1,1))
plot(0,0,xlab="",ylab="")
popViewport()

pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
plot(hbin, xlab = "X", ylab = "Y", legend=F, newpage=FALSE)
grid.rect(gp=gpar(col="red", lwd=1))
popViewport()

pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
par(plt=gridPLT())
par(new=TRUE)
plot(0,0)
grid.rect(gp=gpar(col="red", lwd=1))
popViewport()


pushViewport(viewport(layout.pos.col=2,layout.pos.row=2))
grid.rect(gp=gpar(col="red", lwd=1))
plot(hbin, legend=0, xlab = "X", ylab = "Y", newpage=FALSE)
popViewport()





# and again
grid.newpage()
pushViewport(viewport(layout=grid.layout(2, 2)))
pushViewport(viewport(layout.pos.col=1,layout.pos.row=1))
plot(0,0)
popViewport()

pushViewport(viewport(layout.pos.col=2,layout.pos.row=1))
plot(hbin, style="lattice", legend=0, xlab = "X", ylab = "Y", newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
plot(hbin, legend=0, xlab = "X", ylab = "Y", newpage=FALSE)
popViewport()

pushViewport(viewport(layout.pos.col=1,layout.pos.row=2))
plot(0,0)
popViewport()



















START ANOTHER THING
# push the same viewport several times
grid.newpage()
vp <- viewport(width=0.5, height=0.5)
pushViewport(vp)
grid.rect(gp=gpar(col="blue"))
grid.text("Quarter of the device",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))
pushViewport(vp)
grid.rect(gp=gpar(col="red"))
grid.text("Quarter of the parent viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
popViewport(2)


# push several viewports then navigate amongst them
grid.newpage()
grid.rect(gp=gpar(col="grey"))
grid.text("Top-level viewport",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="grey"))
pushViewport(plot(0,0))


if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.7, name="A"))
grid.rect(gp=gpar(col="blue"))
grid.text("1. Push Viewport A",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.1, width=0.3, height=0.6,
  just="left", name="B"))
grid.rect(gp=gpar(col="red"))
grid.text("2. Push Viewport B (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="red"))
if (interactive()) Sys.sleep(1.0)
upViewport(1)
grid.text("3. Up from B to A",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(x=0.5, width=0.4, height=0.8,
  just="left", name="C"))
grid.rect(gp=gpar(col="green"))
grid.text("4. Push Viewport C (in A)",
  y=unit(1, "npc") - unit(1, "lines"), gp=gpar(col="green"))
if (interactive()) Sys.sleep(1.0)
pushViewport(viewport(width=0.8, height=0.6, name="D"))
grid.rect()
grid.text("5. Push Viewport D (in C)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
upViewport(0)
grid.text("6. Up from D to top-level",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="grey"))
if (interactive()) Sys.sleep(1.0)
downViewport("D")
grid.text("7. Down from top-level to D",
  y=unit(1, "npc") - unit(2, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("B")
grid.text("8. Seek from D to B",
  y=unit(1, "npc") - unit(2, "lines"), gp=gpar(col="red"))
pushViewport(viewport(width=0.9, height=0.5, name="A"))
grid.rect()
grid.text("9. Push Viewport A (in B)",
  y=unit(1, "npc") - unit(1, "lines"))
if (interactive()) Sys.sleep(1.0)
seekViewport("A")
grid.text("10. Seek from B to A (in ROOT)",
  y=unit(1, "npc") - unit(3, "lines"), gp=gpar(col="blue"))
if (interactive()) Sys.sleep(1.0)
seekViewport(vpPath("B", "A"))
grid.text("11. Seek from\nA (in ROOT)\nto A (in B)")
popViewport(0)


























tester=data.frame("n"=NA,"kde50"=NA)


for(i in 1:50){
      hr50Size = homeRange(
        easting = d2$easting, 
        northing = d2$northing,  
        tagName = paste(results9$tagName[cTag], ", ", i, " days", sep=""),
        n=i+300,
        lims = c(cmd$reefEN$easting-hrRange[1],cmd$reefEN$easting+hrRange[1],
                 cmd$reefEN$northing-hrRange[2],cmd$reefEN$northing+hrRange[2]),
        reefEN=cmd$reefEN, sdlEN=cmd$sdlEN, 
        prob=cProb50, drawplot=TRUE     )
      
      tester[i+150,] = c(i,hr50Size)
}      


tester50.300 = tester
 
plot(tester50.300$n,tester50.300$kde50,pch=19, xlim=c(0,300), ylim=c(15,25))
points(tester$n[151:200]+250, tester$kde50[151:200],pch=19 )














