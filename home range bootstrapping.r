################################################################################
################################################################################
### Home Ranges
################################################################################
################################################################################
#
# THE LIMITS YOU USE WHEN CALCULATING THE KDE AFFECT THE ANSWER, SO FOR ALL
# FISH MAKE SURE TO USE THE SAME LIMITS ON EASTING AND NORTHING.
#
# DO SOME WORK TO CHECK THE hr ESTIMATE SENSITIVITY TO THE LIMITS AND n GRID
# CELLS IN EACH DIRECTION.
#
# USE BOOT STRAPPING TO FIND CONFIDENCE INTERVALS ON HR ESTIMATES.  ABOUT 1000
# RUNS OF THE BOOT STRAP IS ABOUT RIGHT.  BE SURE TO CHECK THAT THE MEAN OF
# ALL THE BOOT STRAPS IS ABOUT EQUAL TO THE HR ESTIMATE WITH ALL THE DATA.  USE
# 95% QUANTILES TO APPROXIMATE THE 95% CI.



I worked this out someplace already.  I'm not sure where.  I did it during my
first visit to McMaster.




I found this in 'chapter 3 part 2.r'
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