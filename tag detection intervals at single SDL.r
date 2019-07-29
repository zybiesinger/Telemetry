# I want to look at the detection intervals of individual tags/beacons at 
# each SDL.  This is data that has already been decompressed and symbols built.
#  I'm looking at files named like 'TxId1.toa' in the daily folders.  These
# files have 25 columns, 5 for each SDL. The columns are 
# 1. time, 2. fraction of time, 3., 4., 5,.

# this is a copy of 'importToaData' modified to take any funky directory or file
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
############################################################################## 
# This function returns a list of 8 items.  1-5 are data.frames for each of
# the SDLs containing the toa data for that SDL.  
# 6 is the tag number, 7 indicates the deployment name, 8 is a tag/beacon/sentinel 
# switch.
importToaDataHere = function(tagName, deployment, directory){    
  # 'tagName' is the tag or beacon number with a leading letter, b1, t13, s79600
  # 'deployment' is the experiment designation, i.e. IF43 
  
  tagID = substr(tagName,2,100)
  tagType = substr(tagName,1,1)
  
  # settings that change for each deployment
  for (i in 1:length(md)){ # i loops through all deployments
    if (deployment == md[[i]]$deployment){
      tempDir = directory
      stopUtime = md[[i]]$stopUtime
    }  
  }
  setwd(directory)
  cDir = getwd()

  # read all *.toa data for 'tag' into a single data.frame
  # list the folders and files in the current directory
  fileList = list.files(cDir) 
  # create an empty data.frame
  toaData = data.frame() 
  # look through all directories for files for 'tag'
  tempDir = cDir
  for (i in 1:length(fileList)){ # look at all the files/folders in directory
    # if it's a folder, open it, look for a file for 'tag', append to 'toaData'
    setwd(cDir)
    if ( file.info(list.files(path=cDir))[i,2] ){
      setwd(paste(cDir,fileList[i],sep="/"))
      cFile = paste("TxId",tagID,".toa",sep="")
      if (file.exists(cFile)){ # if there's a file, open it and put it in 'toaData'
        toaData=rbind(toaData,read.table(cFile, header=FALSE))#, col.names = columnNames)) 
      } # end 'if file.exists'
    } # end 'if it's a file'
    setwd(cDir)
  } # end for i loop
  
  # if there are no toa files at all for the tag then
#  if ()............. 
  
  # divide into data.frames for each SDL
  d41 = data.frame("utime"=toaData[,1],"fraction"=toaData[,2],"power"=toaData[,3],
    "sType"=toaData[,4],"sValue"=toaData[,5])   
  d42 = data.frame("utime"=toaData[,6],"fraction"=toaData[,7],"power"=toaData[,8],
    "sType"=toaData[,9],"sValue"=toaData[,10])   
  d43 = data.frame("utime"=toaData[,11],"fraction"=toaData[,12],"power"=toaData[,13],
    "sType"=toaData[,14],"sValue"=toaData[,15])   
  d44 = data.frame("utime"=toaData[,16],"fraction"=toaData[,17],"power"=toaData[,18],
    "sType"=toaData[,19],"sValue"=toaData[,20])   
  d45 = data.frame("utime"=toaData[,21],"fraction"=toaData[,22],"power"=toaData[,23],
    "sType"=toaData[,24],"sValue"=toaData[,25])
                             
  # get rid of empty rows
  d41 = d41[d41$utime>0,]    
  d42 = d42[d42$utime>0,]
  d43 = d43[d43$utime>0,]   
  d44 = d44[d44$utime>0,]
  d45 = d45[d45$utime>0,] 
  
  # get rid of points after the time we began recovering the array, stopUtime
  d41 = d41[d41$utime<stopUtime,] 
  d42 = d42[d42$utime<stopUtime,] 
  d43 = d43[d43$utime<stopUtime,] 
  d44 = d44[d44$utime<stopUtime,] 
  d45 = d45[d45$utime<stopUtime,] 
   
  # return the toa data for 'tag' for each SDL
  list("sdl41"=d41, "sdl42"=d42, "sdl43"=d43, "sdl44"=d44, "sdl45"=d45,
    "tagName"=tagName, "deployment"=deployment)    
} # end importToaData()

directory = "F:/Telemetry/2009 experiment/comparing psr and no psr toa files/2009Jun01 no psr "

directory = "F:/Telemetry/2009 experiment/comparing psr and no psr toa files/2009Jun01 psr "

bob = importToaDataHere(tagName="b79500", deployment="if43", directory=directory)

# it appears there's only a problem with b79400 and b79500 with psr.  In these
# cases there are intervals less than every 20 seconds.


sam = bob$sdl41

# look at the time over time
plot(sam$utime)

# now calculate the time interval between receptions
interval1 = vector(mode="numeric", length=(nrow(sam)-1))
for (i in 2:nrow(sam)){
  interval1[i] = sam$utime[i]-sam$utime[i-1]
}

interval1


# I want to 'count.fields' so this has to be a file...
tempF = "deletemezy.txt" 
write.table(interval1, file=tempF, row.names=FALSE)
rfile = file(tempF, open="rt") # open the connection
  
# count fields in each line, a field is separated by white space
fcount = count.fields(rfile, blank.lines.skip=FALSE)   
runs = rle(interval1)



# a histogram with all data
hist(interval1)

# a histogram of just the smallest points
interval2 = interval1[interval1<21]

quinn = hist(interval2, breaks=0:21)











