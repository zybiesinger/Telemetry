###############################################################################
###############################################################################
###############################################################################
# Zy Biesinger. 2019 Apr 11
# Use this file import information that we send to glatos, e.g. receiver deployments
# and recovery, tag and animal IDs. This can be compared to this same data
# we get back from glatos, just to confirm things are good. This will also 
# identify any tags that are released to the wild, but never heard.

importSubmissionData = function(){

  ############################################################################################################
  # load libraries
  ###########################################################################################################
  require(data.table)
  require(readxl)
  require(lubridate)
  require(plyr)
  require(sp)
  require(mapview)
  # END load libraries
  ###########################################################################################################
  
  
  ###########################################################################################################
  # directories...pick 'LNRGLATOS' or 'ELOGLATOS'
  ###########################################################################################################
  oDir = getwd() # when this script is sourced from another file, save the original directory
  stableScriptsDir = "C:/zy/telemetry R scripts/Stable R scripts/"
  submissionDir = paste0("P:/Projects/GLRI/Native Species Program/Projects/Telemetry/",
    "Acoustic Telemetry/GLATOS/Submission forms/")
  # END directories
  ###########################################################################################################
  
  
  ############################################################################################################
  # select GLATOS submission files
  ############################################################################################################
  wFiles = choose.files(default= paste0(submissionDir, "*.*"), 
    caption=paste0("Please select all GLATOS submission files with a name like: ",
      "'LNRLS_GLATOS_*.xlsm' and 'ELOCS_GLATOS_*.xlsm'"),
    multi=TRUE
  )
  # END select GLATOS submission files
  ############################################################################################################
  
  
  ############################################################################################################
  # import receiver locations and times from our GLATOS submission forms
  ############################################################################################################
  # lists to hold data from all submission files
  deployList = list()
  recoveryList = list()
  
  # read in from all files in wFiles and combine into one df
  for (i in 1:length(wFiles)){
    # now the deployment data #############################################################
    # read from excel
    rawData = read_excel( wFiles[i], sheet="Deployment", skip=1)
    # of all variables, we only want these...
    deploy = as.data.frame(rawData[,c("GLATOS_ARRAY", "INS_SERIAL_NO","GLATOS_DEPLOY_DATE_TIME",
      "CONSECUTIVE_DEPLOY_NO","DEPLOY_LAT","DEPLOY_LONG")])
    rm(rawData)
    # rename variables
    names(deploy)=c("array","recID","dati","deployNum","lat","long")
    # fix the time zone, "America/New_York", not UTC...there must be an easier way
    deploy$dati = ymd_hm(deploy$dati)
    # set correct class
    deploy$recID = as.factor(deploy$recID)
    deploy$lat = as.numeric(deploy$lat)
    deploy$long = as.numeric(deploy$long)
    # look at incomplete lines ... hopefully a df of zero rows
    deploy[(is.na(deploy$lat)) | (is.na(deploy$long)),]
    # drop incomplete lines
    deploy = deploy[(!is.na(deploy$lat)) & (!is.na(deploy$long)),]
    # add an indicator flag...deploy or recover
    deploy$action = "deploy"  
    # store deploy data for this one file
    deployList[[i]] = deploy
    
    
    # now the recovery data #############################################################  
    rawData = read_excel( wFiles[i], sheet="Recovery", skip=1 )
    # pick desired columns and rename
    recovery = as.data.frame(rawData[,c("GLATOS_ARRAY", "INS_SERIAL_NUMBER","GLATOS_RECOVER_DATE_TIME",
      "CONSECUTIVE_DEPLOY_NO")])
    names(recovery) = c("array", "recID","dati","deployNum")
    rm(rawData)
    # fix the time zone, "America/New_York", not UTC...there must be an easier way
    recovery$dati = ymd_hm(recovery$dati)
    # set correct class
    recovery$recID = as.factor(recovery$recID)
    # add columns for rbind() with deploy
    recovery$long = recovery$lat = as.numeric(NA)
    # look at incomplete lines ... hopefully a df of zero rows
    recovery[(is.na(recovery$recID)) | (is.na(recovery$dati)),]
    # drop incomplete lines
    recovery = recovery[(!is.na(recovery$recID)) & (!is.na(recovery$dati)),]
    # add an indicator flag
    recovery$action = "recover"  
    # store recovery data for this one file
    recoveryList[[i]] = recovery
  } # END for-loop over all files in wFiles
  
  
  # merge the lists into one deloy and one recovery data.frame
  deploy = do.call(rbind, deployList)
  recovery = do.call(rbind, recoveryList)
  
  
  # combine deployment and recovery data into one data.frame 
  temp1 = rbind(deploy, recovery)
  temp2 = temp1[order(temp1$array, temp1$recID, temp1$dati, temp1$deployNum),]
  
  # combine deployment - recovery paired rows into single rows
  recLocs = ddply(temp2, .(recID), function(x){
    #  ii=15; x = temp2[temp2$recID == unique(temp2$recID)[ii],]
    
    # create a new, reshaped data.frame...the first row is junk...this is ugly
    temp3 = x[1,c("array", "recID", "dati", "dati", "lat", "long")]
    names(temp3)[c(3,4)] = c("deployDati", "recoverDati")
    
    pairList = rep(1:100, each=2)  
    tempList = split(x, head(pairList,nrow(x)))
    for(i in 1:length(tempList)){
      # a check 
      if(tempList[[i]][1,]$action != "deploy"){print(paste0("Not a deployment. i=",i,". recID=", temp3$recID[1]))} 
      
      # pick recovery elements if they exist (i.e. is this deployment still in the water?)
      if(nrow(tempList[[i]]) == 2){
        # a check 
        if(tempList[[i]][2,]$action != "recover"){print(paste0("Not a recovery. i=",i,". recID=", temp3$recID[1]))} 
        holder1 = tempList[[i]][2,]$dati
      } else { # else the receiver is still in the water...
        # all this extra stuff is to force GMT
        holder1 = as.POSIXct(unclass(as.POSIXct(Sys.time(), 
          origin="1970-1-1", tz="EST5EDT")), origin="1970-1-1", tz="GMT") 
      }
      
      # pick deployment elements and tack on the recovery dati 
      holder2 = tempList[[i]][1,c("array", "recID","dati","lat","long")]
      # rename and combine
      names(holder2)[names(holder2) == "dati"] = "deployDati"
      holder2$recoverDati = holder1
      # rearrange
      holder3 = holder2[,c("array", "recID","deployDati","recoverDati","lat","long")]
      temp3 = rbind(temp3, holder3) 
    } # end for i loop
    
    # drop the empty first row...this is ugly...how do you really do this?   WHAT IS THIS LINE FOR?
    temp3 = temp3[-1,] 
    return(temp3)
  }) # end ddply()
  
  ## a check to compare temp2 and temp3
  #ii = ii + 1
  #temp2[temp2$recID == unique(temp2$recID)[[ii]],]
  #recLocs[theAnswer$recID == unique(theAnswer$recID)[[ii]],]
  
  # see missing data
  recLocs[ is.na(recLocs$lat) | is.na(recLocs$long), ]
  # drop missing data
  mapLocs = recLocs[ !is.na(recLocs$lat) & !is.na(recLocs$long), ]
  
  
  
  # plot deployment lines. (recycled) colors show when a unique receiver was in the water 
  # (at any location). No receiver color-line should ever overlap itself. That would mean
  # the GLATOS submission package indicates a receiver was in the water in two places at
  # the same time. I.e. no receiver should have a deployment date before it's preceeding
  # recovery date.
  pdf("Receiver Deployment Dates.pdf", width=15, height=30)
  plot(0,0, xlim=c(min(recLocs$deployDati), max(recLocs$recoverDati)), 
    ylim=c(0, nrow(recLocs)), xlab="", ylab="Individual receivers shown by color", xaxt="n", main="Receiver deployment dates")
  daterange = c(as.POSIXlt(min(recLocs$deployDati)), as.POSIXlt(max(recLocs$recoverDati)) )
  axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b %Y", las=2)
  for(i in 1:nrow(recLocs)){
    points(c(recLocs$deployDati[i], recLocs$recoverDati[i]), rep(i,2), type="l", 
      col=recLocs$recID[i])
  }  
  dev.off()
  
  
  coords = data.frame(x=mapLocs$long, y=mapLocs$lat)
  crs<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  plot(coords, pch=19)
  
  mapLocs.spdf = SpatialPointsDataFrame(coords=coords, data=mapLocs, proj4string = CRS(crs))
  mapView( mapLocs.spdf,  map.types = mapviewGetOption("basemaps"), zcol="array" )
  # import receiver locations and times from our GLATOS submission forms
  ############################################################################################################
  
  
  ############################################################################################################
  # import tagging data from our GLATOS submission forms
  ############################################################################################################
  #
  # This file lists all tags actually released, even if a tag wasn't detected.
  #
  # Currently, 70 tags, including beacons
  #  11850 11851 11852 11853 11854 11855 11856 11857 11858 11859 11860 11861 
  #  11862 11863 11864 11865 11866 11867
  #  11868 11869 11870 11871 11872 11873 11874 11875 11876 11877 11878 11879 
  #  12257 12258 12259 12260 12261 12262
  #  12263 12264 12265 12266 12837 12839 12841 13017 13018 13019 13020 13021 
  #  13022 13023 13024 13025 13026 13027
  #  13028 13029 13030 13031 24632 24633 24634 24635 24636 24637 24638 24639 
  #  24640 24641 24642 24643
  
  ############ List tagID/animalID and date that fish are believed to be dead
  #tagID 11879. Dead on what date
  
  tagList = list()
  
  for (i in 1:length(wFiles)){
    rawData = read_excel( wFiles[i], sheet="Tagging", skip=1 )
    # only want a list of deployed tags (v. detected tag)
    tagID=sort(unique(rawData$TAG_ID_CODE))   ################ zyzyzy change this to animalID when we add it in submission file
    
    # Remove Dead Fish from dataset
    DeadFish=NA
    tagID=tagID[ which(tagID!=DeadFish)]
    temp1 = rawData[,
      c("ANIMAL_ID","TAG_ID_CODE","COMMON_NAME_E","RELEASE_LOCATION","GLATOS_RELEASE_DATE_TIME")]
    names(temp1) = c("animalID", "tagID", "species", "location", "dati")
    tagList[[i]] = temp1[order(temp1$dati),]
    rm(rawData)
  } # END for-loop reading in wFiles
  
  releasedTagsList = as.data.table(do.call(rbind, tagList))
  releasedTagsList[, .N, by=c("species", "location")]
  
  # END import tagging data from our GLATOS submission forms
  ############################################################################################################

  # end and return
  return(list(recLocs = recLocs, releasedTagsList = releasedTagsList))
}


ans = importSubmissionData()


