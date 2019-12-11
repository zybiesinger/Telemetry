######################################################################################################################
######################################################################################################################
######################################################################################################################
# Zy Biesinger. Started 2019 Apr 05
# This script imports raw receiver location data downloaded from GLATOS.  
# Then does a few small diagnostic things. This data can be compared with what we send to 
# glatos, just to confirm that all is good.


importReceiverData = function(){
  ############################################################################################################
  # load libraries
  ###########################################################################################################
  require(devtools)
  require(glatos)
  require(data.table)
  require(tcltk)
  # END load libraries
  ###########################################################################################################
  
  
  ###########################################################################################################
  # directories...pick 'LNRGLATOS' or 'ELOGLATOS'
  ###########################################################################################################
  oDir = getwd() # when this script is sourced from another file, save the original directory
  rootDir = paste0("P:/Projects/GLRI/Native Species Program/Projects/Telemetry/Acoustic Telemetry/")
  # data we send to GLATOS
  submissionDir = paste0(rootDir, "GLATOS/Submission forms/")
  # data sent back from GLATOS
  glatosDir = paste0(rootDir, "GLATOS/")
  # END directories
  ###########################################################################################################
  
  ###########################################################################################################
  # import receiver location data from glatos  
  ###########################################################################################################
  # ...this file has the entire GLATOS receiver list
  # ...choose a file named like "LNRLS_detectionsWithLocs_***"
  wFile = choose.files(default= paste0(glatosDir, "*.*"), 
    caption=paste0("Please select one GLATOS receiver file with a name like: ",
      "'GLATOS_receiverLocations_*.csv'"),
    multi=FALSE, filters = Filters[c("csv")]
  )
  
  recs = read_glatos_receivers(wFile)
  recs = as.data.table(recs)
  # END read in detections data from glatos
  ###########################################################################################################
  
  
  # end and return
  setwd(oDir)
  return(recs)

} # END import_project_data()


recs = import_receiver_data()



