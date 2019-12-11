######################################################################################################################
######################################################################################################################
######################################################################################################################
# Zy Biesinger. Started 2019 Apr 05
# This script imports raw telemetry data downloaded from GLATOS.  
# Then does a few small diagnostic things.
# This file is intended to be sourced from other scripts.


importDetectionData = function(){
  
  
  ############################################################################################################
  # load libraries
  ###########################################################################################################
  require(devtools)
  require(glatos)
  require(data.table)
  require(tcltk)
  require(rlist)
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
  
# # DELETE BELOW
#   ###########################################################################################################
#   # import receiver location data from glatos  
#   ###########################################################################################################
#   # ...this file has the entire GLATOS receiver list
#   # ...choose a file named like "LNRLS_detectionsWithLocs_***"
#   wFile = choose.files(default= paste0(glatosDir, "*.*"), 
#     caption=paste0("Please select one GLATOS receiver file with a name like: ",
#       "'GLATOS_receiverLocations_*.csv'"),
#     multi=FALSE, filters = Filters[c("csv")]
#   )
#   
#   recs = read_glatos_receivers(wFile)
#   recs = as.data.table(recs)
#   # END read in detections data from glatos
#   ###########################################################################################################
# # DELETE ABOVE
  
  ###########################################################################################################
  # import detections (det) data from glatos  
  ###########################################################################################################
  # ...glatos data files come separately for each project, meaning that all and only detections fish identified  
  # ...for this project are included, no matter where they are detected.
  # (Choose between the 'LNRLS' project or the 'ELOCS' project
  #
  detList = list() # each list element holds the data.frame with all detections for one project
  projectCount = 1
  
  # ask if the user wants to select detections from one project or many projects
  pickMultiple = tk_messageBox(type="yesno", 
    message = "Do you want to import detections from more than one project?",
    caption = "How many detections files to import from GLATOS...",
    icon="question"
  )
  
  if (pickMultiple == "no"){
    continue = FALSE
  } else if (pickMultiple == "yes"){
    continue = TRUE
  } else {
    print("Something is wrong with 'pickMultiple'.")
  }
  
  # import data downloaded directly from GLATOS...choose a file named like 
  # "LNRLS_detectionsWithLocs_***"
  # or like "ELOCS_detectionsWithLocs_***"
  
  # list for detection file names for all files we want to import
  wFiles = list()
    
  
  # select the first detection file
  wFile = choose.files(default= paste0(glatosDir, "*.*"), 
    caption=paste0("Please select one GLATOS detection file with a name like: ",
      "'LNRLS_detectionsWithLocs_*.csv' or 'ELOCS_detectionsWithLocs_*.csv'"),
    multi=FALSE, filters = Filters[c("csv")]
  ) 
  
  wFiles = list.append(wFiles, wFile)
  
  # import and save the detection file 
#  temp1 = read_glatos_detections(wFile)
#  detList[[projectCount]] = as.data.table(temp1)
  
  
  # if you want to select multiple detections files, then continue
  while(continue){
    # keep count
    projectCount = projectCount + 1
    
    # tell user what to expect
    importMore = tk_messageBox(type="ok", 
      message = "Please select additional project detection files sequentially in individual windows.",
      caption = "How to select import detection files from multiple projects.",
      icon="info"
    )
    
    # select one detection file
    wFile = choose.files(default= paste0(glatosDir, "*.*"), 
      caption=paste0("Please select one GLATOS detection file with a name like: ",
        "'LNRLS_detectionsWithLocs_*.csv' or 'ELOCS_detectionsWithLocs_*.csv'"),
      multi=FALSE, filters = Filters[c("csv")]
    )  
    wFiles = list.append(wFiles, wFile)
    
    importMore = tk_messageBox(type="yesno", 
      message = "Do you want to import detections from another project?",
      caption = "Would you like to select a detection file from another project?",
      icon="question"
    )
    
    if (importMore == "no"){
      continue = FALSE
    } # else continue remains TRUE
    
    
  } # END while(continue)... don't choose any more detection files
  
  # now that we have a list of files to import, wFiles, let's import them
  for (i in 1:length(wFiles)){
    temp1 = read_glatos_detections(wFiles[[i]])
    detList[[i]] = as.data.table(temp1)
  }
  
  
  # combine multiple detection files into one big file
  det = do.call(rbind, detList)
  rm(detList)
  
  # drop beacon tags or known dead fish
  # COMMING SOON?
  
  # how many detections for each species
  detPerSpecies = det[, .N, by = c("common_name_e", "release_location")]
  numberOfTags = det[, length(unique(animal_id)), by = "common_name_e"]  
  # Write out some basic descriptions
  for (i in 1:nrow(detPerSpecies)){
    print(paste0("There were ", detPerSpecies$N[i], " detections of ",   
      numberOfTags$V1[i], " ", numberOfTags$common_name_e[i], " tags in the ",
      detPerSpecies$release_location[i], " project."))
  }  
    
  # END read in detections data from glatos
  ###########################################################################################################
  
  # end and return
  setwd(oDir)
  return(det)

} # END importDetectionData()


# det = importDetectionData()




