
###############################################################################
###############################################################################
###############################################################################
# This file is intended as the starting place where all the basic,
# non-derived and non-calculated information about each array deployment
# is entered.  Other information is added as it is calcul

#-------------------------------------------------------------------------------

# md stands for metadata, dn stands for telemetry deployment number

# md1 -hb2007 -if41 -2007Dec07     
# md2 -hb2008 -if41 -2008Oct09
# md3 -hb1    -if43 -2009Jun01  
# md4 -sb1    -of43 -2009Jul10  
# md5 -sb2    -oh41 -2009Aug03  
# md6 -hb2    -if41 -2009Aug24  
# md7 -sb3    -os43 -2009Sep14 
# md8 -hb3    -if42 -2009Oct12 
# md9 -sb4    -of43 -2009Nov16


md1 = list(
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  hb2007 = list( # dn=1 2007 full deployment
    deployment = "hb2007",
    year = 2007,
    site = "if41",
    spacing = 50,
    homeDir = paste(dataDir, sdlDir, "/2007/2007Dec07 IF41",sep=""),
    alpsDir = "/ALPS 2011Feb14",  
    sentinelNames = c(),
    beaconNames = c("b79100", "b79200", "b79300", "b79400", "b79500"),    
    fishNames = c("f60200", "f60400", "f60500", "f60700", "f60900"),
    goodFishNames = c(), # this needs to be filled in
    trueNames = c(), # this only applies to hb2008
    otherNames = c("o60800"), 
    allTagNames = NA, # automatically filled later        
    SDLmode="symbol",
    bestBeacon="b79200",
    secondBestBeacon = "b79100", # for positioning the bestBeacon
    timezone="EST",  # EST = GMT - 5 hours
    sdlDownloadDate = "17jan08",
    startDay = "2007/Dec/07",
    startTime = "21:36:00", # GMT
    taggingDay = "2007/Dec/09",  # tagging time is always 00:00:00 GMT
    stopDay = "2008/Jan/15", 
    stopTime = "15:00:00", # GMT                                                 
    startUtime = NA, # automatically filled later
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = "2008Mar11 GPS data.txt",
    gpsTimes = list( # Times corrected from EST to EDT by adding 1 hour
      # ... for more on this, see the ReadMe.docx file in the GPS data folder
      #     for this deployment
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("10:28:00", "10:39:22", "11:24:30", "11:38:16", "11:50:30", 
        "10:49:30", "10:28:00"),
      stop = c("10:33:29", "10:44:13", "11:29:00", "11:43:44", "11:55:37", 
        "10:53:40", "11:55:37")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,
      "northing" = 691.2323
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(8569, 8626, 8570, 8522, 8583),
      "northing" = c(742, 694, 639, 693, 699)
    ),
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b79100","b79200","b79300","b79400","b79500",""), 
      "location" = c("41","42","43","44","45","reef") 
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )  
  ), # end 2008 / if41   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  hb2008 = list( # dn=2 2008 full deployment
    # Remember the SDLs were set to collect in code mode, but with a bad
    # echo filter parameter.  So the results will be run through ALPS as if
    # the data had been collected in symbol mode.  Individual codes will be 
    # treated like symbols.
    deployment = "hb2008",
    year = 2008,
    site = "if41",
    spacing = 125,
    homeDir = paste(dataDir, sdlDir, "/2008/2008Oct09 IF41",sep=""),
    alpsDir = "/ALPS 2011Feb14", 
    sentinelNames = c(),
    # B79200: 80, 81. B79400: 85, 86. B79500: 130. 
    # you must choose a single beacon code...
    beaconNames = c("b80", "b81", "b85", "b86", "b130"),
    #beaconNames = c("b79200", "B79400", "B79500")
    # T60100: 66, 67. T60300: 70, 71. T61100: 46, 47. T61200: 48, 49.
    # T61300: 51, 52.  
    fishNames = c("f66","f67","f70","f71","f46","f47","48","49","f51","f52"),
    # fish tag on REMUS T61500: 55, 56.
    #otherNames = c("f55","f56"),        
    goodFishNames = c(), # this needs to be filled in
    otherNames = c("o61500"),  
    # you can choose a true fish tag and both files for both codes will be included
    trueNames = c("b79200", "b79400", "b79500","f60100", "f60300", "f61100",        
      "f61200","f61300", "o61500"),
    allTagNames = NA, # automatically filled later        
    SDLmode="symbol", # really in code mode but the echo filter was set wrong 
                    # so we analyze it as if in symbol mode 
    bestBeacon="b130",
    secondBestBeacon = "b86", # for positioning the bestBeacon. 85 and 86 are equally good                 
    timezone="EDT",
    sdlDownloadDate = "19dec08",
    startDay = "2008/Oct/09",
    startTime = "17:05:00", # GMT
    taggingDay = "2008/Oct/17",  # tagging time is always 00:00:00 GMT
    stopDay = "2008/Dec/08",  # this was really "2008/Dec/16" but the batteries died on the 6th/7th
    stopTime = "00:00:00", # GMT
    startUtime = NA, # automatically filled later 
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later       
    habmapFileName =  paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = c("2008Dec16 GPS data.txt","2008Dec17 GPS data.txt"), # this needs to be checked.  My files on "2007/8 best estimates are a mess.
    gpsTimes = list( # Times corrected from EDT to EST by subtracting 1 hour
      # ... for more on this, see the ReadMe.docx file in the GPS data folder
      #     for this deployment.
      # Also, the reef was not done on the 16/17 Dec 2008.
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      # these times are actually on two days, but not overlapping so it works
      start = c("", "11:38:00", "12:01:15", "11:12:00", "11:44:00", "11:03:00", "11:03:00"), 
      stop = c("", "11:43:00", "12:05:15", "11:17:00", "11:50:00", "11:07:00", "12:05:15")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,
      "northing" =  691.2323
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" =  c(8574.86, 8684.45, 8568.03, 8471.48, 8582.56),   #bob=c(245174.86, 245284.45, 245168.03, 245071.48, 245182.56)
      "northing" = c(817.0, 686.0, 580.2, 699.5, 699.2)   #bob=c(3262517.0, 3262386.0, 3262280.2, 3262399.5, 3262399.2)
    ), 
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b79100","b79200","b79300","b79400","b79500",""), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )  
  ),# end d2008 / if41   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  hb1 = list( # dn=3 hb1 if43 hb1 if43 hb1 if43 hb1 if43 hb1 if43 hb1 if43
    deployment = "hb1",
    year = 2009,
    site = "if43",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Jun01 IF43 and performance test", sep=""),
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),    
    fishNames = c("f11", "f12", "f13", "f14", "f15", "f16", "f17", "f61000"), 
    goodFishNames = c("f13", "f14", "f16"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    # these other tags were used during the internal array trials   
    otherNames = c("o43", "o44", "o45", "o46", "o47", "o48", "o50", "o51", "o52"),  
    allTagNames = NA, # automatically filled later
    SDLmode="code",   
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "17jun09",
    startDay = "2009/Jun/01",
    startTime = "17:36:00", # GMT
    taggingDay = "2009/Jun/03", # tagging time is always 00:00:00 GMT
    stopDay = "2009/Jun/17", 
    stopTime = "14:30:00", # GMT
    startUtime = NA, # automatically filled later              
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later    
    habmapFileName =  paste(rootDir, 
      "/R summary files/IF43_lines_aligned_HB_SB_bluebox.jpg",sep=""),
    gpsFileName = "2009Jun01 GPS data.txt",
    gpsTimes = list(
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "west","south", "center", "wholepath"),
      start = c("10:57:43", "12:00:42", "12:42:38", "13:29:45", "13:07:46", 
        "11:11:38", "10:50:00"),
      stop = c("11:08:48", "12:05:09", "12:47:55", "13:35:30", "13:13:00", 
        "11:26:00", "13:40:00")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8878.01,
      "northing" = 428.3839
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(8879.925, 8963.893, 8794.494, 8874.637, 8886.520),
      "northing" = c(523.4948, 419.0967, 428.9580, 334.1587, 431.7117)
    ),  
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "", "b79500", "b1", "s79600"), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )   
  ),# end hb1 / if43
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sb1 = list(  # dn=4 sb1 of43 sb1 of43 sb1 of43 sb1 of43 sb1 of43 sb1 of43
    deployment = "sb1", 
    year = 2009,
    site = "of43",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Jul10 of43", sep=""),
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f18", "f19", "f20", "f21", "f22", "f23", "f24", "f61500"), 
    goodFishNames = c(), # NONE...from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c(),  
    allTagNames = NA, # automatically filled later        
    SDLmode="code",   
    bestBeacon="b2",
    secondBestBeacon = "b1", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "28jul09",
    startDay = "2009/Jul/10",
    startTime = "19:05:00", # GMT
    taggingDay = "2009/Jul/13", # tagging time is always 00:00:00 GMT
    stopDay = "2009/Jul/27", 
    stopTime = "15:00:00", # GMT                                           
    taggingUtime = NA, # GMT automatically filled later
    startUtime = NA, # automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/OF43_lines_SBonly_bluebox.jpg",sep=""),
    gpsFileName = c("2009Jul10 GPS data.txt","2009Jul27 GPS data.txt"),
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c(
        "12:33:00", # 2009Jul10 GPS data.txt
        "12:18:00", # 2009Jul27 GPS data.txt
        "11:57:10", # 2009Jul27 GPS data.txt
        "11:35:40", # 2009Jul27 GPS data.txt   
        "11:13:20", # 2009Jul27 GPS data.txt
        "12:42:00", # 2009Jul27 GPS data.txt
        "11:10:00"), # 2009Jul27 GPS data.txt
      stop = c(
        "13:01:30", # 2009Jul10 GPS data.txt
        "12:23:00", # 2009Jul27 GPS data.txt
        "12:02:10", # 2009Jul27 GPS data.txt
        "11:37:18", # 2009Jul27 GPS data.txt
        "11:18:30", # 2009Jul27 GPS data.txt
        "12:47:00", # 2009Jul27 GPS data.txt
        "12:50:00") # 2009Jul27 GPS data.txt
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 1297.066,
      "northing" = 1428.046
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(1307.215, 1390.835, 1296.048, 1216.619, 1305.865),
      "northing" = c(1532.886, 1425.098, 1333.012, 1446.895, 1433.767)
    ),  
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", "s79600"), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )        
  ), # end sb1 / of43
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sb2 = list( # dn=5 sb2 oh41 sb2 oh41 sb2 oh41 sb2 oh41 sb2 oh41 sb2 oh41 
    deployment = "sb2", 
    year = 2009,
    site = "oh41",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Aug03 oh41", sep=""),  
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f25", "f26", "f27", "f28", "f29", "f30", "f31", "f61600"),  
    goodFishNames = c("f26","f28", "f29", "f30", "f31"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c("o61700"), # this one is the diver tag during cleaning ),  
    allTagNames = NA, # automatically filled later       
    SDLmode="code",   
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "20aug09",
    startDay = "2009/Aug/03",
    startTime = "17:20:00", # GMT
    taggingDay = "2009/Aug/04",   # tagging time is always 00:00:00 GMT
    stopDay = "2009/Aug/20", 
    stopTime = "12:00:00", # GMT
    startUtime = NA, # automatically filled later                                
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later
    habmapFileName = paste(rootDir, 
      "/R summary files/OH41_OS43_lines_SBonly_bluebox.jpg",sep=""),
    gpsFileName = "2009Aug03 GPS data.txt",
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("11:12:00", "11:59:15", "12:28:00", 
        "12:52:00", "13:13:00", "11:02:00", "9:50:00"),
      stop = c("11:27:00", "12:03:00", "12:33:00", 
        "12:57:00", "13:18:00", "11:09:00", "13:30:00") 
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 434.8704,
      "northing" = 2059.854 
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(436.2471, 526.2297, 432.4532, 348.1557, 443.2310),
      "northing" = c(2164.789, 2054.926, 1955.684, 2061.206, 2069.637)
    ), 
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" lst.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", "s79600"), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )         
  ), # end sb2 / oh41
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  hb2 = list(  # dn=6 hb2 if41 hb2 if41 hb2 if41 hb2 if41 hb2 if41 hb2 if41
    deployment = "hb2",
    year = 2009,
    site = "if41",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Aug24 if41", sep=""),   
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f32", "f33", "f34", "f35", "f36", "f37", "f38", "f61700"), 
    goodFishNames = c("f33", "f34", "f35", "f36", "f37", "f38"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c(),  
    allTagNames = NA, # automatically filled later            
    SDLmode="code",   
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "08Sep09",
    startDay = "2009/Aug/24",
    startTime = "18:00:00", # GMT
    taggingDay = "2009/Aug/25", # tagging time is always 00:00:00 GMT
    stopDay = "2009/Sep/08", 
    stopTime = "14:00:00", # GMT
    startUtime = NA, # automatically filled later                                
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = "2009Aug24 GPS data.txt",
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("11:25:00", "12:25:00", "12:51:00", "13:13:20", "13:43:00", 
        "11:40:00", "11:25:00"),
      stop = c("11:35:00", "12:28:59", "12:56:00", "13:18:20", "13:48:00", 
        "11:50:00", "13:48:00")
      # Note that the stop time for SDL41-N was changed from 12:30:00 to 
      #   12:28:59 after a visual inspection  
    ), # end gpsTimes
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,
      "northing" = 691.2323
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(8573.610,  8663.719, 8587.707, 8491.073, 8583.340),
      "northing" = c(790.6531, 686.2500, 597.2437, 694.9796, 699.4701)
    ),  
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", "s79600"), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )         
  ), # end hb2 / if41
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sb3 = list( # dn=7 sb3 os43 sb3 os43 sb3 os43 sb3 os43 sb3 os43 sb3 os43
    deployment = "sb3",
    year = 2009,
    site = "os43",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Sep14 os43", sep=""),  
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f39", "f40", "f41", "f42", "f43", "f44", "f45", "f61800"), 
    goodFishNames = c("f39", "f40","f42", "f43"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c("f25", "f26", "f27", "f28", "f29", "f31"), # these are visiting fish tagNames  
    allTagNames = NA, # automatically filled later           
    SDLmode="code",    
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "01oct09",
    startDay = "2009/Sep/14",
    startTime = "19:00:00", # GMT
    taggingDay = "2009/Sep/16", # tagging time is always 00:00:00 GMT
    stopDay = "2009/Oct/01", 
    stopTime = "14:00:00", # GMT
    startUtime = NA, # automatically filled later                                  
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later        
    habmapFileName = paste(rootDir, 
      "/R summary files/OH41_OS43_lines_SBonly_bluebox.jpg",sep=""),
    gpsFileName = "2009Sep14 GPS data.txt",
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("11:55:00", "13:00:00", "13:38:00", "14:15:00", 
        "14:51:00", "12:16:00", "11:50:00"),
      stop = c("12:07:00", "13:06:00", "13:43:00", "14:22:00", 
        "14:57:00", "12:23:00", "15:00:00")
    ), # end gpsTimes
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 346.7775,
      "northing" = 2139.444
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(350.5960, 431.8326, 339.6018, 263.0775, 354.4020),
      "northing" = c(2239.207, 2141.473, 2056.579, 2144.924, 2146.314)
    ),  
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", "s79600"), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame( 
      "ID" = c("min", "max"),
      "easting" = c(0,1),  # automatically filled later
      "northing" = c(0,1)  # automatically filled later
    )        
  ), # end sb3 / os43
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  hb3 = list( # dn=8 hb3 if42 hb3 if42 hb3 if42 hb3 if42 hb3 if42 hb3 if42
    deployment = "hb3",
    year = 2009,
    site = "if42",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Oct12 if42", sep=""), 
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c(),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f46", "f47", "f48", "f50", "f51", "f52", "f61900", "f62000"), 
    goodFishNames = c("f47", "f48", "f51", "f52"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c(),  
    allTagNames = NA, # automatically filled later        
    SDLmode="code",   
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "27Oct09",
    startDay = "2009/Oct/12",
    startTime = "17:30:00", # GMT
    taggingDay = "2009/Oct/13", # tagging time is always 00:00:00 GMT
    stopDay = "2009/Oct/27", 
    stopTime = "14:45:00", # GMT
    startUtime = NA, # automatically filled later                                   
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = "2009Oct12 GPS data.txt",
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("10:39:45","11:42:30","12:08:00","12:45:00","13:18:00",
        "11:02:00","10:39:45"),
      stop = c("10:53:00","11:47:30","12:13:00","12:54:30","13:25:30",
        "11:08:00","13:25:00")
    ), # end gpsTimes
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8718.069,
      "northing" = 706.7241 
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(8721.842, 8798.138,  8725.15, 8632.181, 8720.673),
      "northing" = c(806.6219, 705.793, 605.4938, 711.7141, 718.6453)
    ), 
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", ""), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame( 
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )         
  ), # end hb3 / if42
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sb4 = list(  # dn=9 sb4 of43 sb4 of43 sb4 of43 sb4 of43 sb4 of43 sb4 of43
    deployment = "sb4", 
    year = 2009,
    site = "of43",
    spacing = 100,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Nov16 of43", sep=""), 
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c(),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f53", "f54", "f55", "f56", "f57", "f58", "f59", "f62100"), 
    goodFishNames = c("f54", "f56", "f57","f59", "f62100"), # from inspecting ALPS output  
    trueNames = c(), # this only applies to hb2008
    otherNames = c(),  
    allTagNames = NA, # automatically filled later             
    SDLmode="code",     
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="GMT",
    sdlDownloadDate = "15Dec09",
    startDay = "2009/Nov/16",
    startTime = "18:30:00", # GMT
    taggingDay = "2009/Nov/18", # tagging time is always 00:00:00 GMT 
    stopDay = "2009/Nov/28",      # we picked up on 14 Dec but sdl batteries died between
    stopTime = "00:00:00", # GMT  # 26 and 30 Nov.  Last position solutions come ~27 Nov
    startUtime = NA, # automatically filled later                                         
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later     
    habmapFileName = paste(rootDir, 
      "/R summary files/OF43_lines_SBonly_bluebox.jpg",sep=""),
    gpsFileName = c("2009Jul10 GPS data.txt","2009Jul27 GPS data.txt"),
    gpsTimes = list(
      ID = c("reef","41","42","43","44","45","wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c(
        "12:33:00", # 2009Jul10 GPS data.txt
        "12:18:00", # 2009Jul27 GPS data.txt
        "11:57:10", # 2009Jul27 GPS data.txt
        "11:35:40", # 2009Jul27 GPS data.txt   
        "11:13:20", # 2009Jul27 GPS data.txt
        "12:42:00", # 2009Jul27 GPS data.txt
        "11:10:00"), # 2009Jul27 GPS data.txt
      stop = c(
        "13:01:30", # 2009Jul10 GPS data.txt
        "12:23:00", # 2009Jul27 GPS data.txt
        "12:02:10", # 2009Jul27 GPS data.txt
        "11:37:18", # 2009Jul27 GPS data.txt
        "11:18:30", # 2009Jul27 GPS data.txt
        "12:47:00", # 2009Jul27 GPS data.txt
        "12:50:00") # 2009Jul27 GPS data.txt
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 1297.066,
      "northing" = 1428.046
    ),
    sdlEN = data.frame(
      "ID" = c("41","42","43","44","45"),
      "easting" = c(1307.215, 1390.835, 1296.048, 1216.619, 1305.865),
      "northing" = c(1532.886, 1425.098, 1333.012, 1446.895, 1433.767)
    ),   
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      "beaconID" = c("b2", "b79400", "b79500", "", "b1", ""), 
      "location" = c("41","42","43","44","45","reef")
    ),
    plotLimits = data.frame( 
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )         
  ), # end sb4 / of43
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sp125 = list( # dn=10, 2009 April 23 Spacing Trial on IF4.1 at 125m
    deployment = "sp125",
    year = 2009,
    site = "if41",
    spacing = 125,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Apr23 spacing trials/125m spacing",sep=""),
    alpsDir = "/ALPS 2011Feb14", 
    sentinelNames = c(),
    beaconNames = c("b79400", "b79500"),
    fishNames = c("f2","f61000","f61500"),  
    goodFishNames = c(), # doesn't apply
    otherNames = c(),  
    trueNames = c(), # this only applies to hb2008
    allTagNames = NA, # automatically filled later        
    SDLmode="code", 
    bestBeacon="b79500",   # i need to check this and the second best...
    secondBestBeacon = "b79400", # for positioning the bestBeacon                 
    timezone="EDT",
    sdlDownloadDate = "23apr09",
    startDay = "2009/Apr/23",
    startTime = "16:43:00", # GMT
    taggingDay = NA, # if no fish were tagged, fill this in later with startDay
    stopDay = "2009/Apr/23", 
    stopTime = "18:20:00", # GMT
    startUtime = NA, # automatically filled later                                      
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = c(), # I'll do all this by hand since it's funky 
    gpsTimes = list( # I'll do all this by hand since it's funky 
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("", "","", "", "", "", ""), 
      stop = c("", "","", "", "", "", "")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,
      "northing" =  691.2323
    ),
    sdlEN = data.frame(  # get these from 'GPS position estimates.r'
      # NOTE: 41 is from the target position since there was no GPS data
      # NOTE: 45 is from later estimates of c45
      "ID" = c("41","42","43","44","45"), 
      "easting" =  c(245175.58, 245278.50, 245179.60, 245069.20, 
        245183.3) - eastingOffset, 
      "northing" = c(3262516.20, 3262394.00, 3262282.30, 3262393.40, 
        3262399.00) - northingOffset  
    ), 
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.    
      "beaconID" = c("b79400", "", "", "", "b79500", ""), 
      "location" = c("41", "42", "43", "44", "45", "reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )  
  ),# end sp125 / if41   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sp150 = list( # dn=11, 2009 May 7 Spacing Trial on IF4.1 at 150m
    deployment = "sp150",
    year = 2009,
    site = "if41",
    spacing = 150,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Apr23 spacing trials/150m spacing",sep=""),
    alpsDir = "/ALPS 2011Feb14", 
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f61000","f61500"),       
    goodFishNames = c(), # doesn't apply
    otherNames = c(),  
    trueNames = c(), # this only applies to hb2008
    allTagNames = NA, # automatically filled later        
    SDLmode="code", 
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="EDT",
    sdlDownloadDate = "07May09",
    startDay = "2009/May/07",
    startTime = "15:07:00", # GMT
    taggingDay = NA, # if no fish were tagged, fill this in later with startDay
    stopDay = "2009/May/07", 
    stopTime = "16:54:00", # GMT
    startUtime = NA, # automatically filled later                      
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = c(), # I'll do all this by hand since it's funky 
    gpsTimes = list( # I'll do all this by hand since it's funky 
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("", "","", "", "", "", ""), 
      stop = c("", "","", "", "", "", "")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,
      "northing" =  691.2323
    ),
    sdlEN = data.frame( # get these from 'GPS position estimates.r'
      "ID" = c("41","42","43","44","45"),
      "easting" =  c(245187.5, 245324.9, 245174.4, 245052.0, 245183.3)-eastingOffset, 
      "northing" = c(3262556, 3262402, 3262253, 3262403, 3262399.00)-northingOffset 
    ),           
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      #
      # I don't list them because two beacons were on one SDL:
      # N41 had B2, B79400.  C45 had B1, B79500
      # The sentinel s79600 and T61000 were at the reef
      "beaconID" = c("", "", "", "", "", "s79600"), 
      "location" = c("41", "42", "43", "44", "45", "reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )  
  ),# end sp150 / if41   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  sp100 = list( # dn=12, 2009 May 7 Spacing Trial on IF4.1 at 100m
    deployment = "sp100",
    year = 2009,
    site = "if41",
    spacing = 150,
    homeDir = paste(dataDir, sdlDir, "/2009/2009Apr23 spacing trials/100m spacing",sep=""),  
    alpsDir = "/ALPS 2011Feb14",
    sentinelNames = c("s79600"),
    beaconNames = c("b1", "b2", "b79400", "b79500"),
    fishNames = c("f61000","f61500"),             
    goodFishNames = c(), # doesn't apply
    otherNames = c(),  
    trueNames = c(), # this only applies to hb2008
    allTagNames = NA, # automatically filled later        
    SDLmode="code", 
    bestBeacon="b1",
    secondBestBeacon = "b2", # for positioning the bestBeacon                 
    timezone="EDT",
    sdlDownloadDate = "07May09",
    startDay = "2009/May/07",
    startTime = "18:07:00", # GMT
    taggingDay = NA, # if no fish were tagged, fill this in later with startDay
    stopDay = "2009/May/07", 
    stopTime = "19:52:00", # GMT
    startUtime = NA, # automatically filled later                      
    taggingUtime = NA, # GMT automatically filled later
    stopUtime = NA, # automatically filled later      
    habmapFileName = paste(rootDir, 
      "/R summary files/IF41_IF42_lines_aligned_HBandSB_bluebox.jpg",sep=""),
    gpsFileName = c(), # I'll do all this by hand since it's funky 
    gpsTimes = list( # I'll do all this by hand since it's funky 
      ID = c("reef", "41","42","43","44","45", "wholepath"),
      where = c("reef", "north", "east", "south", "west", "center", "wholepath"),
      start = c("", "","", "", "", "", ""), 
      stop = c("", "","", "", "", "", "")
    ),
    reefEN = data.frame(
      "ID" = "reef",
      "easting" = 8574.76,  # 245174.8-eastingOffset
      "northing" =  691.2323 # 3262391-northingOffset
    ),
    sdlEN = data.frame( # get these from 'GPS position estimates.r'
      "ID" = c("41","42","43","44","45"),
      "easting" =  c(245183.4, 245258.5, 245175.1, 245085.9, 245183.3)-eastingOffset,
      "northing" = c(3262499, 3262391, 3262289, 3262395, 3262399)-northingOffset 
    ), 
    beaconEN= data.frame(
      # the 'beaconID' should list the beacons/sentinel used in this
      # deployment and be in the correct order to match where they were
      # according to the "location" list.
      #
      # I don't list them because two beacons were on one SDL:
      # N41 had B2, B79400.  C45 had B1, B79500
      # The sentinel s79600 and T61000 were at the reef
      "beaconID" = c("", "", "", "", "", "s79600"), 
      "location" = c("41", "42", "43", "44", "45", "reef")
    ),
    plotLimits = data.frame(
      "ID" = c("min", "max"),
      "easting" = c(0,1), # automatically filled later
      "northing" = c(0,1) # automatically filled later
    )  
  )# end sp100 / if41   
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
) # end metadata list



################################################################################
################################################################################
################################################################################
# order summary
deploymentOrder = list(
  "deployment" = vector(mode="character", length = length(md1)),
  "site" = vector(mode="character", length = length(md1))
)
for (i in 1:length(md1)){
 deploymentOrder$deployment[i] = md1[[i]]$deployment
 deploymentOrder$site[i] = md1[[i]]$site
}



################################################################################
################################################################################
################################################################################
# some derived information
md2 = md1
for (i in 1:(length(md2)-0)){ # i cycles through different trials
  # first sentinel, next beacons, finally fish tags
  # so that $allTagNames[1] is always sentinel, for example 
  md2[[i]]$allTagNames = c(md2[[i]]$sentinelNames, md2[[i]]$beaconNames, 
    md2[[i]]$fishNames, md2[[i]]$trueNames, md2[[i]]$otherNames)  
  # get the start and stop times
  md2[[i]]$startUtime = unclass(as.POSIXct(strptime(paste(md2[[i]]$startDay, 
    md2[[i]]$startTime), "%Y/%B/%d %H:%M:%S", tz="GMT"), origin="1970-1-1",
    tz="GMT"))[1]
  if(is.na(md2[[i]]$taggingDay)){
    # make taggingDay = startDay
    md2[[i]]$taggingDay = md2[[i]]$startDay} 
  md2[[i]]$taggingUtime = unclass(as.POSIXct(strptime(paste(md2[[i]]$taggingDay, 
    "00:00:00"), "%Y/%B/%d %H:%M:%S", tz="GMT"), origin="1970-1-1",
    tz="GMT"))[1]
  md2[[i]]$stopUtime = unclass(as.POSIXct(strptime(paste(md2[[i]]$stopDay, 
    md2[[i]]$stopTime), "%Y/%B/%d %H:%M:%S", tz="GMT"), origin="1970-1-1",
    tz="GMT"))[1]
  # get limits for plotting   
  md2[[i]]$plotLimits$easting = c(min(md2[[i]]$sdlEN$easting) - plotBuffer,
    max(md2[[i]]$sdlEN$easting) + plotBuffer)  
  md2[[i]]$plotLimits$northing = c(min(md2[[i]]$sdlEN$northing) - plotBuffer,
    max(md2[[i]]$sdlEN$northing) + plotBuffer)
  # for those deployments when no tagging happened, set the taggingDay equal
  # to startDay
  if(is.na(md2[[i]]$taggingDay)){
    md2[[i]]$taggingDay = md2[[i]]$startDay
  }      
}

#################################################################################
#################################################################################
#################################################################################
## GPS position estimates
## Read in GPS data and calculate reef and SDL best estimates of positions.
## Note that OF4.3 is funky and needs special attention.  See 'GPS position estimates.r'
#
## These numbers are now typed directly into the metadata section above
md3 = md2
#for (i in c(1,3:5)){#length(md3)){  because of43 is funky don't do i=2, do it below
#  rootDir = getwd()
#  gpsDir = paste(md3[[i]]$homeDir,"/GPS data", sep="")
#  setwd(gpsDir)
#  # choose to plot or not 
#  plotThem = TRUE # plotThem = FALSE #
#  # import GPS data for (reef, north, east, south, west, center) and calculate means
#  temp1 = list()
#  for (j in 1:6){ # cycles through reef and 5 SDL locations, 1-6 in list
#    temp1[[j]] = importGPSdata(md3[[i]]$gpsFileName, 
#      md3[[i]]$gpsTimes$start[j], md3[[i]]$gpsTimes$stop[j], 
#      filter=100, plotThem=plotThem)
#  }
#  # save means
#  md3[[i]]$reefEN = data.frame(
#    "ID" = md3[[i]]$gpsTimes$ID[[1]], 
#    "easting"=temp1[[1]]$average$easting, 
#    "northing"=temp1[[1]]$average$northing)
#  md3[[i]]$sdlEN = data.frame(
#    "ID" = md3[[i]]$gpsTimes$ID[2:6],
#    "easting" = c(temp1[[2]]$average$easting, temp1[[3]]$average$easting, 
#      temp1[[4]]$average$easting, temp1[[5]]$average$easting, 
#      temp1[[6]]$average$easting),
#    "northing" = c(temp1[[2]]$average$northing, temp1[[3]]$average$northing, 
#      temp1[[4]]$average$northing, temp1[[5]]$average$northing, 
#      temp1[[6]]$average$northing) 
#  )
#  setwd(rootDir)
#}
#
################################################################################
## repeat the whole thing for the troublesome deployment of43
#for (i in 2:2){  
#  rootDir = getwd()
#  gpsDir = paste(md3[[i]]$homeDir,"/GPS data", sep="")
#  setwd(gpsDir)
#  # choose to plot or not 
#  plotThem = TRUE # plotThem = FALSE #
#  # import GPS data for (reef, north, east, south, west, center) and calculate means
#  temp1 = list()
#  for (j in 1:6){ # cycles through reef and 5 SDL locations, 1-6 in list
#    temp1[[j]] = importGPSdata(md3[[i]]$gpsFileName[2], 
#      # [2] uses "2009Jul27 GPS data.txt"
#      # we want 2009Jul27 for everything but the reef, 2009Jul10 for the reef
#      md3[[i]]$gpsTimes$start[j], md3[[i]]$gpsTimes$stop[j], 
#      filter=100, plotThem=plotThem)
#  }
#  # save means
#  md3[[i]]$reefEN = data.frame(
#    "ID" = md3[[i]]$gpsTimes$ID[[1]],
#    "easting"=temp1[[1]]$average$easting, 
#    "northing"=temp1[[1]]$average$northing)
#  md3[[i]]$sdlEN = data.frame(
#    "ID" = md3[[i]]$gpsTimes$ID[2:6],
#    "easting" = c(temp1[[2]]$average$easting, temp1[[3]]$average$easting, 
#      temp1[[4]]$average$easting, temp1[[5]]$average$easting, 
#      temp1[[6]]$average$easting),
#    "northing" = c(temp1[[2]]$average$northing, temp1[[3]]$average$northing, 
#      temp1[[4]]$average$northing, temp1[[5]]$average$northing, 
#      temp1[[6]]$average$northing) 
#  )
#  setwd(rootDir)
#}
#
## now do the problem 'reef' all be it's lonesome
#i=2
#rootDir = getwd()
#gpsDir = paste(md3[[i]]$homeDir,"/GPS data", sep="")
#setwd(gpsDir)
#
#temp1 = importGPSdata(md3[[2]]$gpsFileName[1], # uses "2009Jul10 GPS data.txt"
#  md3[[2]]$gpsTimes$start[1], md3[[2]]$gpsTimes$stop[1], filter=100, plotThem=plotThem)
#md3[[2]]$reefEN = data.frame("ID" = md3[[i]]$gpsTimes$ID[1] , "easting"=temp1$average$easting, 
#    "northing"=temp1$average$northing)
## end the troublesome array
##################################################################################
#
#             ################### ######################check these positions with my notes of best estimates
#              ### the first three look great.
#
#
##########################################################################
#### now do the 'test' deployment
#
## NOTE THE REGULAR CODE DOESN'T WORK BECAUSE THERE'S NO TEST GPS DATA...
#
#i=8
#
## SO ... FAKE IT...
#md3[[i]]$reefEN = data.frame(
#  "ID" = "reef", 
#  "easting" = 0+eastingOffset, 
#  "northing" = 0+northingOffset
#)
#md3[[i]]$sdlEN = data.frame(
#  "ID" = c("41", "42", "43", "44", "45"),
#  "easting" = c(0,-100,0,100,sqrt((10^2)/2)) + eastingOffset,
#  "northing" = c(100,0,-100,0,sqrt((10^2)/2)) + northingOffset
#) 
       
       
##############################################################################
###############################################################################
##############################################################################
# Okay then...all done with the telemetry deployments          
md=md3
                                                         




##############################################################################
###############################################################################
##############################################################################
# Now for the ADCP deployments.  They don't always coincide with SDL deployments.
#
# Use these start and stop times to trim bad data from the ADCP files.
#
# ADCP metadata -> adcpmd
adcpmd1 = list(
  # I've picked these start and stop dati from looking at the data files
  # and finding where the depth measurements are right.

  adcp200701 = list(
    deployment = "2007adcp01",
    startDati = "2007/Dec/19 16:00:00", #GMT 
    stopDati = "2008/Jan/15 17:00:00", # GMT 
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200801 = list(
    deployment = "2008adcp01",
    startDati = "2008/Oct/10 17:00:00", # GMT 
    stopDati = "2008/Dec/16 14:30:00", # GMT 
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200901 = list(
    # this file has a funky depth record where depth = 0 
    deployment = "2009adcp01",
    startDati = "2009/Jun/01 21:00:00", # GMT
    stopDati = "2009/Aug/20 16:30:00", # GMT    
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200902 = list(
    # On Aug 8 we found ADCP on its side, from looking at the pitch/roll log, 
    # it looks like the ADCP fell over right away.  From this file I'll only 
    # use the temperature and depth information.  When merging ADCP and SDL
    # data, be careful interpolating.  Don't water flow from the previous and
    # following ADCP files. 
    deployment = "2009adcp02",
    startDati = "2009/Aug/24 19:30:00",  # GMT
    stopDati = "2009/Sep/08 15:00:00",  # GMT 
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200903 = list(
    deployment = "2009adcp03",
    startDati = "2009/Sep/08 17:00:00", # GMT 
    stopDati = "2010/Oct/01 16:00:00",  # GMT   
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200904 = list(
    deployment = "2009adcp04",
    startDati = "2009/Oct/13 17:00:00",  # GMT
    stopDati = "2009/Oct/26 14:30:00",  # GMT 
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  ),
  adcp200905 = list(
    deployment = "2009adcp05",
    startDati = "2009/Nov/18 19:00:00", # GMT
    stopDati = "2009/Nov/26 16:09:00", # GMT   
    # The actual recovery dati: 2009/Dec/14 18:30:00
    # This is the dati when the battery died: 2009/Nov/26 16:09:00 GMT
    startUtime = NA, # automatically filled later
    stopUtime = NA # automatically filled later
  )
)        
adcpmd2 = adcpmd1

# calculate the utimes for ADCP dati
for (i in 1:length(adcpmd2)){           
  # get the adcp start and stop times
  adcpmd2[[i]]$startUtime = unclass(as.POSIXct(strptime(adcpmd2[[i]]$startDati, 
    "%Y/%b/%d %H:%M:%S", tz="GMT"), origin="1970-1-1",
    tz="GMT"))[1]
  adcpmd2[[i]]$stopUtime = unclass(as.POSIXct(strptime(adcpmd2[[i]]$stopDati, 
    "%Y/%b/%d %H:%M:%S", tz="GMT"), origin="1970-1-1",
    tz="GMT"))[1]
}  

adcpmd = adcpmd2
