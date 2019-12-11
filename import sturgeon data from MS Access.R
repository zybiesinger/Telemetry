################################################################################
################################################################################
################################################################################
### Started 12 July 2018. Zy Biesinger.
### 2019 March 26. This is in a pretty stable form.
#
#
# This file is used to get sturgeon data out of MS Access into R.
# Tables are read in as data.frames. Data cleaning is a back and forth process
# identifing and correcting mistakes in the origianl Access database.
##
# THIS DOESN'T PRESERVE ALL DATA ABOUT FISH. I EXCLUDE SOME TYPES OF DATA.
# ALSO, I SIMPLY DROP SOME TROUBLESOME DATA
# WITHOUT MUCH THOUGHT, BECAUSE FOR NOW, THEY'RE NOT RELEVANT.
#
# THIS DROPS ANYTHING THAT'S NOT STURGEON...


# WISH LIST:
# - move all the recapture stuff to it's own script and remove it from this script
# - make table showing all recap histories
# - table with all tags for each fish
# - find all duplicate tagIDs and compare with recap check box   
# - why does 'recapHistory' have rows with only one capture? Fish first tagged by someone else!
#   - some are usgs tags with no prior history
#   - one is PIT 421D7F2901N or H, probably matches a fish w/o N or H
#   - one PIT 900118001501397 - possibly applied with a tag reader failure
# - plot tag retention time v. time at large, by tag type
# - check that tagIDs are the right format...eg PIT IDs are longish
# - search for 'tooLong' and fix setlines that are too long


#rm(list=ls())
##############################################################################
# identify pre-existing objects...
##############################################################################
# When this script is sourced by another file, there are existing ojbects. 
# So when I clean up objects from this script, I don't want to remove
# the pre-existing objects.
pre_existing = ls()

# END identify pre-existing objects...
##############################################################################






###############################################################################
# Libraries
###############################################################################
library(data.table)
library(ggplot2)
#library(PBSmapping) # for converting UTM to lat/long, convUL 
library(rgdal)
library(plyr) # for ddply()
library(dplyr) # for select(), distinct()
#library(tidyr)
library(stringr) # for searching through comments, etc.
#library(readxl) # for reading MS excel files
library(lubridate)
library(RODBC) # for talking to MS Access databases  
# END Libraries
###############################################################################





##############################################################################
# set variables and directories
##############################################################################

## Do you want to drop any non sturgeon fish
sturgeonOnly = TRUE
## there will be more data cleaning to do down below if you include non-LS

## Do you want to look at the plots
showPlot = FALSE


# remember the original directory
oDir = getwd()
# root directory
rootDir = paste("P:\\Projects\\GLRI\\Native Species Program\\Projects\\Lake Sturgeon\\",
  "LNR Lake Sturgeon Assessment\\",sep="")
# directory of MS Access database 
dbDir = paste0(rootDir, "Database and data\\") #NiagaraLakeSturgeon2017Mar13.accdb")
# directory of spatial maps of the river
geoDir = paste(rootDir, "pop assessment analysis\\shoreline shapefiles",sep="")

# set the output path 
# outputDir = paste0("P:\\Projects\\GLRI\\Native Species Program\\",
#                    "Projects\\Telemetry\\Radio Telemetry\\Sturgeon radio telemetry data\\",
#                    "From Access to R\\")
# outputDir = paste0("C:\\zy\\R scripts\\From Access to R")

# END set variables and directories
##############################################################################


##############################################################################
# read in from the MS Access database
##############################################################################
dbFile = choose.files(default= paste0(dbDir, "*.*"), 
  caption=paste0("Please select Niagara River Lake Sturgeon MS Access database file with a name like: ",
    "'NiagaraLakeSurgeon.accdb'"),
  multi=FALSE, filters = Filters[c("accdb")]
)

## connect to the database
conAccdb <- odbcConnectAccess2007(dbFile) 
## Fetch the tables from the database. Modify the as-is and string settings as desired
tblBiology = sqlFetch (conAccdb,"tbl Fish Biological Data", as.is=FALSE, stringsAsFactors = FALSE)
tblCollection = sqlFetch (conAccdb,"tbl Fish Collection", as.is=FALSE, stringsAsFactors = FALSE)
tblMarks = sqlFetch (conAccdb,"tbl Mark applied/observed", as.is=FALSE, stringsAsFactors = FALSE)
tblAges = sqlFetch (conAccdb,"tbl AgeEstimates2017", as.is=FALSE, stringsAsFactors = FALSE)
tblSex = sqlFetch (conAccdb,"2012_2013_Final", as.is=FALSE, stringsAsFactors = FALSE)
tblTissues = sqlFetch (conAccdb,"tbl Tissue Data", as.is=FALSE, stringsAsFactors = FALSE)
tblIsotopes = sqlFetch (conAccdb,"tbl SIA results", as.is=FALSE, stringsAsFactors = FALSE)
# Always close the database  
close(conAccdb)
# END read in from the MS Access database
##############################################################################



# To compare PIT numbers from the Biomark reader with PIT numbers on the data sheet...
# ...read in downloads from PIT tag reader
# This is diagnostic code...not normally run. See the "Marks" section.
#
# pitDir = paste0("P:\\Projects\\GLRI\\Native Species Program\\Projects\\Lake Sturgeon\\",
#  "LNR Lake Sturgeon Assessment\\Database and data\\Database and data\\",
#  "PIT tag download and data sheets\\")

## Save the Access tables as R dataframes 
#save(tblBiology, file = paste0(outputDir, "tblBiology.rda"))
#save(tblCollection, file = paste0(outputDir, "tblCollection.rda"))
#save(tblMarks, file = paste0(outputDir, "tblMarks.rda"))
#save(tblAges, file = paste0(outputDir, "tblAges.rda"))
#save(tblSex, file = paste0(outputDir, "tblSex.rda"))
#save(tblTissues, file = paste0(outputDir, "tblTissues.rda"))
#save(tblIsotopes, file = paste0(outputDir, "tblIsotopes.rda"))




###############################################################################
# biology
###############################################################################
temp1 = tblBiology

# NOTE: Sure Sex is actually output from this file inserted into Access

# rename columns 
changeNames = data.frame(
  accessNames = names(temp1),
  rNames = c("collectionID", "fishNum", "fishID", "releaseDate", 
    "releaseTime", "species", "fl.mm", "tl.mm", "girth.mm", "weight.kg",
    "sex", "recap", "markOA", "comment", "markType", "sureSexAccess")
)
names(temp1) <- changeNames$rNames[match(names(temp1), changeNames$accessNames)]

# order by fishID
temp1 = temp1[order(temp1$fishID),]
row.names(temp1) = 1:nrow(temp1)

# drop any that aren't sturgeon
if(sturgeonOnly){ 
  temp1 = temp1[grepl("LAS", temp1$fishID),]
  print(paste0("Dropped ", nrow(tblBiology)-nrow(temp1), " non-sturgeon fish"))
}

# fix dati
temp1$releaseDati = strptime(paste(as.character(temp1$releaseDate),
  substring(as.character(temp1$releaseTime),12)), 
  "%Y-%m-%d %H:%M:%S", tz="EST5EDT")

# standardize
temp1$sex = tolower(temp1$sex) 
temp1$sureSex = tolower(temp1$sureSexAccess) 

# add collection year
temp1$collectionYear = as.factor(as.numeric(substr(temp1$fishID, 0,4)))  
nrow(temp1[ is.na(temp1$collectionYear), ]) # should be 0

# make 'recap' a 'no/yes' instead of '0/1'
unique(temp1$recap) # this must have only '0' and '1' values
temp1$recap = ifelse(temp1$recap == 1, 'yes', 'no')

# check for duplicate fishID
j1 = split(temp1, temp1$fishID)
(j2 = j1[lapply(j1, nrow) > 1])  # this should be an empty list...no more duplicates

# confirm all 'Collection ID'and 'Fish ID' are of appropriate length and match
sum(nchar(temp1$fishID)!=18) # should = 0. All FishIDs are the right length
sum(temp1$collectionID != substr(temp1$fishID,1,13)) # should = 0. Everything matches

# look at pictures
if(showPlot) pairs(~fl.mm + tl.mm + girth.mm + weight.kg, data=temp1)

# how many caught each year
if(showPlot){
  capYear = as.factor(as.POSIXlt(temp1$releaseDati)$year+1900)
  plot(capYear, main="Number of sturgeon caught each year")
}

# weight by year...later when I match up recaps, add horizontal connecting lines to this plot
if(showPlot){
  plot(temp1$releaseDati, temp1$weight.kg, pch=19, main="Fish weights arranged by year")
  temp2 = temp1[temp1$recap == "yes",]
  points(temp2$releaseDati, temp2$weight.kg, pch=19, col="red")
}

# a check
# When is there a 'sex' designation and no 'sureSex' designation? For example, this may happen 
# if a sex determination was made by ultrasound. We determined this sexing method to be unreliable.
# bob = temp1[ (temp1$sex != "unknown") & (temp1$sureSex == "unknown")   ,]


# which fish are recaps? ...might be our recaps of someone else's first catch
# ... because this recap indicator includes fish first tagged by someone else, 
# ... it's not really useful for us and our mark-recap modeling
# ... don't include it in the output from this script.
nrow(temp1[temp1$recap == "yes", ])

# add an index and rearrange
temp1$bID = 1:nrow(temp1)
temp1 = subset(temp1, select = c(bID, fishID, releaseDati, collectionYear, tl.mm, girth.mm,
  weight.kg, sex, sureSexAccess, recap, comment))
biology = temp1
# END biology
###############################################################################



###############################################################################
# collection
###############################################################################
temp1 = tblCollection
changeNames = data.frame(
  accessNames = names(temp1),
  rNames = c("collectionID","waterbody","gearType","numOfHooks",
  "setDate", "setTime","pullDate", "pullTime","setNum", "startDepth", 
  "endDepth", "waterTemp","northDDstart","westDDstart","datum","northDDend",
  "westDDend","fishObserved","locationComments","gearComments","otherComments")
)
names(temp1) <- changeNames$rNames[match(names(temp1), changeNames$accessNames)]

# reorder  
temp1 = temp1[order(temp1$collectionID),]
row.names(temp1) = 1:nrow(temp1)

# check for duplicate collectionID
j1 = split(temp1, temp1$collectionID)
(j2 = j1[lapply(j1, nrow) > 1]) # this should be an empty list...no more duplicates
 
# check for missing dates
nrow(temp1[is.na(temp1$setDate),]) # should = 0
# drop one 2014 setline that wasn't recovered
temp1 = temp1[!is.na(temp1$pullDate),]
nrow(temp1[is.na(temp1$pullDate),]) # should = 0

# if set and pull times are NA, replace with 'noon'
temp1$setTime[is.na(temp1$setTime)] = "1899-12-30 12:00:00 EST"
temp1$pullTime[is.na(temp1$pullTime)] = "1899-12-30 12:00:00 EST"

# set and pull dati
temp1$setDati = strptime(paste(as.character(temp1$setDate), 
  substring(as.character(temp1$setTime),12)), "%Y-%m-%d %H:%M:%S", tz="EST5EDT")
temp1$pullDati = strptime(paste(as.character(temp1$pullDate), 
  substring(as.character(temp1$pullTime),12)), "%Y-%m-%d %H:%M:%S", tz="EST5EDT")

# check for missing Dati
sum(is.na(temp1$setDati)) # should = 0 
sum(is.na(temp1$pullDati)) # should = 0 

# does 'pullDati' ever come before 'setDati'
temp2 = temp1[temp1$pullDati < temp1$setDati,]
sum(temp1$pullDati < temp1$setDati) # should = 0

# check for temp1$setNum
nrow(temp1[is.na(temp1$setNum), ]) # should = 0 

# check all longitudes are negative (meaning in the western hemisphere)... these should be fixed in MS Access
# sum( !(is.na(temp1$westDDstart)) & temp1$westDDstart > 0 )
temp1$westDDstart[ (temp1$westDDstart > 0) & (!is.na(temp1$westDDstart)) ] = 
  -temp1$westDDstart[ (temp1$westDDstart > 0) & (!is.na(temp1$westDDstart)) ]
sum( !(is.na(temp1$westDDstart)) & temp1$westDDstart > 0 ) # should be zero
# sum( !(is.na(temp1$westDDend)) & temp1$westDDend > 0 )
temp1$westDDend[ (temp1$westDDend > 0) & (!is.na(temp1$westDDend)) ] = 
  -temp1$westDDend[ (temp1$westDDend > 0) & (!is.na(temp1$westDDend)) ]
sum( !(is.na(temp1$westDDend)) & temp1$westDDend > 0 ) # should be zero


# look at depths (meters)
if(showPlot){
  ggplot(temp1, aes(x = startDepth)) +
    geom_histogram()
  bob = temp1[temp1$endDepth != -999, ]
  ggplot(bob, aes(x = endDepth)) +
    geom_histogram()
  ggplot(bob, aes(x=startDepth, y=endDepth)) +
    geom_point()
  ggplot(bob, aes(x=startDepth - endDepth)) +
    geom_histogram(bin=1)
}

# look at distance between start and end setline locations (in lat/long units) --- this should really be utm zyzyzy
if(showPlot){
  temp1$distance = sqrt( (temp1$westDDend - temp1$westDDstart)^2 + (temp1$northDDend - temp1$northDDstart)^2 ) 
  tooLong = temp1[(temp1$distance > 0.002) & (!is.na(temp1$distance)), ]
  
  ggplot(temp1, aes(x=distance)) +
    geom_histogram()
  
  ggplot(temp1, aes(x=distance)) +
    geom_histogram() +
    scale_x_continuous(limits = c(0, 0.0025)) # ... removes data
    #coord_cartesian(xlim=c(0, 0.0025))  # ...doesn't remove data
}


if(showPlot){
  # plot locations and look for outliers
  # Import LNR shoreline shape files
  ogrInfo(geoDir, "niagara_arc")
  usShore = readOGR(dsn=geoDir, layer="niagara_arc") # "Clarke_1866_Albers"
  caShore = readOGR(dsn=geoDir, layer="ont_west_arc") # "Clarke_1866_Albers"
  ontarioShore = readOGR(dsn=geoDir, layer="ontarioshore_arc") # "Clarke_1866_Albers"
  
  # compare and convert projections of baselayer maps 
  #proj4string(usShore);   proj4string(caShore);   proj4string(ontarioShore)
  # convert to WGS84
  mapCrs <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  usShore = spTransform(usShore, CRS=mapCrs)
  caShore = spTransform(caShore, CRS=mapCrs)
  ontarioShore = spTransform(ontarioShore, CRS=mapCrs)
  
  # convert setline locations to spatial data...FOR VISUAL INSPECTIONS DROP LINES WITHOUT LOCATION DATA...ZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZY
  temp2 = temp1[ (!is.na(temp1$northDDstart)) & (!is.na(temp1$northDDstart))  ,c("westDDstart","northDDstart")]
  setLocs = SpatialPoints(coords = temp2, 
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  # look at entire range of setLocs to identify locations far wrong
  plot(setLocs, pch=19)
  # plot(setLocs, pch=19, xlim=c(-79.0054, -79.0052), ylim=c(43.16,43.31))
  plot(usShore, add=T)
  plot(caShore, add=T)
}

# condense comments
temp1$comments = paste(temp1$locationComments, temp1$gearComments, 
  temp1$otherComments, sep=". ")

# add an index and rearrange
temp1$cID = 1:nrow(temp1)
collection = subset(temp1, select = c(cID, collectionID, waterbody, gearType, numOfHooks,
  setDati, pullDati, setNum, startDepth, endDepth, northDDstart, westDDstart, northDDend, 
  westDDend, comments))
# collection
###############################################################################



############################################################################
# marks ... for now, duplicates are left in until we decide what to do
############################################################################
temp1 = tblMarks
changeNames = data.frame(
  accessNames = names(temp1),
  rNames = c("fishID", "observedApplied", "tagType", "tagLocation", 
  "tagID", "comments")
)
names(temp1) <- changeNames$rNames[match(names(temp1), changeNames$accessNames)]

temp1 = temp1[order(temp1$fishID),]
row.names(temp1) = 1:nrow(temp1)

# drop any that aren't sturgeon
if(sturgeonOnly){ 
  temp1 = temp1[grepl("LAS", temp1$fishID),]
  print(paste0("Dropped ", nrow(tblMarks)-nrow(temp1), " non-sturgeon fish"))
}

# which fish don't have fishIDs
temp1[is.na(temp1$fishID),] # this should = 0 
# drop any without fishIDs for now ### fix these in the database instead of dropping them
temp1 = temp1[!is.na(temp1$fishID),]
# which fish don't have 'appliedObserved'
temp1[is.na(temp1$observedApplied),] # this should = 0 
# drop any without 'appliedObserved' for now ### fix these in the database instead of dropping them
temp1[is.na(temp1$observedApplied),] # this should = 0 
temp1 = temp1[!is.na(temp1$observedApplied),]

# remove PIT numbers for times when reader wasn't working...see the comments
#temp1$tagID[grepl("XXX", temp1$tagID)] = NA
temp1 = temp1[ !grepl("XXX", temp1$tagID), ]


# # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * 
# # compare PIT numbers from MS access with numbers downloaded from the PIT tag reader
#   ### This block is just for looking at stuff, it doesn't make any changes. 
#   ### I use it to try and find PIT ID transcription errors, etc.
#   ### ... read in the PIT reader download
#   pitTagReaderData = read_excel(paste0(pitDir, "2018Oct01 PIT tag reader memory download.xlsx"))
#   pitTagReaderData$Timestamp = mdy_hms( pitTagReaderData$Timestamp )
#   pitTagReaderData$date = date(pitTagReaderData$Timestamp)
# 
#   # ...pick out only PIT tags and with dates and tagIDs
#   dbPits = temp1[temp1$tagType == "PIT",c("fishID", "tagID")]
#   dbPits$dbDate = ymd(str_sub(dbPits$fishID, 1, 8))
#   dbPits = dbPits[, c("tagID", "dbDate")]
# 
#   # ...these dates are the date the setline was deployed, not the date the fish was caught and processes
#   # ...that was typically the next day...not always, but most of the time...
#   dbPits$dbDate = dbPits$dbDate + days(1)
#   # order these to make the merge look prettier
#   dbPits$pitDate = paste(dbPits$dbDate, dbPits$tagID, sep="   ")
#   dbPits = dbPits[, c("dbDate", "pitDate")]
# 
#   # ...next fetch the PIT numbers from the reader
#   readerPits = pitTagReaderData[, c("Tag Code", "date")]
#   names(readerPits) = c("readerID", "readerDate")
#   readerPits$readerID = str_replace(readerPits$readerID, "\\.", "")
# 
#   readerPits$pitDate = paste(readerPits$readerDate, readerPits$readerID, sep="   ")
#   readerPits = readerPits[, c("readerDate", "pitDate")]
#   # drop any duplicates...i.e. the reader scanned the same tag twice in one day...
#   bob = split(readerPits, f=readerPits$pitDate)
# 
#   bob1 = distinct(readerPits)
#   bob1 = split(bob1, f=bob1$pitDate)
#   sum(lapply(bob1, nrow) != 1) # his should be zero
# 
#   # okay, it works to remove duplicates
#   readerPits = distinct(readerPits)
# 
#   # combine database and reader PIT lists
#   allPits = merge(dbPits, readerPits, all=TRUE, by="pitDate")
#   allPits$date = if_else(!is.na(allPits$dbDate), allPits$dbDate, allPits$readerDate)
#   # ... If a date appears in the reader but not the database, the tag may have been scanned but
#   # ... not implanted into a fish. Or there may be a transcription error.
#   # ... If a data appears in the database then it should appear in the reader, always. If not 
#   # ... there was an error, the reader didn't work, got the wrong number or wasn't used.
# 
#   allPits = allPits[order(allPits$date), ]
#   #splitPits = split(allPits, f=allPits$date)splitPits = split(allPits, f=allPits$)
# 
#   head(dbPits)
#   head(readerPits)
# 
#   # I think I want a list of days, and then a list of tags on each day from the db and reader
#   dbSplit = split(dbPits, dbPits$dbDate)
# 
#   # order these to make the merge look prettier
#   readerOrder = order(readerPits$readerDate, readerPits$readerID)
#   readerPits = readerPits[readerOrder,]
# 
#   head(dbPits)
#   head(readerPits)
# 
#   pitsList = merge(dbPits, readerPits, all=TRUE, by.x="dbID", by.y="readerID")
# # Stopcomparing PIT numbers from MS access with numbers downloaded from the PIT tag reader
# # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * # * 
  

# check for missing values
temp1[is.na(temp1$fishID),]
sum(is.na(temp1$fishID)) # should = 0
#
temp1[is.na(temp1$observedApplied),]
sum(is.na(temp1$observedApplied)) # should = 0
#
temp1[is.na(temp1$tagType),]
sum(is.na(temp1$tagType)) # should = 0
# drop tagID with NA... PIT tags that weren't scanned (see temp1$comments) and fin clips
temp1 = temp1[!is.na(temp1$tagID),]
sum(is.na(temp1$tagID)) # should = 0

# check for duplicates
temp2 = temp1[temp1$observedApplied =="Applied",]
temp2$check = paste0(temp2$tagType, temp2$tagID)
sum(duplicated((temp2$check))) # wish this were zero...see below
#temp2[duplicated(temp2$check),]
# look at just duplicates
temp3 = split(temp2, temp2$tagID)
temp3 = temp3[lapply(temp3, nrow) > 1]
temp4 = temp2[temp2$tagID %in% names(temp3),]
temp4 = temp4[order(temp4$tagID),]
### Duplicates on non-LS won't be confused with LS...so don't worry about this, especially if they were dropped
### Two radio frequencies (150.184 and 150.264) were used twice on 
### LS: in 2011 and 2013. We're not doing work with radio data now, so 
### these tags are only useful if we catch a fish and end up removing the
### radio tag and using the ID to identify the fish. So, don't worry about
### this problem. Finally, some PIT tags didn't scan correctly in the field and were recorded incorrectly. 
### There's no solution to this.

# check for 'observed' tags that were never 'applied', meaning someone else applied that tag
observedIDs = temp1[temp1$observedApplied == "Observed", c("tagID", "fishID")]
unmatchedTags = data.frame("tagID"=NA, "fishID"=NA)
# for each 'observed' ID see if there's a corresponding 'applied' (NOTE:there shouldn't be for USGS tagged fish)
ii = 1 # a simple counter
for(i in 1:nrow(observedIDs)){
  bob = temp1[temp1$tagID == observedIDs$tagID[i],]
  if(!("Applied" %in% bob$observedApplied)){
    # if there's no 'applied' day for this tagID...
    unmatchedTags[ii,] = c(bob$tagID, bob$fishID)
    ii = ii + 1
    print(paste0("tagID ", bob$tagID, " was never applied. ", bob$fishID ))
  }
}
# some of these are clearly other people's tags....drop them for now
others = unmatchedTags[c(1:3,6,10,12),]   
ours = unmatchedTags[c(4,5,7:9,11,13),]


# some of these "observed" tags probably were "applied" by us, but typo errors make them look different...
# use regex 'fuzzy matching'to find close matching
maybeMatches = list()
for (i in 1:nrow(ours)){
  zz = temp1[agrep(ours$tagID[i], temp1$tagID, ignore.case=TRUE, max.distance = 1),]
  
  maybeMatches[[i]] = data.frame(
    what = c("recorded ID", rep("possible match", nrow(zz))),
    tagID = c(ours$tagID[i], zz$tagID),
    fishID = c(ours$fishID[i], zz$fishID)
  )
}
# maybeMatches # THERE ARE SOME UN-RESOLVED ISSUES HERE. WE HAVE RECAPS WITH NO PRIOR INFO?
# ... Even after looking at the downloaded data from the handheld PIT tag reader, 
# ... I can't clearly link any of these, so for now, I'll just leave them unresolved.


# LET'S TAKE A LOOK AT RECAPS...################################################################
# There are two kinds: 1. recaps from tags we applied. 2. from tags other people applied, e.g. USGS
# Also, there are different fundamental kinds of fish capture/recapture histories:
# 1. capture with no tag applied
# 2. capture with tag1 applied
# 3. capture with tag1 applied, recapture with tag1 observed and tag2 applied
# 4. capture with tag1 applied, recapture with tag1 observed and tag2 applied, recapture with tag1/tag2 observed
# 5. capture with tag1 applied, recapture with tag1 observed and tag2 applied, recapture with tag2 observed
# 6. capture with someone else's tag1 observed (and our tag applied)
#

# The goal of the following code is to handle all types of histories and identify unique individuals
# I will create a list, where each element is a data.frame with all the biological data for one unique fish
# I will create a data.frame where each row is one unique fish, showing all fishIDs belonging to that fish

### some diagnostic checks
# how many fish are marked as recaps
b1 = biology[biology$recap == "yes", c("fishID", "releaseDati","comment")]
nrow(b1) # 114
b1$source = "biology"
# how many tags (unique fish) are listed as observed
m1 = temp1[temp1$observedApplied =="Observed",]  # temp1 is from tblMarks
   # in m1, fishIDs will appear multiple times because one fish has multiple tags
   # and each tag gets its own line in tblMarks/temp1...hence the 'unique' when creating m2
m2 = data.frame(fishID = unique(m1$fishID), source="marks")
nrow(m2) # 114
# compare fish from both lists...shouldn't be any "NA"
r = merge(b1[,c("fishID","source")], m2, by="fishID", all=TRUE)
# print a warning if m1 and m2 don't agree
if(nrow(b1) != nrow(m2)){ 
  print(paste0("The number of 'recap' fish in tblBiology doesn't match the number ",
    "of 'observed' fish in tblMarks"))
}


# To identify the full cap/recap history of one fish...in simple cases just look at a tagID that appears multiple
# times: applied/observed/observed. But when a fish is captured and tagged, then recaptured, identified by the 
# first tag and retagged with a new tag, then recaptured and identified only by the new tag, you have
# to do the following.....

# create an empty list, where each element holds a data.frame with all info for unique fishIDs
# ... non-recaps as well as recaps
uniqueFishID = list()

for (i in 1:nrow(biology)){ # i will count through all fishIDs in biology
  # create empty lists to hold all the fishIDs and tagIDs associated with one individual
  fullFishIDlist = c()
  fullTagIDlist = c()
  
  # pick one fishID from 'biology' 
  fullFishIDlist = biology$fishID[i]
  
  # if the fish was ever tagged or not
  if ( !(fullFishIDlist %in% temp1$fishID) ){ #  if this fish was never tagged...
    # ... then return one line from 'biology' and don't look any further
    uniqueFishID[[i]] = biology[i,]
    
  } else { # else it was tagged...
    # get all tagIDs for this one fishID
    fullTagIDlist = temp1$tagID[ (temp1$fishID %in% fullFishIDlist) ]
    
    
    continue = TRUE
    count = 1 # an emergency escape to avoid an infinite loop
    while ( continue ){ 
      # get all lines from marks corresponding to all the fish in fullFishIDlist OR tags in fullTagIDlist
      # ... this could be 1 or more lines
      df1 = temp1[ (temp1$fishID %in% fullFishIDlist) | (temp1$tagID %in% fullTagIDlist), ]
      # get (potentially) new fishIDs and tagIDs 
      newFishIDlist = unique(df1$fishID)
      newTagIDlist = unique(df1$tagID)
      # compare the new list with the full list
      # ...if fullFishIDlist includes everything in newFishIDlist AND fullTagIDlist includes everything in newTagIDlist
      # ...then we've found everything so exit the while-loop
      if ( all(newFishIDlist %in% fullFishIDlist) & all(newTagIDlist %in% fullTagIDlist) ){
        continue = FALSE
      }
      # an escape to escape in infinte loop...this should never happen, but...
      count = count + 1
      if(count == 10){ 
        continue = FALSE
        print(paste0("Infinite while-loop at i = ", i))
      }
      
      # update the full list to include the new list
      fullFishIDlist = newFishIDlist
      fullTagIDlist = newTagIDlist
      
  } # END while loop
  # exiting the while-loop means we've found all fishIDs and tagIDs associated with this unique fishID...
  # ...add the 'biology' data associated with fishIDs in fullFishIDlist to 'uniqueFishID'
    uniqueFishID[[i]] = biology[ (biology$fishID %in% fullFishIDlist)  ,]
  
  } # end if-else
} # END for i in 1:nrow(biology)

# at this point uniqueFishID has duplicates in the list elements...a uniqueFishID list entry is created for each fishID associated with a unique fish
# ... is it true that the element-index for uniqueFishID always equals one of the values in the uniqueFishID[[i]]$bID?
# ... in otherwords, when you look at biology', is bID sequential with no missing values
sum(biology$bID == 1:length(biology$bID)) == length(biology$bID)  # this should be TRUE
#
for (i in 1:length(uniqueFishID)){ # if this doesn't print anything, then bID is sequential
  if( !(i %in% uniqueFishID[[i]]$bID) ){ print("Boo") } 
}  

# use bID to identify duplicates
# ... a fish with more than one bID has duplicate catch histories located at the uniqueFish list elements
# ... corresponding to the non-first bIDs, e.g. uniqueFish[[157]]: all bIDs but 157...
duplicate = rep(FALSE, length(uniqueFishID))
for (i in 1:length(uniqueFishID)){
  # grab the non-first bID for a unique fish
  droppers = tail(uniqueFishID[[i]]$bID, -1)    
  duplicate[ droppers ] = TRUE
}
# drop all the duplicate catch histories
uniqueFish = uniqueFishID[ !duplicate ]
# add unique uID, for each individual
for (i in 1:length(uniqueFish)){
  uniqueFish[[i]]$uID = 1000 + i
}
# at this point uniqueFish includes all non-recap and recap fish...but no duplicate records
# ...it's currently a list, but I think a data.frame will be more useful...see below
# ...when I make it a data.frame 

# a check...does each fishID in 'bioloby' occur once, and only once, in 'uniqueFish'
# are there duplicates in 'biology'
z1 = biology$fishID 
sum(duplicated(z1)) # should be zero...no duplicates
# are there duplicates in 'uniqueFish'
z2 = ldply(uniqueFish, function(z){
  # i=58; z = uniqueFish[[i]]
  allbid = c( z$fishID, rep(NA, 7-length(z$fishID) ) ) 
  return(z = allbid)
})
z2 = as.character(unlist(z2))
z2 = z2 [ !is.na(z2) ]
sum(duplicated(z2)) # should be zero...no duplicates
# do all fishID in 'biology' still occur once and only once in 'uniqueFish'
sum( !(z1 %in% z2) ) # should be zero...all fishID in 'biology' occur in 'uniqueFish'
sum( !(z2 %in% z1) ) # should be zero...all fishID in 'uniqueFish' occur in 'biology'


# ... keep only list elements with recaptures (could be one or more linea, but 'recap' will include 
# ... at least one 'yes')
keepers = rep(FALSE, length(uniqueFish))
for (i in 1:length(uniqueFish)){
  if ("yes" %in% uniqueFish[[i]]$recap){ keepers[i] = TRUE }
}
# keep only recaps 
uniqueRecaps = uniqueFish[ keepers ]

# recreate recapHistory1: capture/recapture histories for all recaps
recapHistory1 = data.frame("first"=NA, "second"=NA, "third"=NA, "fourth"=NA, "fifth"=NA, "sixth"=NA, 
  "seventh"=NA, "recaps" = NA)
for (i in 1:length(uniqueRecaps)){
  fishIDs = uniqueRecaps[[i]]$fishID
  recapHistory1[i,] = c(fishIDs, rep(NA, ncol(recapHistory1) - length(fishIDs)) )
}



# make uniqueFish a data.frame instead of a list
# ... don't know why this doesn't work... rbindlist(uniqueFish)
# ... don't know why this doesn't work... ldply(uniqueFish, data.frame)
# uniqueFishList = uniqueFish    # this is just in case I ever want the list version
uniqueFish = do.call("rbind", uniqueFish)
uniqueFish = uniqueFish[ order(uniqueFish$fishID), ]

# save this unique ID for later then stick it onto 'biology'
uniqueID = subset(uniqueFish, select = c('fishID', 'uID'))



# look at recaps...first and subsequent catches...red for "recaps" with only one capture 
if(showPlot){
  plot(ymd(c("19980101","20190101")), c(1, nrow(recapHistory1)), xlab="Date", ylab="Unique Fish", type="n",
       bty="l")
  for (i in 1:nrow(recapHistory1)){
    wColor = ifelse( is.na(recapHistory1$second[i]), "red", "black")
    points(ymd( lapply(recapHistory1[i,], substr, start=1, stop=8) ), rep(i,ncol(recapHistory1)), type="b", pch=19, col=wColor)
  }
}
# again...but include sizes
### ...find weight for each fishID in recapHistory1
#recapHistory1[1,]

findWeight = function(fishID){
  if(is.na(fishID)){ return(NA)
  } else {
    return(biology[biology$fishID == fishID, ]$weight.kg)
  }  
}
findTL = function(fishID){
  if(is.na(fishID)){ return(NA)
  } else {
    return(biology[biology$fishID == fishID, ]$tl.mm  )
  }  
}

recapHistory1Weight = recapHistory1
recapHistory1TL = recapHistory1

for(i in 1:nrow(recapHistory1)){
  for(j in 1:ncol(recapHistory1)){
    recapHistory1TL[i,j] = findTL(recapHistory1[i,j])
  }
}
for(i in 1:nrow(recapHistory1)){
  for(j in 1:ncol(recapHistory1)){
    recapHistory1Weight[i,j] = findWeight(recapHistory1[i,j])
  }
}


if(showPlot){
  plot(ymd(c("19980101","20190101")), range(biology$tl.mm, na.rm=TRUE), xlab="Date", ylab="tl.mm", 
       type="n", bty="l")
  for (i in 1:nrow(recapHistory1)){
    wColor = ifelse(is.na(recapHistory1$second[i]), "red", "black")
    points(ymd(lapply(recapHistory1[i,], substr, start=1, stop=8)), 
           recapHistory1TL[i,], type="b", pch=19, col=wColor)
  }
  
  plot(ymd(c("19980101","20190101")), range(biology$weight.kg, na.rm=TRUE), xlab="Date", ylab="weight.kg", 
       type="n", bty="l")
  for (i in 1:nrow(recapHistory1)){
    wColor = ifelse(is.na(recapHistory1$second[i]), "red", "black")
    points(ymd(lapply(recapHistory1[i,], substr, start=1, stop=8)), 
           recapHistory1Weight[i,], type="b", pch=19, col=wColor)
  }
}

# look at days-at-large v. weight gain between first and last capture
for(i in 1:nrow(recapHistory1)){
  recapHistory1[i,]  
  recapHistory1TL[i,]
  recapHistory1Weight[i,]
  
}


### drop recaps that don't have any original tagging info...(i.e. there's only one fishID)
recapHistory2 = recapHistory1[ !is.na(recapHistory1[,"second"]), ]
### keep only recaps without original tagging info...
recapHistory3 = recapHistory1[ is.na(recapHistory1[,"second"]), ]


# AS ADDRESSED ABOVE...WHEN I DROP ROWS WITHOUT A FISHID IN "second" FROM recapHistory...do the same for temp1
# IF a fishID appears in this data.frame, then it's marked as 'Observed' in the Access database. 
# Some of these have only one date,
# meaning we only handled that fish once. 1. Some of these are fish first tagged by , e.g., USGS: we didn't 
# handle the 
# original tagging, we only observed it upon recapturing it. 2. Some of these are problem fish with mistakes 
# to address. These kind should be corrected in the MS Access database. Take a look...

# identify any recaptured fish that were not first caught by FWS...i.e. they were tagged by someone 
# else, like USGS
# Look at each fish individually...
#
# These were tagged by someone else...see 'biology$comment' and 'temp2$comments'...as of 2018 Sept 
# these are the 
# words indicating someone else tagged a fish: c("USGS", Genesee", "ESF")
# 20100811NRB05LAS15 'USGS'
# 20130417LNR03LAS01 'Genesee
# 20150716NRB05LAS01 'USGS'
# 20170530LNR03LAS01 'USGS'
# 20180523LNR08LAS01 'ESF'

# look at only 'observed' tags
temp5 = temp1[temp1$observedApplied == "Observed", ]

# identify fish first handled and tagged by someone else...look at the comments
# use these keywords to pick out fishIDs for fish tagged by someone else, from 'biology' and 'temp2' (from marks)
#...from 'biology' (b1)
which = grepl("USGS", b1$comment, ignore.case=TRUE) | grepl("Genesee", b1$comment, ignore.case=TRUE) | 
  grepl("ESF", b1$comment, ignore.case=TRUE) 
wFishID = b1[which, ]$fishID #
# ...from 'marks' (temp)
which = grepl("USGS", temp5$comments, ignore.case=TRUE) | grepl("Genesee", temp5$comments, ignore.case=TRUE) | 
  grepl("ESF", temp5$comments, ignore.case=TRUE) 
wFishID = unique(c(temp5[which, ]$fishID, wFishID))

# these are fish we know someone else tagged
#recapHistory1[recapHistory1$first %in% wFishID,]  
# these are fish we don't know who tagged them, maybe us, maybe someone else
#recapHistory1[!(recapHistory1$first %in% wFishID) & (is.na(recapHistory1$second)),]



# add an index and rearrange
temp1$mID = 1:nrow(temp1)
marks = subset(temp1, select = c(mID, fishID, observedApplied, tagType, tagLocation, tagID, comments))

# END marks ... for now, duplicates are left in until we decide what to do
############################################################################



###############################################################################
# ages  
###############################################################################
# these from Jonah Withers...Note that some of these are marked by Jonah as not useable.
temp1 = tblAges           
temp1 = select(temp1, FishID, TissueID, Age_estimate, Confidence, Comments, Use)
changeNames = data.frame(
  accessNames = names(temp1),
  rNames = c("fishID", "tissueID", "age", "confidence", "comments", "use")
)
names(temp1) <- changeNames$rNames[match(names(temp1), changeNames$accessNames)]
  
temp1$age = as.integer(temp1$age)


# drop any that aren't sturgeon ... also drops any without fishID
if(sturgeonOnly){ 
  temp1 = temp1[grepl("LAS", temp1$fishID),] 
  print(paste0("Dropped ", nrow(tblAges)-nrow(temp1), " non-sturgeon fish"))
}   

# drop any that Jonah marked as not useable...I NEED TO ASK HIM HOW HE FLAGED THESE
if(showPlot){
  plot(temp1$confidence, temp1$use, pch=19)
  
  plot(jitter(temp1$use,1) ~ temp1$confidence, pch=19) # GENERALLY? lower confidence is marked "0", don't use
  #temp1 = temp1[temp1$use == 1,]
}

# re-order
temp1 = temp1[order(temp1$fishID),]

# calculate ageClass from 'age' and capture year shown in 'fishID'
which = (!is.na(temp1$age))  # which fish have age estimates
temp1$yearClass[which] = 
  as.numeric(substr(temp1$fishID[which],1,4)) - temp1$age[which]
temp1$yearClass = as.factor(temp1$yearClass)

# look for duplicate fishID...if there are duplicates, look in old versions of this file for "fixing-code"
j1 = split(temp1, temp1$fishID)
(j2 = j1[lapply(j1, nrow) > 1]) # an empty list means no duplicates

# look at year class distrubution
if(showPlot){
  hist(as.numeric(as.character(temp1$yearClass)), 
    breaks = min(as.numeric(as.character(temp1$yearClass)), na.rm=TRUE):max(as.numeric(as.character(temp1$yearClass)), na.rm=TRUE))
}

if(showPlot){
  ggplot(temp1, aes( as.numeric(as.character(yearClass)) )) +
    geom_histogram(binwidth=1)
}

# add an index
temp1$aID = 1:nrow(temp1)
# condense and rearrange
temp1 = temp1[,c("aID","fishID","age","yearClass","confidence","use","comments")]
ages = temp1

# ages  
###############################################################################



###############################################################################
# sex
##############################################################################
# 
# These sex determinations are from tblSex are from biospy.  and blood hormone analysis.
# 
# Sex determinations in tblSex are from biospy (gonad sample sent to lab and definetively determined sex).
# In addition, we took blood plasma samples and did hormone (T and E2) analysis was completed. From these a statistical
# relationship between hormone levels and sex was fitted, making it possible to determine sex from homrone
# levels. We treat this determination as non-definitive. 
#
# Additionally in 'biology' from above (observations from the field data sheets), sometimes we directly observed
# milt or eggs. This observation is a definitive sex determination. We also sometimes guessed at sex from ultrasound
# images. This is non-definitive. If the field notes say we saw gametes, then take it as definitive. If the notes
# just say 'male' or 'female' with no explanation of how we know...this is non-definitive. 
# Further, in tblSex, the 'T' column is testosterone levels. 'E2' is estrogen levels. I think empty cells means
# the lab test was completed with the result that there were no detectable levels. 
# Generally: (High T + low E2 = mature male)      (High T + high E2 = mature female). But more factors go into the stats model.
# 
# Also, note that because two radio tags with the same frequency were put into different fish, they become problematic here.
# And since there es always another tag to indicate recaps, I'll simply drop all radio tag data in this section.
#
# Also, this tblSex doesn't include all the data we have. We have hormone blood samples taken which have not been analyzed.
# They need to be sent and evenually included.

temp1 = tblSex
changeNames = data.frame(
  accessNames = names(temp1),
  rNames = c("someID","plasma","biopsy","pitID","date", "dateN", 
  "tl.mm", "girth.mm", "weight.kg", "sex", "stage", "tSteroid", 
  "e2steroid", "comments")
)
names(temp1) <- changeNames$rNames[match(names(temp1), changeNames$accessNames)]

temp1 = temp1[order(temp1$date),]
row.names(temp1) = 1:nrow(temp1)
 
# drop an empty line 
temp1 = temp1[!is.na(temp1$pitID),]
            
# according to Dimitry, analysis was done for blank cells for 'T' and 'E2'. The hormone values were too low to detect.
# I'll write that in to clarify. These cells don't represent 'no data', they represent 'undetectable'
temp1$tSteroid [ is.na(temp1$tSteroid) ] = "undetectable"
temp1$e2steroid [ is.na(temp1$e2steroid) ] = "undetectable"

# check for duplicates  # some of these duplicates are from recaptures, others are simply duplicate samples
j1 = split(temp1, temp1$pitID)
j2 = j1[lapply(j1, nrow) > 1]
whichNames = names(j2)

# I WANT TO INCLUDE HORMONE DATA IN THE FINAL sTable AND OTHER STUFF, BUT HOW TO DO THAT WHEN THERE ARE TWO LINES FOR ONE FISH


# fixes
# 900118001500764 ...cap and recap... I'm only after sex determination...drop one
# temp1[which(temp1$pitID == whichNames[1]),]  
temp1 = temp1[-which(temp1$pitID == whichNames[1])[2],]   
# 985121023607766 ...cap and double recap... drop two   
# temp1[which(temp1$pitID == whichNames[2]),]
temp1 = temp1[-which(temp1$pitID == whichNames[2])[c(1,3)],]   
# 985121023625355 ...replicate. drop one
# temp1[which(temp1$pitID == whichNames[3]),]   
temp1 = temp1[-which(temp1$pitID == whichNames[3])[2],]   
# 985121023641435 ...replicate. drop one   
# temp1[which(temp1$pitID == whichNames[4]),]   
temp1 = temp1[-which(temp1$pitID == whichNames[4])[2],]   
# 985161000805246 ...cap and recap... I'm only after sex determination...drop one   
# temp1[which(temp1$pitID == whichNames[5]),]
temp1 = temp1[-which(temp1$pitID == whichNames[5])[2],]  
# check for duplicates
j1 = split(temp1, temp1$pitID)
(j2 = j1[lapply(j1, nrow) > 1]) # should be an empty list


# do a quick check...do tl.mm, girth.mm in temp1, match with data in biology
for(i in 1:nrow(temp1)){
  temp2 = temp1[temp1$pitID[i] == temp1$pitID, c("tl.mm", "girth.mm")]
   # use 'pitID' in 'marks' to find this fish in 'biology'
  cFish = marks[marks$tagID ==  temp1$pitID[i], "fishID"] # this will be one or more rows...
  cBiology = biology[biology$fishID %in% cFish, c("tl.mm", "girth.mm")] # this will be one or more rows...
  # do any of these match temp2?
  temp3 = FALSE
  for (j in 1:nrow(cBiology)){
    if(all(temp2 == cBiology[j, ], na.rm=TRUE)){ temp3=TRUE}  
  }
  if(!temp3){print(paste0("Size data in 'Sex' table don't match 'biological' table for i = ", i, ". "))}
}

# i = 91. Weights don't match for this one. The MS Access table, "2012_2013_Final" is wrong. For now, do nothing; we don't use it. 

# condense and rearrange
temp1 = subset(temp1, select = c(pitID, sex, stage, comments))

temp1$sex[(temp1$sex == "U")] = "unknown"
#temp1$sex[is.na(temp1$sex) | (temp1$sex == "U")] = "unknown"
temp1$sex[(temp1$sex == "Male") | (temp1$sex == "M")] = "male"
temp1$sex[(temp1$sex == "Female") | (temp1$sex == "F")] = "female"
#temp1$stage[is.na(temp1$stage)] = "unknown"

# At this point 'temp1' has data from MS Access database table "2012_2013_Final" which has biopsy and hormone data.
# This sex determination represents 'known', 'confident' sex determinations.
# There's only one row per PIT tag. For each of those pit tags, what does the 'biolgy table show?

# Look at 'temp1' and 'biology' to gather all the information we have about sex into one table()


# 'biologySex' is from 'biology$sex' = low confidence without further corroboration
# 'guessedSex' is from sex determination from any source: 
#  - biologySex, ultrasound, etc.
#  - notes in 'biology' with no supporting evidence, e.g. no 'gametes observed' = low confidence
# 'ultrasoundSex' is from 'biology$comments: what did the ultrasound show? = medium confidence.
# 'estimatedSex' is from hormone analysis and the descriminant function = medium confidence
# 'biospySex' is from biospy of gonads = high confidence
# 'sureSex' is our final sex determination
#
# More confident determinations, will not propogate downward. E.G. when we confidently know sex from observing eggs, that 
# determination will not appear in the 'biopsySex' column if a biopsy was inconclusive or not done.



columns = c("fishID", "fID2", "fID3","fID4", "fID5", "recapFishIDs", "recapSex",
  "biopsyDone", "biopsySex", "biopsyStage",  "hormoneT", "hormoneE2", "biopsyComments", 
  "biologySex", "biologyComments", 
  "guessedSex", "ultrasoundSex","estimatedSex", "sureSex")
sTable = data.frame(matrix(vector(), 0, length(columns),
  dimnames=list(c(), columns)),
  stringsAsFactors=F)
# because there's some duplication of radio tagIDs and because all recaps can be identified without radio tagIDs...just drop them
shortMarks = marks[marks$tagType != "Radio",]

for (i in 1:length(biology$fishID)){
  # for current fish...even if it's recaptured multiple times 
  #   i=961

  # reset
  cFish1 = cTags1 = cFish2 = cTags2 = NA
  
  # pick one fish. This should always be a real ID, not NA
  cFish1 = biology$fishID[i]
  
  # get all tagIDs for this/these fishIDs...this may be NA if there were no tags on this fish
  if( sum(shortMarks$fishID == cFish1) > 0 ) { 
    cTags1 = shortMarks$tagID[shortMarks$fishID == cFish1]
  } else {
    cTags1 = NA
  }
  
  continue = TRUE
  while (continue){ # while there are new fishIDs or new tagIDs then continue searching for more...
   
    # go back to 'marks' and see if any other fishIDs match any of cTags1...this remembers all fishIDs and tagIDs
    if( any(!is.na(cTags1)) ) { # if there are any tags at all then get all the fishIDs for all cTags1
      cFish2 = unique(shortMarks$fishID[shortMarks$tagID %in% cTags1]) 
    } else { #...there must be no tags...so don't add any fishIDs to the list
      cFish2 = cFish1
    }
    # for al lthe fishIDs in cFish2, find all tagIDs 
    if( sum(shortMarks$fishID %in% cFish2) > 0 ){ # if all the fishIDs in cFish2 have some tags associated with them then...
      cTags2 = unique(shortMarks$tagID[shortMarks$fishID %in% cFish2])
    } else { # ...else there are no tags on any cFish2, then 
      cTags2 = cTags1
    }
    
    # check to see if we have all the fishIDs and tagIDs for this fish...
    # ...if we didn't any new fish or tags to cFish2 or cTags2, then stop looking
    if( all(cFish2 %in% cFish1) & all(cTags2 %in% cTags1) ) { continue = FALSE }
    
    # make cFish1 and cFish2 be complete lists
    cFish1 = cFish2
    cTags1 = cTags2
    
  } # end while-loop
  # when you exit this while-loop then you have all the fishIDs and tagIDs for this individual fish...
  # ... the following code only works for fish with 5 or fewer fishIDs...check
  if(length(cFish1) > 5){print(paste("BAD: More than 5 fishIDs in cFish1 for i = ", i))}
  
  # force current fishID (biology$fishID[i]) to be first in cFish1
  cFish1 = c(biology$fishID[i], cFish1[!grepl(biology$fishID[i], cFish1)]) 
  
  # ...now save all the fishIs for this individual fish into sTable
  sTable[i, 1:length(cFish1)] = cFish1
  
  # gather other fishIDs...if this fish is a recap, what other fishIDs are associated
  if(length(cFish1) > 1){ 
    sTable$recapFishIDs[i] = paste( cFish1[!grepl(biology$fishID[i], cFish1)], collapse=". ") 
  } else { 
    sTable$recapFishIDs[i] = NA
  }
  
  # add a flag indicating if the current fishID is a recap (1) or the original capture (0)
  currentFishID = biology$fishID[i]   # compare with cFish1
  # compare the capture dati of all cFish1
  currentFishDati = biology$releaseDati[i]
  allFishDati = biology$releaseDati[ biology$fishID %in% cFish1 ]
  # is currentFishDati earlier than all other allFishDati? Yes---then this is the original capture
  sTable$recapSex[i] = ifelse( currentFishDati == min(allFishDati), "no", "yes" )
  
  
  ### sex determination from 'biology$sex' and 'biology$comments ### this is a low confidence determinations so far ###################
  sTable$biologySex[i] = biology$sex[i]
  # store the sex determinations and comments for this fish and all recaps of this fish into the comments. Then manually look through these, for now. 
  sTable$biologyComments[i] = paste0(
    "(All determinations: ", 
    paste(biology$sex[biology$fishID %in% cFish1], collapse=". "),
    ".) ",
    paste(biology$comment[biology$fishID %in% cFish1], collapse=". ")
  )   
  
  # look at all captures of this fish for sex determinations and save as 'guessedSex'...check for conflicts
  # ...look in 'biology$sex' and 'biology$comment'
  z1 = z2 = z3 = NA
  z1 = grepl(" male", sTable$biologyComments[i], ignore.case=TRUE) # THE SPACE BEFORE 'male' IS NECESSARY
  z2 = grepl("female", sTable$biologyComments[i], ignore.case=TRUE)
   
  # ...check for conflicts
  if (z1 & z2) {print(paste("BAD: sex determination conflict for i = ", i))}
  # ...save sex determination
  if (z1 & z2)   { z3 = "male/female"
  } else if (z1) { z3 = "male"
  } else if (z2) { z3 = "female"
  } else         { z3 = "unknown" }
  sTable$guessedSex[i] = z3
  
  ### sex determination from biopsies (temp1) ### this is a high confidence determinations ###################
  #
  # ideally there's only one PIT tag for this individual...check...there may be 0, 1 or more...
  listAllPITS = unique(shortMarks$tagID[ (shortMarks$tagID %in% cTags1) & (shortMarks$tagType == "PIT") ])
  ### if there aren't any PIT tags, then there's no biopsy or homrone data
  if( length(listAllPITS) > 0 ){ # if there are pit tags then do this...

    # take the list of all PIT tags...could be 0, 1, 2 or 3 rows and get sex determination from 'temp1', if present
    z1 = z2 = z3 = NA
    z1 = temp1$sex[temp1$pitID %in% listAllPITS]
    z2 = NA
    if (length(z1) == 0)                              { z2 = "unknown"
    } else if (("male" %in% z1) & ("female" %in% z1)) { z2 = "BAD"
    } else if ( "male" %in% z1)                       { z2 = "male"
    } else if ( "female" %in% z1)                     { z2 = "female"
    } else                                            { z2 = "unknown" }
  
    # also save stage and comments from biopsy/hormone data
    if(any(temp1$pitID %in% listAllPITS)){
      sTable$biopsyDone[i] = "yes"
    } else {
      sTable$biopsyDone[i] = "no"
    }
    sTable$biopsyStage[i] = paste( temp1[temp1$pitID %in% listAllPITS, "stage"], collapse = ". ")
    sTable$biopsyStage[ is.na(sTable$biopsyStage) | (sTable$biopsyStage == "") | 
      (sTable$biopsyStage == "NA")] = "unknown"
    sTable$biopsyComments[i] = paste( temp1[temp1$pitID %in% listAllPITS, "comments"], collapse = ". ") 
    
  } else { # if there aren't any PIT tags for this fish then do this
    z2 = "unknown"   
  }  # end if(there are PIT tags)
    
  # store sex determination from biopsy
  sTable$biopsySex[i] = z2 
  
  # add biopsy sex determinations to 'guessedSex'...check for conflicts
  z1 = z2 = z3 = NA
  z1 = sTable$guessedSex[i]
  z2 = sTable$biopsySex[i]
  if( z1 == z2 )                                              { z3 = z1
  } else if( any(c(z1,z2)=="male") & any(c(z1,z2)=="female") ){ z3 = z2; print(paste0("Conflict with biopsySex. i=", i) )
  } else                                                      { z3 = c(z1, z2)[ c(z1,z2) != "unknown" ] }
  sTable$guessedSex[i] = z3
  
  # in the future we will have 'estimated sex' for all fish with blood hormone analysis...for now: nothing
  sTable$estimatedSex[i] = "unknown"
   
  # find sex determinations from ultrasound
  z1 = z2 = z3 = NA
  usWords = c("ultrasound", "ultra sound", " us ")
  if( any(sapply(usWords, grepl, x=sTable$biologyComments[i], ignore.case=TRUE)) ) {
    z1 = sTable$guessedSex[i]
  } else {
    z1 = "unknown"
  }
  sTable$ultrasoundSex[i] = z1
  
  # add biopsy sex determinations to 'guessedSex'...check for conflicts
  z1 = z2 = z3 = NA
  z1 = sTable$guessedSex[i]
  z2 = sTable$ultrasoundSex[i]
  if( z1 == z2 )                                              { z3 = z1
  } else if( any(c(z1,z2)=="male") & any(c(z1,z2)=="female") ){ z3 = z1; print(paste0("Conflict with ultrasoundSex. i=", i) )
  } else                                                      { z3 = c(z1, z2)[ c(z1,z2) != "unknown" ] }
  sTable$guessedSex[i] = z3
  
  # use 'biopsySex' and confirmed 'biology' sex determinations to get 'sureSex'...with conflict checking
  z1 = z2 = z3 = NA
  # ...is there a biopsySex
  z1 = sTable$biopsySex[i]  
  
  # ...is there a confirmed 'biology' sex (look at the comments for key words)
  # ...read the 'biology' comments...are there notes clarifying 'biology$sex', e.g. ("US" for ultrasound. "gametes", "sperm")
  # ...key words instilling greater certainty: (mature, gamete, egg, milt, gravid, ripe, express, gonad, observe, stage 5, testes) 
  ######################################### THE SPACE BEFORE ' mature' IS NECESSARY TO AVOID 'immature'
  yesWords = c(" mature", "gamete", "egg", "milt", "gravid", "ripe", "express", "gonad", "observe", "obs", "stage", 
               "teste", "biopsy")
  # Also, if we have a sex determination and we saw inside the fish when inplanting a radio or acoustic tag, then the sex
  # determination is reliable.
  internalTagPresent = nrow(marks [ (marks$fishID %in% cFish1) & 
                (marks$tagType %in% c("Radio", "Acoustic")) & (marks$tagLocation %in% c("Ventral")),   ]) > 0
  # if any of these words appear in the comments, then put high confidence in 'guessedSex'
  if( (any(sapply(yesWords, grepl, x=sTable$biologyComments[i], ignore.case=TRUE))) | internalTagPresent ) {
    z2 = sTable$guessedSex[i]
  } else {
    z2 = "unknown"
  }
  
  # compare 'biopsySex' (z1) and 'biology' (z2) sex determinations...conflict checking     
  if( z1 == z2 ){ # if they're the same, take one
    z3 = z1
  } else if( any(c(z1,z2)=="male") & any(c(z1,z2)=="female") ){ z3 = "BAD: biology"
  } else                                                      { z3 = c(z1, z2)[ c(z1,z2) != "unknown" ] }
  # save the 'sureSex'    
  sTable$sureSex[i] = z3
  
  # do 'ultrasoundSex' and 'sureSex' ever conflict?
  z1 = z2 = z3 = NA
  z1 = sTable$ultrasoundSex
  z2 = sTable$sureSex
  if( any(c(z1,z2)=="male") & any(c(z1,z2)=="female") ){ # if they're conflicting...BAD
    z3 = "BAD: ultrasound"
  } 

  # print out for debugging
  #print(paste0("i = ", i, ". fishID = ", biology$fishID[i], ". recaps = ", length(cFish1)-1, ". TagIDs = ", 
  #  paste(cTags1, collapse=", "), ". Number of PIT tags = ", length(listAllPITS), "."))
} # end for i-loop 


# confirm that 'guessedSex' includes all possible sex determinations
test1 = subset(sTable, select = c(guessedSex, biopsySex, biologySex, ultrasoundSex, sureSex))
conflictChecking = function(x){ # x = test1[14,]   ... a single row of test1
  answer = NA
  if(any(x == "male") & any(x == "female")) { answer = TRUE
  } else { answer = FALSE }
  return(answer)
}
test1$conflict = apply(test1, 1, conflictChecking)
# any conflicts???
test1[ test1$conflict, ] # there should be zero rows

test2 = subset(sTable, select = c(biopsySex, biologySex, ultrasoundSex, sureSex))
checkGuessedSex = function(x){ # x = test1[14,]   ... a single row of test1
  answer = NA
  if( any(x == "male") ) { answer = "male" 
  } else if( any(x == "female") ) { answer = "female" 
  } else { answer = "unknown" }
  
  return( answer )
}

test2$anySex = apply(test2, 1, checkGuessedSex)
# does this agree with 'guessedSex'
test2$guessedSex = test1$guessedSex
test2$check = ifelse( (test2$anySex != "unknown") & (test2$guessedSex == "unknown"), "bad", "good" ) 
test2[ test2$check == "bad", ] # there should be 0 rows



# make some columns to be as.factors
makeFactors = c("biopsyDone", "biopsySex", "biopsyStage", "biologySex", "guessedSex", 
  "ultrasoundSex", "estimatedSex", "sureSex")
sTable[makeFactors] = lapply(sTable[makeFactors], as.factor)

# add an index
sTable$sID = 1:nrow(sTable)
temp2 = subset(sTable, select = c(sID, fishID, recapFishIDs, recapSex, biopsySex, biopsyStage, biopsyComments, 
  biologySex, guessedSex, ultrasoundSex, estimatedSex, sureSex))
sex = temp2

# sex
##############################################################################


# str(ages)
# str(marks)
# str(biology)
# str(collection)
# str(sex)  
    




##############################################################################
##############################################################################
# IMPORTANT THINGS TO REMEMBER
#
# 1. Any fish that was recaptured has more than one FishID. FishIDs don't identify
#    unique individuals.



##############################################################################
# merge fish-specific and collection-specific and tag-specific data.frames
##############################################################################

# attach uID to biology
m0 = merge(biology, uniqueID, by='fishID', all=TRUE)
# check that 'biology' is still the right length
nrow(biology) == nrow(m0)  # this should be TRUE

# combine all fish stuff into one data.frame to do some exploration
m1 = merge(m0, ages, by="fishID", all=TRUE)   # looks good   
# are they any fish with ages and no other data
# m1[ is.na(m1$bID), ] # we don't have any records matching these 6 fish...drop them
m1 = m1[ !is.na(m1$bID), ]
# summary(m1)

# don't put tagIDs in here, it's too much of a pain to add this detail
#m2 = merge(m1, marks, by="fishID", all=TRUE) # looks good
m2 = m1

# add sex data...but drop the 'recap' marker
m3 = merge(m2, sex, by="fishID", all=TRUE)
# do all the pitID found in 'sex' occur in m2
length(unique(sex$fishID));      length(sex$fishID);   # if they're equal then no duplicates. good.
sum(!(sex$fishID %in% m3$fishID))  # if this == 0, then all sex$fishID occur in m3. That's good.
# compare 'recap' and 'recapSex'. They should agree always. WHY DON'T THEY? ZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZYZY
sum(m3$recap != m3$recapSex)   # this should = 0
bob = m3[m3$recap != m3$recapSex, ]   

# re-sort by fishID and rearrange
m3 = m3[order(m3$fishID),]
m3$mID = 1:nrow(m3)  
m4 = m3
# summary(m4)


m5 = subset(m4, select=c(uID, 
  #mID, bID, aID, sID,
  fishID, recapFishIDs, recap, recapSex, releaseDati, tl.mm, girth.mm, weight.kg,
  age, yearClass, collectionYear, biologySex, biopsySex, biopsyStage, 
  guessedSex, ultrasoundSex, estimatedSex, sureSex, sureSexAccess, comment, comments, biopsyComments)
)

# look for correct ranges, etc, for NAs in bad places
# summary(m5)
sum( m5$sureSex != m5$sureSexAccess)  # should be 0

# str(ages)
# str(marks)
# str(biology)
# str(collection)
# str(sex)
# str(m5)



# # looks
# pairs(m5[,c("tl.mm","girth.mm","weight.kg","age")])
# 
# # age distribution
# ggplot(m5, aes(x=age, fill=sureSex )) +  
#   geom_histogram(binwidth=1) 
# 
# ggplot(m5, aes(x=age, fill=guessedSex )) +  
#   geom_histogram(binwidth=1) 
# 
# # age distribution over time
# ggplot(m5, aes(x=age, fill=sureSex )) +  
#   geom_histogram(binwidth=1) +
#   facet_wrap(~collectionYear)
# 
# 
# # year class strength
# ggplot(m5, aes(x=yearClass, fill=sureSex )) +  
#   geom_histogram(binwidth=1) 
#   
# # number caught each year
# ggplot(m5, aes(x=collectionYear, fill=sureSex )) +  
#   geom_bar() 
#   
# # number in each reproductive stage
# # how many fish in each sex collected each year
# ggplot(m5, aes(x=biopsySex, y=biopsyStage )) +  
#   geom_jitter(width=0.2, height=0.2) 
#    
# # length - weight by collection year
# ggplot(m5, aes(x=tl.mm, y=weight.kg )) +  
#   geom_smooth() +
#   geom_point() +
#   facet_wrap(~collectionYear)
#   
# ggplot(m5, aes(x=tl.mm, y=weight.kg, colour=collectionYear )) +  
#   geom_text(aes(label=fishID))
#   
# # length - weight by age
# ggplot(m5, aes(x=tl.mm, y=weight.kg, colour=age )) +  
#   geom_point() +
#   scale_color_continuous(name="Age", low = "blue", high = "red") 
#   
# # age length 
# ggplot(m5, aes(x=age, y=tl.mm )) +  
#   geom_point() +
# #  geom_text(aes(label=fishID))
#   geom_smooth(color="black")
# 
# # age length by sex
# ggplot(m5, aes(x=age, y=tl.mm, colour=sureSex )) +  
#   geom_point() +
#   geom_smooth(aes(color = sureSex ))  + facet_wrap(~sureSex) 
# 
# # weight trends over time by sex ?????????????????????????????????
# ggplot(m5, aes(x=collectionYear, y=weight.kg, fill=sureSex )) +  
#   geom_violin(alpha=0.5) +
#   facet_wrap(~sureSex)
# 
# ggplot(m5, aes(x=collectionYear, y=weight.kg )) +  
#   geom_violin(alpha=0.5) 

# double check data types



# END merge fish-specific and collection-specific and tag-specific data.frames
##############################################################################





##############################################################################
# list things to be returned when this script is sources from another file
##############################################################################


# Here's what I want to pass on to other scripts
collectionData = collection 
marksData = marks
biologicalData = m5 # since this whole file is intended to be called from another script, 
                    # biologicalData will be presented to that file.
recapData = recapHistory1  # a list of just fishIDs showing capture/recaputre histories





# write biologicalData as *.csv to move sex determination data into MS Access
#write.table(biologicalData, file = "LS biologicalData2.csv", sep=",", quote=F, row.names=F)        


# when this script is called from other places, I want to forget most stuff
keepers = c("biologicalData", "collectionData", "marksData", "recapData")
keepers = c(keepers, pre_existing)
everything = ls()
trash = everything[!(everything %in% keepers)]

rm(list=trash)      
ls()

# END list things to be returned when this script is sources from another file
##############################################################################


    