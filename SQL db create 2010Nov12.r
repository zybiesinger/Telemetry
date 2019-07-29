# Once I've created databases in MySQL itself I can get R to create and
# fill tables.
#  This file is intended to be used in conjunction with 'fill SQL db.r'  It 


###############################################################################
###############################################################################
###############################################################################
###############################################################################
### This file is intended to get the SDL, ALPS, and ADCP data into a database
### using RMySQL

library(RMySQL)
library(chron)

###############################################################################
# CREATE DATABASE AND TABLES
###############################################################################
###############################################################################

# create connections to db

dbconHB2007 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbHB2007")

dbconHB2008 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbHB2008")

dbconHB1 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbHB1")
dbconHB2 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbHB2")
dbconHB3 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbHB3")
dbconSB1 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbSB1")
dbconSB2 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbSB2")
dbconSB3 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbSB3")
dbconSB4 = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbSB4")


dbList = list(dbconHB2007, dbconHB2008, dbconHB1, dbconHB2, dbconHB3, dbconSB1, 
  dbconSB2, dbconSB3, dbconSB4)
dbListNames = c("dbconHB2007", "dbconHB2008", "dbconHB1", "dbconHB2", "dbconHB3", 
  "dbconSB1", "dbconSB2", "dbconSB3","dbconSB4")

### or ###    
dbList=list(dbconHB2008) 
dbListNames = c("dbconHB2008")


# list all open connections, and try to close them all
dbListConnections(MySQL())

# disconnect all connections
for ( i in length(dbListConnections(MySQL())):1){ 
  dbDisconnect(dbListConnections(MySQL())[[i]])
}

# or disconnect them all by name
for (i in 1:length(dbList)){
  dbDisconnect(dbList[[i]])
}




###########################################################################
# Create tables in multiple databases for all deployments
# Tables for: 
#  1. raw SDL data (tagID and arrival time for each SDL)
#  2. toa data 
#  3. ALPS position output
#  4. misc lines (so far these are footers from raw SDL files
###########################################################################
# 
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER
# THE FOLLOWING LINES PURGE DATABASES AND START OVER

for (i in 1:length(dbList)){ # i loops through all databases
  
  # settings that change for each deployment
  for (j in 1:length(md)){ # i loops through all deployments
    if (tolower(substr(dbListNames[i],6,20)) == md[[j]]$deployment){
      dbSentinelNames = md[[j]]$sentinelNames 
      dbBeaconNames = md[[j]]$beaconNames 
      dbFishNames = md[[j]]$fishNames
      deployment = md[[j]]$deployment 
    }  
  }
  
  ##############################################################################
  # Create tables for raw SDL output 
  #(e.g. from text version of *.bin files: SN265041_17jan08.txt)
  # columns = (date, time, fraction, power, port #, tag ID, type, raw sensor, GPS sync)
  # Sensor tags will have values in columns 7 and 8, ID only tags won't 
  
  # create table names
  sdlTableNames = paste("sdl", 41:45, sep="")
  miscTableName = "miscLines" 
  
  # create empty sdlTables in cDB
  for (j in 1:length(sdlTableNames)){ 
    # if the table already exists, delete it.
    dbphrase = paste("DROP TABLE IF EXISTS", sdlTableNames[j], ";", sep=" ")
    res = dbSendQuery(dbList[[i]], dbphrase)
    # now create the table fresh
    dbphrase = paste(
      "CREATE TABLE", sdlTableNames[j],
      "(",
      "pkey int AUTO_INCREMENT,",
      "utime int, INDEX (utime),", # utime GMT
      "datiG datetime,", # dati in GMT
      "datiL datetime,", # dati in EST / EDT)
      "fraction smallint,",
      "power smallint,",
      "port tinyint,",
      "tagID int, INDEX (tagID),",
      "type tinyint,",
      "rawSensor tinyint,",
      "gpsSync char(1),",
      "PRIMARY KEY (pkey)",
      ");"
    )
    res = dbSendQuery(dbList[[i]], dbphrase) 
  } # end j loop over sdl output tables
      
  ############################################################################# 
  # create table names for toa and ALPS position solutions using the correct tagIDs
  #
  # Sentinel tag
  temp1S=c() 
  temp2S=c() 
  for (j in 1:length(dbSentinelNames)){ # for each sentinel tag...
    # ...make a toa table name for each sentinel and all sdls
    temp1S[j] = paste("toa", dbSentinelNames[j], sep="")
    # ... make an ALPS position solution table
    temp2S[j] = paste("pos", dbSentinelNames[j], sep="")
  }                                           
  # Beacon tags
  temp1B=c()
  temp2B=c()
  for (j in 1:length(dbBeaconNames)){
    # ...make a toa table for each beacon and all sdls
    temp1B[j] = paste("toa", dbBeaconNames[j], sep="")
    # ... make an ALPS position solution table
    temp2B[j] = paste("pos", dbBeaconNames[j], sep="")
  }
  # Fish tags
  temp1F=c()
  temp2F=c()
  for (j in 1:length(dbFishNames)){
    # ...make a toa table for each sdl
    temp1F[j] = paste("toa", dbFishNames[j],sep="")
    # ... make an ALPS position solution table
    temp2F[j] = paste("pos", dbFishNames[j], sep="")
  }
  # join the table names into lists
  toaTableNames = c(temp1S, temp1B, temp1F)
  sdlTableNames = c(temp2S, temp2B, temp2F)
  
  # because the hb2008 deployment was a pain...do all possible tag names 
  if (deployment == "hb2008"){
    toaTableNames = paste("toac", 1:130, sep="")
  }
  
  # create tables for toa files.
  for (j in 1:length(toaTableNames)){
    # if the table already exists, delete it.
    dbphrase = paste("DROP TABLE IF EXISTS", toaTableNames[j], ";", sep=" ")
    res = dbSendQuery(dbList[[i]], dbphrase)
    # now create the fresh table 
    dbphrase = paste(
    "CREATE TABLE", toaTableNames[j],
      "(",
      "pkey int AUTO_INCREMENT,",
      "utime int, INDEX (utime),", # utime GMT
      "datiG datetime,", # dati in GMT
      "datiL datetime,", # dati in EST / EDT)
      "fraction smallint,",
      "power smallint,",
      "sType tinyint,",
      "sValue tinyint,",
      "sdlNumber tinyint, INDEX (sdlNumber),",
      "PRIMARY KEY (pkey)",
      ");"
    )
    res = dbSendQuery(dbList[[i]], dbphrase) 
  }
  
  # create tables for ALPS output.  
  for (j in 1:length(sdlTableNames)){
    # if the table already exists, delete it.
    dbphrase = paste("DROP TABLE IF EXISTS", sdlTableNames[j], ";", sep=" ")
    res = dbSendQuery(dbList[[i]], dbphrase)
    # now create the fresh table 
    dbphrase = paste(
    "CREATE TABLE", sdlTableNames[j],
      "(",
      "pkey int AUTO_INCREMENT,",
      "utime int, INDEX (utime),", # utime GMT
      "datiG datetime,", # dati in GMT
      "datiL datetime,", # dati in EST / EDT)
      "easting double,",
      "northing double,",
      "pressure double,",
      "cn double,",
      "rn double,",
      "dop double,",
      "hid smallint,",
      "count tinyint,",
      "unknown1 double,",
      "unknown2 double,",
      "PRIMARY KEY (pkey)",
      ");"
    )
    res = dbSendQuery(dbList[[i]], dbphrase) 
  }
  ##############################################################################
  # create a table for miscellaneous I don't know how to handle
  # if the table already exists, delete it.
  dbphrase = paste("DROP TABLE IF EXISTS", miscTableName, ";", sep=" ")
  res = dbSendQuery(dbList[[i]], dbphrase)
  # now create the fresh table 
  dbphrase = paste(
    "CREATE TABLE", miscTableName,
    "(miscLine varchar(255));"
  )
  res = dbSendQuery(dbList[[i]], dbphrase)
  
} # end i loop over all databases
###########################################################################
# END Create tables in multiple databases
###########################################################################

# now disconnect from all databases, if you want
for (i in 1:length(dbList)){
  dbDisconnect(dbList[[i]])
}







BELOW HERE ISN'T WORKING BELOW HERE ISN'T WORKING BELOW HERE ISN'T WORKING 
BELOW HERE ISN'T WORKING BELOW HERE ISN'T WORKING BELOW HERE ISN'T WORKING 


# pick one database to use
dbcon = dbconif43

###########################################################################
# Create tables for holding all fish positions.
# These tables are intended for use in playing animations of recorded and
# interpolated fish positions over time.  The tables will also hold ADCP 
# data.  These tables will be filled by ??? *.r ???
###########################################################################
for (i in 1:length(dbList)){
  
  # settings that change for each deployment
  for (j in 1:length(md)){ # i loops through all deployments
    if (tolower(substr(dbListNames[i],6,9)) == md[[j]]$deployment){
      sentinelNames = md[[j]]$sentinelNames 
      beaconNames = md[[j]]$beaconNames 
      fishNames = md[[j]]$fishNames 
    }  
  }
   
  # because each deployment has different numbers of fish...
  # Create column names matching the right number of fish 
  tempNames1 = c("01","02","03","04","05","06","07","08","09","10","11",
    "12","13","14","15","16","17","18","19","20")
  tempNames2 = c()
  for (j in 1:length(fishNames)){
    tempNames2=c(tempNames2,paste("f",tempNames1[j],c("x","y","z","i","s"),sep=""))
    # 'i'nterp -> 'F'ALSE if this is a measured point, 'T'RUE if it's an 'i'nterpolated point
    # 's'tatus -> 'F'ALSE if a 'utime' is before the earliest measured point or after the last, 'T'RUE otherwise 
  }
  tempNames3 = c("utime",tempNames2)  # later, add columns for ADCP data
  
  # create tables that have a column for utime, and 5 columns for each fish
  # create a matrix which will let me automatically set names, then convert to 
  # data.frame which will inherit column names
   
  tempdf1 = as.data.frame(matrix(ncol=(1+5*length(fishNames)),nrow=1,
    dimnames=list(NULL,tempNames3)))
  
  # because I don't know how to create a data.frame of the right length...
  # I'll make a big data.frame and delete the right number of columns
  tempdf1 = data.frame(
    "utime"=as.numeric(7777777),
    "t01x"=as.numeric(NA), "t01y"=as.numeric(NA), 
      "t01z"=as.numeric(NA), "t01i"=as.numeric(NA), "t01s"=as.numeric(NA),
    "t02x"=as.numeric(NA), "t02y"=as.numeric(NA), 
      "t02z"=as.numeric(NA), "t02i"=as.numeric(NA), "t02s"=as.numeric(NA),
    "t03x"=as.numeric(NA), "t03y"=as.numeric(NA), 
      "t03z"=as.numeric(NA), "t03i"=as.numeric(NA), "t03s"=as.numeric(NA),
    "t04x"=as.numeric(NA), "t04y"=as.numeric(NA), 
      "t04z"=as.numeric(NA), "t04i"=as.numeric(NA), "t04s"=as.numeric(NA),
    "t05x"=as.numeric(NA), "t05y"=as.numeric(NA), 
      "t05z"=as.numeric(NA), "t05i"=as.numeric(NA), "t05s"=as.numeric(NA),
    "t06x"=as.numeric(NA), "t06y"=as.numeric(NA), 
      "t06z"=as.numeric(NA), "t06i"=as.numeric(NA), "t06s"=as.numeric(NA),
    "t07x"=as.numeric(NA), "t07y"=as.numeric(NA), 
      "t07z"=as.numeric(NA), "t07i"=as.numeric(NA), "t07s"=as.numeric(NA),
    "t08x"=as.numeric(NA), "t08y"=as.numeric(NA), 
      "t08z"=as.numeric(NA), "t08i"=as.numeric(NA), "t08s"=as.numeric(NA),
    "t09x"=as.numeric(NA), "t09y"=as.numeric(NA), 
      "t09z"=as.numeric(NA), "t09i"=as.numeric(NA), "t09s"=as.numeric(NA),
    "t10x"=as.numeric(NA), "t10y"=as.numeric(NA), 
      "t10z"=as.numeric(NA), "t10i"=as.numeric(NA), "t10s"=as.numeric(NA),
    "t11x"=as.numeric(NA), "t11y"=as.numeric(NA), 
      "t11z"=as.numeric(NA), "t11i"=as.numeric(NA), "t11s"=as.numeric(NA),
    "t12x"=as.numeric(NA), "t12y"=as.numeric(NA), 
      "t12z"=as.numeric(NA), "t12i"=as.numeric(NA), "t12s"=as.numeric(NA),
    "t13x"=as.numeric(NA), "t13y"=as.numeric(NA), 
      "t13z"=as.numeric(NA), "t13i"=as.numeric(NA), "t13s"=as.numeric(NA),
    "t14x"=as.numeric(NA), "t14y"=as.numeric(NA), 
      "t14z"=as.numeric(NA), "t14i"=as.numeric(NA), "t14s"=as.numeric(NA),
    "t15x"=as.numeric(NA), "t15y"=as.numeric(NA), 
      "t15z"=as.numeric(NA), "t15i"=as.numeric(NA), "t15s"=as.numeric(NA),
    "t16x"=as.numeric(NA), "t16y"=as.numeric(NA), 
      "t16z"=as.numeric(NA), "t16i"=as.numeric(NA), "t16s"=as.numeric(NA),
    "t17x"=as.numeric(NA), "t17y"=as.numeric(NA), 
      "t17z"=as.numeric(NA), "t17i"=as.numeric(NA), "t17s"=as.numeric(NA),
    "t18x"=as.numeric(NA), "t18y"=as.numeric(NA), 
      "t18z"=as.numeric(NA), "t18i"=as.numeric(NA), "t18s"=as.numeric(NA),
    "t19x"=as.numeric(NA), "t19y"=as.numeric(NA), 
      "t19z"=as.numeric(NA), "t19i"=as.numeric(NA), "t19s"=as.numeric(NA),
    "t20x"=as.numeric(NA), "t20y"=as.numeric(NA), 
      "t20z"=as.numeric(NA), "t20i"=as.numeric(NA), "t20s"=as.numeric(NA),
    "t21x"=as.numeric(NA), "t21y"=as.numeric(NA), 
      "t21z"=as.numeric(NA), "t21i"=as.numeric(NA), "t21s"=as.numeric(NA),
    "t22x"=as.numeric(NA), "t22y"=as.numeric(NA), 
      "t22z"=as.numeric(NA), "t22i"=as.numeric(NA), "t22s"=as.numeric(NA),
    "t23x"=as.numeric(NA), "t23y"=as.numeric(NA), 
      "t23z"=as.numeric(NA), "t23i"=as.numeric(NA), "t23s"=as.numeric(NA),
    "t24x"=as.numeric(NA), "t24y"=as.numeric(NA), 
      "t24z"=as.numeric(NA), "t24i"=as.numeric(NA), "t24s"=as.numeric(NA),
    "t25x"=as.numeric(NA), "t25y"=as.numeric(NA), 
      "t25z"=as.numeric(NA), "t25i"=as.numeric(NA), "t25s"=as.numeric(NA)
  ) 
  # now delete the right number of columns
  temp1 = length(c(sentinelNames, beaconNames, fishNames))
  rightLength = 1 + temp1 * 5 
  tempdf2 = tempdf1[,1:rightLength]
  
  
  
#  tempdf1test = data.frame(
#    "utime"=NA,
#    "t01x"=NA, "t01y"=NA, "t01z"=NA, "t01r"=NA, "t01s"=NA
#  ) 
  
  # now create the table
  dbWriteTable(dbList[[i]], "fishPaths", tempdf2, overwrite=TRUE, row.Names=0)
#  dbWriteTable(dbList[[i]], "fishPathsTest", tempdf1test, overwrite=TRUE, row.names=0)

  # so far the tables have only one row, they'll get the right number of rows 
  # from somewhere else...I don't know where yet.  'SQL db fill.r' I suppose
  

} # end i loop over all databases







###########################################################################
# END Create tables in multiple databases
###########################################################################
  


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
# create ADCP table...one table for all data
###########################################################################
# the following lines need column names which come from reading in the 
# adcp files in 'importADCPdata()'.  This needs that and that needs this...
# but once the tables are created everythings cool.

# connect to db
dbconADCP = dbConnect(MySQL(), user="root", password="zy0014", dbname="dbADCP")
# set table name
adcpTableName = "adcpData"
# make sure it exists and is empty
dbphrase = paste("DROP TABLE IF EXISTS", adcpTableName, ";", sep=" ")
res = dbSendQuery(dbconADCP, dbphrase) 

# create dbphrase
  # NOTE: 'colNames' comes from internally going through lines of 
  #        importADCPdata() in 'environmental data.r'.
# create the start of dbphrase  
temp1 = c(paste("CREATE TABLE", adcpTableName, 
  "(pkey INT AUTO_INCREMENT, utime INT, INDEX (utime),")) # utime GMT
dbphrase = temp1
# create the middle of dbphrase
for (i in 2:3){
  temp1[i] = paste(colNames[i], "DATETIME,")
  dbphrase = paste(dbphrase, temp1[i])
}

# create the middle of dbphrase
for (i in 4:length(colNames)){
  temp1[i] = paste(colNames[i], "MEDIUMINT,")
  dbphrase = paste(dbphrase, temp1[i])
}
# create the end of dbphrase
temp1[length(colNames)+1] = paste("PRIMARY KEY (pkey));")

dbphrase = paste(dbphrase, temp1[length(colNames)+1])

res = dbSendQuery(dbconADCP, dbphrase) 

 aaa

  # create table names
  miscTableName = "miscLines" 
  
  # create empty sdlTables in cDB
  for (j in 1:length(sdlTableNames)){ 
    # if the table already exists, delete it.
    dbphrase = paste("DROP TABLE IF EXISTS", sdlTableNames[j], ";", sep=" ")
    res = dbSendQuery(dbList[[i]], dbphrase)
    # now create the table fresh
    long = paste(
      "CREATE TABLE", "sam",
      "(",
      "pkey int AUTO_INCREMENT,",
      "utime int, INDEX (utime),", # utime GMT
      "datiG datetime,", # dati in GMT
      "datiL datetime,", # dati in EST / EDT)
      "fraction smallint,",
      "power smallint,",
      "port tinyint,",
      "tagID int, INDEX (tagID),",
      "type tinyint,",
      "rawSensor tinyint,",
      "gpsSync char(1),",
      "PRIMARY KEY (pkey)",
      ");"
    )
    res = dbSendQuery(dbList[[i]], dbphrase) 
  } # end j loop over sdl output tables
       
 
 bbb