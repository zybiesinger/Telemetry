### This file if for reading in raw sonar *.svy so I can find out how much 
### overlap there was in the REMUS swaths. I believe the swaths were 25m on
### each side.

library(PBSmapping)

setwd("C:/zy") 
setwd("C:/zy/data closet/Telemetry/2008/2008Oct07 REMUS data and documentation/AUV 09Oct2008/081009-1st/Sonar")
filename="REMUS000.svy"


# I hate text files, the svy files have text interspersed with data
rfile = file(filename, open="rt") 
EOF = FALSE
navList = list()
good = data.frame(FALSE) # which lines have data
navIndex = 1

# find out which lines have data and save them
while(!EOF){
  temp1 = readLines(rfile,n=1)
  if( !is.na(as.numeric(substr(temp1,1,1))) ){
    good[navIndex,] = TRUE
    navList[[navIndex]] = temp1
    navIndex = navIndex + 1
  }
  
}

# now save the data files back into a text file
filename2 = "svyData1.txt"
rfile2 = file(filename2, open="wt")

for(i in 1:length(navList)){
  writeLines(text=navList[[i]], con=rfile2, sep="\n")
}

# finally read in as a table
temp2 = read.table(filename2,sep="\t")

df1 = data.frame(time=temp2[,5],easting=temp2[,8],northing=temp2[,6])
df2 = data.frame(X=NA,Y=NA)

for(i in 1:nrow(df1)){              
  df2[i,1] = as.numeric(substr(df1$easting[i],1,2)) +
    as.numeric(substr(df1$easting[i],3,12))/60
  df2[i,2] = as.numeric(substr(df1$northing[i],1,2))+
    as.numeric(substr(df1$northing[i],3,12))/60
}


# extract lat/long and convert to UTM, UTM increases northward and eastward
attr(df2, "zone") <- 17
attr(df2, "projection") <- "LL"   
df3 = convUL(df2,km=FALSE)  #X is easting in m, Y is northing in m

plot(df3$X,df3$Y,type="l"),xlim=c(-3800000,-3801000),ylim=c(48820000,48821000))








 