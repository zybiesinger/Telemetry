### This is my attempt to finally master unix time
# I was born on 5 July 1972 about 10:45:00 am.  I was actually in Utah, but I'll 
# proceed here as if I was born in Florida, EDT.
# GMT = EST + 5hrs
# GMT = EDT + 4hrs
# So GMT for my Florida birth dati is 5 July 1972 14:45:00 EDT
# The Epoch converter says that GMT utime for this is: 79195500
#
# Next, one dati outside EDT is: 24 Dec 1972 10:45:00 EST
# GMT for this is 24 Dec 1972 15:45:00 GMT
# Epoch says GMT utime is: 94059900
#
# Summary:
# Local Time (EST/EDT)            GMT                           utime GMT
# 05/07/1972 10:45:00 EDT         05/07/1972 14:45:00 GMT       79195500 GMT
# 24/12/1972 10:45:00 EST         24/12/1972 15:45:00 GMT       94059900 GMT
# test dati...
# 07/12/2007 13:51:52 EST         07/12/2007 18:51:53 GMT       1197053513 GMT
# 30/08/2007 04:00:00 EDT         30/08/2007 08:00:00 GMT       1188460800 GMT

# Read this for more info
# ?DateTimeClasses


################################################################################
### If you have utime this is what to do...
################################################################################
bob = 1197053513                               # So...if bob is utime GMT then...

as.POSIXct(bob, origin="1970-1-1")                 # ...EST wrong...EDT wrong
as.POSIXct(bob, origin="1970-1-1", tz="GMT")       # ...EST correct...EDT correct
as.POSIXct(bob, origin="1970-1-1", tz="EST5EDT")   # ...EST wrong...EDT wrong

as.POSIXlt(bob, origin="1970-1-1")                 # ...EST correct...EDT correct
as.POSIXlt(bob, origin="1970-1-1", tz="GMT")       # ...EST correct...EDT correct
as.POSIXlt(bob, origin="1970-1-1", tz="EST5EDT")   # ...EST correct...EDT correct

# So to get time right: Make sure utime is always GMT, then use POSIXlt, and 
# use tz to specify the time zone you want the 'output' to be. 
#

################################################################################
### If you have dati (EST, EDT, or GMT) this is what to do...
################################################################################
# suppose I have dati as string and want to get utime GMT
dati1 = "6/24/2009 10:30:00" # EST5EDT   1245853800 utime GMT
dati1b = "6/24/2009 10:30:00" # EST always   1245853800 utime GMT
dati2 = "6/24/2009 14:30:00" # GMT   1245853800 utime GMT 
dati3 = "1/24/2009 10:30:00" # EST   1232811000 utime GMT
dati4 = "1/24/2009 15:30:00" # GMT   1232811000 utime GMT

# to convert from character to POSIXlt (doesn't have utime GMT in it)
d1=strptime(dati1, "%m/%d/%Y %H:%M:%S", tz="EST5EDT")
d1b=strptime(dati1b, "%m/%d/%Y %H:%M:%S", tz="EST")
d2=strptime(dati2, "%m/%d/%Y %H:%M:%S", tz="GMT")
d3=strptime(dati3, "%m/%d/%Y %H:%M:%S", tz="EST5EDT")
d4=strptime(dati4, "%m/%d/%Y %H:%M:%S", tz="GMT")

# to convert from character to POSIXct  (has utime GMT)
d11=as.POSIXct(strptime(dati1, "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
d11b=as.POSIXct(strptime(dati1b, "%m/%d/%Y %H:%M:%S", tz="EST"), origin="1970-1-1", tz="EST")
d22=as.POSIXct(strptime(dati2, "%m/%d/%Y %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT")
d33=as.POSIXct(strptime(dati3, "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT")
d44=as.POSIXct(strptime(dati4, "%m/%d/%Y %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT")

# to convert from POSIXct to utime GMT
d111=unclass(d11)[1]
d111b=unclass(d11b)[1]
d222=unclass(d22)[1]
d333=unclass(d33)[1]
d444=unclass(d44)[1]

# if you have a date/time as POSIXct/POSIXlt in readable form but EST, to show 
# the equivalent GMT...the only way I know is to convert to utime (GMT) and 
# return it to readable form with as.POSIXlt specifying the desired tz
#    ...f1 == d111
f1 = unclass(as.POSIXct(strptime(dati1, "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))[1]
f2 = unclass(as.POSIXct(strptime(dati2, "%m/%d/%Y %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT"))[1]
f3 = unclass(as.POSIXct(strptime(dati3, "%m/%d/%Y %H:%M:%S", tz="EST5EDT"), origin="1970-1-1", tz="EST5EDT"))[1]
f4 = unclass(as.POSIXct(strptime(dati4, "%m/%d/%Y %H:%M:%S", tz="GMT"), origin="1970-1-1", tz="GMT"))[1]

# pick one for example
as.POSIXlt(f1, origin="1970-1-1", tz="EST5EDT")   # ...EST correct...EDT correct
as.POSIXlt(f1, origin="1970-1-1", tz="GMT")       # ...EST correct...EDT correct


















################################################################################
### Now, other manipulations and observations...
################################################################################
 

sam1 = as.POSIXct(bob, origin="1970-1-1", tz="GMT")
sam2 = as.POSIXlt(bob, origin="1970-1-1", tz="GMT")
### sam is actually a number but gets dispalyed as text
unclass(sam1) # unclassed is utime and tz
unclass(sam2) # unclassed is like sam2$hour = zzz, etc. doesn't hold utime as a number
 
# suppose I have dati as string and want to get utime GMT
dati1 = "6/24/2009 10:30:00" # EDT   1245853800 utime GMT
dati2 = "6/24/2009 14:30:00" # GMT   1245853800 utime GMT
  
dati3 = "1/24/2009 10:30:00" # EST   1232811000 utime GMT
dati4 = "1/24/2009 15:30:00" # GMT   1232811000 utime GMT

two=strptime(dati1, "%m/%d/%Y %H:%M:%S", tz="EST5EDT")
three=strptime(dati2, "%m/%d/%Y %H:%M:%S", tz="GMT") 

two2 = as.POSIXlt(two, format="%m/%d/%Y %H:%M:%S", tz="EST5EDT")
three3 = as.POSIXlt(three, format="%m/%d/%Y %H:%M:%S", tz="GMT")
 
two22 = unclass(two2)
three33 = unclass(three3)

# all the above works as expected, but how do I get utime GMT out? only POSIXct 
# has utime, so convert to POSIXct
two222 = as.POSIXct(two) 
two2211 = unclass(two222)

three333 = as.POSIXct(three)
three3311 = unclass(three333)
#            both of these unclassed results give the same utime number with 
#            different tz assignments
# ...so what happens when you extract the number
q1 = two2211[1]
q2 = three3311[1]
#           they are the same
# ...or what happens when you convert back to a string dati
as.POSIXct(two222) # should equal two2
as.POSIXct(three333) # should equal three3

as.POSIXlt(two222) # should equal two2
as.POSIXlt(three333) # should equal three3
#          this seems to work correctly

# ...or what happens when you convert a utime (with tz attribute still attached) back to a string dati
as.POSIXct(two2211, origin="1970-1-1") # should equal two2   # WRONG
as.POSIXct(two2211, origin="1970-1-1", tz="EST5EDT") # should equal two2   # WRONG
#
as.POSIXct(three3311, origin="1970-1-1") # should equal three3  # WRONG
as.POSIXct(three3311, origin="1970-1-1", tz="GMT") # should equal three3  # RIGHT   
# what about POSIXlt?
as.POSIXlt(two2211, origin="1970-1-1") # should equal two2   # GIVES RIGHT ANSWER WITH ERROR MESSAGE
as.POSIXlt(two2211, origin="1970-1-1", tz="EST5EDT") # should equal two2   # GIVES RIGHT ANSWER WITH ERROR MESSAGE
#
as.POSIXlt(three3311, origin="1970-1-1") # should equal three3  # WRONG WITH ERROR MESSAGE, APPEARS TO ASSUME LOCAL TZ
as.POSIXlt(three3311, origin="1970-1-1", tz="GMT") # should equal three3  # GIVES RIGHT ANSWER WITH ERROR MESSAGE
# SO THIS LINE OF CONVERSION IS WONKY

# ...or what happens when you convert a utime (without tz attribute) back to a string dati
as.POSIXct(q1, origin="1970-1-1") # should equal two2   # WRONG
as.POSIXct(q1, origin="1970-1-1", tz="EST5EDT") # should equal two2   # WRONG
#
as.POSIXct(q2, origin="1970-1-1") # should equal three3  # WRONG
as.POSIXct(q2, origin="1970-1-1", tz="GMT") # should equal three3  # RIGHT
# what about POSIXlt?
as.POSIXlt(q1, origin="1970-1-1") # should equal two2   # RIGHT, APPEARS TO ASSUME LOCAL TZ, WHICH IS CORRECT FOR Q1 
as.POSIXlt(q1, origin="1970-1-1", tz="EST5EDT") # should equal two2   # RIGHT
as.POSIXlt(q1, origin="1970-1-1", tz="GMT") # should equal two2   # RIGHT WITH ANSWER IN GMT

#
as.POSIXlt(q2, origin="1970-1-1") # should equal three3  # RIGHT, BUT WITH EDT NOT GMT
as.POSIXlt(q2, origin="1970-1-1", tz="GMT") # should equal three3  # RIGHT, IN GMT
as.POSIXlt(q2, origin="1970-1-1", tz="EST5EDT") # should equal three3  #


Conclusions...
1. If you have utime numbers without tz attributes, then make sure they are 
   utime GMT and use POSIXlt.  POSIXlt assumes utime is GMT, and uses the
   specified tz to give the correct (time and time zone) dati.

2. If you're dealing with a number in POSIXct format (and maybe POSIXlt, but I didn't
   confirm it), then things work correctly.  

3. It looks like if utime is in GMT, then as.POSIXct works if you specify GMT, 
   otherwise it assumes utime is in local tz.  I think specifying tz in as.POSIXct 
   tells R only what tz to attach to the answer.  It doesn't take tz to mean
   anything about the utime.  I think specifying tz in as.POSIXlt tells R which tz 
   utime is in, and the answer comes out in that same tz. 




OLD CODE BELOW HERE...



The following is help from Ben when I was using Ubuntu:


[1] "POSIXt"  "POSIXct"
> ?as.POSIXt
No documentation for 'as.POSIXt' in specified packages and libraries:
you could try 'help.search("as.POSIXt")'
> ?as.POSIXct
> as.POSIXct(1199232039)
Error in as.POSIXct.numeric(1199232039) : 'origin' must be supplied
> ?as.POSIXct
> as.POSIXct(1199232039,origin="1970-1-1")
[1] "2008-01-02 00:00:39 EST"
> as.POSIXct(1199232039,origin="1970-1-1",tz="GMT")
[1] "2008-01-02 00:00:39 GMT"
> as.POSIXct(1199232039,origin="1970-1-1",tz="EST")
[1] "2008-01-02 00:00:39 EST"
> as.POSIXct(1199232039,origin="1970-1-1",tz="EDT")
[1] "2008-01-02 00:00:39 UTC"
> as.POSIXct(1199232039,origin="1970-1-1")
[1] "2008-01-02 00:00:39 EST"
> as.POSIXct(1199232007,origin="1970-1-1")
[1] "2008-01-02 00:00:07 EST"
> ?as.POSIXct



Try this:
as.POSIXct(1282335844, origin="1970-1-1")


sam = as.POSIXct(1200414927, origin="1970-1-1", tz="GMT")
### sam is actually a number but gets dispalyed as text
unclass(sam)


bob = "2008-01-02 00:00:07"
### bob is actually text and can't be unclass-ed
bobby = as.numeric(strptime(bob, "%Y-%m-%d %H:%M:%S"))

unixBob = unclass(bobby)

### now check
as.POSIXct(unixBob,origin="1970-1-1")


unclass(as.POSIXct(1197226533, origin="1970-1-1", tz="GMT"))
unclass(as.POSIXct(1197226533, origin="1970-1-1"))












### this is deleted form 'importAdcpData.r'
  ### convert to unix time
  # as.POSIXct expects GMT not local time...
  dates = paste(timeData$month,timeData$day,paste("200",timeData$year,sep=""),sep="/")
  times = paste(timeData$hour,timeData$min,"00",sep=":")
  dati = paste(dates, times) # this is EST/EDT not GMT
  temp1 = strptime(dati, format="%m/%d/%Y %H:%M:%S")
  temp2 = 
           
  
  ### learning code...keep for future re-learning
  bob=head(paste(dates, times)) # EDT
  sam = strptime(bob, format="%m/%d/%Y %H:%M:%S") # EDT 
  ssam = strptime(bob, format="%m/%d/%Y %H:%M:%S", tz="EST5EDT") # EDT 
  bbb = as.POSIXct(ssam, format="%m/%d/%Y %H:%M:%S", tz="GMT")+18000 # GMT
  ddd = unclass(bbb) # turns into GMT unix time
  eee = as.POSIXct(ddd,origin="1970-1-1")-18000
  
  
  
  
  
  ddddati = unclass(as.POSIXct(strptime(paste(dates, times),"%m/%d/%y %H:%M:%S")))-18000-zeroTime 



day="06/12/2009"; 
timeEDT="10:30:00" # EDT
timeGMT="14:30:00" # GMT

two=strptime(paste(day, timeEDT), "%m/%d/%Y %H:%M:%S", tz="EST5EDT")
three=strptime(paste(day, timeGMT), "%m/%d/%Y %H:%M:%S", tz="GMT") 

two2 = as.POSIXct(two, format="%m/%d/%Y %H:%M:%S", tz="EST5EDT")
three3 = as.POSIXct(three, format="%m/%d/%Y %H:%M:%S", tz="GMT")

two22 = unclass(two2)
three33 = unclass(three3)

as.POSIXct(two22,origin="1970-1-1")
as.POSIXct(two22,origin="1970-1-1",tz="EST5EDT")

as.POSIXct(three33,origin="1970-1-1")
as.POSIXct(three33,origin="1970-1-1",tz="GMT") 






d3$utime[1] --- 1197071513






as.POSIXct(1197071513, origin="1970-1-1")
as.POSIXct(1197071513, origin="1970-1-1", tz="GMT")    # correct if number is already GMT
as.POSIXct(1197071513, origin="1970-1-1", tz="EST5EDT")



as.POSIXlt(1197071513, origin="1970-1-1")    # correct if number is already GMT
as.POSIXlt(1197071513, origin="1970-1-1", tz="GMT")   # correct if number is already GMT
as.POSIXlt(1197071513, origin="1970-1-1", tz="EST5EDT")   # correct if number is already GMT


  