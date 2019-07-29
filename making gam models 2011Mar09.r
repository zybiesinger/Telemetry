# This is a file for creating code for all gazillion gam models for chapter 3.

# If there are 5 model variables to be included then all possible combinations
# are below.  Look at this website for help...
# http://www.mathsisfun.com/combinatorics/combinations-permutations-calculator.html

# There should be 31 (=5+10+10+5+1) possible combinations
# ... I've removed waterDepth and eaaL


# If my possible choices are numbers 1-5 then these are the combinations:


# ones - 1 2 3 4 5
# twos - 12 13 23 14 24 34 15 25 35 45
# threes - 123 124 134 234 125 135 145 235 245 345
# fours - 1234 1235 1245 1345 2345
# fives - 12345
                                               
                                               
load("C:/zy/Telemetry/R summary files/z0 2011Mar16.rdata")

# create some empty lists to hold answers
dtrList = list()
speedList = list()
altList = list()
dtrAicList = data.frame(model=NA, df=NA, rsq=NA, AIC=NA)                                 
speedAicList = data.frame(model=NA, df=NA, rsq=NA, AIC=NA)                                 
altAicList = data.frame(model=NA, df=NA, rsq=NA, AIC=NA)                                 

# DTR ##########################################################################
# specify all the possible models then add interaction term: (lunarIndex,tod)
# ones
dtrList[[1]]=dtr~s(temperature)
dtrList[[2]]=dtr~s(magL)
dtrList[[3]]=dtr~s(dirL,bs="cc")
dtrList[[4]]=dtr~s(tod,bs="cc")              
dtrList[[5]]=dtr~s(lunarIndex,bs="cc")
# ...ones with by=tagName
dtrList[[6]]=dtr~s(temperature, by=tagName)
dtrList[[7]]=dtr~s(magL, by=tagName)
dtrList[[8]]=dtr~s(dirL,bs="cc", by=tagName)
dtrList[[9]]=dtr~s(tod,bs="cc", by=tagName)
dtrList[[10]]=dtr~s(lunarIndex,bs="cc", by=tagName)

# twos
dtrList[[11]]=dtr~s(temperature)+s(magL)
dtrList[[12]]=dtr~s(temperature)+s(dirL,bs="cc")
dtrList[[13]]=dtr~s(temperature)+s(tod,bs="cc")
dtrList[[14]]=dtr~s(temperature)+s(lunarIndex,bs="cc")
dtrList[[15]]=dtr~s(magL)+s(dirL,bs="cc")
dtrList[[16]]=dtr~s(magL)+s(tod,bs="cc") 
dtrList[[17]]=dtr~s(magL)+s(lunarIndex,bs="cc")
dtrList[[18]]=dtr~s(dirL,bs="cc")+s(tod,bs="cc")
dtrList[[19]]=dtr~s(dirL,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[20]]=dtr~s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[21]]=dtr~s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ... twos with by=tagName
dtrList[[22]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)
dtrList[[23]]=dtr~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)
dtrList[[24]]=dtr~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[25]]=dtr~s(temperature, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[26]]=dtr~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
dtrList[[27]]=dtr~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)  
dtrList[[28]]=dtr~s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[29]]=dtr~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[30]]=dtr~s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[31]]=dtr~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[32]]=dtr~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# threes
dtrList[[33]]=dtr~s(temperature)+s(magL)+s(dirL,bs="cc")
dtrList[[34]]=dtr~s(temperature)+s(magL)+s(tod,bs="cc")
dtrList[[35]]=dtr~s(temperature)+s(magL)+s(lunarIndex,bs="cc")
dtrList[[36]]=dtr~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")
dtrList[[37]]=dtr~s(temperature)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[38]]=dtr~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[39]]=dtr~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
dtrList[[40]]=dtr~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
dtrList[[41]]=dtr~s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[42]]=dtr~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[43]]=dtr~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)  
dtrList[[44]]=dtr~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[45]]=dtr~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...threes with by=tagName
dtrList[[46]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
dtrList[[47]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[48]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[49]]=dtr~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[50]]=dtr~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[51]]=dtr~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[52]]=dtr~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
dtrList[[53]]=dtr~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[54]]=dtr~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[55]]=dtr~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[56]]=dtr~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  
dtrList[[57]]=dtr~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[58]]=dtr~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# fours
dtrList[[59]]=dtr~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
dtrList[[60]]=dtr~s(temperature)+s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[61]]=dtr~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[62]]=dtr~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
dtrList[[63]]=dtr~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[64]]=dtr~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
dtrList[[65]]=dtr~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[66]]=dtr~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ...fours with by=tagName
dtrList[[67]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
dtrList[[68]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[69]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                
dtrList[[70]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)        
dtrList[[71]]=dtr~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)         
dtrList[[72]]=dtr~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
dtrList[[73]]=dtr~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                        
dtrList[[74]]=dtr~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)          

# fives 
dtrList[[75]]=dtr~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
dtrList[[76]]=dtr~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...fives with by=tagName
dtrList[[77]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
dtrList[[78]]=dtr~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  



# SPEED ########################################################################
# specify all the possible models then add interaction term: (lunarIndex,tod)
# ones
speedList[[1]]=speed~s(temperature)
speedList[[2]]=speed~s(magL)
speedList[[3]]=speed~s(dirL,bs="cc")
speedList[[4]]=speed~s(tod,bs="cc")              
speedList[[5]]=speed~s(lunarIndex,bs="cc")
# ...ones with by=tagName
speedList[[6]]=speed~s(temperature, by=tagName)
speedList[[7]]=speed~s(magL, by=tagName)
speedList[[8]]=speed~s(dirL,bs="cc", by=tagName)
speedList[[9]]=speed~s(tod,bs="cc", by=tagName)
speedList[[10]]=speed~s(lunarIndex,bs="cc", by=tagName)

# twos
speedList[[11]]=speed~s(temperature)+s(magL)
speedList[[12]]=speed~s(temperature)+s(dirL,bs="cc")
speedList[[13]]=speed~s(temperature)+s(tod,bs="cc")
speedList[[14]]=speed~s(temperature)+s(lunarIndex,bs="cc")
speedList[[15]]=speed~s(magL)+s(dirL,bs="cc")
speedList[[16]]=speed~s(magL)+s(tod,bs="cc") 
speedList[[17]]=speed~s(magL)+s(lunarIndex,bs="cc")
speedList[[18]]=speed~s(dirL,bs="cc")+s(tod,bs="cc")
speedList[[19]]=speed~s(dirL,bs="cc")+s(lunarIndex,bs="cc")
speedList[[20]]=speed~s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[21]]=speed~s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ... twos with by=tagName
speedList[[22]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)
speedList[[23]]=speed~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)
speedList[[24]]=speed~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[25]]=speed~s(temperature, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[26]]=speed~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
speedList[[27]]=speed~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)  
speedList[[28]]=speed~s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[29]]=speed~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[30]]=speed~s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[31]]=speed~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[32]]=speed~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# threes
speedList[[33]]=speed~s(temperature)+s(magL)+s(dirL,bs="cc")
speedList[[34]]=speed~s(temperature)+s(magL)+s(tod,bs="cc")
speedList[[35]]=speed~s(temperature)+s(magL)+s(lunarIndex,bs="cc")
speedList[[36]]=speed~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")
speedList[[37]]=speed~s(temperature)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
speedList[[38]]=speed~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[39]]=speed~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
speedList[[40]]=speed~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
speedList[[41]]=speed~s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
speedList[[42]]=speed~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[43]]=speed~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)  
speedList[[44]]=speed~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[45]]=speed~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...threes with by=tagName
speedList[[46]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
speedList[[47]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[48]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[49]]=speed~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[50]]=speed~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[51]]=speed~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[52]]=speed~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
speedList[[53]]=speed~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[54]]=speed~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[55]]=speed~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[56]]=speed~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  
speedList[[57]]=speed~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[58]]=speed~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# fours
speedList[[59]]=speed~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
speedList[[60]]=speed~s(temperature)+s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
speedList[[61]]=speed~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[62]]=speed~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
speedList[[63]]=speed~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[64]]=speed~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
speedList[[65]]=speed~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[66]]=speed~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ...fours with by=tagName
speedList[[67]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
speedList[[68]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[69]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                
speedList[[70]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)        
speedList[[71]]=speed~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)         
speedList[[72]]=speed~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
speedList[[73]]=speed~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                        
speedList[[74]]=speed~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)          

# fives 
speedList[[75]]=speed~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
speedList[[76]]=speed~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...fives with by=tagName
speedList[[77]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
speedList[[78]]=speed~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  


# ALT ##########################################################################
# specify all the possible models then add interaction term: (lunarIndex,tod)
# ones
altList[[1]]=altitude~s(temperature)
altList[[2]]=altitude~s(magL)
altList[[3]]=altitude~s(dirL,bs="cc")
altList[[4]]=altitude~s(tod,bs="cc")              
altList[[5]]=altitude~s(lunarIndex,bs="cc")
# ...ones with by=tagName
altList[[6]]=altitude~s(temperature, by=tagName)
altList[[7]]=altitude~s(magL, by=tagName)
altList[[8]]=altitude~s(dirL,bs="cc", by=tagName)
altList[[9]]=altitude~s(tod,bs="cc", by=tagName)
altList[[10]]=altitude~s(lunarIndex,bs="cc", by=tagName)

# twos
altList[[11]]=altitude~s(temperature)+s(magL)
altList[[12]]=altitude~s(temperature)+s(dirL,bs="cc")
altList[[13]]=altitude~s(temperature)+s(tod,bs="cc")
altList[[14]]=altitude~s(temperature)+s(lunarIndex,bs="cc")
altList[[15]]=altitude~s(magL)+s(dirL,bs="cc")
altList[[16]]=altitude~s(magL)+s(tod,bs="cc") 
altList[[17]]=altitude~s(magL)+s(lunarIndex,bs="cc")
altList[[18]]=altitude~s(dirL,bs="cc")+s(tod,bs="cc")
altList[[19]]=altitude~s(dirL,bs="cc")+s(lunarIndex,bs="cc")
altList[[20]]=altitude~s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[21]]=altitude~s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ... twos with by=tagName
altList[[22]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)
altList[[23]]=altitude~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)
altList[[24]]=altitude~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)
altList[[25]]=altitude~s(temperature, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[26]]=altitude~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
altList[[27]]=altitude~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)  
altList[[28]]=altitude~s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[29]]=altitude~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
altList[[30]]=altitude~s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[31]]=altitude~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[32]]=altitude~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# threes
altList[[33]]=altitude~s(temperature)+s(magL)+s(dirL,bs="cc")
altList[[34]]=altitude~s(temperature)+s(magL)+s(tod,bs="cc")
altList[[35]]=altitude~s(temperature)+s(magL)+s(lunarIndex,bs="cc")
altList[[36]]=altitude~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")
altList[[37]]=altitude~s(temperature)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
altList[[38]]=altitude~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[39]]=altitude~s(temperature)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
altList[[40]]=altitude~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
altList[[41]]=altitude~s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
altList[[42]]=altitude~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[43]]=altitude~s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)  
altList[[44]]=altitude~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[45]]=altitude~s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...threes with by=tagName
altList[[46]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)
altList[[47]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)
altList[[48]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[49]]=altitude~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
altList[[50]]=altitude~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[51]]=altitude~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[52]]=altitude~s(temperature, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
altList[[53]]=altitude~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
altList[[54]]=altitude~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[55]]=altitude~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[56]]=altitude~s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  
altList[[57]]=altitude~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[58]]=altitude~s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)

# fours
altList[[59]]=altitude~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")
altList[[60]]=altitude~s(temperature)+s(magL)+s(dirL,bs="cc")+s(lunarIndex,bs="cc")
altList[[61]]=altitude~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[62]]=altitude~s(temperature)+s(magL)+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
altList[[63]]=altitude~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[64]]=altitude~s(temperature)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
altList[[65]]=altitude~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[66]]=altitude~s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod) 
# ...fours with by=tagName
altList[[67]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)
altList[[68]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[69]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                
altList[[70]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)        
altList[[71]]=altitude~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)         
altList[[72]]=altitude~s(temperature, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)
altList[[73]]=altitude~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)                        
altList[[74]]=altitude~s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)          

# fives 
altList[[75]]=altitude~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")
altList[[76]]=altitude~s(temperature)+s(magL)+s(dirL,bs="cc")+s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(lunarIndex,tod)
# ...fives with by=tagName
altList[[77]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)
altList[[78]]=altitude~s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName)+s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+s(lunarIndex,tod)  



# For each model, fit the gam, calculate the AIC, save the gam externally, remove the gam
                            
# ... and because you need two in the AIC command to get the df output...  
library(mgcv)
seedGamFit = gam(dtrList[[1]], data=z0)

for (i in 78:78){#length(dtrList)){
  # fit the gams
  dtrGamAns = gam(dtrList[[i]], data=z0)
  speedGamAns = gam(speedList[[i]], data=z0)
#  altGamAns = gam(altList[[i]], data=z0)
  # calculate AIC
  dtrAicAns =   AIC(seedGamFit, dtrGamAns)
  speedAicAns = AIC(seedGamFit, speedGamAns)
#  altAicAns =   AIC(seedGamFit, altGamAns)
  # store results (model number, d.f., adjusted r^2, AIC)
  dtrAicList[i,] =   c(i, dtrAicAns  [[2,1]], summary(dtrGamAns)$r.sq, dtrAicAns  [[2,2]])
  speedAicList[i,] = c(i, speedAicAns[[2,1]], summary(dtrGamAns)$r.sq, speedAicAns[[2,2]])
#  altAicList[i,] =   c(i, altAicAns  [[2,1]], summary(altGamAns)$r.sq, altAicAns  [[2,2]])
  
  # THE RESULT OF THIS GETS SAVED HERE
  save("dtrAicList", file="C:/zy/Telemetry/R summary files/dtrAicList 2011Mar30.rdata")
  save("speedAicList", file="C:/zy/Telemetry/R summary files/speedAicList 2011Mar30.rdata")
#  save("altAicList", file="C:/zy/Telemetry/R summary files/altAicList 2011Mar30.rdata")
 
}        

# THE RESULT OF THIS GETS SAVED HERE
save("dtrAicList", file="C:/zy/Telemetry/R summary files/dtrAicList 2011Mar30.rdata")
save("speedAicList", file="C:/zy/Telemetry/R summary files/speedAicList 2011Mar30.rdata")
save("altAicList", file="C:/zy/Telemetry/R summary files/altAicList 2011Mar30.rdata")



# THE RESULTS ARE READ IN HERE
load("C:/zy/Telemetry/R summary files/dtrAicList 2011Mar30.rdata")
load("C:/zy/Telemetry/R summary files/speedAicList 2011Mar30.rdata")
load("C:/zy/Telemetry/R summary files/altAicList 2011Mar30.rdata")



# calculate AIC values relative to the minimum AIC value
dtrAicList[,4]   = dtrAicList  [,4]-min(dtrAicList[,4])
speedAicList[,4] = speedAicList[,4]-min(speedAicList[,4])
altAicList[,4]   = altAicList  [,4]-min(altAicList[,4])

# pick the five best
dtrSortedAIC =   dtrAicList  [ order(dtrAicList[,4]),]
speedSortedAIC = speedAicList[ order(speedAicList[,4]),]
altSortedAIC =   altAicList  [ order(altAicList[,4]),]

 plot(altAicList$AIC, altAicList$rsq)

# top dtr models: 78,  72, 70, 52, 77
# ...remaining 'drop-one' models: 67, 68, 74

# top speed models: 32, 31, 10, 21, 9

# top altitude models: 78, 70, 72, 52, 77
# ...remaining 'drop-one' models: 67, 68, 74


library(mgcv)
# compare dtr models
model1 = gam(dtrList[[78]], data=z0) # 
model2 = gam(dtrList[[72]], data=z0) # 
model3 = gam(dtrList[[70]], data=z0) # 
model4 = gam(dtrList[[52]], data=z0) # 
model5 = gam(dtrList[[77]], data=z0) # 

model6 = gam(dtrList[[67]], data=z0) # 
model7 = gam(dtrList[[68]], data=z0) # 
model8 = gam(dtrList[[74]], data=z0) # 

RSStable = c(
   sum(residuals(model1)^2),
   sum(residuals(model2)^2),
   sum(residuals(model3)^2),
   sum(residuals(model4)^2),
   sum(residuals(model5)^2),
   sum(residuals(model6)^2),
   sum(residuals(model7)^2),
   sum(residuals(model8)^2)
) 

RSStable = RSStable - min(RSStable) 

AIC(seedGamFit, model1, model2)


# compare speed models
model9 = gam(speedList[[32]], data=z0)
model10 = gam(speedList[[31]], data=z0)
model11 = gam(speedList[[10]], data=z0)
model12 = gam(speedList[[21]], data=z0)
model13 = gam(speedList[[9]], data=z0)

RSStable = c(
   sum(residuals(model9)^2),
   sum(residuals(model10)^2),
   sum(residuals(model11)^2),
   sum(residuals(model12)^2),
   sum(residuals(model13)^2)
) 

RSStable = RSStable - min(RSStable) 

# compare alt models
model14 = gam(altList[[2]], data=z0)
model15 = gam(altList[[4]], data=z0)
model16 = gam(altList[[7]], data=z0)
model17 = gam(altList[[16]], data=z0)
model18 = gam(altList[[9]], data=z0)
bob= gam(altList[[32]], data=z0)

altR2List = cbind(altAicList,"r2"=rep(NA,nrow(altAicList)))
for (i in 1:nrow(altAicList)){
  altR2List$r2[[i]] = summary.gam(
} 

summary.gam(model18)$r.sq

















##############################################################################
# start with the full model and remove each variable one at a time and see how
# much the AIC decreases
model1 = gam(modelList[[127]], data=z0)
model2 = gam(modelList[[120]], data=z0)
model3 = gam(modelList[[121]], data=z0)
model4 = gam(modelList[[122]], data=z0)
model5 = gam(modelList[[123]], data=z0)
model6 = gam(modelList[[124]], data=z0)
model7 = gam(modelList[[125]], data=z0)
model8 = gam(modelList[[126]], data=z0)

bob=AIC(model1, model2, model3, model4, model5, model6, model7, model8)

minaic = min(bob$AIC)
bob$deltaAIC=bob$AIC-minaic






bob1=gam(dtr~s(tod,bs="cc")+s(lunarIndex,bs="cc")+s(temperature)+s(magL)+s(dirL,bs="cc"), data=z0)
bob2=gam(dtr~s(tod,bs="cc", by=tagName)+s(lunarIndex,bs="cc", by=tagName)+
  s(temperature, by=tagName)+s(magL, by=tagName)+s(dirL,bs="cc", by=tagName), data=z0)






sum(z0$habType=="white", na.rm=T)   +
sum(is.na(z0$habType)) +
sum(z0$habType=="black", na.rm=T)


fraction of time over black
sum(z0$habType=="black", na.rm=T) / nrow(z0)





