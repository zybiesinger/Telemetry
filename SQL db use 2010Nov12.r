### This file is temporarily intended to follow 'SQL db create.r' and
### . 'SQL db fill.r'  I want to explore the newlly filled tables 





library(RMySQL)

# create connections to db
dbcon2008 = dbConnect(MySQL(), user="root", password="zy0014", dbname="db2008")             
dbList = list(dbcon2008)
dbListNames = c("dbcon2008")

dbcon = dbList[[1]] 
             

# make sure the dbTable is empty of lines before starting
dbphrase = "SELECT DISTINCT tagID FROM sdl41;"  
res = dbGetQuery(dbcon, dbphrase) 

dc = res$tagID

trueCodes = c(1:57, 65:67, 70:72, 77:86, 119:120, 129:130)
falseCodes = c(58:64, 68:69, 73:76, 87:118, 121:128) 

sdc = sort(as.numeric(dc))
uniques = sdc

# delete the ones that should be present and see what's left
t1 = sdc[sdc>57]
t2 = t1[(t1<65) | (t1>66)]
t3 = t2[(t2<70) | (t2>72)]
t4 = t3[(t3<77) | (t3>86)]
t5 = t4[(t4<119) | (t4>120)]
t6 = t5[(t5<129) | (t5>130)]


# so dc has all numbers from 1 to 130, and only some of them from tags

dbphrase = "SELECT COUNT(*) FROM sdl41 WHERE tagID='1';"
res = dbGetQuery(dbcon, dbphrase) 
                         
dbphrase = "SELECT * FROM sdl41 LIMIT 5000000;"  
res = dbGetQuery(dbcon, dbphrase) 

df1 = res[,c(1,2,3,6)]

# now count how many times 'uniques' appear in these 50,000 records


uCounts = c()
for (i in 1:length(uniques)){
  uCounts = c(uCounts, length( df1$tagID[ df1$tagID == uniques[i] ] ))
}

plot(uniques, uCounts, type="l")#, ylim=c(0,10))


df2 = data.frame("uniques" = uniques, "uCounts" = uCounts)
df3 = df2[df2$uCounts>0,]
plot(df3$uniques, df3$uCounts, pch=19, cex=0.5)
abline(v=c(seq(from=10,to=140,by=10),130))
lines(x=c(1,57),y=c(-50,-50), col="green", lwd=2)
lines(x=c(65,67),y=c(-50,-50), col="green", lwd=2)
lines(x=c(70,72),y=c(-50,-50), col="green", lwd=2)
lines(x=c(77,86),y=c(-50,-50), col="green", lwd=2)
lines(x=c(119,120),y=c(-50,-50), col="green", lwd=2)
lines(x=c(129,130),y=c(-50,-50), col="green", lwd=2)

# now count how many times 'uniques' appear in the whole database
dbphrase = "SELECT COUNT(*) FROM sdl41;"
res = dbGetQuery(dbcon, dbphrase) #13627100

occurances = list()
for (i in 1:length(uniques)){
  dbphrase = paste("SELECT COUNT(*) FROM sdl41 WHERE tagID='", uniques[i] ,"';")
  occurances[i]= dbGetQuery(dbcon, dbphrase)
}
# make pretty
bob = c()
for (i in 1:length(occurances)){
  bob = c(bob,occurances[[i]])
}
occurances = bob

ocs1 = data.frame("uniques"=1:length(occurances), "uCounts"=occurances)

plot(ocs1, type="s")

# plot counts just for true codes
ocs2 = ocs1[trueCodes ,]







# and now for something completely different
dbphrase = "SELECT * FROM sdl41;"  
res = dbGetQuery(dbcon, dbphrase) 

dbphrase = "SELECT date, time, tagID FROM sdl41 LIMIT 10;"  
res = dbGetQuery(dbcon, dbphrase) 

dbphrase = "SELECT date, time, tagID FROM sdl41 WHERE date > 2008-10-06  LIMIT 10;"  
res = dbGetQuery(dbcon, dbphrase) 
                                                 



# add a column for the primary key, 'pid'
dbphrase = paste("ALTER TABLE", dbTname, "ADD pid int NOT NULL;")   
res = dbSendQuery(dbcon, dbphrase)

# make 'pid' the primary key 
dbphrase = paste("ALTER TABLE", dbTname, "AUTO_INCREMENT=1;") 
res = dbSendQuery(dbcon, dbphrase)









