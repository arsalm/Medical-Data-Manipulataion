# ARSAL MUNAWAR
# PROJECT 2


epwide <- read.table('C:/Users/munaw/Desktop/epilepsy.txt', header = FALSE, skip=35)

names(epwide) = c("ID","Treat","Age","0-8","1-2","2-2","3-2","4-2")

epwide$'0-8' <- epwide$'0-8'/(8)
epwide$'1-2' <- epwide$'1-2'/(2)
epwide$'2-2' <- epwide$'2-2'/(2)
epwide$'3-2' <- epwide$'3-2'/(2)
epwide$'4-2' <- epwide$'4-2'/(2)

#install.packages("tidyr")

library(tidyr)

keycol <- "Period"
valuecol <- "s_week"
gathercols <- c("0-8","1-2","2-2","3-2","4-2")
eplong = gather_(epwide,keycol,valuecol,gathercols)
eplong <- eplong[order(eplong$ID, eplong$Period), ]

#eplong
eplongcopy = eplong      						# a copy of the data
ithlist = list()								# creation of list for ith patient data
notithlist = list()							# creation of list for prediction data

for (i in 1:295){
	ithlist[i] = eplongcopy$s_week[i]				# add row i to a new list
	eplongcopy <- eplongcopy[-c(i),]      			# remove ith row
	regdata <- lm(s_week~Treat+Period, data=eplongcopy)	# reg on rest of data for that column
	summary(regdata)							# regression summary
	
	fitted(regdata)							# outputs predicted values
	notithlist[i] = fitted(regdata)				# saves predicted values to new table
	
	eplongcopy <- eplong						# restore the original data
	
}

print("this is the data for all ith patients")
print(ithlist)

print("this is the data for all patients except the ith patient")
print(notithlist)

#install.packages(c("tables","ggplot2","gdata"))
library(ggplot2)

ggplot(eplong, aes(x=Period, y=s_week)) + geom_point(size=3, shape=1)
ggplot(eplong, aes(x=Treat, y=s_week)) + geom_point(size=3, shape=2