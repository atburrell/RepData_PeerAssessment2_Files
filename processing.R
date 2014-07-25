## Unzip data

setwd("RepData_PeerAssessment2")
bunzip2("repdata_data_StormData.csv.bz2")
list.files()
## Import into RStudip

raw_data <- read.csv("repdata_data_StormData.csv", header=TRUE) ## Read in raw data
event_name <- read.csv("event_name.csv", header=TRUE) ## List of 48 accepted events

library(plyr)
library(ggplot2)

## Main clean

data <- raw_data
data$BGN_DATE <- strptime(data$BGN_DATE, format = "%m/%d/%Y %H:%M:%S" )
data <- data[(data$BGN_DATE >= "1996-0-01"),] ## http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype
data$EVTYPE <- toupper(data$EVTYPE)
data$PROPDMGEXP <- toupper(data$PROPDMGEXP)
data$CROPDMGEXP <- toupper(data$CROPDMGEXP)


exclude_flood <- event_name[c(1:14, 16:48),] ## "flood" and "flash flood" are too similar for grep, remove "flood" from list
exclude_flood <- revalue(exclude_flood, c("Hurricane (Typhoon)" = "Hurricane")) ## "Hurricane (Typhoon)" too specific

##################################### Harmful

harmful <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data=data, sum)
harmful <- harmful[(harmful$FATALITIES > 0 & harmful$INJURIES > 0),]


for (name in exclude_flood){ ## grep loop to harmonize names based on 48 accepted
  change_list <- grep(name, harmful$EVTYPE, ignore.case=TRUE)
  for (rownum in change_list){
    harmful[rownum, "EVTYPE"] <- name
  }
}

harmful$EVTYPE <- gsub("TSTM WIND", "Thunderstorm Wind", harmful$EVTYPE) ## TSTM WIND near top of list, no grep matches

for (rownum in grep("cold", harmful$EVTYPE, ignore.case=TRUE)){ ## grep loop for cold and wind chill, where previous loop also not efficient
  harmful[rownum, "EVTYPE"] <- "Cold/Wind Chill"
  }

for (rownum in grep("Flood", harmful$EVTYPE, ignore.case=TRUE)){ ## grep loop for special case of "Flood"
  if(length(grep("Flash", harmful[rownum, "EVTYPE"])) == 0 & length(grep("Coastal", econ[rownum, "EVTYPE"])) == 0){
    harmful[rownum, "EVTYPE"] <- "Flood"
  }
}

harmful <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, data=harmful, sum) ## Condense list after names harmonized
harmful <- harmful[with(harmful, order(-FATALITIES, -INJURIES)), ] ## Order list for fatalities and injuries
harmful <- harmful[1:15,] ## Reduce list to top 15

harmful ## Show top 15
ggplot(harmful, aes(x=FATALITIES, y=INJURIES, color=EVTYPE)) + geom_point(size=4) ## Scatterplot to show distance from origin

##################################### Economic

econ <- data
econ <- econ[(econ$PROPDMG > 0 | econ$CROPDMG > 0),]


require(plyr)
econ$PROPDMGEXP <- revalue(econ$PROPDMGEXP, c("K" = 10^3, "M" = 10^6, "B" = 10^9))
econ$CROPDMGEXP <- revalue(econ$CROPDMGEXP, c("K" = 10^3, "M" = 10^6, "B" = 10^9))

econ$PROPDMGEXP <- as.numeric(econ$PROPDMGEXP)
econ$CROPDMGEXP <- as.numeric(econ$CROPDMGEXP)

econ$PROPDMG <- econ$PROPDMG * econ$PROPDMGEXP
econ$CROPDMG <- econ$CROPDMG * econ$CROPDMGEXP



econ <- aggregate(cbind(PROPDMG, CROPDMG) ~ EVTYPE, data=econ, sum) ## Aggregation #1 to reduce df size


## Name Cleaning

for (name in exclude_flood){ ## grep loop to harmonize names based on 48 accepted
  change_list <- grep(name, econ$EVTYPE, ignore.case=TRUE)
  for (rownum in change_list){
    econ[rownum, "EVTYPE"] <- name
  }
}

for (rownum in grep("Flood", econ$EVTYPE, ignore.case=TRUE)){ ## grep loop for special case of "Flood"
  if(length(grep("Flash", econ[rownum, "EVTYPE"])) == 0 & length(grep("Coastal", econ[rownum, "EVTYPE"])) == 0){
    econ[rownum, "EVTYPE"] <- "Flood"
  }
}

for (rownum in grep("Fires*", econ$EVTYPE, ignore.case=TRUE)){ ## grep loop for special case of "Fire"
    econ[rownum, "EVTYPE"] <- "Wildfire"
}

for (rownum in grep("Thu", econ$EVTYPE, ignore.case=TRUE)){ ## grep loop for special case of "Thunderstorm Wind"
  econ[rownum, "EVTYPE"] <- "Thunderstorm Wind"
}



econ$EVTYPE <- gsub("TSTM WIND", "Thunderstorm Wind", econ$EVTYPE) ## TSTM WIND near top of list, no grep matches
econ$EVTYPE <- gsub("URBAN/SML STREAM FLD", "Flood", econ$EVTYPE, ignore.case=TRUE) ## Missed by grep loops
econ$EVTYPE <- gsub("Typhoon", "Hurricane", econ$EVTYPE, ignore.case=TRUE) ## Missed by grep loops


econ <- aggregate(cbind(PROPDMG, CROPDMG) ~ EVTYPE, data=econ, sum) ## Aggregation #2 to combine pattern matches
econ <- econ[with(econ, order(-PROPDMG, -CROPDMG)), ] ## Order list for fatalities and injuries
econ <- econ[1:15,]

econ
ggplot(econ, aes(x=PROPDMG, y=CROPDMG, color=EVTYPE)) + geom_point(size=4) ## Scatterplot to show distance from origin

#########################################

library(data.table)
states <- data[,c("STATE", "EVTYPE")]
states <- data.table(states)

health <- grep("Tornado", states$EVTYPE, ignore.case=TRUE)
health <- append(health, grep("heat", states$EVTYPE, ignore.case=TRUE))

economy <- grep("flood", states$EVTYPE, ignore.case=TRUE)
economy <- append(economy, grep("hurricane", states$EVTYPE, ignore.case=TRUE))
economy <- append(economy, grep("ice storm", states$EVTYPE, ignore.case=TRUE))

master_health <- states[health, count(STATE)] ## Determine frequency count for each state for health event
master_economy <- states[economy, count(STATE)] ## Determine frequency count for each state for economic event

master_health <- master_health[with(master_health, order(-freq)), ] ## Order list for incidence
master_economy <- master_economy[with(master_economy, order(-freq)), ] ## Order list for incidence


head(master_health)
head(master_economy)


