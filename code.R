library(ggplot2)
library(gridExtra)

# loading and exploring data
data <- read.csv("./data/StormData.csv")
dim(data)
str(data)

# subsetting required columns
columns <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
req_data <- data[ , columns]

# data processing

# checking NA values
sum(is.na(req_data$FATALITIES))
sum(is.na(req_data$INJURIES))
sum(is.na(req_data$PROPDMG))
sum(is.na(req_data$PROPDMGEXP))
sum(is.na(req_data$CROPDMG))
sum(is.na(req_data$CROPDMGEXP))
# no NA values

#problem in exponants
table(req_data$PROPDMGEXP)
req_data$PROPDMGEXP <- sub("0|1|2|3|4|5|6|7|8", "10", req_data$PROPDMGEXP)
req_data$PROPDMGEXP <- sub("h", "100", req_data$PROPDMGEXP, ignore.case = TRUE)
req_data$PROPDMGEXP <- sub("k", "1000", req_data$PROPDMGEXP, ignore.case = TRUE)
req_data$PROPDMGEXP <- sub("m", "1000000", req_data$PROPDMGEXP, ignore.case = TRUE)
req_data$PROPDMGEXP <- sub("b", "1000000000", req_data$PROPDMGEXP, ignore.case = TRUE)
req_data$PROPDMGEXP <- sub("\\+", "1", req_data$PROPDMGEXP)
req_data$PROPDMGEXP <- sub("\\-|\\?", "0", req_data$PROPDMGEXP)
req_data[req_data$PROPDMGEXP == "", "PROPDMGEXP"] <- 0
table(req_data$PROPDMGEXP)
req_data$PROPDMGEXP <- as.numeric(req_data$PROPDMGEXP)
req_data$property.cost <- req_data$PROPDMG * req_data$PROPDMGEXP

table(req_data$CROPDMGEXP)
req_data$CROPDMGEXP <- sub("0|2", "10", req_data$CROPDMGEXP)
req_data$CROPDMGEXP <- sub("k", "1000", req_data$CROPDMGEXP, ignore.case = TRUE)
req_data$CROPDMGEXP <- sub("m", "1000000", req_data$CROPDMGEXP, ignore.case = TRUE)
req_data$CROPDMGEXP <- sub("b", "1000000000", req_data$CROPDMGEXP, ignore.case = TRUE)
req_data$CROPDMGEXP <- sub("\\?", "0", req_data$CROPDMGEXP)
req_data[req_data$CROPDMGEXP == "", "CROPDMGEXP"] <- 0
table(req_data$CROPDMGEXP)
req_data$CROPDMGEXP <- as.numeric(req_data$CROPDMGEXP)
req_data$crop.cost <- req_data$CROPDMG * req_data$CROPDMGEXP

req_data$total_damage <- req_data$property.cost + req_data$crop.cost

# Q1
d1 <- aggregate(req_data$FATALITIES, list(req_data$EVTYPE), sum)
d2 <- aggregate(req_data$INJURIES, list(req_data$EVTYPE), sum)
names(d1) <- c("Event", "Fatalities")
names(d2) <- c("Event", "Injuries")
health_dmg <- merge(d1,d2)
rm(d1,d2)
health_dmg <- health_dmg[order(-health_dmg$Fatalities,-health_dmg$Injuries),]
head(health_dmg)
top_health_dmg <- health_dmg[1:5,]
p <- ggplot(top_health_dmg, aes(x=Event, y=Fatalities)) +
      geom_bar(stat="identity") +
      xlab("Events") +
      ylab("Cases of damage to health") +
      theme(axis.text.x = element_text(angle = 90, size=6)) +
      ylim(0,6000)
q <- ggplot(top_health_dmg, aes(x=Event, y=Injuries)) +
      geom_bar(stat="identity") +
      xlab("Events") +
      ylab("Cases of damage to health") +
      theme(axis.text.x = element_text(angle = 90, size=6)) +
      ylim(0,100000)
grid.arrange(p, q, ncol=2)

# Q2
economy_dmg <- aggregate(req_data$total_damage, list(req_data$EVTYPE), sum)
names(economy_dmg) <- c("Events", "Total_damage")
economy_dmg <- economy_dmg[order(-economy_dmg$Total_damage),]
head(economy_dmg)
top_economy_dmg <- economy_dmg[1:5,]
ggplot(top_economy_dmg, aes(x=Events, y=Total_damage/1000000000)) +
      geom_bar(stat = "identity") +
      ylab("Damage (in Billion dollars)") +
      theme(axis.text.x = element_text(size=8))
