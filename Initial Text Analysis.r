setwd("C:\\Users\\David\\Dropbox\\Stanford-IM\\Epic Research\\AccessLog2\\Paging Data Analysis")
dir()


dataFileLocation <- "C:\\Users\\David\\Dropbox\\Stanford-IM\\Epic Research\\AccessLog2"


library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)


data1 <- read.csv(paste(dataFileLocation, "\\SoM Resident Paging Data 2013 v1.csv",sep = ""), stringsAsFactors = FALSE)
str(data1)
data2 <- read.csv(paste(dataFileLocation, "\\SoM Resident Paging Data 2014 v1.csv",sep = ""), stringsAsFactors = FALSE)
data3 <- read.csv(paste(dataFileLocation, "\\SoM Resident Paging Data 2015 v1.csv",sep = ""), stringsAsFactors = FALSE)
data4 <- read.csv(paste(dataFileLocation, "\\SoM Resident Paging Data 2016 v1.csv",sep = ""), stringsAsFactors = FALSE)
data5 <- read.csv(paste(dataFileLocation, "\\SoM Resident Paging Data 2017 v1.csv",sep = ""), stringsAsFactors = FALSE)

data <- rbind(data1,data2,data3,data4,data5)
str(data)


data$datetime <- strptime(data$DATE_ENTERED, "%m/%d/%Y %H:%M")
data$weekday <- as.factor(weekdays(strptime(data$DATE_ENTERED, "%m/%d/%Y %H:%M")))
data$time <- strptime(str_sub(data$DATE_ENTERED, -5, -1), "%H:%M") # data$time <- format(data$datetime, "%H:%M")
data$datetime <- as.POSIXct(data$datetime)
data$weekday <- factor(data$weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))


# Check that it doesn't match any non-number
numbers_only <- function(x) grepl("^[-0123456789, pex]+$", x)

data$numberpageonly <- numbers_only(data$MESSAGE_TEXT)

ggplot(data = data, aes(x = time, fill = weekday, group = weekday)) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Pages Every 5 Minutes") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))

#ggsave("PagingFrequencyOver24Hours.png", width = 19, height = 7)

data$forwardingpage <- grepl("Please do not reply to this message.", data$MESSAGE_TEXT)
head(data[data$forwardingpage,],20)


ggplot(data = data, aes(x = time, fill = forwardingpage )) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Pages Every 5 Minutes") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))

# ggsave("PagingFrequencyOver24HoursBinnedByForwardingPages.png", width = 19, height = 7)


head(data[data$forwardingpage & data$time < strptime("00:30", "%H:%M") ,],20)

ggplot(data = data[data$numberpageonly,], aes(x = time, fill = weekday )) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Pages Every 5 Minutes") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))

# ggsave("NumberPageFrequencyOver24Hours.png", width = 19, height = 7)


data$forwardingpage <- grepl("Please do not reply to this message.", data$MESSAGE_TEXT)
head(data[data$forwardingpage,],20)
data$saysPlease <- grepl("Please|plz|please|pls|PLEASE", data$MESSAGE_TEXT)
data$call <- grepl("Call|CALL|call", data$MESSAGE_TEXT)
data$order <- grepl("Write|write|WRITE|order|Order|order|Replete|replete", data$MESSAGE_TEXT)
data$ASAP <- grepl("asap|ASAP", data$MESSAGE_TEXT)
data$criticalLab <- grepl("critic|Critic|crit lab|Crit lab|CRITICAL", data$MESSAGE_TEXT)
data$diet <- grepl("diet|DIET|NPO", data$MESSAGE_TEXT)

data$MESSAGE_TEXT <- str_replace_all(data$MESSAGE_TEXT,"\n","")

data$text <- ""
########## IGNORING PLEASE ########
#data[data$saysPlease,]$text <- paste(data[data$saysPlease,]$text ,"please")
data[data$call ,]$text <- "Call Back" # paste(data[data$call ,]$text ,"callback")
data[data$order ,]$text <- "Order" #paste(data[data$order ,]$text ,"order")
data[data$criticalLab ,]$text <- "Critical Lab"
data[data$diet ,]$text <- "Diet" #paste(data[data$order ,]$text ,"order")
data[data$ASAP ,]$text <- paste(data[data$ASAP ,]$text ,"ASAP")
data[data$forwardingpage,]$text <- " Forwarding Pager"

write.csv(data, "combinedTextsWithSomeProcessing.csv")

write.csv(data$MESSAGE_TEXT, "justText.csv")


ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = text ))  + scale_x_datetime(breaks = date_breaks("6 hour"),
  minor_breaks = date_breaks("3 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Text Analysis Of Pages") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar( bin = 60*10) + facet_wrap(~text)

# position="fill",

#ggsave("DistributionOfPageSubjects_Facet_Unscaled.png")

ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = text ))  + scale_x_datetime(breaks = date_breaks("6 hour"),
  minor_breaks = date_breaks("3 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Text Analysis Of Pages") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar( bin = 60*10) + facet_wrap(~text, scale = "free_y")

# position="fill",

#ggsave("DistributionOfPageSubjects_Facet_Scaled.png")



ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = text ))  + scale_x_datetime(breaks = date_breaks("6 hour"),
  minor_breaks = date_breaks("3 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Text Analysis Of Pages") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar(position="fill", bin = 60*10) #+ facet_wrap(~text, scale = "free_y")

# position="fill",

ggsave("DistributionOfPageSubjects_Proportion.png")




ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = saysPlease ))  + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Proportion of Pages Saying Please") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar(position="fill", bin = 60*5)

ggsave("ProportionOfPagesWithPlease.png")


ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = order ))  + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Proportion of Pages Asking for Order") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar(position="fill", bin = 60*5)

ggsave("ProportionOfPagesAskingForOrder.png")

ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = call ))  + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Proportion of Pages Asking for Call Back") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar(position="fill", bin = 60*5)

ggsave("ProportionOfPagesAskingForCallback.png")

ggplot(data = data[!data$forwardingpage,], aes(x = time, fill = criticalLab ))  + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Proportion of Pages Asking for Call Back") + 
 labs(weekday = "Day Of Week") + theme(text = element_text(size = 18))+ geom_bar(position="fill", bin = 60*5)

ggsave("ProportionOfPagesCriticalLabs.png")


head(data[data$saysPlease  & !data$forwardingpage,],20)



# Text Analysis #

library(tm)

myCorpus <- Corpus(VectorSource(data$MESSAGE_TEXT))



tdm = TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,stopwords = c("new", "year", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))

#Convert as matrix
m = as.matrix(tdm)

#Get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

#Create data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#Plot wordcloud

library(wordcloud)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))




myCorpus <- tm_map(myCorpus, content_transformer(tolower))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))














############################


last_plot() + facet_wrap(~weekday, ncol = 1)


last_plot() + scale_fill_brewer(name = "Day Of Week", palette="Set1")
last_plot() + scale_fill_discrete(name = "Day Of Week")
ggsave("24hour_period_Aggregate.png")



last_plot() + scale_fill_brewer(name = "Day Of Week")

last_plot() + scale_colour_brewer(name = "Day Of Week")

ggsave("24hour_period_Sat_Sun_On_Bottom.png")

data$weekday <- factor(data$weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(data = data, aes(x = time, fill = weekday, group = weekday)) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Actions Every 5 Minutes") +
scale_y_continuous(breaks = c(0,5200*2,5200*4,5200*6,5200*8), labels = c("0", "200", "400", "600", "800"))
ggsave("24hour_period_Sat_Sun_On_Bottom3_scaledToWeek.png")



data$weekday <- factor(data$weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(data = data, aes(x = time, fill = weekday, group = weekday)) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Actions Every 5 Minutes") +
scale_y_continuous(breaks = c(0,5200*2,5200*4,5200*6,5200*8), labels = c("0", "50", "100", "150", "200"))
ggsave("24hour_period_Sat_Sun_On_Bottom_scaledToTeam.png")



qplot(data = data, x = data$time, bin = 100) 



ggsave('24hourperiod-Without17162.png')


qplot(data = data[data$weekday == "Monday",], x = data[data$weekday == "Monday",]$time, bin = 100)
ggsave('24hourperiod-Monday-Without17162.png')

qplot(data = data[data$weekday == "Tuesday",], x = data[data$weekday == "Tuesday",]$time, bin = 100)
ggsave('24hourperiod-Tuesday-Without17162.png')

qplot(data = data[data$weekday == "Wednesday",], x = data[data$weekday == "Wednesday",]$time, bin = 100)
ggsave('24hourperiod-Wednesday-Without17162.png')

qplot(data = data[data$weekday == "Thursday",], x = data[data$weekday == "Thursday",]$time, bin = 100)
ggsave('24hourperiod-Thursday-Without17162.png')

qplot(data = data[data$weekday == "Friday",], x = data[data$weekday == "Friday",]$time, bin = 100)
ggsave('24hourperiod-Friday-Without17162.png')

dataWeekend <- data[data$weekday == "Saturday" | data$weekday == "Sunday" ,]


qplot(data = dataWeekend, x = dataWeekend$time, bin = 100)
ggsave('24hourperiod-Weekend-Without17162.png')


qplot(data = data, x = data$datetime, bin = 60*60*24)
ggsave('dailyOverYear.png')

oneResident <- data[data$user_id == 93292,]
str(oneResident)
summary(oneResident$de_pat_id)
qplot(data = oneResident, x = oneResident$datetime, bin = 60*60*24)
write.csv(oneResident, "oneResident.csv")

onePatient <- data[data$de_pat_id == 66558,]
str(onePatient)
qplot(data = onePatient, x = onePatient$datetime, bin = 10)
write.csv(onePatient , "onePatient.csv")



ggplot(data = data, aes(x = time, group = weekday)) + geom_density(fill = weekday, bin = 60*5) 





data$weekday <- factor(data$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data = data, aes(x = time, fill = weekday, group = weekday)) + geom_density(position = "stack", alpha = 0.4) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Relative Frequency")

ggsave("24hourperiod-ByWeekday-LighterColor.png")


data$weekday <- factor(data$weekday, levels = c("Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
ggplot(data = data, aes(x = time, fill = weekday, group = weekday)) + geom_bar( bin = 60*5) + scale_x_datetime(breaks = date_breaks("1 hour"),
  minor_breaks = date_breaks("1 hour"), labels = date_format("%H:%M")) + labs(x = "Time of Day", y = "Number Of Actions Every 5 Minutes")

ggsave("24hourperiod-ByWeekday-BarGraph.png")



newdata <- read.csv("StanfordWardsResidentMinMaxWithout17162or17102-StanfordOnly-FinallySorted-TimeSensitive.-WithCallDay.csv")
newdata$firstActionTime <- strptime(newdata$firstAction, "%H:%M:%OS")
newdata$lastActionTime <- strptime(newdata$lastAction, "%H:%M:%OS")
newdata$difference2 <- as.numeric( newdata$lastActionTime  - newdata$firstActionTime )

newdata <- newdata[newdata$difference2 > 10000,]
newdata <- newdata[newdata$difference2 < 70000,]

qplot(newdata$difference2, fill = newdata$type, bin = 60 * 5)
ggsave("TimeInHospital.png")

qplot(newdata$lastActionTime, fill = newdata$type, binwidth = 60*5)
ggsave("timeOfLastAction.png")