setwd("C:\\Users\\David\\Dropbox\\Stanford-IM\\Epic Research\\AccessLog2\\Paging Data Analysis")
dir()

library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)
library(ggthemes)




data <- read.csv("Figure1Data.csv")
str(data)


ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*NewRatio), color = NewRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure1-RatioOfGenericToBrandName.png", width = 8, height = 9)


ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*NewRatio), color = NewRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure1-RatioOfGenericToBrandName-ForTheBrandNames.png", width = 8, height = 9)



HighValues <- data[data$RNRatio>100,]

ggplot(data, aes(RNRatio, reorder(Generic.Name, -1*NewRatio), color = RNRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
geom_rug(data = HighValues, aes(RNRatio), sides = 'r', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure2-RatioOfGenericToBrandName-RN.png", width = 8, height = 9)


ggplot(data, aes(RNRatio, reorder(Brand.Name, -1*NewRatio), color = RNRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
geom_rug(data = HighValues, aes(RNRatio), sides = 'r', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure2-RatioOfGenericToBrandName-RN-ForTheBrandNames.png", width = 8, height = 9)





ggplot(data, aes(RXRatio, reorder(Generic.Name, -1*NewRatio), color = RXRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure3-RatioOfGenericToBrandName-RX.png", width = 8, height = 9)


ggplot(data, aes(RXRatio, reorder(Brand.Name, -1*NewRatio), color = RXRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure3-RatioOfGenericToBrandName-RX-ForTheBrandNames.png", width = 8, height = 9)



##### ALL IN ONE GRAPH #####



data <- read.csv("Figure1Data-MeltOccupation.csv")
str(data)
HighValues <- data[data$NewRatio>100,]

ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.75, size = 3) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure4-RatioOfGenericToBrandName.png", width = 8, height = 9)


ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.75, size = 3) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("Figure4-RatioOfGenericToBrandName-ForTheBrandNames.png", width = 8, height = 9)



rawdata <- read.csv("MedicationTextsWithProcessing.csv")
str(rawdata)
table(rawdata$Occupation)

#Nursing
33091 + 334 + 1004 + 13 + 47

#Pharmacy
18646 + 1004 + 13 + 783


###### Updated List of Medications ######



data <- read.csv("Figure1Data-V2.csv")
str(data)

HighValues <- data[data$NewRatio>100,]
LowValues <- data[data$NewRatio<0.01,]

ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*NewRatio), color = NewRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 105), labels = c("0.01", "0.1", "1", "10", "100")) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("V2-Figure1-RatioOfGenericToBrandName.png", width = 8, height = 11)

ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*NewRatio), color = NewRatio)) + geom_point(alpha = 0.75, size = 3) + 
scale_x_log10(limits = c(0.01, 100), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 105), labels = c("0.01", "0.1", "1", "10", "100")) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		labels = c(0.1, 0.5, 1, 5, 10)),
		label.hjust = 12)

ggsave("V2-Figure1-RatioOfGenericToBrandName-ForTheBrandNames.png", width = 8, height = 11)


### All Together ###

data <- read.csv("Figure1Data-V2-MeltOccupation-Top30.csv", stringsAsFactors = FALSE)
str(data)
HighValues <- data[data$NewRatio>100,]
#LowValues <- data[data$NewRatio<0.01,]
data <- data[data$RatioForSort>0.01,]

ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
#geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)

ggsave("V3-Figure4-RatioOfGenericToBrandName.png", width = 7, height = 8)

#data[data$Brand.Name == "Tylenol",]$NewRatio <- 0.01

ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
#geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)

ggsave("V3-ForTheLegend.png", width = 7, height = 8)

ggsave("V3-Figure4-RatioOfGenericToBrandName-ForTheBrandNames.png", width = 7, height = 8)



#data <- read.csv("Figure1Data-V2-Top30.csv", stringsAsFactors = FALSE)

ggplot(data,  aes(reorder(Brand.Name, -1*RatioForSort),Sum/3)) + theme_fivethirtyeight() + 
scale_y_continuous( limits = c(0, 15000), breaks = c(0, 5000, 10000,15000), labels = c("0", "5k", "10k", "15k")) +
geom_bar(stat = "identity") + coord_flip() + labs( x = "Number of Pages")

+ guides(guide_legend(title = "Number of Pages")
ggsave("V3-ForTheBrandNames-AndFrequency.png", width = 3.5, height = 6.7)


t.test(data[data$Type == "Nursing",]$NewRatio, data[data$Type == "Pharmacy",]$NewRatio, paired = TRUE)


data$Type <- relevel(as.factor(data$Type), ref = "Overall")

#data <- read.csv("Figure1Data-V2.csv", stringsAsFactors = FALSE)

data$Brand.Name.NumSyllables <- nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X", tolower( data$Brand.Name ))))
data$Generic.Name.NumSyllables <- nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X", tolower( data$Generic.Name ))))
data$NumSyllablesMoreInGenericName <- data$Generic.Name.NumSyllables - data$Brand.Name.NumSyllables

data$BrandNameNumConsonants <- nchar(gsub("[^@]","",gsub("[^aeiouAEIOU]","@",data$Brand.Name)))
data$BrandNameNumVowels <- nchar(gsub("[^@]","",gsub("[aeiouAEIOU]","@",data$Brand.Name)))
data$BrandNameNchar <- nchar(data$Brand.Name)

data$GenericNameNumConsonants <- nchar(gsub("[^@]","",gsub("[^aeiouAEIOU]","@",data$Generic.Name)))
data$GenericNameNumVowels <- nchar(gsub("[^@]","",gsub("[aeiouAEIOU]","@",data$Generic.Name)))
data$GenericNameNchar <- nchar(data$Generic.Name)

data$NumCharMoreInGenericName <- data$GenericNameNchar - data$BrandNameNchar
data$NumConMoreInGenericName <- data$GenericNameNumConsonants - data$BrandNameNumConsonants 
data$NumVowelsMoreInGenericName <- data$GenericNameNumVowels - data$BrandNameNumVowels 

data$MoreLettersInGeneric <- data$GenericNameNchar - data$BrandNameNchar

model1 <- lm(data = data, NewRatio ~ Frequency + Frequency.1 + BrandNameNumConsonants + Type +
BrandNameNumVowels + BrandNameNchar + GenericNameNumConsonants + GenericNameNumVowels + GenericNameNchar + 
NumSyllablesMoreInGenericName + NumCharMoreInGenericName + Brand.Name.NumSyllables + Generic.Name.NumSyllables, na.action=na.omit)
summary(model1)



model2 <- lm(data = data, NewRatio ~ BrandNameNumConsonants + Type + BrandNameNchar + GenericNameNchar + 
NumSyllablesMoreInGenericName + NumCharMoreInGenericName + Brand.Name.NumSyllables + Generic.Name.NumSyllables)
summary(model2)

data$Type <- relevel(as.factor(data$Type), ref = "Pharmacy")

model2 <- lm(data = data, NewRatio ~ Type + NumSyllablesMoreInGenericName )
summary(model2)


