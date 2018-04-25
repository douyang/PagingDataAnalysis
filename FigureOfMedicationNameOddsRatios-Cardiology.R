setwd("C:\\Users\\David\\Dropbox\\Stanford-IM\\Epic Research\\AccessLog2\\Paging Data Analysis")
dir()


#install.packages(c("Rcpp"))

library(ggplot2)
library(plyr)
library(reshape2)
library(strptime)
library(stringr)
library(scales)
library(ggthemes)


### EP ###

data <- read.csv("Figure1Data-V2-MeltOccupation-Cardiology.csv", stringsAsFactors = TRUE)
data <- data[data$Category2 == levels(data$Category2)[1],]
str(data)
HighValues <- data[data$NewRatio>100,]
LowValues <- data[data$NewRatio<0.01,]
#data <- data[data$RatioForSort>0.01 & data$RatioForSort<100  ,]

ggplot(data,  aes(reorder(Brand.Name, -1*RatioForSort),Total.Count/3)) + theme_fivethirtyeight() + 
scale_y_continuous( limits = c(0, 6000), breaks = c(0, 2000, 4000,6000), labels = c("0", "2k", "4k", "6k")) +
geom_bar(stat = "identity") + coord_flip() + labs( x = "Number of Pages")
ggsave("Cardiology-EP-NumberOfPages.png", height = 2.5, width = 3)


ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)


#ggsave("Cardiology-EP-RatioOfGenericToBrandName.png", width = 11, height = 4)


ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)

#ggsave("Cardiology-EP-ForTheLegend.png", width = 11, height = 4)





#### Interventional ####


data <- read.csv("Figure1Data-V2-MeltOccupation-Cardiology.csv", stringsAsFactors = TRUE)
data <- data[data$Category2 == levels(data$Category2)[2],]
str(data)
HighValues <- data[data$NewRatio>100,]
LowValues <- data[data$NewRatio<0.01,]
#data <- data[data$RatioForSort>0.01 & data$RatioForSort<100  ,]

ggplot(data,  aes(reorder(Brand.Name, -1*RatioForSort),Total.Count/3)) + theme_fivethirtyeight() + 
scale_y_continuous( limits = c(0, 6000), breaks = c(0, 2000, 4000,6000), labels = c("0", "2k", "4k", "6k")) +
geom_bar(stat = "identity") + coord_flip() + labs( x = "Number of Pages")
ggsave("Cardiology-Interventional-NumberOfPages.png", height = 2.5, width = 2)

ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)


ggsave("Cardiology-Interventional-RatioOfGenericToBrandName.png", width = 11, height = 4)


ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)

ggsave("Cardiology-Interventional-ForTheLegend.png", width = 11, height = 4)












#### Heart Failure ####


data <- read.csv("Figure1Data-V2-MeltOccupation-Cardiology.csv", stringsAsFactors = TRUE)
data <- data[data$Category2 == levels(data$Category2)[3],]
str(data)
HighValues <- data[data$NewRatio>100,]
LowValues <- data[data$NewRatio<0.01,]
#data <- data[data$RatioForSort>0.01 & data$RatioForSort<100  ,]




ggplot(data,  aes(reorder(Brand.Name, -1*RatioForSort),Total.Count/3)) + theme_fivethirtyeight() + 
scale_y_continuous( limits = c(0, 6000), breaks = c(0, 2000, 4000,6000), labels = c("0", "2k", "4k", "6k")) +
geom_bar(stat = "identity") + coord_flip() + labs( x = "Number of Pages")
ggsave("Cardiology-HF-NumberOfPages.png", height = 8, width = 3)


ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)


ggsave("Cardiology-HF-RatioOfGenericToBrandName.png", width = 11.6, height = 8)


ggplot(data, aes(NewRatio, reorder(Brand.Name, -1*RatioForSort), color = NewRatio, shape = Type)) + 
geom_point(alpha = 0.99, size = 4) + scale_shape_manual( values=c(78, 16, 80))+
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
scale_x_log10(limits = c(0.01, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_fivethirtyeight() + 
scale_color_gradient2(midpoint = 1, low= "blue", mid="black", high= "red", trans = "log", 
breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100"), guide = guide_legend()) + 
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_rug(data = LowValues, aes(NewRatio), sides = 'l', size = 1.2) + 
guides(color = guide_legend(title = "Ratio of Generic Name to Brand Name", 
		title.position = "top", 
		title.hjust = 0, 
		breaks = c(0.01, 0.1, 1, 10, 100)),
		label.hjust = 12)

ggsave("Cardiology-HF-ForTheLegend.png", width = 11.6, height = 8)





t.test(data[data$Type == "Nursing",]$NewRatio , data[data$Type == "Pharmacy",]$NewRatio , paired = TRUE)

# p = 0.022 nursing > pharmacy



data <- read.csv("CardsDrugsDates-V3.csv", stringsAsFactors = FALSE)


data$Type <- relevel(as.factor(data$Type), ref = "Overall")

data$date <- as.Date(strptime(data$Date.of.approval, "%m/%d/%Y"))

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

model1 <- lm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + On.Patent. + date + Category + Category2 + BrandNameNumConsonants +
BrandNameNumVowels + BrandNameNchar + GenericNameNumConsonants + GenericNameNumVowels + GenericNameNchar + 
NumSyllablesMoreInGenericName + NumCharMoreInGenericName + Brand.Name.NumSyllables + Generic.Name.NumSyllables, na.action=na.omit)
summary(model1)


model1 <- lm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + On.Patent. + date + Category + Category2 + 
NumSyllablesMoreInGenericName , na.action=na.omit)
summary(model1)


exp(cbind(OR = coef(model1), confint(model1)))


model2 <- lm(data = data, Ratio.Of.Brand.Generic ~ BrandNameNumConsonants + On.Patent. + date + Category2 + NumSyllablesMoreInGenericName)
summary(model2)



data$Type <- relevel(as.factor(data$Type), ref = "Pharmacy")

model2 <- lm(data = data, Ratio.Of.Brand.Generic ~ Type + NumSyllablesMoreInGenericName )
summary(model2)














ggsave("V3-Figure4-RatioOfGenericToBrandName-ForTheBrandNames.png", width = 7, height = 8)

#data <- read.csv("Figure1Data-V2-Top30.csv", stringsAsFactors = FALSE)

ggplot(data,  aes(reorder(Brand.Name, -1*RatioForSort),Sum/3)) + theme_fivethirtyeight() + 
scale_y_continuous( limits = c(0, 15000), breaks = c(0, 5000, 10000,15000), labels = c("0", "5k", "10k", "15k")) +
geom_bar(stat = "identity") + coord_flip() + labs( x = "Number of Pages")

+ guides(guide_legend(title = "Number of Pages")
ggsave("V3-ForTheBrandNames-AndFrequency.png", width = 3.5, height = 6.7)





#data <- read.csv("Figure1Data-V2.csv", stringsAsFactors = FALSE)







# Multivariate Analysis 


data <- read.csv("CardsDrugsDates-V4-cost.csv", stringsAsFactors = FALSE)

data$datetime <- strptime(data$Date.of.approval, "%m/%d/%Y")
data$year <- data$datetime$year + 1900
data$datetime <- as.POSIXct(data$datetime)




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



model1 <- glm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + datetime + Generic.Cost + On.Patent. + Category)
summary(model1)

model1 <- glm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + datetime + Generic.Cost + Category + Brand.Name.NumSyllables)
summary(model1)


model1 <- glm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + datetime + Generic.Cost + + On.Patent. + NumSyllablesMoreInGenericName )
summary(model1)

model1 <- glm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + datetime + Generic.Cost + Category2 + NumSyllablesMoreInGenericName )
summary(model1)

model1 <- glm(data = data, Ratio.Of.Brand.Generic ~ Total.Count + datetime + Generic.Cost + Category2 + MoreLettersInGeneric )
summary(model1)



data$moreBrand <- data$Ratio.Of.Brand.Generic > 1


model1 <- glm(data = data, moreBrand  ~ Total.Count + datetime + Generic.Cost + On.Patent. + Category2)
summary(model1)

model1 <- glm(data = data, moreBrand ~ Total.Count + datetime + Generic.Cost + Category + Brand.Name.NumSyllables)
summary(model1)


model1 <- glm(data = data, moreBrand  ~ Total.Count + datetime + Generic.Cost +  On.Patent. + NumSyllablesMoreInGenericName )
summary(model1)

model1 <- glm(data = data, moreBrand  ~ Total.Count + datetime + Generic.Cost + Category2 + MoreLettersInGeneric )
summary(model1)


data$hundredpages <- data$Total.Count / 100

finalModel <- glm(data = data, moreBrand  ~ hundredpages + year + Generic.Cost + On.Patent. + Category + NumSyllablesMoreInGenericName )
summary(finalModel )

exp(cbind(OR = coef(finalModel ), confint(finalModel )))


finalModel <- glm(data = data, moreBrand  ~ hundredpages + year + Generic.Cost + On.Patent. + Category + NumSyllablesMoreInGenericName )
summary(finalModel )Brand

exp(cbind(OR = coef(finalModel ), confint(finalModel )))