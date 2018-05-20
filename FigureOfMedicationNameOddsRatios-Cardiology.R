#setwd("C:\\Users\\David\\Dropbox\\Stanford-IM\\Epic Research\\AccessLog2\\Paging Data Analysis")
setwd("P:\\PagingDataAnalysis-master")
dir()


#install.packages(c("Rcpp"))

library(ggplot2)
library(plyr)
#library(reshape2)
#library(strptime)
library(stringr)
library(scales)
library(ggthemes)
library(egg)


data <- read.csv("Figure1Data-V2-MeltOccupation-Cardiology.csv", stringsAsFactors = TRUE)
data <- data[data$Total.Count >= 50,]
str(data)
HighValues <- data[data$NewRatio>100,]
LowValues <- data[data$NewRatio<0.01,]
data <- data[data$RatioForSort>0.01 ,]
date <- as.POSIXct(strptime(data$ApprovalDate, "%m/%d/%Y"))
data$OnPatent <- as.factor(data$OnPatent)


data[data$NewRatio < 0.01,]$NewRatio <- 0.0096
data[data$Ratio.lowerbound < 0.01,]$Ratio.lowerbound<- 0.0096

colortext <- rep(1,96)
colortext[11:13] <- 2
colortext[17:18] <- 2
colortext[27] <- 2


#data$Brand.Name.NumSyllables <- nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X", tolower( data$Brand.Name ))))
#data$Generic.Name.NumSyllables <- nchar( gsub( "[^X]", "", gsub( "[aeiouy]+", "X", tolower( data$Generic.Name ))))
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




Year <- ggplot(data, aes(reorder(Brand.Name, -1*RatioForSort),as.Date(date),color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "FDA Approval", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Year 

centerFigure <- ggplot(data, aes(NewRatio, reorder(Generic.Name, -1*RatioForSort), shape = Type, color = OnPatent)) + 
geom_point(alpha = 0.99, size = 3) + scale_shape_manual( values=c(78, 16, 80)) +
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_errorbarh(size = 1, aes(xmin = Ratio.lowerbound, xmax = Ratio.upperbound, height = 0)) +
scale_x_log10(limits = c(0.0095, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_minimal() +
theme(axis.text.y = element_text(hjust = .5, color = colortext, size = 12),  legend.position="none", axis.title.y = element_blank()) + scale_color_manual(values = c("black", "red")) +
labs( x = "Ratio of Mentions by Trade Name vs. Mentions by Generic Name" ,y = "")
centerFigure 


Frequency <- ggplot(data, aes(reorder(Brand.Name, -1*RatioForSort),Total.Count/3, fill  = OnPatent)) + theme_minimal() + 
scale_y_continuous( limits = c(0, 5000), breaks = c(0, 2500, 5000), labels = c("0", "2.5k", "5k")) +
geom_bar(stat = "identity") + coord_flip() + labs( y = "Number of Pages", x = "") + theme(axis.text.y = element_text(hjust = .5, color = colortext, size = 12), legend.position="none") + 
scale_fill_manual(values = c("black", "red"))
Frequency 



#ggarrange(Frequency , centerFigure, nrow = 1, width = c(3, 0.5))


ggarrange(Year, centerFigure, Frequency, nrow = 1,widths = c(0.5, 3, 0.5))

#ggsave("Demo1-ThreePartFigure.png")


Syllables <- ggplot(data, aes(xend = Brand.Name.NumSyllables, x = Generic.Name.NumSyllables, y= reorder(Brand.Name, -1*RatioForSort),
yend = reorder(Brand.Name, -1*RatioForSort), shape = Type, color = OnPatent)) + 
geom_segment(alpha = 0.99, size = 1, arrow = arrow(length = unit(.1, "inches"))) + theme_minimal() + 
theme( legend.position="none", axis.title.y=element_blank(), axis.text.y  = element_blank()) +
 scale_color_manual(values = c("black", "red")) + scale_x_continuous(limits = c(2, 8), breaks = c(2,4,6,8)) + labs(x = "Difference in Syllables")
Syllables 



ggarrange(Syllables,Year, centerFigure,  Frequency, nrow = 1, ncol = 4, widths = c(0.75, 0.5, 3,  0.5))


ggarrange(Year, centerFigure, Syllables, nrow = 1, ncol = 3, widths = c( 0.5, 3,  0.75))

data$logPrice <- log(data$AverageWholeSaleUnitPrice)
Price <- ggplot(data, aes(reorder(Brand.Name, -1*RatioForSort), AverageWholeSaleUnitPrice, color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "Average Unit Price", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Price 


ggarrange( Year, Syllables, centerFigure, Frequency, Price, nrow = 1,widths = c(0.5, 0.5, 3, 0.5, 0.5))









# SortByWholeSalePrice

Year <- ggplot(data, aes(reorder(Brand.Name, AverageWholeSaleUnitPrice),as.Date(date),color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "FDA Approval", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Year 

centerFigure <- ggplot(data, aes(NewRatio, reorder(Generic.Name, AverageWholeSaleUnitPrice), shape = Type, color = OnPatent)) + 
geom_point(alpha = 0.99, size = 3) + scale_shape_manual( values=c(78, 16, 80)) +
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_errorbarh(size = 1, aes(xmin = Ratio.lowerbound, xmax = Ratio.upperbound, height = 0)) +
scale_x_log10(limits = c(0.0095, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_minimal() +
theme(axis.text.y = element_text(hjust = .5, color = ColorText ),  legend.position="none", axis.title.y = element_blank()) + scale_color_manual(values = c("black", "red")) +
labs( x = "Ratio of Mentions by Trade Name vs. Mentions by Generic Name" ,y = "")
centerFigure 

Frequency <- ggplot(data, aes(reorder(Brand.Name, AverageWholeSaleUnitPrice),Total.Count/3, fill  = OnPatent)) + theme_minimal() + 
scale_y_continuous( limits = c(0, 5000), breaks = c(0, 2500, 5000), labels = c("0", "2.5k", "5k")) +
geom_bar(stat = "identity") + coord_flip() + labs( y = "Number of Pages", x = "") + theme(axis.text.y = element_text(hjust = .5, color = ColorText ), legend.position="none") + 
scale_fill_manual(values = c("black", "red"))
Frequency 


Syllables <- ggplot(data, aes(xend = Brand.Name.NumSyllables, x = Generic.Name.NumSyllables, y= reorder(Brand.Name, AverageWholeSaleUnitPrice),
yend = reorder(Brand.Name, AverageWholeSaleUnitPrice), shape = Type, color = OnPatent)) + 
geom_segment(alpha = 0.99, size = 1, arrow = arrow(length = unit(.1, "inches"))) + theme_minimal() + 
theme( legend.position="none", axis.title.y=element_blank(), axis.text.y  = element_blank()) +
 scale_color_manual(values = c("black", "red")) + scale_x_continuous(limits = c(2, 8), breaks = c(2,4,6,8)) + labs(x = "Difference in Syllables")
Syllables 


data$logPrice <- log(data$AverageWholeSaleUnitPrice)
Price <- ggplot(data, aes(reorder(Brand.Name, AverageWholeSaleUnitPrice), AverageWholeSaleUnitPrice, color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "Average Unit Price", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Price 


ggarrange( Year, Syllables, centerFigure, Frequency, Price, nrow = 1,widths = c(0.5, 0.5, 3, 0.5, 0.5))








colortext <- rep(1,96)
colortext[10:12] <- 2
colortext[4] <- 2
colortext[2] <- 2
colortext[18] <- 2



# SortByFrequency

Year <- ggplot(data, aes(reorder(Brand.Name, Total.Count),as.Date(date),color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "FDA Approval", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Year 

centerFigure <- ggplot(data, aes(NewRatio, reorder(Generic.Name, Total.Count), shape = Type, color = OnPatent)) + 
geom_point(alpha = 0.99, size = 3) + scale_shape_manual( values=c(78, 16, 80)) +
geom_rug(data = HighValues, aes(NewRatio), sides = 'r', size = 1.2) + 
geom_errorbarh(size = 1, aes(xmin = Ratio.lowerbound, xmax = Ratio.upperbound, height = 0)) +
scale_x_log10(limits = c(0.0095, 105), breaks = c(0.01, 0.1, 1, 10, 100), labels = c("0.01", "0.1", "1", "10", "100")) + theme_minimal() +
theme(axis.text.y = element_text(hjust = .5, color = colortext ),  legend.position="none", axis.title.y = element_blank()) + scale_color_manual(values = c("black", "red")) +
labs( x = "Ratio of Mentions by Trade Name vs. Mentions by Generic Name" ,y = "")
centerFigure 

Frequency <- ggplot(data, aes(reorder(Brand.Name, Total.Count),Total.Count/3, fill  = OnPatent)) + theme_minimal() + 
scale_y_continuous( limits = c(0, 5000), breaks = c(0, 2500, 5000), labels = c("0", "2.5k", "5k")) +
geom_bar(stat = "identity") + coord_flip() + labs( y = "Number of Pages", x = "") + theme(axis.text.y = element_text(hjust = .5, color = colortext ), legend.position="none") + 
scale_fill_manual(values = c("black", "red"))
Frequency 


Syllables <- ggplot(data, aes(xend = Brand.Name.NumSyllables, x = Generic.Name.NumSyllables, y= reorder(Brand.Name, Total.Count),
yend = reorder(Brand.Name, Total.Count), shape = Type, color = OnPatent)) + 
geom_segment(alpha = 0.99, size = 1, arrow = arrow(length = unit(.1, "inches"))) + theme_minimal() + 
theme( legend.position="none", axis.title.y=element_blank(), axis.text.y  = element_blank()) +
 scale_color_manual(values = c("black", "red")) + scale_x_continuous(limits = c(2, 8), breaks = c(2,4,6,8)) + labs(x = "Difference in Syllables")
Syllables 


data$logPrice <- log(data$AverageWholeSaleUnitPrice)
Price <- ggplot(data, aes(reorder(Brand.Name, Total.Count), AverageWholeSaleUnitPrice, color  = OnPatent)) + theme_minimal() + 
geom_point(stat = "identity", size = 2) + coord_flip() + labs( y = "Average Unit Price", x= "") + theme(axis.text.y  = element_blank(), legend.position="none") + 
scale_color_manual(values = c("black", "red"))
Price 


ggarrange( Year, Syllables, centerFigure, Frequency, Price, nrow = 1,widths = c(0.5, 0.5, 3, 0.5, 0.5))





