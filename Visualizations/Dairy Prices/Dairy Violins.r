
#Requisite libraries
library(ggplot2)
library(scales)
library(plyr)

#Data from http://ggplot2.tidyverse.org/reference/facet_grid.html
data1 <- read.csv("Datamart-Export_DY_WK100-40 Pound Block Cheddar Cheese Prices and Sales_20170829_122601.csv")
data2 <- read.csv("Datamart-Export_DY_WK100-Butter Prices and Sales_20170829_122601.csv")

#Formatting character entries
data1$Sales <- gsub(",", "",data1$Sales)
data1$Sales <- as.numeric(data1$Sales)

#Subsetting
Curtail <- data1[c(1:265),c(1,5)]

#One year up to the current date
Curtail <- Curtail [c(1:255),]

#Reordering months by chronology - tough to do without manual post setting.
Curtail$Month[1:15] <- "l"
Curtail$Month[16:40] <- "k"
Curtail$Month[41:60] <- "j"
Curtail$Month[61:80] <- "i"
Curtail$Month[81:105] <- "h"
Curtail$Month[106:125] <- "g"
Curtail$Month[126:145] <- "f"
Curtail$Month[146:165] <- "e"
Curtail$Month[166:190] <- "d"
Curtail$Month[191:210] <- "c"
Curtail$Month[211:235] <- "b"
Curtail$Month[236:255] <- "a"
Monthtitles <- c("Sept. 2016","Oct. 2016","Nov. 2016","Dec. 2016","Jan. 2017","Feb. 2017","March 2017","Apr. 2017","May 2017","June 2017","July 2017","Aug. 2017")

#Some heavy readjustment of labels, for ggplot's sensitivities.
Curtail$ButterSales <- data2$Sales[1:255]
Curtail$ButterSales <- gsub(",", "",Curtail$ButterSales)
Curtail$ButterSales <- as.numeric(Curtail$ButterSales)
medians <- data.frame(c(0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,0,0,0))
colnames(medians) <- c("ChedMedian","ButterMedian")

#Ordering cheese sales dates
Curtail$ChedMedian[1:15] <- median(Curtail$Sales[Curtail$Month=="l"])
Curtail$ChedMedian[16:40] <- median(Curtail$Sales[Curtail$Month=="k"])
Curtail$ChedMedian[41:60] <- median(Curtail$Sales[Curtail$Month=="j"])
Curtail$ChedMedian[61:80] <- median(Curtail$Sales[Curtail$Month=="i"])
Curtail$ChedMedian[81:105] <- median(Curtail$Sales[Curtail$Month=="h"])
Curtail$ChedMedian[106:125] <- median(Curtail$Sales[Curtail$Month=="g"])
Curtail$ChedMedian[126:145] <- median(Curtail$Sales[Curtail$Month=="f"])
Curtail$ChedMedian[146:165] <- median(Curtail$Sales[Curtail$Month=="e"])
Curtail$ChedMedian[166:190] <- median(Curtail$Sales[Curtail$Month=="d"])
Curtail$ChedMedian[191:210] <- median(Curtail$Sales[Curtail$Month=="c"])
Curtail$ChedMedian[211:235] <- median(Curtail$Sales[Curtail$Month=="b"])
Curtail$ChedMedian[236:255] <- median(Curtail$Sales[Curtail$Month=="a"])

#And butter
Curtail$ButterMedian[1:15] <- median(Curtail$ButterSales[Curtail$Month=="l"])
Curtail$ButterMedian[16:40] <- median(Curtail$ButterSales[Curtail$Month=="k"])
Curtail$ButterMedian[41:60] <- median(Curtail$ButterSales[Curtail$Month=="j"])
Curtail$ButterMedian[61:80] <- median(Curtail$ButterSales[Curtail$Month=="i"])
Curtail$ButterMedian[81:105] <- median(Curtail$ButterSales[Curtail$Month=="h"])
Curtail$ButterMedian[106:125] <- median(Curtail$ButterSales[Curtail$Month=="g"])
Curtail$ButterMedian[126:145] <- median(Curtail$ButterSales[Curtail$Month=="f"])
Curtail$ButterMedian[146:165] <- median(Curtail$ButterSales[Curtail$Month=="e"])
Curtail$ButterMedian[166:190] <- median(Curtail$ButterSales[Curtail$Month=="d"])
Curtail$ButterMedian[191:210] <- median(Curtail$ButterSales[Curtail$Month=="c"])
Curtail$ButterMedian[211:235] <- median(Curtail$ButterSales[Curtail$Month=="b"])
Curtail$ButterMedian[236:255] <- median(Curtail$ButterSales[Curtail$Month=="a"])

#Graph
ggplot(data=Curtail, aes(x=Month,y=Sales)) + 
geom_violin(adjust = 1/2,width = 1,aes(fill="Cheddar"),width = 1,adjust = 1/2,
	kernel="rectangular") +
geom_violin(aes(x=Month,y=ButterSales,fill="Butter"),width = 1.3,
	kernel="rectangular") +
stat_summary(fun.y=median, geom="smooth", aes(group=1), lwd=1, alpha=.09, color="#FF9900",fill="#FF9999") + 
stat_summary(aes(y=ButterSales,group=1),fun.y=median, geom="smooth", lwd=1,alpha=.09,color="#FFFF99") +
theme(axis.text.x = element_text(size=10),
	legend.position = c(0.5, 0.5),
	panel.background = element_rect(fill = "#CCCCCC")) +
scale_y_continuous("Sales",breaks=c(seq(2000000,16000000,500000)),labels = dollar) +
scale_x_discrete(labels=Monthtitles) +
ggtitle("USDA Cheddar and Butter Sales Reported Closing Sales Per Day Per Month Over One Year") +
scale_fill_manual(values=c("#FFFF99", "#FF9900"),name="    Dairy Products \nMedians Connected",
	breaks=c("Cheddar","Butter")) +
coord_cartesian(ylim=c(2400000,15500000))

#Export
ggsave("Dairy Violins.png", plot = last_plot(), device = "png", 
       path = "~path",
       scale = 2, width = 6.5, height = 3, units = "in",
       dpi = 150, limitsize = TRUE)

