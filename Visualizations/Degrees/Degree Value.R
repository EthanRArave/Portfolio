#Requisite libraries
library(ggplot2)
library(scales)
library(reshape)

#Data from https://www.kaggle.com/wsj/college-salaries
data1 <- read.csv("degrees-that-pay-back.csv")

#Subsetting
toplot <- data1[c(1:3,5:8)]
toplot <- toplot[order(toplot[,2],decreasing = TRUE),]

#Formatting character entries. 
toplot$Undergraduate.Major <- factor(toplot$Undergraduate.Major, levels=unique(toplot$Undergraduate.Major))
toplot$Starting.Median.Salary <- as.character(toplot$Starting.Median.Salary)
toplot$Starting.Median.Salary <- gsub(",", "", toplot$Starting.Median.Salary)
toplot$Starting.Median.Salary <- gsub("[$]", "", toplot$Starting.Median.Salary)
toplot$Starting.Median.Salary <- as.numeric(toplot$Starting.Median.Salary)
toplot$Mid.Career.Median.Salary <- as.character(toplot$Mid.Career.Median.Salary)
toplot$Mid.Career.Median.Salary <- gsub(",", "", toplot$Mid.Career.Median.Salary)
toplot$Mid.Career.Median.Salary <- gsub("[$]", "", toplot$Mid.Career.Median.Salary)
toplot$Mid.Career.Median.Salary <- as.numeric(toplot$Mid.Career.Median.Salary)

#Instituting a new column entry for % difference between the two values
change <- (toplot$Mid.Career.Median.Salary - toplot$Starting.Median.Salary)/(toplot$Starting.Median.Salary)
change <- signif(change, digits=3)
change <- change*100
change <- round(change,0)
change <- as.character(change)
change <- paste(change,"%",sep='')

#A little extra formatting
X <- c(rep("",50),as.character(change))
x = c(as.character(toplot$Undergraduate.Major))
y1 = toplot$Starting.Median.Salary
y2 = toplot$Mid.Career.Median.Salary
to_plot <- data.frame(x=x,y1=y1,y2=y2)
to_plot$x <- as.character(to_plot$x)
to_plot[14,1] <- "Management Info. Sys."
to_plot <- to_plot[order(to_plot[,2],decreasing = TRUE),]
to_plot$x <- factor(to_plot$x, levels=unique(to_plot$x))
melted <- melt(to_plot, id="x")
bars = c("Median income right out of school","Median income 10 years out of school")
colors<-c("#66CCFF","#3399FF")

#Graph 
ggplot(melted,aes(x=x,y=value,fill=variable)) + 
geom_bar(stat="identity",position="identity", alpha=.37) +
ggtitle ("Degree Value") +
theme(axis.text.x = element_text(angle = 35, hjust = 1),
	legend.position = c(0.8, 0.7),
	panel.background = element_rect(fill = "#FFFFFF"),
	panel.grid.major.x = element_blank(),
	panel.grid.major.y = element_line(size=.14, color="#000000")) +
	scale_y_continuous("Median Annual Income",breaks=c(seq(30000,110000,5000)),labels = dollar) +
	coord_cartesian(ylim=c(30000,110000)) +
	xlab("Degree") +
	theme(axis.title.x = element_text(size = 12, vjust=-.3)) +
	scale_fill_manual(name="Median Income",values=colors,labels=c("10 Years Into Career","Right out of school")) +
	geom_text(aes(label=X),size = 2.5, hjust = 0.5, vjust = -.5, position = "stack") +
	guides(fill = guide_legend(title = "%s = increase from starting to 10 year salary ", 
		title.position = "bottom",element_text(size = 8,angle = 0,face="bold")))

#Export
ggsave("Degree-Value.png", plot = last_plot(), device = "png", 
       path = "path",
       scale = 2, width = 5.5, height = 3, units = "in",
       dpi = 150, limitsize = TRUE)
