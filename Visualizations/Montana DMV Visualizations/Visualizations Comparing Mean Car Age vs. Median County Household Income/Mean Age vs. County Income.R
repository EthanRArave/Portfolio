#Requisite libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(ggthemes)
library(scales)
library(shiny)

#Data personally generated, see readme for notes. 
Cars <- read.csv("CarsPop.csv",header=TRUE)

#Subsetting data
Vis2dat <- Cars[Cars$Location != "RDC Driver Control",]
Carcount <- count(Vis2dat,Location)
Vis2dat$Model_Age <- (2018 - Vis2dat$Year)
Vis2dat$Location <- gsub("County","",Vis2dat$Location)
colnames(Carcount) <- c("Location","veh") 

#Creating category of passenger car, critical to the visualization.
pass = c("Ford","Luxury","Daimler","General Motors","Chrysler","Mitsubishi","Toyota","Hyundai","Audi","Honda","Volkswagen","Sport Vehicle","BMW","Nissan","SUV","Mazda","Volvo","Renault","AMC","Saab","Antique")
Vis2dat$Passenger = 0
Vis2dat$Passenger[Vis2dat$Manufacturer %in% pass] <- 1
Vis2Pass  <- Vis2dat %>% group_by(Location,MedianIncome,CountyPopulation,Passenger) %>% summarise(mean = mean(Model_Age, na.rm = TRUE))
Vis2dat <- Vis2dat %>% group_by(Location,MedianIncome,CountyPopulation) %>% summarise(mean = mean(Model_Age, na.rm = TRUE))
Vis2dat[5] <- Carcount[2]
Vis2dat$Normveh <- (Vis2dat$veh/Vis2dat$CountyPopulation) 

#This is for a very strange work around for getting hovertext to opperate in this layered environment.
odd_indexes<-seq(1,112,2)
even_indexes<-seq(2,112,2)

#Creating and storing ggplot object 1 - luxury vehicles only
plot1 <- ggplot(data = Vis2dat,aes(y=mean,x=MedianIncome)) + 
geom_point() +
scale_x_continuous("Median Household Income",breaks=c(seq(30000,80000,5000)),labels = dollar) +
scale_y_continuous("Mean Car Age",breaks=c(seq(8,14,.25))) +
theme(axis.text.x = element_text(size=9),plot.margin=unit(c(.5,.5,1.5,.5),"cm")) +
geom_smooth(method = lm) +
annotate("text", label = "R2-Adj = .3011 ", x = 34500, y = 8.1, size = 3, colour = "black") +
ggtitle("Montana Counties' Vehicle Mean Age Vs. Houshold Income. <br> Luxury Vehicles Only")
plot1 <- plotly_build(plot1)

#Using hovertext work around
plot1$x$data[[1]]$text <- paste(Vis2dat$Location, "County","<br>",
                           "Median Household Income:", dollar(Vis2dat$MedianIncome), "<br>",
                           "Mean Vehicle Age:", round(Vis2dat$mean,1),"years old", "<br>",
                           round(Vis2dat$Normveh,2), "Vehicles per resident")


#Creating ggplot object for second plot showing split between luxury and passanger vehicles
ggplotly(plot1) %>% config(staticPlot = FALSE, displayModeBar = FALSE, workspace = 
TRUE, sendData = FALSE, displaylogo = FALSE)
plot2 <- ggplot(data = Vis2Pass,aes(y=mean,x=MedianIncome,color=factor(Passenger))) + 
geom_point() +
scale_x_continuous("Median Household Income",breaks=c(seq(30000,80000,5000)),labels = dollar) +
scale_y_continuous("Mean Car Age",breaks=c(seq(8,14,.25))) +
theme(axis.text.x = element_text(angle = 30, hjust = 1,size=9),plot.margin=unit(c(.5,.5,1.5,.5),"cm")) +
stat_smooth(method = lm, formula= y ~ x,se = FALSE) +
scale_color_discrete(name="Passenger <br> Vehicles",labels=c("Passenger","Non")) +
annotate("text", label = "R2-Adj = .1922 ", x = 34000, y = 8.1, size = 3, colour = "red") +
annotate("text", label = "R2-Adj = .2492 ", x = 34000, y = 8.3, size = 3, colour = "blue") +
ggtitle("Montana Counties' Vehicle Mean Age Vs. Houshold Income. <br> Hover for information")
plot2 <- plotly_build(plot2)

#Fixing second layer hovertext
plot2$x$data[[1]]$text <- paste(Vis2Pass$Location[odd_indexes], "County","<br>",
                           "Median Household Income:", dollar(Vis2Pass$MedianIncome[odd_indexes]), "<br>",
                           "Mean Vehicle Age:", round(Vis2Pass$mean[odd_indexes],1),"years old", "<br>")
plot2$x$data[[2]]$text <- paste(Vis2Pass$Location[even_indexes], "County","<br>",
                           "Median Household Income:", dollar(Vis2Pass$MedianIncome[even_indexes]), "<br>",
                           "Mean Vehicle Age:", round(Vis2Pass$mean[even_indexes],1),"years old", "<br>")

#Plotting
ggplotly(plot2) %>% config(staticPlot = FALSE, displayModeBar = FALSE, workspace = 
TRUE, sendData = FALSE, displaylogo = FALSE)




