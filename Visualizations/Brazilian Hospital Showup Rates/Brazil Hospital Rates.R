#Requisite libraries
library(gridExtra) 
library(shiny)
library(ggthemes)
library(plotly)
library(scales)
library(ggplot2)

#Data from https://www.kaggle.com/joniarroba/noshowappointments/data
data1 <- read.csv("Hospitals May-2016 CLEAN.csv")

#This dataset is all in categorical values, which need to be modified. This code was written as part of a group project, I would
#currently handle this issue with a dplyr loop.
Neighborhood_list <- levels(data1$Neighbourhood) 
data1$SMS_received[1] <- 0
valuelist=list()
for (i in Neighborhood_list){
if (sum(data1$Neighbourhood==i) > 1500)
{
valuelist <- append(valuelist,i)
}
}

#Here's our new set
Parsedata <- data1[data1$Neighbourhood %in% valuelist,]

#Making subset data of the parsed data for the top layer
Newdata <- data.frame(Neighborhood=c(unique(as.character(Parsedata$Neighbourhood))),"PeopleNum"=c(0),"NoShow"=c(0),"SMS"=c(0),"MeanAge"=c(0))
for (i in unique(as.character(Parsedata$Neighbourhood))){
print(i)
Newdata$PeopleNum[Newdata$Neighborhood==as.character(i)] <- sum(Parsedata$Neighbourhood==i)
Newdata$NoShow[Newdata$Neighborhood==as.character(i)] <- sum(Parsedata$Neighbourhood==i & Parsedata$No.show == "Yes")
Newdata$SMS[Newdata$Neighborhood==as.character(i)] <- sum(Parsedata$Neighbourhood==i & Parsedata$SMS_received == 1)
Newdata$MeanAge[Newdata$Neighborhood==as.character(i)] <- mean(Parsedata$Age[Parsedata$Neighbourhood==i])
}
Newdata$ID <- seq(1,28)
Parsedata$NID <- 0 
for(i in unique(Newdata$Neighborhood)){
  d <- Newdata$ID[Newdata$Neighborhood==i]
  Parsedata$NID[Parsedata$Neighbourhood==i] <- (d-1)
}
Newdata$x <- Newdata$NoShow/Newdata$PeopleNum
Newdata$y <- Newdata$SMS/Newdata$PeopleNum
Parsedata$Show <- 0 
nbhood <- NULL
disphood <- NULL

#Shiny app creation, starting with input values, tying up layers.
ui <- fluidPage(  
  titlePanel("Brasil Regional Hospitals"),
  plotlyOutput("plot1"),
  plotOutput("click"))
label = "%SMS Recieved Prior to Appointment"
server <- function(input, output) {

  #Layer 1
 output$plot1 <- renderPlotly({
    plot1 <- ggplot(Newdata,aes(x*100,y,size=PeopleNum,color=MeanAge, text = paste("No Show: ", round(x*100, 2), "%", "</br>", "</br>SMS Sent: ", round(y*100, 2), "%", "</br>Neighborhood: ", Neighborhood, "</br>Patient Num.: ", PeopleNum, "</br>Average Age: ", round(MeanAge)))) +
      geom_point() +
      scale_color_continuous(low="#33FFFF",high="#000099",limits=c(29,45)) +
      scale_size_continuous(range = c(5, 25)) +
      scale_y_continuous("%SMS Recieved Prior to Appointment",breaks=c(seq(.12,.45,.01)),labels = function(label) sprintf('%8.2s',percent(label))) +
      scale_x_continuous("%No Shows",breaks=c(seq(0,500,.5))) +
      ggtitle("% of Doctor's Appointments Missed In Neighborhood Hospitals in Brasil Plotted vs. % SMS Sent Prior to Visit \n      Size = Population")
    ggplotly(plot1, tooltip = "text") %>% 
    config(staticPlot = FALSE, displayModeBar = FALSE, workspace = TRUE, sendData = FALSE, displaylogo = FALSE)

  #Layer 2 - draws from click city using the NID column. 
  })
output$click <- renderPlot({
    nbhood <<- event_data("plotly_click")
    disphood <- Parsedata$Neighbourhood[Parsedata$NID==nbhood$pointNumber]
    p1 <- ggplot(data=Parsedata[Parsedata$NID==nbhood$pointNumber,],aes(x=Age,y=Hipertension,fill=No.show)) +
      geom_histogram(stat="identity") + 
      scale_fill_manual(values = c("#33CC33","#CC0000"), labels = c("Show", "No-Show"), name = c("Appointment Outcome")) +
      scale_x_continuous("Age Of Patient",breaks=c(seq(5,100,5))) +
      scale_y_continuous("Hypertension Rates",breaks=c(seq(0,100,2))) + 
      coord_cartesian(xlim=c(5,100))
    p2 <- ggplot(data=Parsedata[Parsedata$NID==nbhood$pointNumber,],aes(x=Age,y=Diabetes,fill=No.show)) +
      geom_histogram(stat="identity") +
      scale_fill_manual(values = c("#33CC33","#CC0000"), labels = c("Show", "No-Show"), name = c("Appointment Outcome")) +
      scale_x_continuous("Age Of Patient",breaks=c(seq(5,100,5))) +
      scale_y_continuous("Diabetes Rates",breaks=c(seq(0,100,2))) + 
      coord_cartesian(xlim=c(5,100))
    p3 <- ggplot(data=Parsedata[Parsedata$NID==nbhood$pointNumber,],aes(x=Age,y=Alcoholism,fill=No.show)) +
      geom_histogram(stat="identity") +
      scale_fill_manual(values = c("#33CC33","#CC0000"), labels = c("Show", "No-Show"), name = c("Appointment Outcome")) +
      scale_x_continuous("Age Of Patient",breaks=c(seq(5,100,5))) +
      scale_y_continuous("Alcoholism Rates",breaks=c(seq(0,100,2))) + 
      coord_cartesian(xlim=c(5,100))
    p4 <- ggplot(data=Parsedata[Parsedata$NID==nbhood$pointNumber,],aes(x=Age,y=Handcap,fill=No.show)) +
      geom_histogram(stat="identity") +
      scale_fill_manual(values = c("#33CC33","#CC0000"), labels = c("Show", "No-Show"), name = c("Appointment Outcome")) +
      scale_x_continuous("Age Of Patient",breaks=c(seq(5,100,5))) +
      scale_y_continuous("Other Handicap Rates",breaks=c(seq(0,100,2))) + 
      coord_cartesian(xlim=c(5,100))
    grid.arrange(p1,p2,p3,p4, nrow = 2,top = paste(disphood,"Region Ailment Distributions"))

  })
}
shinyApp(ui, server)





