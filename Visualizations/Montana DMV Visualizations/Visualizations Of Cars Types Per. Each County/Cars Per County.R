#Requisite libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape)
library(ggthemes)
library(scales)
library(shiny)

#Data personally generated, see readme for notes
Cars <- read.csv("CarsPop.csv",header=TRUE)

#Subsetting data
Vis3dat <- Cars[Cars$Location != "RDC Driver Control",]
Vis3dat$Location <- gsub("County","",Vis3dat$Location)
Vis3dat$Location <- trimws(Vis3dat$Location)
Counts <- count(Vis3dat,Location,CountyPopulation)
colnames(Counts) <- c("Location","CountyPopulation","Vehicles")
Counts$Percap <- Counts$Vehicles/Counts$CountyPopulation
Vis3all <- Vis3dat %>% group_by(Manufacturer) %>% summarise(Count = n())
Vis3dat <- Vis3dat %>% group_by(Location,Manufacturer,CountyPopulation) %>% summarise(Count = n())
Vis3all$Location <-  "All"
Vis3all$CountyPopulation <- sum(as.numeric(unique(Counts$CountyPopulation)))
Vis3all$Vehicles <- sum(as.numeric(unique(Vis3dat$Count)))
Vis3all <- Vis3all[,c(3,1,4,2,5)]

#This was the solution to maintaining zeros across countys that had 0's for the vehicle count for that manufacturer (or vis versa)
Countcheck <- expand.grid(unique(Vis3dat$Manufacturer),unique(Vis3dat$Location))
colnames(Countcheck) <- c("Manufacturer","Location")
Vis3dat <- full_join(Vis3dat,Countcheck)
Vis3dat$Count[is.na(Vis3dat$Count)] <- 0 

#Continuing to subset
Vis3dat$CountyPopulation[is.na(Vis3dat$Count)] <- Counts$CountyPopulation[match(Vis3dat$Location,Counts$Location)]
Vis3dat <- bind_rows(Vis3dat, Vis3all)
Vis3dat$Vehicles <- Counts$Vehicles[match(Vis3dat$Location,Counts$Location)]
Vis3all$Manufacturer <- as.character(Vis3all$Manufacturer)
CountyList <- sort(unique(as.character(Vis3dat$Location)))

#Fitting UI elements for shiny plot
Capita <- list("Count","Percapita")
ui <- fluidPage(
  sidebarPanel(
  selectInput(inputId= "County",
  label="Select County", choices = CountyList, selected="All"),
  radioButtons(inputId= "Type",
  label='Select Net or Percapita Vehicle Display', selected= "Count",
  choices = Capita),width = 3),
  mainPanel(plotlyOutput("plot1")))

#Creating server (output) on shiny elements
server <- function(input, output) {
output$plot1 <- renderPlotly({
dat <- Vis3dat %>% filter(Location == input$County) 
Vis3all <- Vis3all[order(Vis3all$Count,decreasing = TRUE),]
dat$Manufacturer <- ordered(dat$Manufacturer, levels = c(Vis3all$Manufacturer))
dat$Percapita <- (dat$Count/dat$CountyPopulation)
top <- max(range(dat[,input$Type],na.rm=TRUE))
if (top < 1){
gtype <- "Vehicles Per Person"  
step <- round(((top)/15),3)
}
else{
gtype <- "Vehicle Count" 
step <- round(((top)/15),0)
}
Count <- 
plot1 <- 

#These if elses generate the correct graph between count and percapita
#Generating two seperate layers based out y output 
if (input$Type == "Count"){
ggplot(data = dat,aes(x=Manufacturer,y=Count)) + 
geom_bar(stat="identity") +
scale_y_continuous(paste(gtype),breaks=c(seq(0,top,step)),labels = comma) +
theme(axis.text.x = element_text(angle = 35, hjust = .5,size=8.5),plot.margin=unit(c(.5,.5,1.9,.5),"cm"))}
else{
ggplot(data = dat,aes(x=Manufacturer,y=Percapita)) + 
geom_bar(stat="identity") +
scale_y_continuous(paste(gtype),breaks=c(seq(0,top,step)),labels = comma) +
theme(axis.text.x = element_text(angle = 35, hjust = .5,size=8.5),plot.margin=unit(c(.5,.5,1.9,.5),"cm"))}

#Plotting bottom layer
ggplotly(plot1,width = 920, height = 670 ) %>% config(staticPlot = FALSE, displayModeBar = FALSE, workspace = TRUE, sendData = FALSE, displaylogo = FALSE)
})
}
shinyApp(ui = ui, server = server)

