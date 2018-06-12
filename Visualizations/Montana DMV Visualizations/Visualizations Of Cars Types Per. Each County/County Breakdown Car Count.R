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
Vis4dat <- Cars[Cars$Location != "RDC Driver Control",]
Vis4dat$Location <- gsub("County","",Vis4dat$Location)
Vis4dat$Location <- trimws(Vis4dat$Location)
Counts <- count(Vis4dat,Location,CountyPopulation)
colnames(Counts) <- c("Location","CountyPopulation","Count")
Vis4dat <- Vis4dat %>% group_by(Location,Manufacturer,CountyPopulation) %>% summarise(Count = n())
Counts$Manufacturer <- "All"
Counts <- Counts[,c(1,4,2,3)]

#Listing Counts
Countcheck <- expand.grid(unique(Vis4dat$Manufacturer),unique(Vis4dat$Location))
colnames(Countcheck) <- c("Manufacturer","Location")
Vis4dat <- full_join(Vis4dat,Countcheck)
Vis4dat$Count[is.na(Vis4dat$Count)] <- 0 
Vis4dat$CountyPopulation[is.na(Vis4dat$Count)] <- Counts$CountyPopulation[match(Vis4dat$Location,Counts$Location)]
Vis4dat <- bind_rows(Vis4dat, Counts)

#Manufacturer List
ManufacturerList <- sort(unique(as.character(Vis4dat$Manufacturer)))
Capita <- list("Count","Percapita")

#Establishing plotly ui
ui <- fluidPage(
  sidebarPanel(
  selectInput(inputId= "Manufacturer",
  label="Select Manufacturer", choices = ManufacturerList, selected="All"),
  radioButtons(inputId= "Type",
  label="Select net vehicle or percapita vehicle display", selected= "Count",
  choices = Capita),width = 3),
  mainPanel(plotlyOutput("plot1")))

#Bringing in server (output)
server <- function(input, output) {
output$plot1 <- renderPlotly({
dat <- Vis4dat %>% filter(Manufacturer == input$Manufacturer) 
Counts <- Counts[order(Counts$Count,decreasing = TRUE),]
dat$Location <- ordered(dat$Location, levels = c(Counts$Location))
dat$Percapita <- (dat$Count/dat$CountyPopulation)
top <- max(range(dat[,input$Type],na.rm=TRUE))
bot <- 0

#Splitting output by count vs. percapita selection
if (top < 3){
gtype <- "Vehicles Per Person"  
step <- round(((top-bot)/15),3)
}
else{
gtype <- "Vehicle Count" 
step <- round(((top-bot)/15),0)
}
Count <- 
plot1 <- 
if (input$Type == "Count"){

#Generating secondary layer output 
ggplot(data = dat,aes(x=Location,y=Count)) + 
geom_bar(stat="identity") +
scale_y_continuous(paste(gtype),breaks=c(seq(0,top,step)),labels = comma) +
theme(axis.text.x = element_text(angle = 35, hjust = .5,size=8.5),plot.margin=unit(c(.5,.5,1.9,.5),"cm"))}
else{
ggplot(data = dat,aes(x=Location,y=Percapita)) + 
geom_bar(stat="identity") +
scale_y_continuous(paste(gtype),breaks=c(seq(0,top,step)),labels = comma) +
theme(axis.text.x = element_text(angle = 35, hjust = .5,size=8.5),plot.margin=unit(c(.5,.5,1.9,.5),"cm"))
}
ggplotly(plot1,width = 920, height = 670 ) %>% config(staticPlot = FALSE, displayModeBar = FALSE, workspace = TRUE, sendData = FALSE, displaylogo = FALSE)
})
}
shinyApp(ui = ui, server = server)
