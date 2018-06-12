#Requisite library
library(plotly)

#Data from:
#badteeth - https://www.kaggle.com/angelmm/healthteethsugar
#sug - https://www.cia.gov/library/publications/the-world-factbook/rankorder/2034rank.html
#Texture - https://www.gapminder.org/
#Obese - https://www.cia.gov/library/publications/the-world-factbook/rankorder/2228rank.html
badteeth <- read.csv("badteeth.csv")
sug <- read.csv("sugar_consumption.csv")
Texture <- read.csv("gapminderDataFiveYear.csv")
Obese <- read.csv("c2228.csv")

#Heavy refactoring of datasets
badt <- badteeth[1:190,c(1,2)]
sugy <- sug[,c(1,45)]
sugy <- na.omit(sugy)
colnames(sugy) <- c("country","g_per_day")
colnames(badt) <- c("country","rotperkid")
Texture <- Texture[Texture$year=='2002',]
Obese <- Obese[,2:3]
colnames(Obese) <- c("country","Obese")

#Mering disparate sets
Test <- merge(sugy,badt,by="country")
Test <- merge(Obese,Test,by="country")
Test <- merge(Test,Texture,by="country")
Test$RoundRot <- round(Test$rotperkid,digits=0)
Test$RoundRot <- as.character(Test$RoundRot)
Test$RoundRot[Test$RoundRot=="0"] <- "0 Teeth Rotted On Avg."
Test$RoundRot[Test$RoundRot=="1"] <- "1 Rotted Tooth On Avg."
Test$RoundRot[Test$RoundRot=="2"] <- "2 Rotted Teeth On Avg."
Test$RoundRot[Test$RoundRot=="3"] <- "3 Rotted Teeth On Avg."
Test$RoundRot[Test$RoundRot=="4"] <- "4 Rotted Teeth On Avg."
Test$RoundRot[Test$RoundRot=="5"] <- "5+ Rotted Teeth On Avg."
Test$RoundRot[Test$RoundRot=="6"] <- "5+ Rotted Teeth On Avg."
Test$Rotteeth <- Test$rotperkid * Test$pop

#Defining plotly parameters
Colors <- c("#FFFFFF","#FFFF00","#FFCC99","#CC9966","#775500","#993300")
y <- list(
  title = 'Avg. Daily Percapita Grams of Sugar Consumed',
  tickvals = seq(0,200,25),
  backgroundcolor="rgb(232, 238, 247)",
  showbackground=TRUE,
  range = c(0,200),
  titlefont= list(
  size = 8)
)

x <- list(
  title = 'Obesity Rate',
  tickvals = seq(0,40,5),
  backgroundcolor="rgb(232, 238, 247)",
  showbackground=TRUE,
  range = c(0,40),
  titlefont= list(
  size = 11)
)

z <- list(
  title = 'Life Expectancy',
  tickvals = seq(40,80,5),
  backgroundcolor="rgb(232, 238, 247)",
  showbackground=TRUE,
  range = c(35,80),
  titlefont= list(
  size = 11)
)

#Generating visualization
plot <- plot_ly(data=Test,type="scatter3d",z=~lifeExp,y=~g_per_day,x=~Obese,color =~RoundRot,colors =~Colors ,
  size=~Rotteeth,hoverlabel=list(font=list(size=10)),text=~paste("Country:",country,"\b","\n",
	"Avg. Daily Grams of Sugar Consumed:",g_per_day,"\n","Obesity Rate:", Obese,"%","\n",
	"Average Rotted Teeth Rer Child:",rotperkid,"\n","Average Life Expectancy:",lifeExp,"years"),hoverinfo="text",marker = list(sizemode = "diameter",opacity=.65),
	 sizes = c(13,70),mode = 'markers') %>% layout(title = 'Countries By Sugar Consumption and Health Statistics: Colored by Avg. Rotted Teeth Per Child \n Rough Rotted Teeth Per Capita as Size',
	 scene = list(xaxis = x,
                yaxis = y,
                 zaxis = z), paper_bgcolor='rgb(233,233,233)')
plot <- config(p=plot,displayModeBar = F, showLink = F)
plot

