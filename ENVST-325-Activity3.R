library(ggplot2) 
library(dplyr)
library(lubridate)
#In-class Prompts
#Prompt 1
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere 
#in base R and in ggplot2.
temp_anomalies <- read.csv("climate-change.csv")

colnames(temp_anomalies)
temp_anomalies$Entity <- as.factor(temp_anomalies$Entity)
combined_data <- subset(temp_anomalies, Entity %in% c("Northern Hemisphere", 
                                                  "Southern Hemisphere"))
combined_data$Day <- as.Date(combined_data$Day)
# Plots the temperature anomalies in both hemispheres via ggplot
ggplot(combined_data, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_point() + geom_line() +
  labs(x = "Year", y = "Temperature Amamolies for North and South Hemispheres" + 
         theme_classic()) 

#Prompt 2
#Plot the total all time emissions for the United States, Mexico, and Canada.

datCO2 <- read.csv("annual-co-emissions-by-region.csv")
# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)
name.Ent

# subset data for just North America
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()

#Homework Prompts
#Prompt 1: Make a graph that communicates about emissions from any countries of 
#your choice. Explain how you considered principles of visualization in making 
#your graph.
#Subset data for mainly United States
# make a plot of United States for annual fossil fuels emissions (billions of tons of CO2) 
# new data frame for United States
USA <- datCO2[datCO2$Entity == "United States",]
plot(USA$Year, # x-axis (year)
     USA$CO2, # y data (Annual fossil fuel emissions (billions of tons of CO2))
     type = "b", #b = points and lines
     pch = 20, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2) for the USA", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n",
     col = "#00008B") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
legend("topleft",
       c("United States"),
       col=c("#00008B"),
       pch=19, bty= "n")
#Question 2:
#You are tasked with communicating the change in world air temperatures 
#and CO2 emissions to a broad audience in visually appealing graphs. 
#Make two graphs to present in your word document side by side. 
#Plot world CO2 emissions on one graph and world air temperature 
#anomalies on the other graph.

#World CO2 Emissions:
World_CO2_emissions <- datCO2[datCO2$Entity == "World",]

ggplot(data = World_CO2_emissions, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="World fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#809555")) #specify colors using hex codes that include transparency
  
  
  
# Subset data for World temperature anomalies
World_air_temperature_emissions <- temp_anomalies[temp_anomalies$Entity == "World",]
World_air_temperature_emissions$Year <- year(World_air_temperature_emissions$Day)
# Create the plot
ggplot(data = World_air_temperature_emissions, 
       aes(x = Year, y = temperature_anomaly, color = Entity, group = Entity)) + 
  geom_point() +  # Points at data points
  geom_line() +   # Connect data points with lines
  labs(x = "Year", y = "World Air Temperature Anomalies (°C)") +  # Axis labels
  theme_classic() +
  scale_color_manual(values = c("#1b5e5e"))  # Customize color

#Prompt 3:
#Look up any type of environmental data of your interest in our world in data 
#(link in tutorial). Download the csv and upload it to RStudio Cloud. Remake the graph.
#You may make the graph exactly as it is or alter it to present the data in a different 
#format. Explain how you considered principles of visualization in making your graph. 
#Explain the main conclusion of the graph.
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere 
#in base R and in ggplot2.
ggplot(Countries, aes(x = Year, 
                      y = Learning.Adjusted.Years.of.School, 
                      fill = Entity)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  labs(x = "Year", 
       y = "Ages of Adjusted Student Years in School", 
       title = "Learning Adjusted Years of School",
       subtitle = "USA, Canada, and China over Time (Years 2010-2020)") +
  theme_classic(base_size = 14) + 
  scale_fill_manual(values = c("#f56d66","#ffad2a", "blue")) +  
  theme(legend.position = "bottom")  

  
