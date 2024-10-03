install.packages("ggplot2")
install.packages("lubridate")
install.packages("dplyr")

library(ggplot2) 
library(dplyr)
library(lubridate)

#Prompt 1
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2.

temp_anomalies <- read.csv("climate-change.csv")
colnames(temp_anomalies)
temp_anomalies$Entity <- as.factor(temp_anomalies$Entity)
# make a vector of all levels
hemisphere.Ent <- levels(temp_anomalies$Entity)
hemisphere.Ent

plot(temp_anomalies$Entity, temp_anomalies$temperature_anomaly)

# make a plot of the Southern, Northern Hemispheres, and the world ----

southern_data <- subset(temperature_anomaly, Entity == "Southern Hemisphere")
northern_data <- subset(temperature_anomaly, Entity == "Northern Hemisphere")
world_data <- subset(temperature_anomaly, Entity == "World")

combined_data <- rbind(southern_data, northern_data, world_data)
combined_data$Hemisphere <- factor(combined_data$temperature_anomaly, 
                                   levels = c("Southern Hemisphere", "Northern Hemisphere", "World"))

ggplot(combined_data, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_line(size = 1) +  # Create line plots for each Hemisphere
  geom_point(size = 2) + # Add points to the lines
  labs(title = "Temperature Anomalies Over Time", 
       x = "Year, Day, Month", 
       y = "Temperature Anomaly (Degrees Fahrenheit)") +
  theme_minimal() +
  scale_color_manual(values = c("Southern Hemisphere" = "blue", 
                                "Northern Hemisphere" = "red", 
                                "World" = "green"))




#Falsehood


# make a plot of US CO2 ----

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

# read in data
# cloud is always lowercase
datCO2 <- read.csv("annual-co-emissions-by-region.csv")
# check column names
colnames(datCO2)
# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

name.Ent

plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year") #x axis label
#########

# makes another plot of US CO2----
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
#######

#Makes a third plot of US CO2-----
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")


ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ #axis labels
  theme_classic()

# subset data for just
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()


#Prompt 2 - Plot the total all time emissions for the United States, Mexico, and Canada.
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("red","darkgreen", "blue")) #specify colors using hex codes that include transparency


# subset CO2 to meet conditons - Violins and box plots
compCO2 <- datCO2[datCO2$Year >= 1950 & datCO2$Entity == "France" |
                    datCO2$Year >= 1950 & datCO2$Entity == "India" |
                    datCO2$Year >= 1950 &  datCO2$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ # add grey 
  #boxplots and make them smaller to fit in the violin (width)
  #than normal with thinner lines (size_ than normal
  theme_classic()+ # get rid of ugly gridlines
  labs(x = "Country", y="Annual emissions (tons CO2)")

#Areas plot
ggplot(data=compCO2, aes(x=Year, y=CO2, fill=Entity))+ # data
  geom_area() # geometry

#ribbons plots
ggplot(data=compCO2,
       aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+ #fill works for polygons/shaded areas 
  geom_ribbon(alpha=0.5 )+ #fill in with 50% transparency
  labs(x="Year", y="Annual emissions (tons CO2)")

b <- ggplot(data=compCO2,aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+
  geom_ribbon(alpha=0.5 )+
  labs(x="Year", y="Carbon emissions (tons CO2)") +
  theme_classic()

b + annotate("segment", # line label
             x=1991, # start x coordinate
             y=2450000000, # start y coordinate
             xend=1991, # end x coordinate
             yend=2600000000) + # end y coordinate
  annotate("text", # add text label
           x=1991, # center of label x coordinate
           y= 2700000000, # center of label y coordinate
           label="end of USSR") # label to add






plot(dat$CO2$Year, dat$CO2$CO2, type = "b",
     slab = "Year", ylab = "CO2 emissions (tons)")

NA_CO <- datCO2 %>%
  filter(Entity == "United States"| Entity == "Mexico"| Entity == "Canada")

ggplot(NA_CO, aes(x=Year, y = CO2, color = Entity)) + geom_line()+
  labs(x="Year", y="CO2 emissions")+theme_classic()

l