##Load the libraries
library(gridExtra)
library(ggmap)
library(maptools)
library(maps)

##Set working directory
getwd() #First look to see what is the current directory 
#setwd("/Users/chris/Desktop") #Then set the directory to where you want to retrieve and print data

##Read in data
df <-  data.frame(read.csv("events.csv"), header=TRUE)  #read csv file
attach(df)  #attach dataset

##Look at the coumn headings and the first few rows of dataset
names(df)
head(df)

#Calculating descriptive statistics
##Mean
meanPop <- mean(df$dead_and_missing) #population mean
mean2015 <- mean(subset(df, Year == 2015)$dead_and_missing) #2015 mean
# Create the vector. x <- c(12,7,3,4.2,18,2,54,-21,8,-5)
x <- c(df$dead_and_missing)
# Find the median. median.result <- median(x)
medianPop <- median(df$dead_and_missing)
median2015 <- as.numeric(median(subset(df, Year == 2015)$dead_and_missing))

#Standard Deviation
sdPop <- sd(df$dead_and_missing) #population standard deviation
sd2015 <- sd(subset(df, Year == 2015)$dead_and_missing) #mean standard deviation

#Mode
modePop <- as.numeric(names(sort(-table(df$dead_and_missing)))[1]) #population mode; sort the dataset and read the first row (most frequent)

#we need to do this a little bit differently for the 2015 sample
values2015 <- df[ which(df$Year==2015), ] #create an object for only 2015 data
modevalues2015<- values2015$dead_and_missing #extract the variable for dead and missing
mode2015 <- as.numeric(names(sort(-table(modevalues2015)))[1]) #sort the dataset and read the first row (most frequent)

#Creating table
Samples = c("Population", "2015") #Create an object for the labels
Mean = c(meanPop, mean2015) #Create an object for the means
Median = (c(medianPop, median2015))
Mode = (c(modePop, mode2015))
`Std Dev` = (c(sdPop, sd2015))
data.for.table = data.frame(Samples, Mean, Median, Mode, `Std Dev`)

#Printing a table (you can use the same setup for printing other types of objects
png("Output_Table.png") #Create an object to print the table to
grid.table(data.for.table, row.names(NULL)) #Create table
dev.off() #Print table

#Create and print a histogram
#hist(df$dead_and_missing, breaks = 20, main = "Dana Huget's Histogram", xlab = "Number of Dead and Missing")

#You will see that this histogram is highly positively skewed, as the vast majority of records have a value of 1. 
#In order to get a better sense of how the data is distributed, we can graph the log value of each record. 
#We do this in the code below that prints a histogram with log values.

pdf("Output_Histogram_.pdf")
#qplot(df$dead_and_missing, geom="histogram", main = "Dana Huget's Histogram",
     # xlab = "Number of Dead and Missing", ylab = "Frequency") + scale_x_continuous(trans="log10")
#qplot(df$dead_and_missing, geom="histogram", main = "Dana Huget's Histogram",
     # xlab = "Number of Dead and Missing", ylab = "Frequency") + scale_y_log10()
qplot(df$dead_and_missing, geom="histogram", main = "Dana Huget's Histogram",
      xlab = "Number of Dead and Missing", ylab = "Frequency", log = "y") #+ geom_bar(fill="#FF9999", colour="black")
dev.off() 

png("Output_Histogram.png") #Create an object to print the table to
hist(df$dead_and_missing, breaks = 20, main = "Dana Huget's Histogram", xlab = "Number of Dead and Missing")
#hist(log(df$dead_and_missing), breaks = 20, main = "Dana Huget's Histogram", xlab = "Number of Dead and Missing")
#hist(log(df$dead_and_missing), breaks = 20, main = "Dana Huget's Histogram", xlab = "Number of Dead and Missing", ylim=c(0,200))
dev.off() #Print histogram

#Creating bar graph
sum2010 = sum(subset(df, Year == 2010)$dead_and_missing)  #Create an object for the total in 2004
sum2011 = sum(subset(df, Year == 2011)$dead_and_missing)
sum2012= sum(subset(df, Year == 2012)$dead_and_missing)
sum2013 = sum(subset(df, Year == 2013)$dead_and_missing)
sum2014= sum(subset(df, Year == 2014)$dead_and_missing)
sum2015 = sum(subset(df, Year == 2015)$dead_and_missing)

years = c("2010","2011","2012","2013","2014", "2015")  #Create labels for the bar graph

pdf("Output_BarGraph.pdf") #Create an object to print the bar graph 
barplot(c(sum2010,sum2011,sum2012, sum2013,sum2014, sum2015), names.arg=years, main = "Dana Huget's Bar Graph", ylab= "Annual Dead and Missing Total", xlab = "Year", ylim=c(0,5000)) #Create the bar graph
dev.off() #Print bar graph

#Creating maps
#First example
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(df$longitude ,df$latitude , col="red", pch=16)

#Second example
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() + mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point(aes(x=df$longitude, y=df$latitude) ,color="blue", size = log10(df$dead_and_missing))# df$dead_and_missing) 
mp

#Slightly better looking. Here is another type of map that we can create with a more aesthetically pleasing basemap.
#world.map <- get_map(location = c(lon = -40.0, lat = 20.0), zoom = 4)
world.map <- get_map(location = c(lon = -40.0, lat = 20.0), zoom = 2)

library(Hmisc)
label(df$dead_and_missing)
describe(df$dead_and_missing)
#Remove labels:
Dead_and_Missing <- clear_labels(df$dead_and_missing)
`Number of Dead and Missing` = df$dead_and_missing
world.map <- get_map(location = c(lon = mean(range(df$longitude)), lat = mean(range(df$latitude))), zoom = 3)
ggmap(world.map) + ggtitle("Dana Huget's Map") + 
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  geom_point(data = df, aes(x = df$longitude, y = df$latitude, size = `Number of Dead and Missing`, colour = `Number of Dead and Missing`)) +
  scale_colour_gradient( high = "black", low = "lightpink")

ggmap(world.map) + ggtitle("Dana Huget's Map") +
  geom_point(data = df, aes(x = df$longitude, y = df$latitude, colour = `Number of Dead and Missing`)) +
  scale_colour_gradient( high = "black", low = "lightpink")

meanLat <- mean(df$latitude)
meanLon <- mean(df$longitude)
Longitude <- df$longitude
ggmap(world.map) + ggtitle("Refugee Crisis Map") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
  geom_point(data = df, aes(x = df$longitude, y = df$latitude, size = `Number of Dead and Missing`, colour = `Number of Dead and Missing`)) +
  scale_colour_gradient( high = "black", low = "lightpink") +
  geom_point(aes(x = meanLon, y = meanLat), data = df, shape=23, fill="red", color="darkred", size=2) +
  #label = "italic(R) ^ 2 == 0.75", parse = TRUE
  annotate("text", label = "Mean Centre", fontface=2, x = meanLon+1, y = meanLat, hjust = 0, color = "white", size=3)

