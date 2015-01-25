## Exploratory data analysis Project 2
## For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.

# load data
if (!exists("NEI")) NEI <- readRDS("summarySCC_PM25.rds")
if (!exists("SCC")) SCC <- readRDS("Source_Classification_Code.rds")

# load libraries
library(dplyr)
library(ggplot2)

NEI$type <- factor(NEI$type)
NEI$Pollutant <- factor(NEI$Pollutant)
NEI$year <- factor(NEI$year)

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
total_emission_by_year <- tapply(NEI$Emissions,NEI$year,sum)
png(filename="plot1.png")
plot(names(total_emission_by_year),total_emission_by_year,type="l", xlab="Year", ylab="Total emissions")
dev.off()

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == 24510) from 1999 to 2008? Use the base plotting system to make a plot answering this question.
baltimore <- subset(NEI, fips=="24510")
baltimore_emissions <- aggregate(Emissions ~ year, baltimore, sum)
png(filename="plot2.png")
plot(baltimore_emissions$year, baltimore_emissions$Emissions, main="Total Baltimore emissions by year", xlab="year", ylab="Total emissions by year")
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008?
baltimore <- subset(NEI, fips=="24510")
baltimore_emissions <- aggregate(Emissions ~ year+type, baltimore, sum)
ggplot(baltimore_emissions,aes(x=year,y=Emissions,color=type,group=type)) + geom_point() + geom_line()
ggsave(file="plot3.png",width=5,height=5,dpi=100)

#4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
if (!exists("dataset")) dataset <- merge(NEI,SCC)
coal_comb_data <- subset(dataset, grepl("[Cc]omb.*[Cc]oal",dataset$Short.Name),c("Emissions", "year","Short.Name")) 
coal_comb_emissions <- aggregate(Emissions ~ year,coal_comb_data,sum)
ggplot(data=coal_comb_emissions,aes(x=year,y=Emissions,group=1)) + geom_line() + geom_point( size=4, shape=21, fill="white") + xlab("Year") + ylab("Emissions") + ggtitle("Total United States PM2.5 Coal Combustion Emissions")
ggsave(file="plot4.png",width=5,height=5,dpi=100)    

#5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
baltimore_vehicle <- subset(dataset,fips=="24510" & type=="ON-ROAD",c("Emissions","year"))
baltimore_vehicle_emissions <- aggregate(Emissions ~ year,baltimore_vehicle,sum)
ggplot(baltimore_vehicle_emissions,aes(x=year,y=Emissions,group=1)) + geom_point() + geom_line() + ggtitle("Motor Vehicle PM2.5 Emissions in Baltimore")
ggsave(file="plot5.png",width=5,height=5,dpi=100) 

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time in motor vehicle emissions?
vehicle_data <- subset(dataset,(fips=="24510" | fips=="06037") & type=="ON-ROAD",c("Emissions","year","fips"))
vehicle_data$city <- factor(vehicle_data$fips, levels=c("24510","06037"), labels=c("Baltimore","Los Angeles"))
vehicle_data$fips <- NULL
vehicle_emissions <- aggregate(Emissions ~ year+city, vehicle_data, sum)
ggplot(vehicle_emissions,aes(x=year,y=Emissions,color=city,group=city)) + geom_point() + geom_line() + ggtitle(expression("Motor Vehicle Emission Levels in LA and Baltimore"))
ggsave(file="plot6.png",width=5,height=5,dpi=100) 
