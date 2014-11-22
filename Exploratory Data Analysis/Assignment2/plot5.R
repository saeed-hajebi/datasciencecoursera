NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

png(filename = "datasciencecoursera/Exploratory Data Analysis/Assignment2/figure/plot5.png",
    width = 580, height = 380, units = "px", pointsize = 12, bg = "white")

library(plyr)
motor <- ddply(NEI[NEI$fips == "24510",], .(year), summarise, TotalEmissions = sum(Emissions))

library(ggplot2)
ggplot(motor, aes(year, TotalEmissions)) +
    geom_line() + geom_point() +
    labs(title = "Total Emissions from Motor Vehicles in Baltimore City (1999-2008)",
         x = "Year", y = "Total Emissions")

dev.off()
