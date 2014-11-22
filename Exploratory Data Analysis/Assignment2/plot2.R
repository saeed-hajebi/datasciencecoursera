NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
# SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

NEIsubset <- NEI[NEI$fips == "24510", ]  #Baltimore City, Maryland (fips == 24510)

totEm = aggregate(NEIsubset$Emissions, list(NEIsubset$year), FUN = "sum")

png(filename = "datasciencecoursera/Exploratory Data Analysis/Assignment2/figure/plot2.png",
    width = 480, height = 380, units = "px", pointsize = 12, bg = "white")

plot(totEm, type = "l", xlab = "Year",
     main = 'Total Emissions from 1999 to 2008 (Baltimore City, Maryland)',
     ylab = 'Total PM2.5 Emission')

dev.off()


