NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

png(filename = "datasciencecoursera/Exploratory Data Analysis/figure/plot1.png", 
    width = 480, height = 380, units = "px", pointsize = 12, bg = "white")

totEm = aggregate(NEI$Emissions, list(NEI$year), FUN = "sum")

plot(totEm, type = "l", xlab = "Year",
     main = 'Total Emissions from 1999 to 2008 (United States)',
     ylab = 'Total PM2.5 Emission')

dev.off()
