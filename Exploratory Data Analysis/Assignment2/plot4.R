NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

NEIsubset <- NEI[NEI$fips == "24510", ]  #Baltimore City, Maryland (fips == 24510)

totEm = aggregate(NEIsubset$Emissions, list(NEIsubset$year), FUN = "sum")

png(filename = "datasciencecoursera/Exploratory Data Analysis/Assignment2/figure/plot4.png",
    width = 580, height = 380, units = "px", pointsize = 12, bg = "white")

coal <- grep("coal", SCC$Short.Name, ignore.case = T)
coal <- SCC[coal, ]
coal <- NEI[NEI$SCC %in% coal$SCC, ]
coalEmissions <- aggregate(coal$Emissions, list(coal$year), FUN = "sum")


plot(coalEmissions, type = "l", xlab = "Year",
     main = 'Total Emissions from Coal Combustion-related Sources- 1999 to 2008 (US)',
     ylab = 'Total PM2.5 Emissions')
     
dev.off()
