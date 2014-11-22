NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

NEIsubset <- NEI[NEI$fips == "24510" | NEI$fips == "06037", ]

png(filename = "datasciencecoursera/Exploratory Data Analysis/Assignment2/figure/plot6.png",
    width = 580, height = 380, units = "px", pointsize = 12, bg = "white")

motor <- grep("motor", SCC$Short.Name, ignore.case = T)
motor <- SCC[motor, ]
motor <- NEIsubset[NEIsubset$SCC %in% motor$SCC, ]

library(ggplot)
g <- ggplot(motor, aes(year, Emissions, color = fips))
g + geom_line(stat = "summary", fun.y = "sum") +
    ylab('Total PM2.5 Emissions') +
    ggtitle("Comparison of Total Emissions From Motor Vehicle Sources
            in Baltimore and Los Angeles (1999 to 2008)") +
    scale_colour_discrete(name = "Group", label = c("Los Angeles","Baltimore"))

dev.off()