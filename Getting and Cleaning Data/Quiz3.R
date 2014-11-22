# Problem 1
url1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url1, 'data/acs.csv')
acsData = read.csv('data/acs.csv', header = T)
agricultureLogical = with(acsData, ACR==3 & AGS==6)
which(agricultureLogical) 
# Results: 125  238  262

# Problem 2
install.packages("jpeg")
library("jpeg")

jpgUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(jpgUrl, 'data/jeff.jpg')

jeffPic = readJPEG('data/jeff.jpg', native = T)
quantile(jeffPic, probs = c(0.30, 0.80))
# Results: -16776939 -10092545 

gdpUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
eduUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(gdpUrl, 'data/gdp.csv')
download.file(eduUrl, 'data/edu.url')

gdpData = read.csv('data/gdp.csv', header = T)
eduData = read.csv('data/edu.csv', header = T)

gdpData = gdpData[5:194,]

df = as.data.frame(merge(gdpData, eduData, by.x = "X", by.y = "CountryCode"))

library(plyr)
arrange(df,desc(Gross.domestic.product.2012)) # or df[with(df, order(Gross.domestic.product.2012)), ]

nonOECD = subset(df, Income.Group='High income: nonOECD')
OECD = subset(df, Income.Group='High income: OECD')




