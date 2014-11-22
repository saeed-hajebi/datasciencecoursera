# Problem 1
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
filename = 'data/acs.csv'
download.file(url, filename)
acsData = read.csv(filename, header = T)

strsplit(names(acsData), "wgtp")[123]
# ""   "15"

# Problem 2
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
filename = 'data/GDP.csv'
download.file(url, filename)
gdpData = read.csv(filename, header = T)
ml = gdpData[5:194,5]
cleanml = gsub(',','',ml)
cleanml = as.numeric(cleanml)

