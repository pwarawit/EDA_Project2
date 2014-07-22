# Plot2.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot2
# Written by PanaEk Warawit on July 22, 2014
# Q2. Have total emissions from PM2.5 decreased in the Baltimore City, 
#   Maryland (fips == "24510") from 1999 to 2008? 
#   Use the base plotting system to make a plot answering this question.

plot2 <- function(){
    
    zipfile <- "NEI_data.zip"
    fileurl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    sccfile <- "Source_Classification_Code.rds"
    pm25file <- "summarySCC_PM25.rds"
    
    # Check if the sccfile and pm25file already exist. If exists, move on.
    if(!(file.exists(sccfile) && file.exists(pm25file))){
        # Some text files missing - check if zip file exists
        if (!file.exists(zipfile)){
            # print ("No zip file, need to both download and unzip")
            download.file(fileurl, zipfile)
            unzip(zipfile)
        } else {
            # print ("Zip file exists, but not RDS files, just need to unzip")
            unzip(zipfile)
        }
        
    }
    
    # Read data into scc and pm25
    pm25 <- readRDS(pm25file)
    scc <- readRDS(sccfile)
    
    # Subsetting only Baltimore data
    baltimore <- pm25[pm25$fips=="24510",]
    
    # Aggregate data 
    total_pm25 <- aggregate(baltimore$Emissions, by=list(baltimore$year), FUN="sum")
    names(total_pm25) <- c("year","pm25")
    
    # Plot 
    png(filename = "plot2.png", width = 480, height = 480, bg="transparent")
    plot(total_pm25$pm25 ~ total_pm25$year, type="l",
         main="Total PM2.5 emissions in Baltimore, Maryland",
         xlab="Year",
         ylab="PM2.5 Emission (tons)",
         col="red")
    dev.off()
    
}
