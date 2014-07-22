# Plot1.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot1
# Written by PanaEk Warawit on July 22, 2014

plot1 <- function(){

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
    
    # Aggregate data 
    total_pm25 <- aggregate(pm25$Emissions, by=list(pm25$year), FUN="sum")
    names(total_pm25) <- c("year","pm25")
    
    # Plot 
    png(filename = "plot1.png", width = 480, height = 480, bg="transparent")
    plot(total_pm25$pm25 ~ total_pm25$year, type="l",
         main="Total PM2.5 emissions in the United States",
         xlab="Year",
         ylab="PM2.5 Emission (tons)",
         col="red")
    dev.off()
    
}
