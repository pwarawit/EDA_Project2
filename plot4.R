# Plot4.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot4
# Written by PanaEk Warawit on July 22, 2014
# Q4 - Across the United States, how have emissions from coal combustion-related 
#   sources changed from 1999–2008?

plot4 <- function(){
    
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
    
    # Subsetting only SCC that is "Coal-Combustion" related
    scc_coal <- subset(scc, grepl("*Coal*", Short.Name) & grepl("*Combustion*",SCC.Level.One))
    
    # Subsetting only SCC data from scc_coal
    pm25_coal <- subset(pm25, SCC %in% scc_coal$SCC)
    
    # Aggregate data by year
    total_pm25_coal <- aggregate(pm25_coal$Emissions, by=list(pm25_coal$year), FUN="sum")
    names(total_pm25_coal) <- c("year","pm25")
    
    # Plot 
    png(filename = "plot4.png", width = 480, height = 480, bg="transparent")
    plot(total_pm25_coal$pm25 ~ total_pm25_coal$year, type="l",
         main="Coal-Combustion related PM2.5 emissions in US",
         xlab="Year",
         ylab="PM2.5 Emission (tons)",
         col="red")
    dev.off()
        
}
