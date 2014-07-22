# Plot5.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot5
# Written by PanaEk Warawit on July 22, 2014
# Q5 - How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

plot5 <- function(){
    
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
    
    # Define motor vehicle sources - as source starts with "Mobile" in scc$EI.Sector
    #   and pm25$type="*road*"
    # Subset pm25 data with both criteria above
    # Subsetting only SCC that is "Coal-Combustion" related
    scc_mobile <- subset(scc, grepl("^Mobile*", EI.Sector))
    
    # Subsetting only SCC data from scc_mobile and type have word ROAD in it
    #   and in the county of Baltimore (fips="24510")
    pm25_motor <- subset(pm25, SCC %in% scc_mobile$SCC & grepl("*ROAD",type) & fips=="24510")
    
    # Aggregate data by year
    pm25_baltimore_motor <- aggregate(pm25_motor$Emissions, by=list(pm25_motor$year), FUN="sum")
    names(pm25_baltimore_motor) <- c("year","pm25")
    
    # Plot 
    png(filename = "plot5.png", width = 480, height = 480, bg="transparent")
    plot(pm25_baltimore_motor$pm25 ~ pm25_baltimore_motor$year, type="l",
         main="Motor Vehical PM2.5 emissions in Baltimore",
         xlab="Year",
         ylab="PM2.5 Emission (tons)",
         col="red")
    dev.off()
    
}
