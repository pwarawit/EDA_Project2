# Plot6.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot6
# Written by PanaEk Warawit on July 22, 2014
# Q6 - Compare emissions from motor vehicle sources in Baltimore City with 
#   emissions from motor vehicle sources in Los Angeles County, California 
#   (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

plot6 <- function(){
    
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
    #   and in the county of Baltimore (fips="24510") or LA (fips="06037")
    pm25_motor <- subset(pm25, SCC %in% scc_mobile$SCC & grepl("*ROAD",type) 
                         & (fips %in% c("24510","06037")))
    
    # Replace the fips with County name for easy plot
    pm25_motor$fips <- gsub("24510", "Baltimore", pm25_motor$fips)
    pm25_motor$fips <- gsub("06037", "Los Angeles", pm25_motor$fips)
    
    # Aggregate data by year
    pm25_compare <- aggregate(pm25_motor$Emissions, 
                              by=list(pm25_motor$fips, pm25_motor$year), FUN="sum")
    names(pm25_compare) <- c("County", "year","pm25")
    
    
    # Plot using ggplot2
    library(ggplot2)
    #png(filename = "plot6.png", width = 480, height = 480, bg="transparent")
    qplot(year, pm25, data=pm25_compare, col=County) +
        geom_line(size = 1) +
        xlab("Year") +
        ylab("PM2.5 Emission (ton)") +
        ggtitle("Motor Vehicle PM2.5 Emissions Comparision")
    dev.copy(png, file = "plot6.png")
    dev.off()
    
}
