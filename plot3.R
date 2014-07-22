# Plot3.R
# This script is part of the Exploratory Data Analysis - Project 2 - plot3
# Written by PanaEk Warawit on July 22, 2014
# Q3 - Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#   nonroad) variable, which of these four sources have seen decreases in 
#   emissions from 1999–2008 for Baltimore City? Which have seen increases in 
#   emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
#   answer this question.

plot3 <- function(){
    
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
    
    # Aggregate data by year and type
    baltimore_type <- aggregate(baltimore$Emissions, 
                                by=list(baltimore$year,baltimore$type), FUN="sum")
    names(baltimore_type) <- c("year", "type","pm25")
    
    # Plot using ggplot2
    library(ggplot2)
    #png(filename = "plot3.png", width = 480, height = 480, bg="transparent")
    qplot(year, pm25, data=baltimore_type, col=type) +
        geom_line(size = 1) +
        xlab("Year") +
        ylab("PM2.5 Emission (ton)") +
        ggtitle("PM2.5 emissions in Baltimore by Type")
    dev.copy(png, file = "plot3.png") ## Copy my plot to a PNG file
    dev.off()
    
}
