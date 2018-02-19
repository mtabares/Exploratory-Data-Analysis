## Project 1 - Exploratory data Analysis

## Step 1: Load and clean the table

## Loading the data
data2plot <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
                        colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric',
                                       'numeric','numeric'))

#### Formating date field
data2plot$Date <- as.Date(data2plot$Date, "%d/%m/%Y")
#### We will only be using data from the dates 2007-02-01 and 2007-02-02
data2plot <- subset(data2plot, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
## Remove incomplete observation
data2plot <- data2plot[complete.cases(data2plot),]

## Combining Date and Time columns
dt2plot <- paste(data2plot$Date, data2plot$Time)
## Naming a vector
dt2plot <- setNames(dt2plot, "DateTime")
## Remove Date and Time column
data2plot <- data2plot[ ,!(names(data2plot) %in% c("Date","Time"))]
## Add DateTime column
data2plot <- cbind(dt2plot, data2plot)
## Format dateTime Column
data2plot$dt2plot <- as.POSIXct(dt2plot)

## Step 2: Plot the "PLOT 1"
## Creating the histogram
hist(data2plot$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
## Save file and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()