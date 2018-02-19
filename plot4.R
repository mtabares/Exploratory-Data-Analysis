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

## Step 2: Plot the "PLOT 4"
## Create Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data2plot, {
  plot(Global_active_power~dt2plot, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dt2plot, type="l", ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dt2plot, type="l", ylab="Energy sub metering", xlab="")
  
  lines(Sub_metering_2~dt2plot,col='Red')
  lines(Sub_metering_3~dt2plot,col='Blue')

  legend("topright", lty=1, col=c("black","red","blue"),
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.3)
  

  plot(Global_reactive_power~dt2plot, type="l", ylab="Global_Rective_Power",xlab="")
})
## Saving to file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()