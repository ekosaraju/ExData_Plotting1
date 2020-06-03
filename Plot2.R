library(ggplot2)
library(tidyr)
library(lubridate)
library(ggpubr)
epc <- read.table("household_power_consumption.txt",sep=";",na.strings="?",header=TRUE)

# Change Date column to date format using as.Date, Time to hms format using lubridate.
# Add a new column date_time to concatenate date and time for plotting.
# Chain all with %>% function, add it back to the same data frame.

epc <- epc %>% mutate(
                        Date = as.Date(epc$Date, "%d/%m/%Y"),
                        Time = hms(epc$Time),
                        date_time = ymd_hms(paste(Date,Time))
                      )

# Filter the data frame to get the desired records between the required date range.

epc_filtered <- epc %>%
    filter (between (Date, as.Date("2007-02-01"),as.Date("2007-02-02")))


# Plot 2: Line chart based on date_time vs global active power (kw)

plot_2 <- ggplot(epc_filtered,aes(x=date_time,y=Global_active_power)) +
            geom_line() +
            scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+ # drop some x-axis labels as it is too noisy.
            xlab("") + # adding the x-axis label
            ylab("Global Active Power (kilowatts)") + # adding the y-axis label
            theme(plot.title = element_text(hjust = 0.5)) #center the plot title

plot_2