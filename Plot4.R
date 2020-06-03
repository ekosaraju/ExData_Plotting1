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

# Draw plots a, b, c and d and use them to create plot 4 in the end.

# Plot a: Line chart based on date_time vs global active power (kw)

plot_a <- ggplot(epc_filtered,aes(x=date_time,y=Global_active_power)) +
            geom_line() +
            scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+ # drop some x-axis labels as it is too noisy.
            xlab("") + # adding the x-axis label
            ylab("Global Active Power (kilowatts)") + # adding the y-axis label
            theme(plot.title = element_text(hjust = 0.5)) #center the plot title

# Plot b: Line chart based on date_time vs voltage

plot_b <- ggplot(epc_filtered,aes(x=date_time)) +
  geom_line(aes(y=Voltage)) +
  # drop some x-axis labels as it is too noisy.
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

# Plot c: Energy sub metering

plot_c <- ggplot(epc_filtered,aes(x=date_time)) +
            geom_line(aes(y=Sub_metering_1, color = "metering_1")) +
            geom_line(aes(y=Sub_metering_2, color = "metering_2")) +
            geom_line(aes(y=Sub_metering_3,color = "metering_3")) +
            # drop some x-axis labels as it is too noisy.
            scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
            ggtitle("Global Active Power") +
            ylab("Energy sub metering")+
            theme(plot.title = element_text(hjust = 0.5),legend.position = "top", 
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=8)
                  )+
            guides(fill=guide_legend(nrow=2,byrow=TRUE))

# Plot d: Line chart based on date_time vs global reactive power

plot_d <- ggplot(epc_filtered,aes(x=date_time)) +
            geom_line(aes(y=Global_reactive_power)) +
            # drop some x-axis labels as it is too noisy.
            scale_x_discrete(guide = guide_axis(check.overlap = TRUE))




# Put all the required four plots in one image.

plot_4 <- ggarrange(plot_a, plot_b, plot_c, plot_d,
                        ncol = 2, nrow = 2
)

plot_4
