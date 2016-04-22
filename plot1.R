library(dplyr)
source("load_data.R")

# Plots a histogram of frequency per global active power in 0.5kW bins to the device with the given id.
# Assumes the current device if one is not specified, and loads the dataset if it is not provided.
# May not be concurrency safe, as it mutates the current device setting during execution.
generate_plot1 <- function(data = load_data(),
                           device_id = dev.cur()) {
  last_id <- dev.cur()
  dev.set(device_id)
  hist(x = data$Global_active_power,
       col = "red",
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)",
       ylab = "Frequency",
       breaks = 12)
  dev.set(last_id)
}

print_plot1 <- function() {
  png(filename = "plot1.png",
      width = 400, height = 400,
      units = "px", pointsize="12",
      bg = "white")
  generate_plot1()
  dev.off()
}