library(dplyr)
library(lubridate)
library(ggplot2)
source("load_data.R")

# Assumes the current device if one is not specified, and loads the dataset if it is not provided.
# May not be concurrency safe, as it mutates the current device setting during execution.
generate_plot2 <- function(data = load_data(),
                           device_id = dev.cur()) {
  last_id <- dev.cur()
  dev.set(device_id)
  
  # Fetches the names of the weekdays in question and truncates them to a presentable vector of strings
  full_weekday_names <- weekdays(dmy(c("1/2/2007", "2/2/2007", "3/2/2007")))
  short_weekday_names <- sapply(full_weekday_names, function (day_name) { substr(day_name, start = 1, stop = 3) })
  
  min_time <- min(time(data$Time))
  max_time <- max(time(data$Time))
  half_time <- min_time + (max_time - min_time)/2
  
  plot(x = time(data$Time),
       y = data$Global_active_power,
       type = "l",# Specify a line plot
       xaxt = "n",# Suppress x axis so we may draw it manually
       ylab = "Global Active Power(kW)",
       xlab = "")
  
  # Apply weekday names across the x axis
  axis(1, c(min_time, half_time, max_time),
       labels = short_weekday_names)
  
  dev.set(last_id)
}

print_plot2 <- function() {
  png(filename = "plot2.png",
      width = 400, height = 400,
      units = "px", pointsize="12",
      bg = "white")
  generate_plot2()
  dev.off()
}