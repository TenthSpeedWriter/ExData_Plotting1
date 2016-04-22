library(dplyr)
library(lubridate)
library(ggplot2)
source("load_data.R")

# Assumes the current device if one is not specified, and loads the dataset if it is not provided.
# May not be concurrency safe, as it mutates the current device setting during execution.
generate_plot3 <- function(data = load_data(),
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
       y = data$Sub_metering_1,# Initialize graph with submetering set 1
       col = "black",# in black
       type = "l",# Specify this as a line plot
       xaxt = "n",# Suppress x axis so we may draw it manually
       ylab = "Energy Sub-metering",
       xlab = "")
  
  lines(data$Sub_metering_2,
        col="red")
  
  lines(data$Sub_metering_3,
        col="blue")
  
  # Apply weekday names across the x axis
  axis(1, c(min_time, half_time, max_time),
       labels = short_weekday_names)
  
  legend("topright",
         title = "Sub-metering Values",
         legend = c("Sub-meter 1",
                    "Sub-meter 2",
                    "Sub-meter 3"),
         fill = c("black",
                  "red",
                  "blue"),
         text.width = 1)
  
  dev.set(last_id)
}

print_plot3 <- function() {
  png(filename = "plot3.png",
      width = 400, height = 400,
      units = "px", pointsize="12",
      bg = "white")
  generate_plot3()
  dev.off()
}