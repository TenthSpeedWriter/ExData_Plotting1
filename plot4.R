source("plot2.R")

data <- load_data()

plot_weekday_names <- function() {
  full_weekday_names <- weekdays(dmy(c("1/2/2007", "2/2/2007", "3/2/2007")))
  short_weekday_names <- sapply(full_weekday_names,
                                function (day_name) {
                                  substr(day_name,
                                         start = 1,
                                         stop = 3)
                                  })
  
  min_time <- min(time(data$Time))
  max_time <- max(time(data$Time))
  half_time <- min_time + (max_time - min_time)/2
  
  axis(1, c(min_time, half_time, max_time),
       labels = short_weekday_names)
}

generate_voltage_plot <- function(data = data,
                                  device_id = dev.cur()) {
  last_id <- dev.cur()
  dev.set(device_id)
  
  plot(x = time(data$Time),
       y = data$Voltage,
       type = "l",# Specify a line plot
       xaxt = "n",# Suppress x axis so we may draw it manually
       ylab = "Voltage",
       xlab = "")
  
  plot_weekday_names()
  
  dev.set(last_id)
}

generate_sub_meter_plot <- function(data = data,
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
  
  dev.set(last_id)
}

generate_global_reactive_power_plot <- function(data = data,
                                                device_id = dev.cur()) {
  last_id <- dev.cur()
  dev.set(device_id)
  
  plot(x = time(data$Time),
       y = data$Global_reactive_power,
       type = "l",# Specify a line plot
       xaxt = "n",# Suppress x axis so we may draw it manually
       ylab = "Global Reactive Pwr(kW)",
       xlab = "")
  
  plot_weekday_names()
  
  dev.set(last_id)
}

generate_plot4 <- function(data = data,
                           device_id = dev.cur()) {
  last_id <- dev.cur()
  dev.set(device_id)
  
  par(mfrow=c(2, 2))# Create a two row, two column layout
  generate_plot2()# Plot G.A.P. over time
  
  generate_voltage_plot()
  
  generate_sub_meter_plot()
  
  generate_global_reactive_power_plot()
  
  dev.set(last_id)
}

print_plot4 <- function(data = data) {
  png(filename = "plot4.png",
      width = 400, height = 400,
      units = "px", pointsize="12",
      bg = "white")
  generate_plot4()
}