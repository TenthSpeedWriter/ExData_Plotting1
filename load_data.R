library(dplyr)

# Returns all records from the dates of 1st & 2nd February, 2007
load_data <- function (data_path = "household_power_consumption.txt") {
  energy_data <- read.table(data_path,
                            na.strings = "?",
                            header = T,
                            sep = ";")
  data_in_range <- filter(energy_data,
                          energy_data$Date == "1/2/2007" | energy_data$Date == "2/2/2007")
  data_in_range
}