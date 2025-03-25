#* @apiTitle Taxi Tip Prediction API
#* @apiDescription Predicts tip quantiles based on day, time period, and borough

library(plumber)
library(dplyr)
library(lubridate)
library(pins)

pin_name_base <- "nyc_pin_workshop" #make sure this matches 02_load_data.qmd param

# Read data from pin
board <- board_connect(auth = "auto")
pin_name <- sprintf("%s/%s", board$account, pin_name_base)
data <- board |> pin_read(pin_name)

#* Get tip quantile predictions based on input parameters
#* @param day_of_week Day of the week (Monday through Sunday)
#* @param period Period of day (Late Night, Morning, Afternoon, Evening)
#* @param borough Borough name
#* @post /predict
function(day_of_week, period, borough) {
  time_period <- period
  
  # Process the data
  filtered_data <- data |>
    filter(
      wday == day_of_week,
      period == time_period,
      Borough == borough
    )
  
  # Calculate quantiles
  quantiles <- filtered_data |>
    summarise(
      q10 = quantile(avg_tip, 0.1, na.rm = TRUE),
      q50 = quantile(avg_tip, 0.5, na.rm = TRUE),
      q90 = quantile(avg_tip, 0.9, na.rm = TRUE)
    )
  
  # Return results as a list
  list(
    q10 = as.numeric(quantiles$q10),
    q50 = as.numeric(quantiles$q50),
    q90 = as.numeric(quantiles$q90)
  )
}

#* Get available boroughs
#* @get /boroughs
function() {
  unique_boroughs <- unique(data$Borough)
  return(list(boroughs = unique_boroughs))
}

#* Get available periods
#* @get /periods
function() {
  list(periods = c("Late Night", "Morning", "Afternoon", "Evening"))
} 