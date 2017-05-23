### Install and import required packages
library("devtools")
library("dplyr")
library("tidyr")
library("ranger")
library("stringdist")
library("timeDate")
install_github("Ram-N/weatherData")
library("weatherData")

### Train model
# load("data/processed_uber_nyc.RData")
# 
# # Change data type of day and hour column
# agg_data$day = as.factor(agg_data$day)
# agg_data$hour = as.factor(agg_data$hour)
# 
# # Remove NA
# agg_data = agg_data[complete.cases(agg_data$zone), ]
# 
# ### Cluster zones to reduce number of levels
# hourly_pickups_zone = agg_data %>%
#   group_by(hour, locationID) %>%
#   summarize(mean_pickup = mean(pickups)) %>%
#   spread(hour, mean_pickup)
# hourly_pickups_zone[is.na(hourly_pickups_zone)] = 0
# 
# mydata = hourly_pickups_zone[, 2:25]
# 
# # Optimal number of sone clusters was 4
# zone_cluster = kmeans(x = mydata
#                       , centers = 4
#                       , nstart = 123
#                       , iter.max = 100)
# # Join cluster IDs with locationIDs
# hourly_pickups_zone$clusterID = zone_cluster$cluster
# zone_cluster = hourly_pickups_zone[, c("locationID", "clusterID")]
# rm(hourly_pickups_zone)
# 
# # Add clusterIDs to train and test sets
# agg_data = agg_data %>%
#   left_join(zone_cluster, by = "locationID")
# agg_data$clusterID = as.factor(agg_data$clusterID)
# 
# ### Cluster hours to reduce number of levels
# zone_pickups_hourly = agg_data %>%
#   group_by(locationID, hour) %>%
#   summarize(mean_pickup = mean(pickups)) %>%
#   spread(locationID, mean_pickup)
# zone_pickups_hourly[is.na(zone_pickups_hourly)] = 0
# 
# mydata = zone_pickups_hourly[, 2:ncol(zone_pickups_hourly)]
# 
# # Optimal number of hour clusters was 8
# hour_cluster = kmeans(x = mydata
#                       , centers = 8
#                       , nstart = 123
#                       , iter.max = 100)
# # Join hourIDs with hour
# zone_pickups_hourly$hourID = hour_cluster$cluster
# hour_cluster = zone_pickups_hourly[, c("hour", "hourID")]
# rm(zone_pickups_hourly)
# 
# # Add hourIDs to train and test sets
# agg_data = agg_data %>%
#   left_join(hour_cluster, by = "hour")
# agg_data$hourID = as.factor(agg_data$hourID)
# 
# # Fit model
# fit = ranger(pickups ~ hourID + clusterID + day + is_holiday + mean_temp_F
#              , data = agg_data
#              , num.trees = 5000
#              , mtry = 3
#              , write.forest = TRUE)
# 
# ### Perform some setup so predict function is faster
# # Get weather data for the last five years
# # Because of limitations of package, have to do a year at a time
# current_year = as.integer(format(Sys.Date(), "%Y"))
# years = seq(2012, current_year, 1)
# weather = data.frame(date = character(), mean_temp_F = integer())
# for (year in years) {
#   start_date = as.character(paste(year
#                                   , "-01-01"
#                                   , sep = ""))
#   end_date = as.character(paste(year
#                                 , "-12-31"
#                                 , sep = ""))
#   max_date = Sys.Date() - 1
#   if (as.Date(end_date) > max_date) {
#     end_date = max_date
#   }
# 
#   temp_weather = getWeatherForDate("JFK"
#                                    , start_date
#                                    , end_date)
#   temp_weather = temp_weather[c("Date", "Mean_TemperatureF")]
#   colnames(temp_weather) = c("date", "mean_temp_F")
#   temp_weather$date = as.character(temp_weather$date)
#   weather = rbind(weather, temp_weather)
# }
# 
# # Dimension tables
# loc_zone_dim = unique(agg_data[, c("clusterID", "locationID", "zone")])
# hour_dim = unique(agg_data[, c("hourID", "hour")])
# hour_dim$hourID = as.integer(hour_dim$hourID)
# hour_dim$hour = as.integer(hour_dim$hour)
# 
# save(fit, weather, loc_zone_dim, hour_dim
#      , file = "data/demo_model_files.RData")

load("data/demo_model_files.RData")

pred_pickups = function(hour, date, location_name) {
  # Get the hourID for the hour
  hourID = hour_dim$hourID[[match(hour, hour_dim$hour)]]
  
  # Get day of week
  day = format(as.POSIXct(date), "%a")
  
  # Get the weather for that date
  if (is.na(match(date, weather$date))) {
    # Use 2016 temp
    date_2016 = paste("2016"
                      , strsplit(date, "-")[[1]][2]
                      , strsplit(date, "-")[[1]][3]
                      , sep = "-")
    date_index = match(date_2016, weather$date)
  } else {
    date_index = match(date, weather$date)
  }
  mean_temp_F = weather$mean_temp_F[date_index]
  
  # Get holiday status for that date
  year = as.numeric(strsplit(date, "-")[[1]][1])
  holidays = holidayNYSE(as.numeric(year))
  holidays = format(holidays@Data, "%Y-%m-%d")
  is_holiday = date %in% holidays
  
  # Match given location to nearest one in dataset
  nearest_match_index = amatch(location_name
                               , loc_zone_dim$zone
                               , method = "cosine"
                               , maxDist = 0.2)
  if (is.na(nearest_match_index)) {
    # If no match, return prompt
    nearest_match = as.character(loc_zone_dim$zone[[92]])
    locationID = 92
    not_found_message = "Sorry, supplied location couldn't be matched. Try SoHo, World Trade Center, or West Village."
    return(not_found_message)
  } else {
    nearest_match = as.character(loc_zone_dim$zone[[nearest_match_index]])
    locationID = loc_zone_dim$locationID[[nearest_match_index]]
  }
  clusterID = loc_zone_dim$clusterID[[match(locationID, loc_zone_dim$locationID)]]
  
  pred = predict(fit, dat = data.frame(hourID = as.factor(hourID)
                                       , clusterID = as.factor(clusterID)
                                       , day = as.factor(day)
                                       , is_holiday = is_holiday
                                       , mean_temp_F = mean_temp_F))
  return(paste("Predicted demand: "
               , signif(pred$predictions, 3)
               , " pickups"
               , sep = ""))
}