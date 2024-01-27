library(sf)
library(leaflet)
library(osmdata)
library(rvest)
library(dplyr)
library(lubridate)
library(tmaptools)
library(sp)
library(raster)
library(gstat)
library(tmap)
library(spatstat)
library(maptools)
library(raster)

data_directory <- paste0(getwd(),"/data")
croatia_shapefile <- read_sf("/Users/frangrenko/Developer/faks/lokacije/hr_1km.shp")
scraper <- function() {
  if (length(list.files(data_directory)) > 0){
    return()
  }
  current_time <- Sys.time()
  start_hour <- hour(current_time)
  end_hour <- start_hour + 23
  count <- 0
  for (i in start_hour:end_hour) {
    current_value <- i %% 24  # Wrap around to 0 when it exceeds 23
    if (nchar(as.character(current_value)) == 1){
      current_value <- paste0("0",current_value)
    }
    url <- paste0("https://meteo.hr/podaci.php?section=podaci_vrijeme&param=hrvatska1_n&sat=",current_value)
    # Read the HTML content of the webpage
    webpage <- read_html(url)
    
    # Extract the table data using CSS selectors
    table_data <- webpage %>%
      html_table(fill = TRUE)
    
    # Assuming the table of interest is the first one on the page
    table_df <- table_data[[1]]
    if (!file.exists(data_directory)){
      dir.create(data_directory, recursive = TRUE)
    }
    # Save the table data to a CSV file
    file_name = paste0(data_directory, "/table_data_",as.character(count), "_",current_value , ".csv")
    write.csv(table_df,file_name , row.names = FALSE)
    count <- count + 1
  }
}

mark_locations <- function() {
  #Load csv file
  csv_data <- read.csv(paste0(data_directory,"/",list.files(data_directory)[1]))
  locations <- csv_data[,1]
  temperatures <- csv_data[,4]
  
  replace_characters <- function(text) {
    if(text == "Puntijarka (Medvednica)"){
      text <- "Medvednica"
    }
    if(startsWith(text,"Gorice")){
      text <- "Gorice Brod Posavina"
    }
    text <- gsub("č", "c", text)
    text <- gsub("ć", "c", text)
    text <- gsub("š", "s", text)
    text <- gsub("đ", "d", text)
    text <- gsub("ž", "z", text)
    text <- gsub("Č", "C", text)
    text <- gsub("Ć", "C", text)
    text <- gsub("Š", "S", text)
    text <- gsub("Đ", "D", text)
    text <- gsub("Ž", "Z", text)
    
    words <- unlist(strsplit(text, " "))
    if (length(words) > 0 && words[length(words)] == "A") {
      words <- words[-length(words)]
      text <- paste(words, collapse = " ")
    }
    
    text <- paste(text, "Croatia")
    
    return(text)
  }
  
  locations <- lapply(locations,replace_characters)
  
  # Function to geocode location using OSM Nominatim
  geocode_osm <- function(location) {
    osm_query <- tmaptools::geocode_OSM(location)
    if (!is.null(osm_query) && !any(is.na(osm_query[c("x", "y")]))) {
      return(c(osm_query$coords[[1]], osm_query$coords[[2]]))
    } else {
      return(NULL)
    }
  }
  
  # Create a map
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 30, zoom = 2)
  
  longitude <- c()
  latitude <- c()
  # Add markers for each location
  marker_data <- NULL
  for (loc in locations) {
    result <- geocode_osm(loc)
    if (!is.null(result)) {
      longitude <- append(longitude,result[1])
      latitude <- append(latitude,result[2])
      map <- map %>%
        addMarkers(
          lng = result[1],
          lat = result[2],
          popup = paste("Location: ", loc)
        )
      marker_data <- rbind(marker_data, result)
    }
  }
  
  if (!is.null(marker_data)) {
    map <- map %>% fitBounds(
      lng1 = min(marker_data[, 1]),
      lat1 = min(marker_data[, 2]),
      lng2 = max(marker_data[, 1]),
      lat2 = max(marker_data[, 2])
    )
  }
  
  
  # Display the map
  map
  
  
  
  stations <- as.data.frame(cbind(longitude, latitude))
  return_list <- list(locations = locations, temperatures = temperatures, stations = stations)
  return(return_list)
}

spatial_points <- function(result_df){
  pts <- SpatialPoints(result_df)
  
  class (pts)
  showDefault(pts)
  
  df <- data.frame(ID=1:nrow(result_df))
  ptsdf <- SpatialPointsDataFrame(pts, data=df)
  
  plot(ptsdf)
  return(ptsdf)
}


scraper()
mark_locations_data <- mark_locations()

locations <- mark_locations_data$locations
stations <- mark_locations_data$stations
temperatures <- mark_locations_data$temperatures

spatial_points <- spatial_points(locations_data)

kriging <- function(locations, temperatures, grid_resolution = 0.1) {

  locations$X <- coordinates(locations)[, 1]
  locations$Y <- coordinates(locations)[, 2]
  
  # Create a regular grid for kriging
  grid <- expand.grid(X = seq(min(locations$X), max(locations$X), by = grid_resolution),
                      Y = seq(min(locations$Y), max(locations$Y), by = grid_resolution))
  grid_nrow <- nrow(grid)
  coordinates(grid) <- c("X", "Y")
  
  # Convert to SpatialPointsDataFrame
  points_grid <- SpatialPointsDataFrame(grid, data.frame(ID = 1:grid_nrow))
  
  # Fit a second-order polynomial model
  f.2 <- as.formula(temperatures ~ X + Y + I(X*X) + I(Y*Y) + I(X*Y))
  m.2 <- lm(f.2, data = locations)
  
  # Predict temperatures using the polynomial model
  temperatures_prediction <- predict(m.2, newdata = points_grid)
  
  # Add prediction to the grid data
  grid$temperature_prediction <- temperatures_prediction
  r <- raster(grid)
  
  # Specify the projection of raster to EPSG:4326 (assuming long lat coordinates)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
  # Mask the raster to the shape of Croatia
  r.m <- mask(r, croatia_shapefile)
  
  # Create tmap object
  tm_result <- tm_shape(r.m) +
    tm_raster(n = 10, palette = "RdBu", title = "Predicted temperature") +
    tm_shape(locations) + tm_dots(size = 0.2) +
    tm_legend(legend.outside = TRUE)
  
  # Print the map
  print(tm_result)
  
  # Return the tmap object
  return(tm_result)
}

# Perform kriging
kriging_result <- kriging(mark_locations_data$stations, mark_locations_data$temperatures)

