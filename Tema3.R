rm(list=ls())
install.packages("sf")
library(sf)
# library(leaflet)
# library(osmdata)
# library(rvest)
# library(dplyr)
# library(lubridate)
# library(tmaptools)
# library(sp)
# library(raster)
# library(gstat)
# library(tmap)
# library(spatstat)
# library(maptools)
# library(raster)

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
  print(map)
  
  
  
  stations <- as.data.frame(cbind(longitude, latitude))
  return_list <- list(locations = locations, temperatures = temperatures, stations = stations)
  return(return_list)
}

spatial_points <- function(result_df) {
  sf_pts <- st_as_sf(result_df, coords = c("longitude", "latitude"), crs = st_crs(4326))
  
  plot(sf_pts)
  return(sf_pts)
}






scraper()
mark_locations_data <- mark_locations()

locations <- mark_locations_data$locations
stations <- mark_locations_data$stations
temperatures <- mark_locations_data$temperatures

spatial_points <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = st_crs(4326))

# Add temperature information to spatial points
spatial_points$temperatures <- temperatures

variogram_model <- variogram(temperatures ~ 1, data = spatial_points)


# Example initial values (you may need to adjust these based on your data)
initial_values <- c(range = 1000, sill = 200, nugget = 20)

# Fit variogram with custom initial values
kriging_model <- fit.variogram(variogram_model, model = vgm("Sph", psill = initial_values[2], range = initial_values[1], nugget = initial_values[3]))


# Create a gstat object
kriging_gstat <- gstat(id = "temperatures", formula = temperatures ~ 1, data = spatial_points, model = kriging_model)


# Perform kriging
# Perform kriging
# Create a raster grid covering your study area
raster_grid <- raster::raster(extent(croatia_shapefile), resolution = c(0.1, 0.1))

# Convert the spatial points to a raster grid
spatial_points_raster <- rasterize(spatial_points, raster_grid, field = "temperatures")

kriging_result <- predict(kriging_gstat, raster_grid)
kriging_raster <- raster::raster(kriging_result$var1.pred)

# Plot the digital map with contours of Croatia
tm_shape(croatia_shapefile) +
  tm_borders() +
  tm_raster(kriging_raster, palette = "RdYlBu", title = "Temperature") +
  tm_layout(title = "Spatial Distribution of Temperature")



