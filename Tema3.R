rm(list=ls())

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
library(raster)
library(rgdal)

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






scraper()
mark_locations_data <- mark_locations()

locations <- mark_locations_data$locations
stations <- mark_locations_data$stations
temperatures <- mark_locations_data$temperatures

stanice <- data.frame(
  x = stations$longitude,
  y = stations$latitude,
  temperature = temperatures
)
spatial_points = sp::SpatialPointsDataFrame(coords = cbind(stanice$x, stanice$y), data = stanice, proj4string = sp::CRS(projargs = "+init=epsg:32631"))

var = gstat::variogram(object = temperature~1, locations = spatial_points)

fit_var = gstat::fit.variogram(object = var, model = gstat::vgm(psill = 0.2, nugget = 0.01, range = 20, model = "Exp"))

grid_cells <- st_make_grid(st_bbox(spatial_points), cellsize = c(0.1, 0.1))

krig <- gstat::krige(formula = temperature~1, locations = spatial_points, newdata = grid_cells, model = fit_var)

r <- raster(krig)
r.m <- mask(r, croatia_shapefile)


# Plot the kriged results with specified breaks and colors
tm_shape(r.m) +
  tm_raster(n=10,palette = "RdBu", 
            title="Predicted temperature") +
  # Add spatial points as dots
  tm_shape(spatial_points) + tm_dots(size = 0.2) +
  # Add legend outside the map
  tm_layout(legend.outside = TRUE)


