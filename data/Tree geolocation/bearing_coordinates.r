library(sf)
library(tidyverse)

# Example data structure - replace with your actual data
# Each row represents a bearing/distance FROM one point TO another point
data <- tibble(
  from_point = c(1, 1, 2, 3, 2, 5, 4, 7, 8, 6),  # starting point
  to_point = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 7),   # ending point
  bearing = c(45, 90, 135, 180, 270, 315, 0, 45, 90, 225),  # in degrees
  distance = c(100, 150, 200, 100, 175, 125, 150, 180, 140, 95)  # in meters
)

# Known coordinates for point 1 (UTM Zone 16N)
# Replace these with your actual coordinates
known_point <- tibble(
  point_id = 1,
  easting = 500000,  # example UTM easting
  northing = 4000000  # example UTM northing
)

# Function to calculate new point from bearing and distance
calculate_point_from_bearing <- function(easting, northing, bearing, distance) {
  # Convert bearing to radians
  bearing_rad <- bearing * pi / 180
  
  # Calculate new coordinates
  # Bearing is measured clockwise from north
  new_easting <- easting + distance * sin(bearing_rad)
  new_northing <- northing + distance * cos(bearing_rad)
  
  return(list(easting = new_easting, northing = new_northing))
}

# Initialize coordinates with the known point
coordinates <- known_point %>%
  mutate(calculated = FALSE)  # track which points are calculated

# Get all unique point IDs
all_points <- unique(c(data$from_point, data$to_point))
remaining_points <- setdiff(all_points, known_point$point_id)

# Iteratively calculate points until all are done
max_iterations <- length(remaining_points) * 2  # safety limit
iteration <- 0

while (length(remaining_points) > 0 && iteration < max_iterations) {
  iteration <- iteration + 1
  points_calculated_this_round <- 0
  
  # Look for connections where we know the starting point
  for (i in 1:nrow(data)) {
    from_id <- data$from_point[i]
    to_id <- data$to_point[i]
    
    # Check if we know the 'from' point but not the 'to' point
    if (from_id %in% coordinates$point_id && !(to_id %in% coordinates$point_id)) {
      from_coords <- coordinates %>% filter(point_id == from_id)
      
      new_coords <- calculate_point_from_bearing(
        easting = from_coords$easting,
        northing = from_coords$northing,
        bearing = data$bearing[i],
        distance = data$distance[i]
      )
      
      coordinates <- bind_rows(
        coordinates,
        tibble(
          point_id = to_id,
          easting = new_coords$easting,
          northing = new_coords$northing,
          calculated = TRUE
        )
      )
      
      remaining_points <- setdiff(remaining_points, to_id)
      points_calculated_this_round <- points_calculated_this_round + 1
    }
  }
  
  # If we didn't calculate any new points this round, we're stuck
  if (points_calculated_this_round == 0) {
    warning(paste("Unable to calculate", length(remaining_points), 
                  "points. They may not be connected to the known point."))
    cat("Remaining points:", paste(remaining_points, collapse = ", "), "\n")
    break
  }
}

# Sort by point_id for easier viewing
coordinates <- coordinates %>% 
  arrange(point_id) %>%
  select(-calculated)

# Convert to sf object with UTM Zone 16N projection
# EPSG:32616 is UTM Zone 16N (Northern Hemisphere)
# Use EPSG:32716 for UTM Zone 16S (Southern Hemisphere)
points_sf <- st_as_sf(
  coordinates,
  coords = c("easting", "northing"),
  crs = 32616  # UTM Zone 16N
)

# View results
print("Calculated coordinates:")
print(coordinates)
print("\nSpatial features:")
print(points_sf)

# Create line segments showing the connections
segments_sf <- data %>%
  left_join(coordinates %>% rename(from_point = point_id, 
                                    from_easting = easting, 
                                    from_northing = northing), 
            by = "from_point") %>%
  left_join(coordinates %>% rename(to_point = point_id,
                                    to_easting = easting,
                                    to_northing = northing),
            by = "to_point") %>%
  filter(!is.na(from_easting) & !is.na(to_easting)) %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(
    matrix(c(from_easting, from_northing, to_easting, to_northing), 
           ncol = 2, byrow = TRUE)
  ), crs = 32616)) %>%
  st_as_sf()

# Optional: Convert to lat/lon if needed
points_latlon <- st_transform(points_sf, crs = 4326)

# Plot the network
ggplot() +
  geom_sf(data = segments_sf, color = "gray60", linewidth = 0.5) +
  geom_sf(data = points_sf, size = 3, color = "blue") +
  geom_sf_text(data = points_sf, aes(label = point_id), 
               nudge_x = 50, nudge_y = 50, size = 3.5) +
  theme_minimal() +
  labs(title = "Calculated Point Positions",
       subtitle = "UTM Zone 16N Coordinates - Points connected by bearings")
