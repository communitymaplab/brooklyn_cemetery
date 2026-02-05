library(tidyverse)
library(googlesheets4)
library(sf)

# 1. READ DATA
# Read data from the "Mapping_Data" sheet
data <- read_sheet("https://docs.google.com/spreadsheets/d/1HsUItZaFWvv1XXC5flqRdwNJEtuV8fvr-eNzEuQj0dw/edit?usp=sharing") %>%
  set_names(c("time","name","o_name","o_type","d_name","d_type",
              "heading","dist_ft","dist_in"))

# Ensure 'row' column exists to act as a unique ID
if (!"row" %in% colnames(data)) {
  data <- data %>% mutate(row = row_number())
}

# 2. CREATE FUNCTIONS
# Function to calculate END point from START point
calculate_point_forward <- function(x, y, distance, azimuth) {
  azimuth_rad <- azimuth * pi / 180
  tibble(
    end_x = x + distance * sin(azimuth_rad),
    end_y = y + distance * cos(azimuth_rad)
  )
}

# Function to calculate START point from END point
calculate_point_backward <- function(x_end, y_end, distance, azimuth) {
  azimuth_rad <- azimuth * pi / 180
  tibble(
    start_x = x_end - distance * sin(azimuth_rad),
    start_y = y_end - distance * cos(azimuth_rad)
  )
}

# 3. ITERATIVELY CALCULATE ALL POINTS
max_iterations <- 100
for (iteration in 1:max_iterations) {
  print(paste("Running iteration:", iteration))
  data_before <- data
  
  # Forward calculation
  forward <- data_before %>%
    filter(!is.na(start_x) & !is.na(start_y) & is.na(end_x)) %>%
    rowwise() %>%
    mutate(calc = list(calculate_point_forward(start_x, start_y, Distance_m, Azimuth_deg))) %>%
    ungroup() %>%
    select(row, calc) %>% 
    unnest(calc, ptype = tibble(end_x = double(), end_y = double())) %>%
    select(row, end_x, end_y) 
  
  # Backward calculation
  backward <- data_before %>%
    filter(is.na(start_x) & !is.na(end_x) & !is.na(end_y)) %>%
    rowwise() %>%
    mutate(calc = list(calculate_point_backward(end_x, end_y, Distance_m, Azimuth_deg))) %>%
    ungroup() %>%
    select(row, calc) %>% 
    unnest(calc, ptype = tibble(start_x = double(), start_y = double())) %>%
    select(row, start_x, start_y) 
  
  # Update Data
  if (nrow(forward) > 0) {
    data <- data %>%
      left_join(forward, by = "row") %>%
      mutate(end_x = ifelse(is.na(end_x.x), end_x.y, end_x.x),
             end_y = ifelse(is.na(end_y.x), end_y.y, end_y.x)) %>%
      select(-ends_with(".x"), -ends_with(".y"))
  }
  
  if (nrow(backward) > 0) {
    data <- data %>%
      left_join(backward, by = "row") %>%
      mutate(start_x = ifelse(is.na(start_x.x), start_x.y, start_x.x),
             start_y = ifelse(is.na(start_y.x), start_y.y, start_y.x)) %>%
      select(-ends_with(".x"), -ends_with(".y"))
  }
  
  # Propagation
  points_list_start <- data %>% filter(!is.na(start_x)) %>% select(Point_Name = Start_Point, x = start_x, y = start_y)
  points_list_end <- data %>% filter(!is.na(end_x)) %>% select(Point_Name = End_Point, x = end_x, y = end_y)
  
  all_known_points <- bind_rows(points_list_start, points_list_end) %>%
    filter(!is.na(Point_Name)) %>% 
    group_by(Point_Name) %>%
    summarise(x = first(x), y = first(y), .groups = 'drop') 
  
  data <- data %>%
    select(-any_of(c("x", "y"))) %>% 
    left_join(all_known_points, by = c("Start_Point" = "Point_Name")) %>%
    mutate(start_x = ifelse(is.na(start_x), x, start_x), start_y = ifelse(is.na(start_y), y, start_y)) %>%
    select(-x, -y) 
  
  data <- data %>%
    select(-any_of(c("x", "y"))) %>% 
    left_join(all_known_points, by = c("End_Point" = "Point_Name")) %>%
    mutate(end_x = ifelse(is.na(end_x), x, end_x), end_y = ifelse(is.na(end_y), y, end_y)) %>%
    select(-x, -y) 
  
  if (isTRUE(all.equal(data_before, data))) { break }
}

# 4. EXPORT
points_summary <- bind_rows(
  data %>% select(Point_Name = Start_Point, x = start_x, y = start_y),
  data %>% select(Point_Name = End_Point, x = end_x, y = end_y)
) %>%
  filter(!is.na(Point_Name) & !is.na(x)) %>%
  group_by(Point_Name) %>%
  summarise(x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE), .groups = 'drop')

write_csv(points_summary, "points_summary.csv")

