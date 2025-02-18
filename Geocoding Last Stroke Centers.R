install.packages(c("dplyr", "stringdist", "fuzzyjoin"))

pacman::p_load(
  rio,                                     # to import data
  openxlsx,                                # Open Excel
  here,                                    # to locate files
  skimr,                                   # Provides summary statistics, Data type info, visual summaries
  dplyr,                                   # Data manipulation > Filtering, Transformation, Joining, etc.
  tidyverse,                               # to clean, handle, and plot the data (includes ggplot2 package)
  stringdist,
  fuzzyjoin,
  janitor,                                 # to clean column names
  haven,                                   # Can read SAS, SPSS and Stata files. Data Import/Export
  sas7bdat,                                # Can read data files in the SAS binary data format
  spdep,                                   # (Possibly not useful) Conducts spatial statistics
  groundhog,                                # Loads packages & their dependencies as available on chosen date on CRAN to create reproducability without worrying about updates
  units
  )


# Load your data
str <- read.csv("//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Raw/ArcGIS_Geocoded.csv")
geo <- read.csv("//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Clean/Total_2023_Geo.csv")


str <- str %>%
  select(USER_Certify, USER_OrganizationId, USER_OrganizationName, USER_CertificationProgram, USER_StreetAddress, 
         USER_City, USER_State, USER_PostalCode, X, Y,USER_HasDuplicates) %>%
  rename_with(~ gsub("^USER_", "", .))

final <- rbind(geo, str)

# Remove the word "Advanced" from all rows in CertificationProgram
final <- final %>%
  mutate(CertificationProgram = gsub("Advanced ", "", CertificationProgram)) %>%
  mutate(CertificationProgram = gsub("Ctr", "Center", CertificationProgram))

# Convert the data frame to an sf object
final_sf <- st_as_sf(final, coords = c("X", "Y"), crs = 4326) 

# Transform the coordinates to meters (CRS 3857)
final_sf_meters <- st_transform(final_sf, crs = 4326)

# Calculate the pairwise distances between all points
dist_matrix <- st_distance(final_sf_meters)

# Define a threshold for distance (300 feet = 91.44 meters)
threshold <- set_units(91.44, "m")  # Assign the unit to the threshold value


# Create a function to rank the CertificationProgram
rank_certification <- function(cert_program) {
  ranks <- c(
    "Comprehensive Stroke Center" = 1,
    "Thrombectomy Capable Stroke Center" = 2,
    "Primary Stroke Center" = 3,
    "Acute Stroke Ready Hospital" = 4
  )
  return(ranks[cert_program])
}

# Apply the rank function to the CertificationProgram column
final_sf_meters$rank <- sapply(final_sf_meters$CertificationProgram, rank_certification)

# Identify which rows are within 300 feet (91.44 meters) of each other
close_points <- which(dist_matrix < threshold, arr.ind = TRUE)

# Identify which rows are within 91.44 meters of each other (returns pairs of rows)
#close_points <- which(dist_matrix < threshold & dist_matrix > 0, arr.ind = TRUE)

# Create a group assignment for close points
group_assignments <- rep(NA, nrow(final_sf_meters))

# Initialize group counter
group_counter <- 1

# Identify which rows are within 300 feet of each other
#close_points <- which(dist_matrix < threshold, arr.ind = TRUE)

# Create a list of rows to remove based on lower ranks
rows_to_remove <- c()
for (i in 1:nrow(close_points)) {
  row1 <- close_points[i, 1]
  row2 <- close_points[i, 2]
  
  # Compare ranks and remove the row with the lower rank
  if (final_sf_meters$rank[row1] > final_sf_meters$rank[row2]) {
    rows_to_remove <- c(rows_to_remove, row2)
  } else {
    rows_to_remove <- c(rows_to_remove, row1)
  }
}

# Any ungrouped points are in their own group
group_assignments[is.na(group_assignments)] <- seq(max(group_counter, na.rm = TRUE), 
                                                   length.out = sum(is.na(group_assignments)))

# Add group assignments to the data frame
final_sf_meters$group <- group_assignments


# For each group, keep only the row with the highest rank
final_filtered <- final_sf_meters %>%
  group_by(group) %>%
  slice_min(order_by = rank) %>%  # Keep the row with the minimum (highest) rank
  ungroup()


# Extract x and y coordinates from the geometry and add them as columns
coordinates <- st_coordinates(final_filtered)
final_filtered$x <- coordinates[, 1]  # X coordinates
final_filtered$y <- coordinates[, 2]  # Y coordinates


final_filtered <- final_filtered %>%
  st_drop_geometry() %>%
  select(Certify, OrganizationId, OrganizationName, CertificationProgram, StreetAddress, 
         City, State, PostalCode, x, y)


# Remove the rows with lower ranks
#final_filtered <- final_sf_meters[-unique(rows_to_remove), ]


write_csv(final_filtered, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Clean/Total_2023_Clean_Final.csv")


