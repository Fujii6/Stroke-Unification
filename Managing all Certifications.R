# Title: Stroke Center Table
# Author: Yui Fujii
# Start Date: 2/14/2023
# Most Recent Update Date: 2/7/2024
# First, load the pacman package


pacman::p_load(
  rio,                                     # to import data
  openxlsx,                                # Open Excel
  here,                                    # to locate files
  skimr,                                   # Provides summary statistics, Data type info, visual summaries
  dplyr,                                   # Data manipulation > Filtering, Transformation, Joining, etc.
  tidyverse,                               # to clean, handle, and plot the data (includes ggplot2 package)
  janitor,                                 # to clean column names
  haven,                                   # Can read SAS, SPSS and Stata files. Data Import/Export
  sas7bdat,                                # Can read data files in the SAS binary data format
  spdep,                                   # (Possibly not useful) Conducts spatial statistics
  groundhog                                # Loads packages & their dependencies as available on chosen date on CRAN to create reproducability without worrying about updates
)
# Run groundhog to indicate the date of the packages that should be ran
groundhog.library(rio, '2023-10-02')
groundhog.library(here, '2023-10-02')
groundhog.library(skimr, '2023-10-02')
groundhog.library(dplyr, '2023-10-02')
groundhog.library(janitor, '2023-10-02')
groundhog.library(png, '2023-10-02')
groundhog.library(jpeg, '2023-10-02')
groundhog.library(sas7bdat, '2023-10-02')
groundhog.library(spdep, '2023-10-02')


# Assign file directory to import data sets 

file_path_jc <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2023/Clean/jc_2023_Clean_Final.csv"
file_path_hfap <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/HFAP/2023/Clean/hfap_2023_Clean_Final.csv"
file_path_dnv <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/DNV/2023/Clean/dnv_2023_Clean_Final.csv"
file_path_state <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/States/2023/Clean/State_2023_Clean_Final.csv"


# Import Files ------------------------------------------------------------
# Remove all stroke Rehabilitation (SR) facilities
  # We want to remove all SR facilities because they will look like false
  # duplicates. 
# All tables will need to be formatted properly


jc <- read.csv(file_path_jc) %>% 
  filter(CertificationProgram != "Stroke Rehabilitation") %>%
  mutate(OrganizationId = paste("jc", OrganizationId, OrganizationName, CertificationProgram, sep = "_"))

hfap <- read.csv(file_path_hfap) %>%
  mutate(OrganizationId = paste("hfap", OrganizationId, OrganizationName, CertificationProgram, sep = "_"))

dnv <- read.csv(file_path_dnv) %>%
  mutate(
    PostalCode = NA,
    OrganizationId = paste("dnv", OrganizationId, OrganizationName, CertificationProgram, sep = "_"))

state <- read.csv(file_path_state) %>% 
  mutate(
    OrganizationId = paste("state", OrganizationId, OrganizationName, CertificationProgram, sep = "_"))

# Combine file
stacked_table <- rbind(jc, hfap, dnv, state)



# Clean Address and City------------------------------------------------------------
# Clean the address for comparison purposes.

# Add a new column 'CleanedAddress' to stacked_table
stacked_table$CleanedAddress <- tolower(stacked_table$StreetAddress)
stacked_table$City <- tolower(stacked_table$City)

# Step 1: Remove all punctuation and collapse
stacked_table$CleanedAddress <- gsub("[[:punct:]]", "", stacked_table$CleanedAddress)
stacked_table$City <- gsub("[[:punct:]]", "", stacked_table$City)

# Step 2: Replace abbreviated words with full words
abbreviation_mapping <- c("ave" = "avenue", "blvd" = "boulevard", "bldg" = "building", "ct" = "court",
                          "dr" = "drive", "expwy" = "expressway", "expy" = "expressway", "hwy" = "highway", "ln" = "lane",
                          "pkwy" = "parkway", "pl" = "place", "plz" = "plaza", "rd" = "road",
                          "st" = "street", "ter" = "terrace", "trl" = "trail", "wy" = "way",
                          "apt" = "apartment", "ste" = "suite", "fl" = "floor", "rm" = "room",
                          "dept" = "department", "bldg" = "building", "corp" = "corporation",
                          "ln" = "lane", "hwy" = "highway", "ctr" = "center", "pl" = "place",
                          "ext" = "extension", "int" = "intersection", "jct" = "junction",
                          "mt" = "mount", "opas" = "overpass", "pi" = "pier", "pt" = "point",
                          "riv" = "river", "spg" = "spring", "sqr" = "square", "sta" = "station",
                          "str" = "stream", "uni" = "union", "vlg" = "village", "vly" = "valley",
                          "cir" = "circle", "alc" = "alcove", "byu" = "bayou",
                          "cyn" = "canyon", "ct" = "court", "exp" = "expressway", "frt" = "fort",
                          "hvn" = "haven", "jct" = "junction", "mnr" = "manor", "mt" = "mount",
                          "plz" = "plaza", "prk" = "park", "sta" = "station", "trce" = "trace",
                          "trk" = "track", "via" = "viaduct", "vlg" = "village", "dr" = "drive",
                          "ln" = "lane", "rd" = "road", "st" = "street", "ct" = "court",
                          "blvd" = "boulevard", "sq" = "square", "aly" = "alley", "pl" = "place",
                          "ter" = "terrace", "pkwy" = "parkway", "hwy" = "highway", "trl" = "trail",
                          "way" = "way", "row" = "row", "cres" = "crescent", "pl" = "place",
                          "cl" = "close", "prom" = "promenade", "sq" = "square", "pde" = "parade",
                          "tce" = "terrace", "qy" = "quay", "crs" = "cross", "grn" = "green",
                          "arc" = "arcade", "gln" = "glen", "rg" = "rise", "ct" = "court",
                          "cct" = "circuit", "mew" = "mews", "bend" = "bend", "pt" = "point",
                          "isle" = "isle", "way" = "way", "tce" = "terrace", "cct" = "circuit",
                          "hts" = "heights", "rdg" = "ridge", "bluff" = "bluff",  "n" = "north", 
                          "s" = "south", "e" = "east", "w" = "west", "ne" = "northeast", 
                          "nw" = "northwest", "se" = "southeast", "sw" = "southwest")

# Loop through the abbreviation mapping and replace
for (abbreviation in names(abbreviation_mapping)) {
  stacked_table$CleanedAddress <- gsub(paste0("\\b", abbreviation, "\\b"), abbreviation_mapping[abbreviation], stacked_table$CleanedAddress)
}


# # Step 2: Remove "PO Box" from CleanedAddress
stacked_table$CleanedAddress <- gsub("po\\s*box", "", stacked_table$CleanedAddress, ignore.case = TRUE)

# Step 3: Replace spelled-out numbers with actual numbers
number_mapping <- c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5",
                    "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "zero" = "0",
                    "first" = "1", "second" = "2", "third" = "3", "fourth" = "4", "fifth" = "5",
                    "sixth" = "6", "seventh" = "7", "eighth" = "8", "ninth" = "9", "tenth" = "10",
                    "eleventh" = "11", "twelfth" = "12", "thirteenth" = "13", "fourteenth" = "14",
                    "fifteenth" = "15", "sixteenth" = "16", "seventeenth" = "17", "eighteenth" = "18",
                    "nineteenth" = "19", "twentieth" = "20")

for (number_word in names(number_mapping)) {
  stacked_table$CleanedAddress <- gsub(paste0("\\b", number_word, "\\b"), number_mapping[number_word], stacked_table$CleanedAddress, ignore.case = TRUE)
}

# Step 4: Remove th, rd, nd, st from all numbers
stacked_table$CleanedAddress <- gsub("(\\d+)(th|rd|nd|st)\\b", "\\1", stacked_table$CleanedAddress)
stacked_table$CleanedAddress <- gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", stacked_table$CleanedAddress)
stacked_table$CleanedAddress <- gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2", stacked_table$CleanedAddress)



# Format Table for Census Geocoder -----------------------------------------
# We need an easier way to identify duplicates. For this reason,
# our data is required to be formatted properly. The formatting
# scheme we are following is in line with what the Census Geocoder
# requires the table to look like so the tool can properly 
# preform the geocode!

# Rename all column headers and only keep those that follow the census table scheme
stacked_table <- stacked_table %>% 
  mutate(
    `Unique ID` = OrganizationId,  # Renaming column OrganizationId to Unique ID
    `Street address` = str_squish(CleanedAddress),  # Renaming column StreetAddress to Street address
    ZIP = PostalCode,) %>%
  select(`Unique ID`, `Street address`, City, State, ZIP)  # Selecting only the specified columns

write_csv(stacked_table, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Total_2023_Census_Geo_Needed.csv")

# Remove multiple tables
rm(dnv, hfap, jc, state)

# Export the table we just created and use this table to 
# upload onto the census tool. Copy and paste this link:
# https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form
# Upload the table and select "Public_AR_Curren" as the Benchmark and
# "Current_Current" as the Vintage.



# Import Geocoded Data back into R ----------------------------------------------------

# Assign file directory to import data sets 
file_path_state_geo <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Raw/Total_2023_Geo_Partial.csv"

# Read data from CSV file
stacked_table <- read.csv(file_path_state_geo)

# The Census Geocoder disorganizes the table when it is processed.
# This requires us to cleane the table and organize it properly for our purpose


###### Reorganize header ##################################
  # The tool moves the the header columns elsewhere. We need to first locate it.
  # Find the index of the row containing "Unique ID"
header_index <- which(stacked_table == "Unique ID", arr.ind = TRUE)[1, 1] 
# Extract the original header row
original_header <- colnames(stacked_table) 
# Extract the address part
address <- unlist(strsplit(original_header[2], "\\."))  # Split by period
address[1] <- gsub("^X", "", address[1])  # Remove 'X' from the first part
# Find the indices of non-empty elements
non_empty_indices <- grep("\\S", address)
# Subset address_parts to include only non-empty elements
address <- address[non_empty_indices]
# Join the last four words with commas
if (length(address) >= 4) {
  last_four_words <- paste(address[(length(address) - 3):length(address)], collapse = ", ")
  first_words <- paste(address[1:(length(address) - 4)], collapse = " ")
  formatted_address <- paste(first_words, last_four_words, sep = " ")}
# Extract the coordinates and reformat them
coordinates_parts <- unlist(strsplit(original_header[6], "(?<=\\d{2}\\.\\d{14})\\.(?=\\d{2})", perl = TRUE))
coordinates_parts <- gsub("^X\\.", "", coordinates_parts)
formatted_coordinates <- paste(coordinates_parts, collapse = ",")
# Replace the second element in the original_header vector with the formatted address
original_header[2] <- formatted_address
# Replace the sixth element in the original_header vector with the formatted coordinates
original_header[6] <- formatted_coordinates
# Replace the column names with the values from the row where "Unique ID" is found
colnames(stacked_table) <- stacked_table[header_index, ]
# Remove the row with "Unique ID"
stacked_table <- stacked_table[-header_index, ]
# Insert the original header row as the first row of the dataframe
stacked_table <- rbind(original_header, stacked_table)



####### Replace Header with Proper Names ######################
  # Replace column names from the third column onwards with temporary names
temp_column_names <- names(stacked_table)
temp_column_names[3:length(temp_column_names)] <- paste0("Temp", 1:(length(temp_column_names) - 2))
names(stacked_table) <- temp_column_names

# Remove columns 3, 4, 5, and 8
stacked_table <- stacked_table %>%
  select(-c(3, 4, 5, 7, 8)) 

# Rename columns 6 and 7
stacked_table <- stacked_table %>%
  mutate(Temp4 = gsub("X", "", Temp4))

stacked_table <- stacked_table %>% 
  separate(
    col = 'Unique ID',
    into = c("Certify", "OrganizationId", "OrganizationName", "CertificationProgram"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    OrganizationName = gsub("\\.", " ", OrganizationName),
    CertificationProgram = gsub("\\.", " ", CertificationProgram)
  ) %>% 
  separate(
    col = 'Street address, City, State, ZIP',
    into = c("StreetAddress", "City", "State", "PostalCode"),
    sep = ",",
    remove = FALSE
  ) %>% 
  separate(
    col = 'Temp4',
    into = c("X","Y"),
    sep = ",",
    remove = FALSE
  ) %>%
  select("Certify", "OrganizationId", "OrganizationName", "CertificationProgram",
         "StreetAddress", "City", "State", "PostalCode", "X", "Y")




# Separate Table ----------------------------------------------------------
# Split data for those that have X & Y Coordinates or no X & Y Coordinates
# This means not all hospitals were geocoded. We split the two here. 

##### Filter rows where column X is not empty ################
    # Table that contains X & Y Coordinates
    geoid <- stacked_table %>%
      filter(!is.na(X) & X != "")
##### Filter rows where column X is empty ##################
    # Table that contains NO X & Y Coordinates
    no_geoid <- stacked_table %>%
      filter(!is.na(X) & X == "")
    
    
# Identify duplicates #1: Use Geocodes (X & Y Coordinates) -----------------------------------------------------

  # Identify duplicates based on X & Y Coordinates
  # in the geoid table (table that has X & Y coordinates)
   dup_geoid <- geoid %>%
     filter(duplicated(X) | duplicated(X, fromLast = TRUE))

   non_dup_geoid <- geoid %>%
     group_by(X) %>%
     filter(n() == 1) %>%
     ungroup()
   
   # Count occurrences of each value in column X
   counts <- geoid %>% 
     group_by(X) %>% 
     summarise(count = n())

          # Filter rows where the count is greater than 2
          dup3_geoid <- geoid %>%
            inner_join(counts, by = "X") %>%
            filter(count > 2)
          
          # Filter rows where the count is greater than 2
          dup2_geoid <- dup_geoid %>%
            inner_join(counts, by = "X") %>%
            filter(count <= 2)
          
          dup_comb_geoid <- rbind(dup3_geoid, dup2_geoid)
          
          
          stroke_agency_rank <- c(
            "jc",
            "hfap",
            "dnv",
            "state"
          )    

          # Here we isolate rows that have the same Effective Date and only keep
          # the rows that contains the higher Stroke Center Ranking. 
          filtered_dup_geoid <- dup_comb_geoid %>%
            group_by(X) %>%
            filter(n() > 1) %>%
            arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
            slice_head(n = 1) %>%
            ungroup() %>%
            select(-count)
          
          
     # Remove rows with the same X value as dup3_stacked_geoid
     Final_no_dup_geoid <- rbind(filtered_dup_geoid, non_dup_geoid)
          
############## Remove multiple tables ########################
     rm(dup_geoid, non_dup_geoid, counts, dup3_geoid,
        dup2_geoid, dup_comb_geoid, filtered_dup_geoid)
##############################################################
     
# Similarity Function -----------------------------------------------------

# Function to calculate Jaccard similarity between two strings
jaccard_similarity <- function(str1, str2) {
  set1 <- unlist(strsplit(str1, " "))
  set2 <- unlist(strsplit(str2, " "))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  similarity <- intersection / union
  return(similarity)
}

# Similarity for no_geoid (Table without X & Y Coordinates) -------------------------------------------------


J_similarity_matrix <- outer(no_geoid$StreetAddress,
                           no_geoid$StreetAddress, 
                           Vectorize(jaccard_similarity))                       # Calculate Jaccard similarity for each pair of rows
diag(J_similarity_matrix) <- 0                                                    # Exclude self-comparisons (rows compared with themselves)
J_threshold <- 0.6                                                                # Threshold for considering duplicates
J_duplicate_matrix <- J_similarity_matrix > J_threshold                               # Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
no_geoid$HasDuplicates <- apply(J_duplicate_matrix, 2, any)                       # Create a new column indicating whether each row has duplicates


no_dup_ngo <- no_geoid[!no_geoid$HasDuplicates, ]
dup_ngo <- no_geoid[no_geoid$HasDuplicates, ]



# Similarity for the first no duplicate group from the No geoID table -------------------------------------------------


J_duplicate_matrix <- outer(no_dup_ngo$StreetAddress, 
                           no_dup_ngo$StreetAddress, 
                           Vectorize(jaccard_similarity))                       # Calculate Jaccard similarity for each pair of rows
diag(J_duplicate_matrix) <- 0                                                    # Exclude self-comparisons (rows compared with themselves)
threshold <- 0.5                                                                # Threshold for considering duplicates
J_duplicate_matrix <- J_duplicate_matrix > threshold                               # Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
no_dup_ngo$HasDuplicates <- apply(J_duplicate_matrix, 2, any)                     # Create a new column indicating whether each row has duplicates


no_dup2_ngo <- no_dup_ngo[!no_dup_ngo$HasDuplicates, ]
dup2_ngo <- no_dup_ngo[no_dup_ngo$HasDuplicates, ]


# Here we try to identify duplicates again but by comparing geographic locations
# We filter twice, first by state than even further b y City

st_dup2_ngo <- dup2_ngo %>%
  filter(duplicated(State) | duplicated(State, fromLast = TRUE))
dup3_ngo <- st_dup2_ngo %>%
  filter(duplicated(City) | duplicated(City, fromLast = TRUE))


# We then produce another set of duplicates and 
# Duplicate: dup3_ngo
# None Duplicate: fin_non_dup_ngo
dup3_ngo <- dup3_ngo %>%
  arrange(City) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(City, Group) %>%
  arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

fin_non_dup_ngo <- anti_join(no_dup_ngo, dup3_ngo)


################ Remove multiple tables #######################
rm(geoid, no_geoid, no_dup_ngo,  no_dup2_ngo, dup2_ngo, st_dup2_ngo)
###############################################################

# Similarity for the first duplicate group from the No geoID table -------------------------------------------------

t_dup_ngo <- dup_ngo %>%
  filter(duplicated(City) | duplicated(City, fromLast = TRUE))

f_dup_ngo <- anti_join(dup_ngo, t_dup_ngo)


J_similarity_matrix <- outer(f_dup_ngo$StreetAddress, 
                           f_dup_ngo$StreetAddress, 
                           Vectorize(jaccard_similarity))                       # Calculate Jaccard similarity for each pair of rows
diag(J_similarity_matrix) <- 0                                                    # Exclude self-comparisons (rows compared with themselves)
threshold <- 0.6                                                                # Threshold for considering duplicates
J_duplicate_matrix <- J_similarity_matrix > threshold                               # Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
f_dup_ngo$HasDuplicates <- apply(J_duplicate_matrix, 2, any)                      # Create a new column indicating whether each row has duplicates


no_dup_ngo2 <- f_dup_ngo[!f_dup_ngo$HasDuplicates, ]
dup_ngo2 <- f_dup_ngo[f_dup_ngo$HasDuplicates, ]

t_dup_ngo2 <- dup_ngo2 %>%
  filter(duplicated(State) | duplicated(State, fromLast = TRUE))

f_nodup_ngo <- anti_join(dup_ngo2, t_dup_ngo2)

########### Use duplicate for the geoID table to identify none duplicates

st_dup_ngo2 <- t_dup_ngo %>%
  filter(duplicated(StreetAddress) | duplicated(StreetAddress, fromLast = TRUE))

st2_dup_ngo2 <- t_dup_ngo %>%
  filter(!(duplicated(StreetAddress) | duplicated(StreetAddress, fromLast = TRUE)))

st_dup_ngo2 <- st_dup_ngo2 %>%
  arrange(City, StreetAddress) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(City, Group) %>%
  arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()


st2_dup_ngo2 <- st2_dup_ngo2 %>%
  arrange(State, City) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(State, City, Group) %>%
  arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

t_dup_ngo2 <- t_dup_ngo2 %>%
  arrange(State, StreetAddress) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(Group) %>%
  arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()


################# Remove multiple tables #######################
rm(dup_ngo, f_dup_ngo, dup_ngo2, t_dup_ngo)
################################################################

Final_nodup1 <- rbind(f_nodup_ngo, no_dup_ngo2, fin_non_dup_ngo) %>%
  select("Certify", "OrganizationId", "OrganizationName", "CertificationProgram",
         "StreetAddress", "City", "State", "PostalCode", "X", "Y")

Final_nodup2 <- rbind(dup3_ngo, st_dup_ngo2, st2_dup_ngo2, t_dup_ngo2) %>%
  select("Certify", "OrganizationId", "OrganizationName", "CertificationProgram",
         "StreetAddress", "City", "State", "PostalCode", "X", "Y")






# Similarity for dnv_curr -------------------------------------------------

Tot <- rbind(Final_no_dup_geoid, Final_nodup1, Final_nodup2)



# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(Tot$StreetAddress, Tot$StreetAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
Tot$HasDuplicates <- apply(duplicate_matrix, 2, any)


no_dup_tot <- Tot[!Tot$HasDuplicates, ]
dup_tot <- Tot[Tot$HasDuplicates, ]

################# Remove multiple tables #######################
rm(f_nodup_ngo, no_dup_ngo2,no_dup2_ngo, fin_non_dup_ngo,
   dup3_ngo, st_dup_ngo2, st2_dup_ngo2, t_dup_ngo2)
################################################################



dup2_tot <- dup_tot %>%
  filter(duplicated(State) | duplicated(State, fromLast = TRUE))

nodup_tot <- anti_join(no_dup_tot, dup2_tot)



state_dup_tot <- dup2_tot %>%
  filter(duplicated(City) | duplicated(City, fromLast = TRUE))

state2_dup_tot <- dup2_tot %>%
  filter(!(duplicated(City) | duplicated(City, fromLast = TRUE))) %>%
  select("Certify", "OrganizationId", "OrganizationName", "CertificationProgram",
         "StreetAddress", "City", "State", "PostalCode", "X", "Y")

state_tdup_tot <- state_dup_tot %>%
  arrange(City, StreetAddress) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(City, Group) %>%
  filter(!duplicated(Certify) & !duplicated(Certify, fromLast = TRUE)) %>%
  arrange(match(Certify, stroke_agency_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()%>%
  select("Certify", "OrganizationId", "OrganizationName", "CertificationProgram",
         "StreetAddress", "City", "State", "PostalCode", "X", "Y")


fin_dup_tot <- anti_join(state_dup_tot, state_tdup_tot)

fin_dup_tot <- fin_dup_tot %>%
  arrange(City, StreetAddress) %>%
  mutate(Group = rep(1:(n() %/% 2), each = 2)) %>%
  group_by(City, Group) %>%
  filter(!duplicated(Certify) & !duplicated(Certify, fromLast = TRUE))

Total <- anti_join(Tot, fin_dup_tot)


################# Remove multiple tables #######################
rm(fin_dup_tot, fin_dup_tot,state_tdup_tot, state2_dup_tot,
   state_dup_tot, nodup_tot, dup2_tot, dup_tot, no_dup_tot,
   Final_nodup1, Final_nodup2, no_dup_tot)
################################################################

#####################Final Geocoding########################

##### Filter rows where column X is not empty ############
# Table that contains X & Y Coordinates
Total_geoid <- Total %>%
  filter(!is.na(X) & X != "")
##### Filter rows where column X is empty ################
# Table that contains NO X & Y Coordinates
Total_no_geoid <- Total %>%
  filter(!is.na(X) & X == "")


# Compare previous year with current (JC) ---------------------------------


jc_filtered <- Total_no_geoid %>% 
  filter(Certify == "jc") %>%
  select(-X, -Y)

dnv_hfap_state_filtered <- Total_no_geoid %>% 
  filter(Certify != "jc")

jc_notmissing2022 <- read.csv("//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2022/Clean/jc_notmissing2022.csv")

joined_data_unique <- jc_notmissing2022 %>%
  distinct(OrganizationId, .keep_all = TRUE)

joined_data <- jc_filtered %>%
  mutate(OrganizationId = as.character(OrganizationId)) %>%
  left_join(
    joined_data_unique %>%
      mutate(OrganizationId = as.character(OrganizationId)) %>%
      select(OrganizationId, X = LONG, Y = LAT), 
    by = "OrganizationId"
  )


combined_data <- rbind(dnv_hfap_state_filtered, joined_data)

# Table that contains X & Y Coordinates
Total_geoid.comb <- combined_data %>%
  filter(!is.na(X) & X != "")
##### Filter rows where column X is empty ################
# Table that contains NO X & Y Coordinates
Total_no_geoid.comb <- combined_data %>%
  filter(!is.na(X) & X == "")

sum(is.na(combined_data$X))

T_data <- rbind(Total_geoid, Total_geoid.comb)


# PLEASE CHANGE WHERE THIS IS SAVED
# SAVE THIS IN THE CURRENT YEAR THAT YOU ARE WORKING ON!!!!!!

write_csv(T_data, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Clean/Total_2023_Geo.csv")
write_csv(Total_no_geoid.comb, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/COMBINED ALL STROKES/2023/Clean/Total_2023_Geo_Needed.csv")






##################### TEST #################################

# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(Total$StreetAddress, Total$StreetAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
Total$HasDuplicates <- apply(duplicate_matrix, 2, any)


ZZZ_NO_DUP <- Total[!Total$HasDuplicates, ]
ZZZ_DUP <- Total[Total$HasDuplicates, ]

###########################################################































file_path_comb <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Historical/From AREB/Copy of Certified stroke centers and General Hospitals_2009-2013.xlsx"

# Import Files ------------------------------------------------------------
# Remove all stroke Rehabilitation (SR) facilities
# We want to remove all SR facilities because they will look like false
# duplicates. 
# All tables will need to be formatted properly

comb <- read.xlsx(file_path_comb) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(OrganizationId = paste(Confirmed.ED, PSC, Yes.PSC, PSC.2010, Yes.PSC.2010,  New.PSC.2010, Multicampus.2010, PSC.2011,
                                Yes.PSC.2011, New.PSC.2011, Multi.campus.2011, PSC.2012, Yes.PSC.2012, New.PSC.2012,
                                Multi.campus.2012, PSC.2013, Yes.PSC.2013, New.PSC.2013, Multi.campus.2013, Tot.PSC.Multiple, MLOCSTCD,
                                State.Stroke.Program, Impl.SSOC, MNAME, sep = "_")) %>%
  mutate(
    `Unique ID` = OrganizationId,  # Renaming column OrganizationId to Unique ID
    `StreetAddress` = str_squish(MLOCADDR),  # Renaming column StreetAddress to Street address
    City = MLOCCITY,
    State = MSTATE,
    ZIP = MLOCZIP) %>%
  select(`Unique ID`, StreetAddress, State, City, ZIP)

# Clean Address and City------------------------------------------------------------
# Clean the address for comparison purposes.

# Add a new column 'CleanedAddress' to comb
comb$CleanedAddress <- tolower(comb$StreetAddress)
comb$City <- tolower(comb$City)

# Step 1: Remove all punctuation and collapse
comb$CleanedAddress <- gsub("[[:punct:]]", "", comb$CleanedAddress)
comb$City <- gsub("[[:punct:]]", "", comb$City)

# Step 2: Replace abbreviated words with full words
abbreviation_mapping <- c("ave" = "avenue", "blvd" = "boulevard", "bldg" = "building", "ct" = "court",
                          "dr" = "drive", "expwy" = "expressway", "expy" = "expressway", "hwy" = "highway", "ln" = "lane",
                          "pkwy" = "parkway", "pl" = "place", "plz" = "plaza", "rd" = "road",
                          "st" = "street", "ter" = "terrace", "trl" = "trail", "wy" = "way",
                          "apt" = "apartment", "ste" = "suite", "fl" = "floor", "rm" = "room",
                          "dept" = "department", "bldg" = "building", "corp" = "corporation",
                          "ln" = "lane", "hwy" = "highway", "ctr" = "center", "pl" = "place",
                          "ext" = "extension", "int" = "intersection", "jct" = "junction",
                          "mt" = "mount", "opas" = "overpass", "pi" = "pier", "pt" = "point",
                          "riv" = "river", "spg" = "spring", "sqr" = "square", "sta" = "station",
                          "str" = "stream", "uni" = "union", "vlg" = "village", "vly" = "valley",
                          "cir" = "circle", "alc" = "alcove", "byu" = "bayou",
                          "cyn" = "canyon", "ct" = "court", "exp" = "expressway", "frt" = "fort",
                          "hvn" = "haven", "jct" = "junction", "mnr" = "manor", "mt" = "mount",
                          "plz" = "plaza", "prk" = "park", "sta" = "station", "trce" = "trace",
                          "trk" = "track", "via" = "viaduct", "vlg" = "village", "dr" = "drive",
                          "ln" = "lane", "rd" = "road", "st" = "street", "ct" = "court",
                          "blvd" = "boulevard", "sq" = "square", "aly" = "alley", "pl" = "place",
                          "ter" = "terrace", "pkwy" = "parkway", "hwy" = "highway", "trl" = "trail",
                          "way" = "way", "row" = "row", "cres" = "crescent", "pl" = "place",
                          "cl" = "close", "prom" = "promenade", "sq" = "square", "pde" = "parade",
                          "tce" = "terrace", "qy" = "quay", "crs" = "cross", "grn" = "green",
                          "arc" = "arcade", "gln" = "glen", "rg" = "rise", "ct" = "court",
                          "cct" = "circuit", "mew" = "mews", "bend" = "bend", "pt" = "point",
                          "isle" = "isle", "way" = "way", "tce" = "terrace", "cct" = "circuit",
                          "hts" = "heights", "rdg" = "ridge", "bluff" = "bluff",  "n" = "north", 
                          "s" = "south", "e" = "east", "w" = "west", "ne" = "northeast", 
                          "nw" = "northwest", "se" = "southeast", "sw" = "southwest")

# Loop through the abbreviation mapping and replace
for (abbreviation in names(abbreviation_mapping)) {
  comb$CleanedAddress <- gsub(paste0("\\b", abbreviation, "\\b"), abbreviation_mapping[abbreviation], comb$CleanedAddress)
}


# # Step 2: Remove "PO Box" from CleanedAddress
comb$CleanedAddress <- gsub("po\\s*box", "", comb$CleanedAddress, ignore.case = TRUE)

# Step 3: Replace spelled-out numbers with actual numbers
number_mapping <- c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5",
                    "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "zero" = "0",
                    "first" = "1", "second" = "2", "third" = "3", "fourth" = "4", "fifth" = "5",
                    "sixth" = "6", "seventh" = "7", "eighth" = "8", "ninth" = "9", "tenth" = "10",
                    "eleventh" = "11", "twelfth" = "12", "thirteenth" = "13", "fourteenth" = "14",
                    "fifteenth" = "15", "sixteenth" = "16", "seventeenth" = "17", "eighteenth" = "18",
                    "nineteenth" = "19", "twentieth" = "20")

for (number_word in names(number_mapping)) {
  comb$CleanedAddress <- gsub(paste0("\\b", number_word, "\\b"), number_mapping[number_word], comb$CleanedAddress, ignore.case = TRUE)
}

# Step 4: Remove th, rd, nd, st from all numbers
comb$CleanedAddress <- gsub("(\\d+)(th|rd|nd|st)\\b", "\\1", comb$CleanedAddress)
comb$CleanedAddress <- gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", comb$CleanedAddress)
comb$CleanedAddress <- gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2", comb$CleanedAddress)



# Format Table for Census Geocoder -----------------------------------------
# We need an easier way to identify duplicates. For this reason,
# our data is required to be formatted properly. The formatting
# scheme we are following is in line with what the Census Geocoder
# requires the table to look like so the tool can properly 
# preform the geocode!

# Rename all column headers and only keep those that follow the census table scheme
comb <- comb %>% 
  mutate(
    `Street address` = str_squish(CleanedAddress)) %>%
  select(`Unique ID`, `Street address`, City, State, ZIP)  # Selecting only the specified columns

write_csv(comb, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Historical/From AREB/comb_Census_Geo_Needed.csv")





# Import Geocoded Data back into R ----------------------------------------------------

# Assign file directory to import data sets 
file_path_hist_geo <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Historical/From AREB/Historical_Geo_Partial.csv"

# Read data from CSV file
comb <- read.csv(file_path_hist_geo)

# The Census Geocoder disorganizes the table when it is processed.
# This requires us to cleane the table and organize it properly for our purpose


###### Reorganize header ##################################
# The tool moves the the header columns elsewhere. We need to first locate it.
# Find the index of the row containing "Unique ID"
header_index <- which(comb == "Unique ID", arr.ind = TRUE)[1, 1] 
# Extract the original header row
original_header <- colnames(comb) 
# Extract the address part
address <- unlist(strsplit(original_header[2], "\\."))  # Split by period
address[1] <- gsub("^X", "", address[1])  # Remove 'X' from the first part
# Find the indices of non-empty elements
non_empty_indices <- grep("\\S", address)
# Subset address_parts to include only non-empty elements
address <- address[non_empty_indices]
# Join the last four words with commas
if (length(address) >= 4) {
  last_four_words <- paste(address[(length(address) - 3):length(address)], collapse = ", ")
  first_words <- paste(address[1:(length(address) - 4)], collapse = " ")
  formatted_address <- paste(first_words, last_four_words, sep = " ")}
# Extract the coordinates and reformat them
coordinates_parts <- unlist(strsplit(original_header[6], "(?<=\\d{2}\\.\\d{14})\\.(?=\\d{2})", perl = TRUE))
coordinates_parts <- gsub("^X\\.", "", coordinates_parts)
formatted_coordinates <- paste(coordinates_parts, collapse = ",")
# Replace the second element in the original_header vector with the formatted address
original_header[2] <- formatted_address
# Replace the sixth element in the original_header vector with the formatted coordinates
original_header[6] <- formatted_coordinates
# Replace the column names with the values from the row where "Unique ID" is found
colnames(comb) <- comb[header_index, ]
# Remove the row with "Unique ID"
comb <- comb[-header_index, ]
# Insert the original header row as the first row of the dataframe
comb <- rbind(original_header, comb)



####### Replace Header with Proper Names ######################
# Replace column names from the third column onwards with temporary names
temp_column_names <- names(comb)
temp_column_names[3:length(temp_column_names)] <- paste0("Temp", 1:(length(temp_column_names) - 2))
names(comb) <- temp_column_names

# Remove columns 3, 4, 5, and 8
comb <- comb %>%
  select(-c(3, 4, 5, 7, 8)) 

# Rename columns 6 and 7
comb <- comb %>%
  mutate(Temp4 = gsub("X", "", Temp4))

comb <- comb %>% 
  separate(
    col = 'Unique ID',
    into = c("Confirmed.ED", "PSC", "Yes.PSC", "PSC.2010", "Yes.PSC.2010",  "New.PSC.2010", "Multicampus.2010", "PSC.2011",
            "Yes.PSC.2011", "New.PSC.2011", "Multi.campus.2011", "PSC.2012", "Yes.PSC.2012", "New.PSC.2012",
            "Multi.campus.2012", "PSC.2013", "Yes.PSC.2013", "New.PSC.2013", "Multi.campus.2013", "Tot.PSC.Multiple", "MLOCSTCD",
            "State.Stroke.Program", "Impl.SSOC", "MNAME"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    across(everything(), ~gsub("\\.", " ", .))
  ) %>% 
  separate(
    col = 'Street address, City, State, ZIP',
    into = c("StreetAddress", "City", "State", "PostalCode"),
    sep = ",",
    remove = FALSE
  ) %>% 
  separate(
    col = 'Temp4',
    into = c("X","Y"),
    sep = ",",
    remove = FALSE
  ) %>%
  select("Confirmed.ED", "PSC", "Yes.PSC", "PSC.2010", "Yes.PSC.2010",  "New.PSC.2010", "Multicampus.2010", "PSC.2011",
         "Yes.PSC.2011", "New.PSC.2011", "Multi.campus.2011", "PSC.2012", "Yes.PSC.2012", "New.PSC.2012",
         "Multi.campus.2012", "PSC.2013", "Yes.PSC.2013", "New.PSC.2013", "Multi.campus.2013", "Tot.PSC.Multiple", "MLOCSTCD",
         "State.Stroke.Program", "Impl.SSOC", "MNAME", "StreetAddress", "City", "State", "PostalCode", "X", "Y")

write_csv(comb, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Historical/From AREB/Stroke_Hospitals_Partial_Geocode_2009-2013.csv")


