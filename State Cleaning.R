# Title: Stroke Center Table
# Author: Yui Fujii
# Start Date: 2/21/2024
# Most Recent Update Date: 2/21/2024
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
  stringdist,                              # Calculate string similarity between values and then filters for cases where there are potential duplicates based on each variable
  hunspell,                                # Can help with spell checking and correction
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
groundhog.library(stringdist, '2024-2-22')
groundhog.library(spdep, '2023-10-02')
groundhog.library(hunspell, '2024-03-05')


# Assign file directory to import data sets 
# File - Directs to the folder you would like to save your files under
# File_path_pre - Directs you to find the previous years data set
# File_path_2023 - Directs you to find the current years data set

#file <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/States/2022"
file_path_state <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/States/2023/Raw/2024_2_15_State Certification_Manual Collection.xlsx"

# Import Files using the directory

state <- read.xlsx(file_path_state)

state <- state %>% 
  arrange(OrganizationId) %>%
  mutate(
    OrganizationId = str_squish(OrganizationId),
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    StreetAddress = str_squish(StreetAddress),
    CertificationProgram = str_squish(CertificationProgram),
    Certify = str_squish(Certify),
    PostalCode = as.character(PostalCode),
  )

# Add a new column 'CleanedAddress' to state
state$CleanedAddress <- tolower(state$StreetAddress)

# Step 1: Remove all punctuation and collapse
state$CleanedAddress <- gsub("[[:punct:]]", "", state$CleanedAddress)

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
  state$CleanedAddress <- gsub(paste0("\\b", abbreviation, "\\b"), abbreviation_mapping[abbreviation], state$CleanedAddress)
}


# # Step 2: Remove "PO Box" from CleanedAddress
state$CleanedAddress <- gsub("po\\s*box", "", state$CleanedAddress, ignore.case = TRUE)

# Step 3: Replace spelled-out numbers with actual numbers
number_mapping <- c("one" = "1", "two" = "2", "three" = "3", "four" = "4", "five" = "5",
                    "six" = "6", "seven" = "7", "eight" = "8", "nine" = "9", "zero" = "0",
                    "first" = "1", "second" = "2", "third" = "3", "fourth" = "4", "fifth" = "5",
                    "sixth" = "6", "seventh" = "7", "eighth" = "8", "ninth" = "9", "tenth" = "10",
                    "eleventh" = "11", "twelfth" = "12", "thirteenth" = "13", "fourteenth" = "14",
                    "fifteenth" = "15", "sixteenth" = "16", "seventeenth" = "17", "eighteenth" = "18",
                    "nineteenth" = "19", "twentieth" = "20")

for (number_word in names(number_mapping)) {
  state$CleanedAddress <- gsub(paste0("\\b", number_word, "\\b"), number_mapping[number_word], state$CleanedAddress, ignore.case = TRUE)
}

# Step 4: Remove th, rd, nd, st from all numbers
state$CleanedAddress <- gsub("(\\d+)(th|rd|nd|st)\\b", "\\1", state$CleanedAddress)
state$CleanedAddress <- gsub("([[:alpha:]])([[:digit:]])", "\\1 \\2", state$CleanedAddress)
state$CleanedAddress <- gsub("([[:digit:]])([[:alpha:]])", "\\1 \\2", state$CleanedAddress)

# Step 5: Split the cleaned address into different columns
state$StreetAddress <- strsplit(state$StreetAddress, "\\s+")

# Convert the list of words to a data frame
state <- cbind(state, do.call(rbind, state$StreetAddress))


# Function to calculate Jaccard similarity between two strings
jaccard_similarity <- function(str1, str2) {
  set1 <- unlist(strsplit(str1, " "))
  set2 <- unlist(strsplit(str2, " "))
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  similarity <- intersection / union
  return(similarity)
}

# Similarity for dnv_curr -------------------------------------------------


# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(state$StreetAddress, state$StreetAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
state$HasDuplicates <- apply(duplicate_matrix, 2, any)


no_dup_state <- state[!state$HasDuplicates, ]
dup_state <- state[state$HasDuplicates, ]


t_dup <- dup_state %>%
  group_by(City) %>%  # Group by cities
  mutate(row_number = row_number()) %>%  # Add row numbers within each group
  filter(n() == 1 | row_number != 2) %>%  # Keep only rows where the group size is 1 or where it's the first row in the group
  select(-row_number)  # Remove the temporary row number column

state_clean <- rbind(no_dup_state, t_dup) %>%
  mutate(
    StreetAddress = CleanedAddress
  ) %>%
  select(-CleanedAddress)

state_clean <- state_clean %>%
  select(OrganizationId, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify, PostalCode)


write_csv(state_clean, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/States/2023/Clean/State_2023_Geo_Needed.csv")
write_csv(state_clean, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/States/2023/Clean/State_2023_Clean_Final.csv")

