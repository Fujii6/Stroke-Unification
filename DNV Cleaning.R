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

file_path_pre <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/DNV/2022/Clean/DNV_Feb2022.xlsx"
file_path_current <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/DNV/2023/Raw/Stroke Certified Hospitals 1_Yui.xlsx"

# Import Files using the directory
# jc_pre - imports the previous years Joint commission data set
# jc_curr - imports the current years Joint commission data set

dnv_pre <- read.xlsx(file_path_pre)
dnv_curr <- read.xlsx(file_path_current)

# Clean data set to a usable state
# First, arrange the Org Id in order
# Second, remove unnecessary characters within the the column Cert.Prog.
# Third, format effect.date. as a date


dnv_pre <- dnv_pre %>% 
  arrange(OrganizationId) %>% 
rename(StreetAddress = "Address") %>%
  mutate(
    Certify = "DNVGL",
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    StreetAddress = str_squish(StreetAddress),
    Date = "Pre",
    PostalCode = as.character(PostalCode),
    CertificationProgram = case_when(
      CertificationProgram == "Primary Stroke Center" ~ "Advanced Primary Stroke Center",
      CertificationProgram == "Comprehensive Stroke Center" ~ "Advanced Comprehensive Stroke Center",
      CertificationProgram == "Acute Stroke Ready" ~ "Acute Stroke Ready Hospital",
      CertificationProgram == "Primary Stroke Center Plus" ~ "Advanced Thrombectomy Capable Stroke Ctr",
      TRUE ~ NA_character_
    )
    )
dnv_pre <- dnv_pre %>%
  select(Date, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify)

dnv_curr <- dnv_curr %>% 
  mutate(
    OrganizationName = str_squish(Site.Name),
    City = str_squish(CITY),
    State = str_squish(STATE),
    StreetAddress = str_squish(ADDRESS),
    CertificationProgram = str_squish(X1),
    Date = "Curr",
    Certify = "DNVGL",
    CertificationProgram = case_when(
      CertificationProgram == "PSC" ~ "Advanced Primary Stroke Center",
      CertificationProgram == "CSC" ~ "Advanced Comprehensive Stroke Center",
      CertificationProgram == "ASR" ~ "Acute Stroke Ready Hospital",
      CertificationProgram == "PSC+" ~ "Advanced Thrombectomy Capable Stroke Ctr",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(Program = X1)

dnv_curr <- dnv_curr %>%
  select(Date, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify)

stacked_table <- rbind(dnv_curr, dnv_pre)
# Add a new column 'ID' to 'stacked_table'
stacked_table$OrganizationId <- seq_len(nrow(stacked_table))




# Clean Address ------------------------------------------------------------


# Add a new column 'CleanedAddress' to stacked_table
stacked_table$CleanedAddress <- tolower(stacked_table$StreetAddress)

# Step 1: Remove all punctuations and collapse
stacked_table$CleanedAddress <- gsub("[[:punct:]]", "", stacked_table$CleanedAddress)

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

# Step 5: Split the cleaned address into different columns
stacked_table$CleanedAddress <- strsplit(stacked_table$CleanedAddress, "\\s+")

# Convert the list of words to a data frame
stacked_table <- cbind(stacked_table, do.call(rbind, stacked_table$CleanedAddress))



# Filter the stacked_table for "Pre" date
dnv_pre <- stacked_table[stacked_table$Date == "Pre", ]

# Filter the stacked_table for "Curr" date
dnv_curr <- stacked_table[stacked_table$Date == "Curr", ]




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



# Similarity for dnv_curr -------------------------------------------------

# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(dnv_curr$CleanedAddress, dnv_curr$CleanedAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
dnv_curr$HasDuplicates <- apply(duplicate_matrix, 2, any)


f_dup_curr <- dnv_curr[!dnv_curr$HasDuplicates, ]
t_dup_curr <- dnv_curr[dnv_curr$HasDuplicates, ]



# Similarity for dnv_pre --------------------------------------------------

# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(dnv_pre$CleanedAddress, dnv_pre$CleanedAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
dnv_pre$HasDuplicates <- apply(duplicate_matrix, 2, any)


f_dup_pre <- dnv_pre[!dnv_pre$HasDuplicates, ]
t_dup_pre <- dnv_pre[dnv_pre$HasDuplicates, ]



# Similarity for f_dup_curr, f_dup_pre --------------------------------------------------

combined_table <- rbind(f_dup_curr, f_dup_pre)


# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(combined_table$CleanedAddress, combined_table$CleanedAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
combined_table$HasDuplicates <- apply(duplicate_matrix, 2, any)



f_dup_tot <- combined_table[!combined_table$HasDuplicates, ]
t_dup_tot <- combined_table[combined_table$HasDuplicates, ]


# Similarity for false_rows_dup, t_dup_curr, t_dup_pre --------------------------------------------------

Ex_table <- rbind(f_dup_tot, t_dup_curr, t_dup_pre)


# Calculate Jaccard similarity for each pair of rows
similarity_matrix <- outer(Ex_table$CleanedAddress, Ex_table$CleanedAddress, Vectorize(jaccard_similarity))

# Exclude self-comparisons (rows compared with themselves)
diag(similarity_matrix) <- 0

# Threshold for considering duplicates
threshold <- 0.6
# Create a matrix of TRUE/FALSE indicating pairs with similarity above the threshold
duplicate_matrix <- similarity_matrix > threshold

# Create a new column indicating whether each row has duplicates
Ex_table$HasDuplicates <- apply(duplicate_matrix, 2, any)



frows_dup <- Ex_table[!Ex_table$HasDuplicates, ]
trows_dup <- Ex_table[Ex_table$HasDuplicates, ]


# Identify duplicates by the variable City
dupdup <- trows_dup[duplicated(trows_dup$City) | duplicated(trows_dup$City, fromLast = TRUE), ]
# Filter the stacked_table for "Pre" date
#dupdup <- dupdup[dupdup$Date == "Curr", ]

# Place non-duplicates into nondup table
nondup <- trows_dup[!duplicated(trows_dup$City) & !duplicated(trows_dup$City, fromLast = TRUE), ]


Fin_no_duplicates <- rbind(frows_dup, nondup)
Fin_no_duplicates <- Fin_no_duplicates %>%
  select(OrganizationId, Date, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify)

Fin_duplicates <- rbind(t_dup_tot, dupdup)
Fin_duplicates <- Fin_duplicates %>%
  select(OrganizationId, Date, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify)


# Filter the stacked_table for "Pre" date
DNV_2023_Geo_Needed <- Fin_no_duplicates[Fin_no_duplicates$Date == "Curr", ]

# Filter the stacked_table for "Pre" date
DNV_2023_Geo_Not_Needed <- Fin_duplicates[Fin_duplicates$Date == "Curr", ]

DNV_2023_Clean_Final <- rbind(DNV_2023_Geo_Needed, DNV_2023_Geo_Not_Needed)
DNV_2023_Clean_Final <- DNV_2023_Clean_Final %>%
  select(OrganizationId, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify)

# PLEASE CHANGE WHERE THIS IS SAVED
# SAVE THIS IN THE CURRENT YEAR THAT YOU ARE WORKING ON!!!!!!

write_csv(DNV_2023_Geo_Needed, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/DNV/2023/Clean/dnv_2023_Geo_Needed.csv")
write_csv(DNV_2023_Clean_Final, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/DNV/2023/Clean/dnv_2023_Clean_Final.csv")

