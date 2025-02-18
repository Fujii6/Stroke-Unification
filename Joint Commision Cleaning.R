# Title: Stroke Center Table
# Author: Yui Fujii
# Start Date: 12/14/2023
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
# File - Directs to the folder you would like to save your files under
# File_path_pre - Directs you to find the previous years data set
# File_path_2023 - Directs you to find the current years data set

file_path_pre <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2022/Clean/jc_notmissing2022.csv"
file_path_current <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2023/Raw/StrokeCertificationList_Dec132023.xlsx"

# Import Files using the directory
# jc_pre - imports the previous years Joint commission data set
# jc_curr - imports the current years Joint commission data set

jc_pre <- read.csv(file_path_pre)
jc_curr <- read.xlsx(file_path_current)

# Clean data set to a usable state
# First, arrange the Org Id in order
# Second, remove unnecessary characters within the the column Cert.Prog.
# Third, format effect.date. as a date


jc_pre <- jc_pre %>% 
  arrange(OrganizationId) %>%
  mutate(
    CertificationProgram = str_squish(CertificationProgram),
    Certify = "Joint",
    Program = str_squish(Program),
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    PostalCode = as.character(PostalCode),
    EffectiveDate = as.Date(EffectiveDate, format = "%m/%d/%Y"
    ))

jc_curr <- jc_curr %>% 
  arrange(OrganizationId) %>%
  mutate(
    CertificationProgram = str_squish(CertificationProgram),
    Certify = "Joint",
    Program = str_squish(Program),
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    PostalCode = as.character(PostalCode),
    EffectiveDate = as.Date(EffectiveDate, format = "%m/%d/%Y"
    ))



# Isolate table so we can identify problematic duplicates
# We will be isolating the tables into multiple tables in this process
# As we isolate them into smaller groups we will be filtering out duplicates.
# Once we have clean and correct tables we will then combine them back into 
# one table.
# *****Please note that in this code, I am not isolating 2022 data sets as
#      much as I do 2023 data sets. This is because the 2022 data set was clean.
#      All duplicates that existed in 2022 were correct duplicates. They 
#      were only duplicates because Stroke Rehabilitation was being considered
#      as a duplicate.But please remember that Stoke rehabilitation and stroke
#      centers can exist in the same facility. It is only problematic when
#      there are two designations for stroke centers. 


# Isolate the duplicates
# We do this to make the identifying process easier. All the problems that
# exist will exist in the duplicates.
# ****Sometimes duplicates appear in more than twos. While the table itself
#     may be irrelevant, it may be helpful to identify to ensure removing
#     rows stay consistent.

# Previous
dp_jc_pre <- jc_pre %>%
  filter(duplicated(OrganizationId) | duplicated(OrganizationId, fromLast = TRUE))

# Current
dp_jc_curr <- jc_curr %>%
  filter(duplicated(OrganizationId) | duplicated(OrganizationId, fromLast = TRUE))

dup_more_than_twice <- dp_jc_curr %>%
  filter(duplicated(OrganizationId) | duplicated(OrganizationId, fromLast = TRUE)) %>%
  group_by(OrganizationId) %>%
  filter(n() > 2)


# Isolate the non-duplicates
# By isolating this, we are creating the first clean table. This will be
# our first table which we combine to in the very end.

nodp_jc_curr <- jc_curr %>%
  filter(!duplicated(OrganizationId) & !duplicated(OrganizationId, fromLast = TRUE))



# Isolate Stroke Rehabilitation
# Here we are creating another clean table to combine in the end.

# Previous
st_re_pre <- dp_jc_pre %>%
  filter(CertificationProgram == "Stroke Rehabilitation")

# Current
st_re_curr <- dp_jc_curr %>%
  filter(CertificationProgram == "Stroke Rehabilitation")

# Isolate the Non-Stroke Rehabilitation
# We isolate a table with no Stroke Rehabilitation to continue filtering
# problems down the line.

# Current
no_str_curr <- dp_jc_curr %>%
  filter(CertificationProgram != "Stroke Rehabilitation")


# Isolate and delete rows
# In this section we are not only isolating but we have reached a point
# where we can directly address the duplication issue and delete them 
# entirely with no issues. (Both tables are the final clean table to combine)
#***** There are two conditions that we are operating on. 
#      1. We are deleting duplicates that are an Effective Date beginning
#         at an older date. We are only keeping rows with the most recent   
#         date
#      2. If the centers have the same Effective Date then we will keep 
#         the row with the higher stroke center capacity level.

# We begin by setting up a ranking system of stroke centers so that
# R can determine which centers to get rid of if rows have the same
# date
stroke_center_rank <- c(
  "Advanced Comprehensive Stroke Center",
  "Advanced Thrombectomy Capable Stroke Ctr",
  "Advanced Primary Stroke Center",
  "Acute Stroke Ready Hospital"
)    

# Here we isolate rows that have the same Effective Date and only keep
# the rows that contains the higher Stroke Center Ranking. 
date_cp_curr <- no_str_curr %>%
  group_by(OrganizationId, EffectiveDate) %>%
  filter(n() > 1) %>%
  arrange(match(CertificationProgram, stroke_center_rank), .desc = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup()

# Here we remove the rows with the same date

Nodate_cp_curr <- no_str_curr %>%
  group_by(OrganizationId, EffectiveDate) %>%
  filter(!duplicated(OrganizationId) & !duplicated(OrganizationId, fromLast = TRUE)) %>%
  ungroup()


# First make a ranking order to identify which stroke centers to keep based
# on hierarchy level

No_dupjc_curr_first <- Nodate_cp_curr %>%
  mutate(EffectiveDate = as.Date(EffectiveDate)) %>%
  group_by(OrganizationId) %>%
  arrange(OrganizationId, desc(EffectiveDate)) %>%
  filter(row_number() == 1 | CertificationProgram == "Stroke Rehabilitation") %>%
  ungroup()


# Combine!!!
# We combine 
#   1. Original table without any duplicates
#   2. All Stroke Rehab. that we isolated from the duplicate table
#   3. The final table that we cleaned to removed all duplicates
#   4. All rows that had the same date but filtered

combined_table <- bind_rows(nodp_jc_curr, st_re_curr, No_dupjc_curr_first, date_cp_curr)

JC_2023_Clean_Final <- combined_table %>%
  select(OrganizationId, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify, PostalCode)

# Combine the clean 2022 and 2023
# It's important to remember that all that combination was to clean and
# create the 2023 data set. Here we need to combine the two data sets
# to identify which centers are new and are then required to run through
# Arc Pro to identify it's geo-location.

fin <- anti_join(combined_table, jc_pre, by = "OrganizationId")


#   To confirm that we did this correctly let's check our work
#   We want to go back and check if the final product we have
#   in fact does not contained any shared Org Ids with
#   the 2022 data set. 

# Check if there are common OrganizationIds 
common_org_ids <- semi_join(fin, jc_pre, by = "OrganizationId")

if (nrow(common_org_ids) > 0) {
  cat("Common OrganizationIds found.")
} else {
  cat("No common OrganizationIds.")
}

# PLEASE CHANGE WHERE THIS IS SAVED
# SAVE THIS IN THE CURRENT YEAR THAT YOU ARE WORKING ON!!!!!!

write_csv(fin, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2023/Clean/jc_2023_Geo_Needed.csv")
write_csv(JC_2023_Clean_Final, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/Joint Commission/2023/Clean/jc_2023_Clean_Final.csv")
