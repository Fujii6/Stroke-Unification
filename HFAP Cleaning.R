# Title: Stroke Center Table
# Author: Yui Fujii
# Start Date: 2/7/2024
# Most Recent Update Date: 2/12/2024
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

file_path_pre <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/HFAP/2022/Clean/HFAP_Jan2022.xlsx"
file_path_current <- "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/HFAP/2023/Raw/HFAP Stroke Centers 10.13.23.xlsx"

# Import Files using the directory
# jc_pre - imports the previous years Joint commission data set
# jc_curr - imports the current years Joint commission data set

hfap_pre <- read.xlsx(file_path_pre)
hfap_curr <- read.xlsx(file_path_current)

# Clean data set to a usable state
# First, arrange the Org Id in order
# Second, remove unnecessary characters within the the column Cert.Prog.
# Third, format effect.date. as a date


hfap_pre <- hfap_pre %>% 
  arrange(OrganizationId) %>%
  mutate(
    Program = str_squish(Program),
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    PostalCode = as.character(PostalCode),
    Address = str_squish(Address)
   # EffectiveDate = as.Date(EffectiveDate, format = "%m/%d/%Y")
    )

# Rename all columns that can be shared among other tables.

hfap_curr <- hfap_curr %>% 
  rename(
    OrganizationId = AOID,
    Program = Site.Program.Type,
    OrganizationName = Facility.Name,
    StreetAddress = Address,
    City = Site.City,
    State = Site.State,
    PostalCode = Site.Zip.Code,
    PhoneNumber = Phone.Number,
    Webpage = Main.Site.Web.page.URL,
    EffectiveDate = 'Start.Date.Accreditation/.Certification.start.date',
    EndingDate = `End.Date.Accreditation/Certification.effective.date`
  )


hfap_curr <- hfap_curr %>% 
  arrange(OrganizationId) %>%
  mutate(
    Program = str_squish(Program),
    OrganizationName = str_squish(OrganizationName),
    City = str_squish(City),
    State = str_squish(State),
    PostalCode = as.character(PostalCode),
    StreetAddress = str_squish(StreetAddress),
    EffectiveDate = as.Date(EffectiveDate, origin = "1900-01-01", format = "%Y-%m-%d"),
    EndingDate = as.Date(EndingDate, origin = "1900-01-01", format = "%Y-%m-%d"),
    Certify = paste("HFAP"),
    CertificationProgram = case_when(
      Program == "PSC" ~ "Advanced Primary Stroke Center",
      Program == "CSC" ~ "Advanced Comprehensive Stroke Center",
      Program == "SRC" ~ "Acute Stroke Ready Hospital",
      Program == "TSC" ~ "Advanced Thrombectomy Capable Stroke Ctr",
      TRUE ~ NA_character_
    )
  )

HFAP_2023_Clean_Final <- hfap_curr %>%
  select(OrganizationId, OrganizationName, City, State, StreetAddress, CertificationProgram, Certify, PostalCode)


# For the Joint Commission table we have to isolate the tables. However,
# HFAP produces a much cleaner data set so we do not have to go through 
# all the filtering.

# Combine the clean Previous and Current years/ 

fin <- anti_join(hfap_curr, hfap_pre, by = "OrganizationId")



#   To confirm that we did this correctly let's check our work
#   We want to go back and check if the final product we have
#   in fact does not contained any shared Org Ids with
#   the 2022 data set. 

# Check if there are common OrganizationIds 
common_org_ids <- semi_join(fin, hfap_pre, by = "OrganizationId")

if (nrow(common_org_ids) > 0) {
  cat("Common OrganizationIds found.")
} else {
  cat("No common OrganizationIds.")
}

# PLEASE CHANGE WHERE THIS IS SAVED
# SAVE THIS IN THE CURRENT YEAR THAT YOU ARE WORKING ON!!!!!!

write_csv(fin, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/HFAP/2023/Clean/hfap_2023_Geo_Needed.csv")
write_csv(HFAP_2023_Clean_Final, "//cdc.gov/project/NCCD_DHDSP_DHDSP-SAA/Datasets/Stroke center certification/HFAP/2023/Clean/hfap_2023_Clean_Final.csv")
