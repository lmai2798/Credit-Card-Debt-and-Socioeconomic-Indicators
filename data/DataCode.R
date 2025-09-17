library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


### Credit Card Debt

# Set path to Excel file
cre_path <- "Project/CreditCard.xlsx"

# Read only the 'creditcard' sheet, skip 8 rows to get to actual data
raw_cc <- read_excel(cre_path, sheet = "creditcard", skip = 8)

# Rename first column to "State"
colnames(raw_cc)[1] <- "State"

# Reshape from wide to long format
long_cc <- raw_cc %>%
  pivot_longer(
    cols = -State,
    names_to = "Quarter",
    values_to = "Credit_Card_Debt_Per_Capita"
  )

# Extract year from quarter label (e.g., "Q4_2012" → 2012)
final_cc <- long_cc %>%
  mutate(
    Year = as.numeric(str_sub(Quarter, -4, -1))
  ) %>%
  filter(Year >= 2006, Year <= 2023) %>%
  select(Year, State, Credit_Card_Debt_Per_Capita) %>%
  arrange(factor(State, levels = state.abb), Year)


# View sample and summary
print(head(final_cc))
cat("States:", length(unique(final_cc$State)), "\n")
cat("Rows:", nrow(final_cc), "\n")

# Save to CSV
write_csv(final_cc, "Project/CreditCardDebt_PerCapita_2006_2023.csv")

# -------------------------------------------------------------------------------------------------------------

### Unemployment Rate

# Set folder path
unemp_path <- "Project/UnemploymentRate"

# List all CSV files
unemp_files <- list.files(path = unemp_path, pattern = "\\.csv$", full.names = TRUE)

# Function to read and process each unemployment file
read_unemp_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Rename columns
  colnames(df)[1:2] <- c("observation_date", "Unemployment_Rate")
  df <- df[, !duplicated(names(df))]
  
  # Extract state name from file name
  state_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Clean and convert
  df <- df %>%
    mutate(
      observation_date = as.Date(observation_date),
      Year = year(observation_date),
      State_Name = str_trim(state_name),
      Unemployment_Rate = as.numeric(Unemployment_Rate)
    ) %>%
    group_by(State_Name, Year) %>%
    summarize(Unemployment_Rate = mean(Unemployment_Rate, na.rm = TRUE), .groups = "drop") %>%
    filter(Year >= 2006, Year <= 2023)
  
  return(df)
}

# Read and combine all files
unemp_data_list <- lapply(unemp_files, read_unemp_file)
combined_unemp <- bind_rows(unemp_data_list)

# Fix mismatched state names
combined_unemp <- combined_unemp %>%
  mutate(State_Name = case_when(
    State_Name == "DistrictOfColumbia" ~ "District of Columbia",
    State_Name == "WashingtonDC" ~ "District of Columbia",
    State_Name == "Washington DC" ~ "District of Columbia",
    State_Name == "NewHampshire" ~ "New Hampshire",
    State_Name == "NewJersey" ~ "New Jersey",
    State_Name == "NewMexico" ~ "New Mexico",
    State_Name == "NewYork" ~ "New York",
    State_Name == "NorthCarolina" ~ "North Carolina",
    State_Name == "NorthDakota" ~ "North Dakota",
    State_Name == "SouthCarolina" ~ "South Carolina",
    State_Name == "SouthDakota" ~ "South Dakota",
    State_Name == "RhodeIsland" ~ "Rhode Island",
    State_Name == "WestVirginia" ~ "West Virginia",
    State_Name == "Nerbraska" ~ "Nebraska",
    TRUE ~ State_Name
  ))

# Create state name → abbreviation mapping
state_lookup <- tibble(
  State_Name = state.name,
  State = state.abb
) %>%
  add_row(State_Name = "District of Columbia", State = "DC")

# Merge with abbreviation
final_unemp <- combined_unemp %>%
  left_join(state_lookup, by = "State_Name") %>%
  select(Year, State, Unemployment_Rate)

# Check for any remaining NA
if (sum(is.na(final_unemp$State)) > 0) {
  warning("There are still NA values in State column. Investigate manually.")
  print(final_unemp %>% filter(is.na(State)) %>% distinct(State_Name))
}

# Save cleaned dataset
write_csv(final_unemp, "Project/UnemploymentRate_2006_2023.csv")

# -------------------------------------------------------------------------------------------------------------

### Population

# Set folder path
pop_path <-setwd("Project/Population")

file_list <- paste0("Pop_", 1:5, ".csv")
pop_data_list <- lapply(file_list, read_csv)

# Merge all data by 'observation_date'
combined_pop <- reduce(pop_data_list, full_join, by = "observation_date")

# Extract year and filter for 2006–2023
combined_pop <- combined_pop %>%
  mutate(Year = year(observation_date)) %>%
  filter(Year >= 2006, Year <= 2023)

# Reshape data from wide to long format
final_pop <- combined_pop %>%
  select(-observation_date) %>%
  pivot_longer(cols = -Year, names_to = "StateCode", values_to = "Population")

# Remove "POP" suffix to keep just state abbreviations (e.g., ALPOP → AL)
final_pop <- final_pop %>%
  mutate(State = str_remove(StateCode, "POP$")) %>%
  select(Year, State, Population) %>%
  arrange(factor(State, levels = state.abb), Year)

# Preview result
print(head(final_pop))

# Save cleaned dataset
write_csv(final_pop,"Project/Population_2006_2023.csv")


# -------------------------------------------------------------------------------------------------------------

### CPI 

# Read raw CPI data
cpi_file <- "Project/CPI.csv"
cpi_data <- read_csv(cpi_file, show_col_types = FALSE)

# Process into annual CPI
annual_cpi <- cpi_data %>%
  rename(observation_date = 1, CPI = 2) %>%
  mutate(
    observation_date = as.Date(observation_date),
    Year = year(observation_date)
  ) %>%
  group_by(Year) %>%
  summarize(CPI = mean(CPI, na.rm = TRUE), .groups = "drop") %>%
  filter(Year >= 2006, Year <= 2023)

# List of all 50 states + DC abbreviations
state_abbrs <- c(state.abb, "DC")

# Expand CPI to state level and round CPI
final_cpi <- expand.grid(Year = annual_cpi$Year, State = state_abbrs) %>%
  left_join(annual_cpi, by = "Year") %>%
  mutate(CPI = round(CPI, 2)) %>%     # ← round to 2 decimal places
  arrange(State, Year)

# View output
head(final_cpi)

# Save to CSV
write_csv(final_cpi, "Project/CPI_StateLevel_2006_2023.csv")


# -------------------------------------------------------------------------------------------------------------

### Education Level

# Set folder path
edu_path <- "Project/EducationLevel"

# List all CSV files
edu_files <- list.files(path = edu_path, pattern = "\\.csv$", full.names = TRUE)

# Function to read and process each education file
read_edu_file <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Rename columns
  colnames(df)[1:2] <- c("observation_date", "Percent_Bachelors")
  df <- df[, !duplicated(names(df))]
  
  # Extract state name from file name
  state_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Clean and convert
  df <- df %>%
    mutate(
      observation_date = as.Date(observation_date),
      Year = year(observation_date),
      State_Name = str_trim(state_name),  # Trim whitespace
      Percent_Bachelors = as.numeric(Percent_Bachelors)
    ) %>%
    filter(Year >= 2006, Year <= 2023) %>%
    select(Year, State_Name, Percent_Bachelors)
  
  return(df)
}

# Read and combine all files
edu_data_list <- lapply(edu_files, read_edu_file)
combined_edu <- bind_rows(edu_data_list)

# Fix mismatched state names
combined_edu <- combined_edu %>%
  mutate(State_Name = case_when(
    State_Name == "DistrictOfColumbia" ~ "District of Columbia",
    State_Name == "WashingtonDC" ~ "District of Columbia",
    State_Name == "Washington DC" ~ "District of Columbia",
    State_Name == "NewHampshire" ~ "New Hampshire",
    State_Name == "NewJersey" ~ "New Jersey",
    State_Name == "NewMexico" ~ "New Mexico",
    State_Name == "NewYork" ~ "New York",
    State_Name == "NorthCarolina" ~ "North Carolina",
    State_Name == "NorthDakota" ~ "North Dakota",
    State_Name == "SouthCarolina" ~ "South Carolina",
    State_Name == "SouthDakota" ~ "South Dakota",
    State_Name == "RhodeIsland" ~ "Rhode Island",
    State_Name == "WestVirginia" ~ "West Virginia",
    State_Name == "Nerbraska" ~ "Nebraska",  
    TRUE ~ State_Name
  ))

# Create state name → abbreviation mapping
state_lookup <- tibble(
  State_Name = state.name,
  State = state.abb
) %>%
  add_row(State_Name = "District of Columbia", State = "DC")  # Add DC manually

# Merge with abbreviation
final_edu <- combined_edu %>%
  left_join(state_lookup, by = "State_Name") %>%
  select(Year, State, Percent_Bachelors)

# Check for any remaining NA
if (sum(is.na(final_edu$State)) > 0) {
  warning("There are still NA values in State column. Investigate manually.")
  print(final_edu %>% filter(is.na(State)) %>% distinct(State_Name))
}

# Save cleaned dataset
write_csv(final_edu, file.path("Project/Education_Bachelors_2006_2023.csv"))

# -------------------------------------------------------------------------------------------------------------

### Merging Data

# Set base path
final_path <- "Project/"

# Read all cleaned datasets
credit_card <- read_csv(file.path(final_path, "CreditCardDebt_PerCapita_2006_2023.csv"), show_col_types = FALSE)
population   <- read_csv(file.path(final_path, "Population_2006_2023.csv"), show_col_types = FALSE)
unemployment <- read_csv(file.path(final_path, "UnemploymentRate_2006_2023.csv"), show_col_types = FALSE)
cpi          <- read_csv(file.path(final_path, "CPI_StateLevel_2006_2023.csv"), show_col_types = FALSE)
education    <- read_csv(file.path(final_path, "Education_Bachelors_2006_2023.csv"), show_col_types = FALSE)
income       <- read_csv(file.path(final_path, "MedianIncome_2006_2023.csv"), show_col_types = FALSE)

# Merge all datasets by Year and State
merged_data <- credit_card %>%
  rename(Credit_Debt = Credit_Card_Debt_Per_Capita) %>%
  left_join(population,   by = c("Year", "State")) %>%
  left_join(unemployment, by = c("Year", "State")) %>%
  left_join(cpi,          by = c("Year", "State")) %>%
  left_join(education,    by = c("Year", "State")) %>%
  left_join(income,       by = c("Year", "State"))

# Round all numeric columns to 2 decimal digits
merged_data <- merged_data %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Check for missing values
cat("Total missing values by column:\n")
print(colSums(is.na(merged_data)))


missing_rows <- merged_data %>%
  filter(if_any(everything(), is.na))

print(missing_rows)

# Remove rows with any NA values
merged_data_clean <- merged_data %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Confirm no missing values
cat("Missing values after cleaning:\n")
print(colSums(is.na(merged_data_clean)))

# Preview cleaned data
print(head(merged_data_clean))

# Save cleaned dataset
write_csv(merged_data_clean, file = file.path(final_path, "Final_Data.csv"))

# Generate full descriptive statistics
summary_stats <- merged_data_clean %>%
  select(where(is.numeric)) %>%
  psych::describe() %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ round(.x, 2)))

# View results
print(summary_stats)



