# Libraries
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# Reading the data
europe_yearly_full_release <- read_csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/Yearly electricity Data/europe_yearly_full_release_long_format.csv")

summary(europe_yearly_full_release)

length(unique(europe_yearly_full_release$Area))

# Define the columns that should appear only once (index columns)
index_cols <- c(
  "Area", "ISO 3 code", "Year", "Area type", "Continent", "Ember region", "EU", "OECD", "G20", "G7", "ASEAN"
)

# Create row identifier within each "Year" and "Area"
europe_yearly_full_release <- europe_yearly_full_release %>%
  group_by(Year, Area) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

# Create wide format with suffix for each column
europe_yearly_full_release_reshaped <- europe_yearly_full_release %>%
  pivot_wider(
    id_cols = all_of(index_cols),
    names_from = row_id,
    values_from = c(Category, Subcategory, Variable, Unit, Value),
    names_sep = "_"
  )

# Reset index
# Flatten the column names
europe_yearly_full_release_reshaped <- europe_yearly_full_release_reshaped %>%
  mutate(row_index = row_number())


cols <- colnames(europe_yearly_full_release_reshaped)
index_cols_present <- index_cols[index_cols %in% cols]

# How many Category_x columns are there
nums <- cols[str_detect(cols, "^Category_[0-9]+$")] %>%
  str_extract("[0-9]+") %>%
  as.integer() %>%
  sort()

max_n <- if (length(nums) > 0) max(nums) else 0

# Build the desired column order
ordered <- index_cols_present
for (n in seq_len(max_n)) {
  for (base in c("Category", "Subcategory", "Variable", "Unit", "Value")) {
    column_name <- paste0(base, "_", n)
    if (column_name %in% cols) {
      ordered <- c(ordered, column_name)
    }
  }
}

# If there are any columns not in ordered, append them at the end
remaining_columns <- cols[!cols %in% ordered]
ordered <- c(ordered, remaining_columns)

# Reorder the DataFrame columns
europe_yearly_full_release_reshaped <- europe_yearly_full_release_reshaped[, ordered]

summary(europe_yearly_full_release_reshaped)

# Filling the missing values with 0
num_cols <- sapply(europe_yearly_full_release_reshaped, is.numeric)
europe_yearly_full_release_reshaped[, num_cols][is.na(europe_yearly_full_release_reshaped[, num_cols])] <- 0

europe_yearly_full_release_reshaped <- europe_yearly_full_release_reshaped %>%
  mutate(across(everything(), ~replace_na(., "0")))


# Saving the data set into CSV file
write_csv(europe_yearly_full_release_reshaped, "C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/europe_yearly_full_release_reshaped.csv")