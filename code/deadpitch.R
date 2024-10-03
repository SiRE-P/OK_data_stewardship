library(tidyverse)
library(janitor)
library(readxl)

#get list of excel files in directory
file_list <- list.files(path = "./data/deadpitch/", pattern = "*.xlsx", full.names = TRUE)

#file <- file_list[6]
#read each file and put names in standard format
read_and_clean <- function(file) {
  sheet_name <- excel_sheets(file) %>% 
    str_subset("RawData")
  
  #NAs are coded as n/a in some sheets and - in others
  df <- read_excel(file, sheet = sheet_name, na = c("n/a", "-"), guess_max = 1e5)
  
  # Clean up column names
  df <- df %>% clean_names()
  
  # Create a named list of old and new column names
  name_changes <- list(
    ona_number = c("ona_number ", "ona_fish_number"),
    date = c("date", "collect_date"),
    run_time = c("run_time", "run_t_ime")
    # Add more here as needed
  )
  
  # Special case: if both "tag_numbers" and "tag_number_s" exist, rename "tag_numbers"
  if (all(c("tag_number", "tag_number_s") %in% names(df))) {
    df <- rename(df, tag_number_alt = tag_number)
    df <- rename(df, tag_number = tag_number_s)
  }
  
  # Apply the name changes
  for (new_name in names(name_changes)) {
    old_names <- name_changes[[new_name]]
    if (any(names(df) %in% old_names)) {
      names(df)[names(df) %in% old_names] <- new_name
    }
  }
  
  # Check for the date column under different names and rename if necessary
  if (!"date" %in% names(df)) {
    stop("No date column found")
  }
  
  # Convert dates to a standard format
  df$date <- as.Date(df$date, format = "%Y%m%d")
  
  df$year <- year(df$date)
  
  # Drop columns that are all NA
  df <- df[, !apply(is.na(df), 2, all)]
  
  df <- df %>% 
    filter(!is.na(ona_number) & !is.na(date))
  
  return(df)
}

#bind all files together
deadpitch.df <- data.frame()
for(i in 1:length(file_list)){
  deadpitch.df <- bind_rows(deadpitch.df, read_and_clean(file_list[i]))
}

#checks on dataframe
summary(deadpitch.df)
str(deadpitch.df)

ggplot(deadpitch.df, aes(x = year, y = fork_length_cm, group = year))+
  geom_boxplot()

ggplot(deadpitch.df, aes(x = year, y = poh_length_cm, group = year))+
  geom_boxplot()
