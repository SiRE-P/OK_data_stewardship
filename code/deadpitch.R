library(tidyverse)
library(janitor)
library(readxl)

#get list of excel files in directory
file_list <- list.files(path = "./data/deadpitch/", pattern = "*.xlsx", full.names = TRUE)

file <- file_list[7]
#read each file and put names in standard format
read_and_clean <- function(file) {
  sheet_names <- excel_sheets(file)
  
  # If there's a "RawData" sheet, read it in
  if (any(grepl("RawData", sheet_names))) {
    raw_data_sheet <- sheet_names[grepl("RawData", sheet_names)][1]  # Take the first "RawData" sheet
    df_list <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-"), guess_max = 1e5) %>% 
      clean_names()
    
  } else {
    # Otherwise, read in all sheets
    df_list <- lapply(sheet_names, function(sheet) {
      df <- read_excel(file, sheet = sheet, na = c("n/a", "-"), guess_max = 1e5) %>% 
         clean_names()
      
      # Exclude rows where poh length is "<24" or ">32" as these are from some sort of summary table and not from the main datasheet
      df <- df %>% filter(!(poh_length_cm %in% c("<24", ">32")))
      
      # Convert columns to the most appropriate data types
      df <- type.convert(df, as.is = TRUE)      
      return(df)
    })
  }
  
  # Combine all data frames into one
  df <- bind_rows(df_list)
  
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
  
  if(file == "./data/deadpitch/SK Adult.BioData Collection Sheet 2014.xlsx"){
    df <- rename(df, location = okr_section, okr_section = location)
  }
  
  # Check for the date column under different names and rename if necessary
  if (!"date" %in% names(df)) {
    stop("No date column found")
  }
  
  # Convert dates to a standard format
  df$date <- as.Date(df$date)
  
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
  print(i)
  deadpitch.df <- bind_rows(deadpitch.df, read_and_clean(file_list[i]))
}

deadpitch.df$okr_section <- str_to_lower(deadpitch.df$okr_section)
deadpitch.df$location <- str_to_lower(deadpitch.df$location)

unique_locations <- deadpitch.df %>% 
  group_by(okr_section, location, year) %>% 
  summarise(n = n()) %>% 
  arrange(year)

#checks on dataframe
summary(deadpitch.df)
str(deadpitch.df)

#check for multiples of ona_number
deadpitch.df %>% 
  group_by(ona_number) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

ggplot(deadpitch.df, aes(x = year, y = fork_length_cm, group = year))+
  geom_boxplot()+
  facet_grid(sampling_event~okr_section)
  
ggplot(deadpitch.df, aes(x = year, y = poh_length_cm, group = year))+
  geom_boxplot()+
  facet_grid(sampling_event~okr_section)

