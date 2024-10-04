library(tidyverse)
library(janitor)
library(readxl)


#read each file and put names in standard format
read_and_clean <- function(file) {
  sheet_names <- excel_sheets(file)
  
  # If there's a "RawData" sheet, read it in
  if (any(grepl("RawData", sheet_names))) {
    raw_data_sheet <- sheet_names[grepl("RawData", sheet_names)][1]  # Take the first "RawData" sheet
    
    #deal with the issue of multiple header lines in at least one year
    if(file == "../../SIRE_data/deadpitch/modern_era/!SK Adult BioData Collection Sheet 2013 2Jun14.xlsx"){
      df_list_hold <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns", "NS", "NR"), guess_max = 1e5) %>% 
        clean_names()
      df_list <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns", "NS", "NR"), guess_max = 1e5, skip = 1) %>% 
        clean_names()
      names(df_list) <- names(df_list_hold)
    } else{
      
      df_list <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns"), guess_max = 1e5) %>% 
        clean_names()
    }
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
  
  if(file == "../../SIRE_data/deadpitch/modern_era/SK Adult.BioData Collection Sheet 2014.xlsx"){
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
  
  #make age_sample_quality a character if present
  if("age_sample_quality" %in% names(df)){
    df$age_sample_quality <- as.character(df$age_sample_quality)
  }
  if("fecundity" %in% names(df)){
    if(is.character(df$fecundity)){
      df$fecundity[df$fecundity=="N"] <- NA #change N to NA for fecundity
      df$fecundity <- as.numeric(df$fecundity)
    }
  }
  if("kidney_vial_number" %in% names(df)){
    df$kidney_vial_number <- as.character(df$fecundity)
  }
  
  return(df)
}

#get list of excel files in directory
file_list <- list.files(path = "../../SIRE_data/deadpitch/modern_era/", pattern = "*.xlsx", full.names = TRUE)

file <- file_list[1]

#bind all files together
deadpitch.df <- data.frame()
for(i in 1:length(file_list)){
  print(i)
  deadpitch.df <- bind_rows(deadpitch.df, read_and_clean(file_list[i]))
}

#deal with 2012 data####
biosampling <- read_excel("../../SIRE_data/deadpitch/2012_era/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Biosampling", skip = 6) %>% clean_names()
aged_otos <- read_excel("../../SIRE_data/deadpitch/2012_era/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Aged Otos with Biodata") %>% clean_names()

df <- left_join(biosampling, aged_otos %>% select(-date, - poh_length_cm, -fork_length_cm, dna_y_n, -sex_m_f, -spawned_y_n_partial, -location, -dna_y_n), by = c("otolith_vial_number"  = "vial_number"))

#names(df)[is.na(match(names(df),names(deadpitch.df)))]
#names(deadpitch.df)
#names(df)

df <- df %>% select(date, fish_number, poh_length_cm, fork_length_cm, sex = sex_m_f, european_age = age_ona_lab, dfo_age_from_otoliths = age_pbs_lab, spawned_y_n_partial, dna_y_n, location, comments, otolith_0_1_2 = otoliths_0_1_2, otolith_tray_number, cell_number, thermal_marks) %>% 
  mutate(hatchery = ifelse(is.na(thermal_marks), NA, ifelse(thermal_marks == "N", "Natural", "Hatchery"))) %>% 
  mutate(otolith_0_1_2 = as.numeric(otolith_0_1_2)) %>% 
  mutate(sampling_event = "Deadpitch") %>% 
  mutate(location = str_to_lower(location))

# Convert dates to a standard format
df$date <- as.Date(df$date)

df$year <- year(df$date)

deadpitch.df <- bind_rows(deadpitch.df, df)

#post processing and checks####
#clean up location and okr_section names
deadpitch.df$okr_section <- str_to_lower(deadpitch.df$okr_section)
deadpitch.df$location <- str_to_lower(deadpitch.df$location)

deadpitch.df %>% 
  group_by(okr_section, location) %>% 
  summarise(n = n()) %>% 
  arrange(okr_section) %>% 
  print(n = 100)


#checks on dataframe
summary(deadpitch.df)
str(deadpitch.df)

#check for multiples of ona_number
deadpitch.df %>%
  filter(!is.na(ona_number)) %>% 
  group_by(ona_number) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

ggplot(deadpitch.df, aes(x = year, y = fork_length_cm, group = year))+
  geom_boxplot()+
  facet_grid(sampling_event~okr_section)

deadpitch.df %>%
  filter(!is.na(poh_length_cm)) %>% 
  group_by(year) %>% 
  summarise(n())

ggplot(deadpitch.df, aes(x = date, y = poh_length_cm))+
  geom_point()+
  facet_grid(sampling_event~okr_section)

