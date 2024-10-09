library(tidyverse)
library(janitor)
library(readxl)

#custom functions###
convert_to_decimal <- function(x) {
  if (!is.na(x) && nchar(x) == 2 && str_detect(x, "^[0-9]+$")) {
    return(as.numeric(paste0(substr(x, 1, 1), ".", substr(x, 2, 2))))
  } else {
    return(x)
  }
}

#2000####
dp_2000 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2000 sockeye biosampling.xls", sheet = "Sheet1") %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(sex = case_when(female == 1 ~ "f", male == 1 ~ "m", TRUE ~ NA)) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(weight_g = weight_kg * 1000) %>% 
  mutate(age = as.character("age")) %>% 
  select(date, sex, age, fork_length_cm, weight_g, location = sampling_location, comments, source)
compare_df_cols(dp_2000)

#2001####
dp_2001 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2001 Sox Biosamples.xls", sheet = "DATA_ALL", na = "not taken") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(fork_length_cm = nose_fork_length_mm /10) %>% 
  mutate(poh_length_cm = poh_mm / 10) %>% 
  mutate(age = as.character(as.numeric(age))) %>% 
  select(date, fish_number, dfo_number, sex = sex_m_f, age, fork_length_cm, poh_length_cm, weight_g, location, fecundity_count_eggs, scales_taken_y_n = scales_y_n, head_taken_y_n = head_y_n, clips_or_hole_punch, crew)
compare_df_cols(dp_2000, dp_2001)

#2002####
dp_2002 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2002 Sox Biosample MSversion.xls", sheet = "biosampling-ORIGINAL") %>% 
  clean_names() %>%
  filter(weight_g != "*") %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(fork_length_cm = fork_length_mm /10) %>% 
  mutate(poh_length_cm = poh_mm / 10) %>%
  mutate(age = as.character(age)) %>% 
  mutate(weight_g = as.numeric(weight_g)) %>% 
  select(date, fish_number, other_reference_number, sex, age, fork_length_cm, poh_length_cm, weight_g, location, fecundity_count_eggs = fecundity_egg_retention, scales_taken_y_n, head_taken_y_n, dna_punch_y_n, comments)
compare_df_cols(dp_2000, dp_2001, dp_2002)

#2003####
dp_2003 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2003 all data.xls", sheet = "Orig Biosample Data") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(sex = case_when(female == 1 ~ "f", male == 1 ~ "m", TRUE ~ NA)) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  mutate(weight_g = as.numeric(weight)) %>% 
  select(date, sex, age, fork_length_cm, weight_g, location = sampling_location, source, comments = age_comment)
compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003)

#2004####
dp_2004 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2004 Biosampling.xls", sheet = "data entry") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date_hold = mdy(date)) %>% 
  mutate(date_hold2 = convert_to_date(ifelse(grepl("\\d{5}", date), date, NA))) %>% 
  mutate(date = convert_to_date(ifelse(is.na(date_hold2), as.character(date_hold), as.character(date_hold2)))) %>% 
  mutate(weight_g = as.numeric(weight_g)) %>% 
  mutate(fork_length_cm = as.numeric(length_cm)) %>% 
  #mutate(ona_no = as.numeric(ona_no)) %>% 
  select(date, ona_number = ona_no, species, sex = sex_m_f, fork_length_cm, weight_g, location = lake_river, comments, scales_taken_y_n = scales_y_n, stomach_y_n, eggs_y_n, kidney_vial, ovarian_fluid_y_n, crew)

ages_2004 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2004 Biosampling.xls", sheet = "Ages from PADS", skip = 2) %>% 
  clean_names()

dp_2004 <- left_join(dp_2004, ages_2004 %>% 
            select(ona_number, x3) %>% 
            mutate(age = as.character(as.numeric(x3)/10))) %>%  #PT - confirmed that this does this correctly. But it does drop the 1Fs
            select(-x3) %>% 
  mutate(ona_number = as.numeric(ona_number))
  
compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004)

#2005####
dp_2005 <- read_excel("../../SIRE_data/deadpitch/2000-2012/2005 Biosample Summary.xls", sheet = "mdb transfer") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  select(date, sample_number, location, species, sex, fork_length_cm = length_cm, weight_g, age, source = data_source)
  
compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005)

#2006####
#no ages in 2006 dead pitch - they are available in the brood stock fish, but those are not included because they select for large fish
#only intact fish are sized
#notes available in the INFO tab of datasheet
dp_2006 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!Adults 2006 Summary.xls", sheet = "Deadpitch") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  select(date, location = reach, sex, fork_length_cm = length_cm, comments)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006)

#2007####
dp_2007 <- read_excel("../../SIRE_data/deadpitch/2000-2012/Skaha_Osys_Dedptch_07.xls", sheet = "Deadpitch", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(
    gilbert_rich_age = as.numeric(gilbert_rich_age),
    total_age = as.numeric(substr(gilbert_rich_age , 1, nchar(gilbert_rich_age ) - 1)),
    age_to_sea = as.numeric(substr(gilbert_rich_age , nchar(gilbert_rich_age ), nchar(gilbert_rich_age ))),
    age = ifelse(is.na(gilbert_rich_age), NA, paste0(age_to_sea - 1, ".", total_age - age_to_sea))) %>% 
  mutate(possibly_mixed_up = ifelse(ona_number > 14769 & ona_number < 14870, TRUE, FALSE)) %>% 
  select(date, ona_number, species, fork_length_cm = length_cm, thermal_marks, age, sex = sex_m_f, location = comments, possibly_mixed_up)

dp_2007 %>% 
  group_by(age) %>% 
  mutate(n = n()) %>% filter(n > 1) %>% 
  ggplot(aes(y = fork_length_cm, x = possibly_mixed_up, group = possibly_mixed_up, color = possibly_mixed_up))+
  geom_violin()+
  facet_wrap(~age)

#the sheet says that some fish were possibly mixed up in the sample trays. Based on length-age relationship, I don't think this occurred

dp_2007$possibly_mixed_up <- NULL

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007)

#2008####
dp_2008 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!Adults 2008 Summary_all_data.xls", sheet = "Deadpitch Data") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(age)) %>% 
  select(date, location, ona_number, species, sex, fork_length_cm = length_cm, age, thermal_marks, comments)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008)

#2009####
dp_2009 <- read_excel("../../SIRE_data/deadpitch/2000-2012/Deadpitch_Thermal_Marks_2009.xls", sheet = "Original", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(age)) %>% 
  select(date, location, ona_number, age, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, comments) %>% 
  filter(!is.na(date))

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009)

#2010####
dp_2010 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!Adults Summary 2010.xls", sheet = "Deadpitch_Data", na = "not aged") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(as.numeric(age))) %>% 
  select(date, location, okr_section = location_b, ona_number, age, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, comments) %>% 
  filter(!is.na(date))

tail(dp_2010)
table(dp_2010$age)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010)

#2011####
#note that the spreadsheet is dated 2012 but the data are from 2011
#there is a Deadpitch_Original (2) sheet that also has PBS ages, but these seem to be out of order in some cases 
dp_2011 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!Deadpitch Data LW 5_Mar_2012.xls", sheet = "Deadpitch_Original", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = european_age) %>% 
  mutate(length_cm = as.numeric(length_cm)) %>% 
  select(date, location, ona_number, age, sample_quality, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, dna_y_n, age_comment = comment, comments) %>% 
  filter(!is.na(date))

tail(dp_2011)
table(dp_2011$age)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010, dp_2011)


#2012####
dp_2012 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Biosampling", skip = 6) %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  select(date, location, sex = sex_m_f, fork_length_cm, poh_length_cm, spawned_y_n = spawned_y_n_partial, dna_y_n, comments, vial_number = otolith_vial_number) %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(location))
  
ages_2012 <- read_excel("../../SIRE_data/deadpitch/2000-2012/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Aged Otos with Biodata") %>% 
  clean_names() %>% 
  select(vial_number, age = age_pbs_lab, age_ona_lab, thermal_marks) %>% 
  mutate(age = as.character(as.numeric(age)))

dp_2012 <- left_join(dp_2012, ages_2012) %>% 
  select(-vial_number)

tail(dp_2012)
table(dp_2011$age)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010, dp_2011, dp_2012)

#combine 2000-2012 data####
dp_2000_2012 <- bind_rows(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010, dp_2011, dp_2012) %>% 
  mutate(age = as.character(as.numeric(age))) %>% 
  mutate(fork_length_cm = ifelse(fork_length_cm == 0, NA, fork_length_cm)) %>% 
  mutate(species = str_to_lower(species)) %>% 
  filter(species %in% c("sk", "ko", NA)) %>% 
  mutate(chinook = grepl(x = comments, "chinook", ignore.case = TRUE)) %>% 
  filter(chinook == FALSE)

#post 2012 data####
#function that reads each file and put names in standard format
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
      if(file == "../../SIRE_data/deadpitch/modern_era/SK Adult.BioData Collection Sheet 2015.xlsx"){ #because 2015 has 2 raw data sheets and the first one does not have any aging data
        
        df_list <- read_excel(file, sheet = "2015 Raw data", na = c("n/a", "-", "ns"), guess_max = 1e5) %>% 
          clean_names
      } else{
        
        df_list <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns"), guess_max = 1e5) %>% 
          clean_names()
      }
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
  df <- bind_rows(df_list) %>% 
    remove_empty(c("rows", "cols"))
  
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
  
  if("dna_results" %in% names(df)){
    df$dna_results <- as.character(df$dna_results)
  }
  
  return(df)
}

#get list of excel files in directory
file_list <- list.files(path = "../../SIRE_data/deadpitch/modern_era/", pattern = "*.xlsx", full.names = TRUE)

#bind all files together
df_2013_onward <- data.frame()
for(i in 1:length(file_list)){
  print(i)
  df_2013_onward <- bind_rows(df_2013_onward, read_and_clean(file_list[i]))
}

#combine 2000-2012 data with post 2012 data####
compare_df_cols(df_2013_onward, dp_2000_2012)
tabyl(df_2013_onward, sampling_event, year)


deadpitch.df <- dp_2000_2012 %>% 
  select(date, okr_section, location, ona_number, sex, dfo_age = age, ona_age = age_ona_lab, age_comment, age_sample_quality = sample_quality, fork_length_cm, poh_length_cm, weight_g, thermal_marks, species, spawned = spawned_y_n, comments) %>% 
  bind_rows(
    df_2013_onward %>% 
  filter(sampling_event == "Deadpitch") %>% #remove broodstock data because it is not random and selects for larger individuals
  select(date, okr_section, location, ona_number, sex, ona_age = european_age, dfo_age = dfo_age_from_otoliths, age_comment = aging_comments, age_sample_quality, fork_length_cm, poh_length_cm, weight_g, hatchery,  spawned, comments)
  )


#clean up combined data###
deadpitch.df <- deadpitch.df %>% 
  mutate(year = year(date)) %>% 
  mutate(okr_section = str_to_lower(okr_section),
       location = str_to_lower(location),
       sex = str_to_lower(sex),
       ona_age = as.character(ona_age)) %>% 
  mutate(hatchery_hold = case_when(thermal_marks %in% c("N", "N,N", "N,N,N") ~ "natural",
                              thermal_marks %in% c("lost", "UNK") ~ "unknown",
                              is.na(thermal_marks) ~ "unknown",
                              TRUE ~ "hatchery")) %>% 
  mutate(hatchery = ifelse(!is.na(hatchery), str_to_lower(hatchery), hatchery_hold)) %>% 
  select(-hatchery_hold) %>% 
  mutate(
    location = str_to_lower(location),  # Convert to lowercase
    location = str_remove(location, "^[0-9]+:"),
    location = str_trim(location),  # Remove extra spaces
    location = str_replace_all(location, " to ", " - "), # standardize separators
    location = str_remove(location, ";.*"), #remove pit tag entries in location
    location = str_replace_all(location, " 97", ""),  # Remove "97 from hwy"
    location = str_replace_all(location, "\\.", ""),  # Remove periods
    location = str_replace_all(location, "broodstock$", "broodstock site"),  # Add "site" to "broodstock" when it's missing
    location = str_replace_all(location, "^enhanced - golf$", "enhanced section - golf bridge"),
    location = str_replace_all(location, "^(golf - kvr|golf bridge - kvr|golf bridge - kvr pilings)$", "golf bridge - kvr bridge"),
    location = str_replace_all(location, "^(golf - kvr|golf bridge - kvr|golf bridge - kvr pilings)$", "golf bridge - kvr bridge"),
    location = str_replace_all(location, "ok falls - vds 17$|okanagan provincial park - vds 17", "ok falls prov park - vds 17"),
    location = str_replace_all(location, "^hwy - enhanced$", "hwy bridge - enhanced section")
  ) %>% 
  mutate(age = ifelse(!is.na(dfo_age), dfo_age, ona_age)) %>% 
  mutate(age_agrees = ifelse(dfo_age == ona_age, "Y", "N")) %>%  #make column to indicate if the dfo and ona_ages agree
  mutate(age_agrees = ifelse(is.na(age_agrees), "one est.", age_agrees)) %>% 
  mutate(age_source = ifelse(!is.na(dfo_age), "DFO", ifelse(!is.na(ona_age), "ONA", NA)))

tabyl(deadpitch.df, year)
tabyl(deadpitch.df, dfo_age, year)
tabyl(deadpitch.df, ona_age, year)
tabyl(deadpitch.df, dfo_age)
tabyl(deadpitch.df, ona_age)
tabyl(deadpitch.df, thermal_marks)
tabyl(deadpitch.df, hatchery, year)
tabyl(deadpitch.df, location, year)
tabyl(deadpitch.df, age, year)
tabyl(deadpitch.df, age_agrees, year)
tabyl(deadpitch.df, age_source, year)

deadpitch.df %>% 
  filter(poh_length_cm > 10) %>% 
  ggplot(aes(x = poh_length_cm, y = fork_length_cm))+
  geom_point()+
  geom_smooth(method = 'lm')

length_model <- lm(fork_length_cm ~ poh_length_cm, deadpitch.df %>% filter(poh_length_cm>10))
summary(length_model)



deadpitch.df %>% 
  ggplot(aes(x = age, y = fork_length_cm, color = age_source, shape = age_agrees))+
  geom_jitter(height = 0, width = 0.25)+
  facet_wrap(~year)+
  scale_shape_manual(values = c(1, 16, 15))

deadpitch.df %>% 
  ggplot(aes(x = ona_age, y = fork_length_cm, color = factor(year)))+
  geom_jitter(height = 0, width = 0.25)+
  scale_color_manual(values = pals::cols25(n = length(unique(deadpitch.df$year))))+
  facet_wrap(~year)

deadpitch.df %>% 
  ggplot(aes(x = age, y = fork_length_cm, color = factor(year)))+
  geom_jitter(height = 0, width = 0.25)+
  scale_color_manual(values = pals::cols25(n = length(unique(deadpitch.df$year))))+
  facet_wrap(~year)




#post processing and checks####
#combine ages
deadpitch.df$dfo_age_processed <- deadpitch.df$dfo_age_from_otoliths
table
  
deadpitch.df$ona_age <- deadpitch.df$european_age
deadpitch.df$european_age <- ifelse() 

#clean up location and okr_section names
deadpitch.df$okr_section <- str_to_lower(deadpitch.df$okr_section)
deadpitch.df$location <- str_to_lower(deadpitch.df$location)

deadpitch.df %>% 
  group_by(okr_section, location) %>% 
  summarise(n = n()) %>% 
  arrange(okr_section) %>% 
  print(n = 100)

#clean up spawned
table(deadpitch.df$spawned)
deadpitch.df$spawned <- str_to_lower(deadpitch.df$spawned)
deadpitch.df$spawned[deadpitch.df$spawned == "n"] <- "no"
deadpitch.df$spawned[deadpitch.df$spawned == "y" | deadpitch.df$spawned == "s" | deadpitch.df$spawned == "yy"] <- "yes"
deadpitch.df$spawned[deadpitch.df$spawned == "p" | deadpitch.df$spawned == "ps"] <- "partial"
table(deadpitch.df$spawned)


#checks on dataframe
summary(deadpitch.df)
str(deadpitch.df)

table(deadpitch.df$sex, useNA = "always")
table(deadpitch.df$hatchery, useNA = "always")
table(deadpitch.df$age_sample_quality, useNA = "always")
table(deadpitch.df$tagged_y_n, useNA = "always")
table(deadpitch.df$european_age, useNA = "always")
table(deadpitch.df$dfo_age_from_otoliths, useNA = "always")

table(deadpitch.df$sex, useNA = "always")

#check for multiples of ona_number
deadpitch.df %>%
  filter(!is.na(ona_number)) %>% 
  group_by(ona_number) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

ggplot(deadpitch.df %>% group_by(year, european_age) %>% summarise(n = n()), aes(x = factor(european_age), y = n, group = european_age))+
  geom_point()+
  facet_wrap(~year)+
  scale_y_log10()

ggplot(deadpitch.df %>% group_by(year, hatchery) %>% summarise(n = n()), aes(x = hatchery, y = n, group = hatchery))+
  geom_point()+
  facet_wrap(~year)+
  scale_y_log10()

ggplot(deadpitch.df %>% group_by(year, hatchery) %>% summarise(n = n()), aes(x = hatchery, y = n, group = hatchery))+
  geom_point()+
  facet_wrap(~year)+
  scale_y_log10()

ggplot(deadpitch.df %>% group_by(year, spawned) %>% summarise(n = n()), aes(x = spawned, y = n, group = spawned))+
  geom_point()+
  facet_wrap(~year)+
  scale_y_log10()

ggplot(deadpitch.df %>% group_by(year, age_sample_quality) %>% summarise(n = n()), aes(x = age_sample_quality, y = n, group = age_sample_quality))+
  geom_point()+
  facet_wrap(~year)

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

deadpitch.df[sample(nrow(deadpitch.df)),] %>% ggplot(aes(x = poh_length_cm, y = fork_length_cm, color = factor(year)))+geom_point()+
  facet_grid(sampling_event~okr_section)

deadpitch.df %>%
  mutate(poh_is_present = !is.na(poh_length_cm),
         forklength_is_present = !is.na(fork_length_cm)) %>%
  group_by(poh_is_present, forklength_is_present) %>%
  summarise(count = n())



deadpitch.df %>% 
  filter(is.na(poh_length_cm), is.na(fork_length_cm))

ggplot(deadpitch.df, aes(x = year, y = weight_g, group = year))+
  geom_boxplot()+
  facet_grid(sampling_event~okr_section)



