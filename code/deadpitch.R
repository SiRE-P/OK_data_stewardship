library(tidyverse)
library(janitor)
library(readxl)
library(ggforce)
library(patchwork)

#custom functions###
convert_to_decimal <- function(x) {
  if (!is.na(x) && nchar(x) == 2 && str_detect(x, "^[0-9]+$")) {
    return(as.numeric(paste0(substr(x, 1, 1), ".", substr(x, 2, 2))))
  } else {
    return(x)
  }
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

euro_age_from_gilbert <- function(x){
  gilbert_rich_age = as.numeric(x)
  total_age = as.numeric(substr(gilbert_rich_age , 1, nchar(gilbert_rich_age ) - 1))
  age_to_sea = as.numeric(substr(gilbert_rich_age , nchar(gilbert_rich_age ), nchar(gilbert_rich_age )))
  age = ifelse(is.na(gilbert_rich_age), NA, paste0(age_to_sea - 1, ".", total_age - age_to_sea))
  return(age)
}

#2000####
dp_2000 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2000 sockeye biosampling.xls", sheet = "Sheet1") %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(sex = case_when(female == 1 ~ "f", male == 1 ~ "m", TRUE ~ NA)) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(weight_g = weight_kg * 1000) %>% 
  mutate(age = as.character(age)) %>% 
  select(date, sex, age, fork_length_cm, weight_g, location = sampling_location, comments, source)%>% 
  filter(!is.na(date))

tail(dp_2000)
tabyl(dp_2000, age)

compare_df_cols(dp_2000)

#2001####
dp_2001 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2001 Sox Biosamples.xls", sheet = "DATA_ALL", na = "not taken") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(fork_length_cm = nose_fork_length_mm /10) %>% 
  mutate(poh_length_cm = poh_mm / 10) %>% 
  mutate(age = as.character(as.numeric(age))) %>% 
  select(date, fish_number, dfo_number, sex = sex_m_f, age, fork_length_cm, poh_length_cm, weight_g, location, fecundity_count_eggs, scales_taken_y_n = scales_y_n, head_taken_y_n = head_y_n, clips_or_hole_punch, crew)%>% 
  filter(!is.na(date))

tail(dp_2001)
compare_df_cols(dp_2000, dp_2001)

#2002####
dp_2002 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2002 Sox Biosample MSversion.xls", sheet = "biosampling-ORIGINAL") %>% 
  clean_names() %>%
  filter(weight_g != "*") %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(fork_length_cm = fork_length_mm /10) %>% 
  mutate(poh_length_cm = poh_mm / 10) %>%
  mutate(age = as.character(age)) %>% 
  mutate(weight_g = as.numeric(weight_g)) %>% 
  select(date, fish_number, other_reference_number, sex, age, fork_length_cm, poh_length_cm, weight_g, location, fecundity_count_eggs = fecundity_egg_retention, scales_taken_y_n, head_taken_y_n, dna_punch_y_n, comments)%>% 
  filter(!is.na(date))

tail(dp_2002)

compare_df_cols(dp_2000, dp_2001, dp_2002)

#2003####
dp_2003 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2003 all data.xls", sheet = "Orig Biosample Data") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(sex = case_when(female == 1 ~ "f", male == 1 ~ "m", TRUE ~ NA)) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  mutate(weight_g = as.numeric(weight)) %>% 
  select(date, sex, age, fork_length_cm, weight_g, location = sampling_location, source, comments = age_comment)%>% 
  filter(!is.na(date))

tail(dp_2003)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003)

#2004####
dp_2004 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2004 Biosampling.xls", sheet = "data entry") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date_hold = mdy(date)) %>% 
  mutate(date_hold2 = convert_to_date(ifelse(grepl("\\d{5}", date), date, NA))) %>% 
  mutate(date = convert_to_date(ifelse(is.na(date_hold2), as.character(date_hold), as.character(date_hold2)))) %>% 
  mutate(weight_g = as.numeric(weight_g)) %>% 
  mutate(fork_length_cm = as.numeric(length_cm)) %>% 
  #mutate(ona_no = as.numeric(ona_no)) %>% 
  select(date, ona_number = ona_no, species, sex = sex_m_f, fork_length_cm, weight_g, location = lake_river, comments, scales_taken_y_n = scales_y_n, stomach_y_n, eggs_y_n, kidney_vial, ovarian_fluid_y_n, crew)

ages_2004 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2004 Biosampling.xls", sheet = "Ages from PADS", skip = 2) %>% 
  clean_names()

dp_2004 <- left_join(dp_2004, ages_2004 %>% 
                       select(ona_number, x3) %>% 
                       mutate(age = as.character(as.numeric(x3)/10))) %>%  #PT - confirmed that this does this correctly. But it does drop the 1Fs
  select(-x3) %>% 
  mutate(ona_number = as.numeric(ona_number))%>% 
  filter(!is.na(date))

tail(dp_2004)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004)

#2005####
dp_2005 <- read_excel("../../okanagan_data/deadpitch/2000-2012/2005 Biosample Summary.xls", sheet = "mdb transfer") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  select(date, sample_number, location, species, sex, fork_length_cm = length_cm, weight_g, age, source = data_source)%>% 
  filter(!is.na(date))

tail(dp_2005)

pen_2005 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling_05.xlsx", sheet = "Sheet1") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = euro_age_from_gilbert(gilbert_rich_age)) %>% 
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number = ona_no, okr_section, age, sex = sex_m_f, fork_length_cm = length_cm, weight_g, age, dna = dna_vial, kidney_vial, eggs_y_n, comments)%>% 
  filter(!is.na(date))

dp_2005 <- bind_rows(dp_2005, pen_2005)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005)

#2006####
#no ages in 2006 dead pitch - they are available in the brood stock fish, but those are not included because they select for large fish
#only intact fish are sized
#notes available in the INFO tab of datasheet
dp_2006 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!Adults 2006 Summary.xls", sheet = "Deadpitch") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  select(date, location = reach, sex, fork_length_cm = length_cm, comments)%>% 
  filter(!is.na(date))

tail(dp_2006)

pen_2006 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling_06.xlsx", sheet = "Sheet1") %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = euro_age_from_gilbert(gilbert_rich_age)) %>% 
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f, fork_length_cm = length, weight_g = weight, dna, eggs_y_n) 
  

dp_2006 <- bind_rows(dp_2006, pen_2006)

tail(dp_2006)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006)

#2007####
dp_2007 <- read_excel("../../okanagan_data/deadpitch/2000-2012/Skaha_Osys_Dedptch_07.xls", sheet = "Deadpitch", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = euro_age_from_gilbert(gilbert_rich_age)) %>% 
  mutate(possibly_mixed_up = ifelse(ona_number > 14769 & ona_number < 14870, TRUE, FALSE)) %>% 
  select(date, ona_number, species, fork_length_cm = length_cm, thermal_marks, age, sex = sex_m_f, location = comments, possibly_mixed_up)%>% 
  filter(!is.na(date))

tail(dp_2007)

#the sheet says that some fish were possibly mixed up in the sample trays. 

dp_2007 %>% 
  group_by(age) %>% 
  mutate(n = n()) %>% 
  ggplot(aes(y = fork_length_cm, x = possibly_mixed_up, group = possibly_mixed_up, color = possibly_mixed_up))+
  geom_jitter(height = 0)+
  facet_wrap(~age)

tabyl(dp_2007$age)

dp_2007 %>% 
  mutate(species_from_oto = case_when(is.wholenumber(as.numeric(age)) ~ "kokanee",
                                      !is.wholenumber(as.numeric(age)) ~ "sockeye",
                                      is.na(age) ~ NA_character_))%>% 
  ggplot(aes(color = possibly_mixed_up, y = fork_length_cm, x = species_from_oto)) +
  geom_jitter()

pen_2007 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling_07.xlsx", sheet = "KO Pen Ch Biosampling", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(as.numeric(euro_age_from_gilbert(age_gilbert_rich))-1)) %>% #subtracting a year because ages look too high for size of fish and the number of age 4s is too high
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f_green, fork_length_cm = length_cm, weight_g = weight_g, dna = dna_y_n, species) 

dp_2007 <- bind_rows(dp_2007, pen_2007)

tail(dp_2007)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007)

#2008####
dp_2008 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!Adults 2008 Summary_all_data.xls", sheet = "Deadpitch Data") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(age)) %>% 
  select(date, location, ona_number, species, sex, fork_length_cm = length_cm, age, thermal_marks, comments) %>% 
  filter(!is.na(date))

pen_2008 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling_08.xlsx", sheet = "KO Pen Ch Biosampling", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = euro_age_from_gilbert(age_gilbert_rich)) %>% 
  mutate(okr_section = "penticton channel") %>% 
  mutate(fecundity = as.character(fecundity)) %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f_green, fork_length_cm = length_cm, weight_g = weight_g, dna = dna_y_n, species, comments, fecundity) 

dp_2008 <- bind_rows(dp_2008, pen_2008)

tail(dp_2008)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008)

#2009####
dp_2009 <- read_excel("../../okanagan_data/deadpitch/2000-2012/Deadpitch_Thermal_Marks_2009.xls", sheet = "Original", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(age)) %>% 
  select(date, location, ona_number, age, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, comments)%>% 
  filter(!is.na(date))

tail(dp_2009)

pen_2009 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO_biosampling_Pen_Channel(1) 2009.xlsx", sheet = "KO Pen Ch Biosampling", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number, okr_section, sex = sex_m_f_green, fork_length_cm = length_cm, weight_g = weight_g, comments, fecundity) 

pen_2009_PADS <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/kok PADS fr nuseds - wkg.xlsx", sheet = "2009") %>% 
  clean_names() %>% 
  mutate(ona_number = as.numeric(container_address))

pen_2009 <- left_join(pen_2009, pen_2009_PADS %>% select(ona_number, gr_age)) %>% 
  mutate(age = euro_age_from_gilbert(gr_age)) %>% 
  select(-gr_age)
  
dp_2009 <- bind_rows(dp_2009, pen_2009)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009)

#2010####
dp_2010 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!Adults Summary 2010.xls", sheet = "Deadpitch_Data", na = "not aged") %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = as.character(as.numeric(age))) %>% 
  select(date, location, okr_section = location_b, ona_number, age, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, comments) %>% 
  filter(!is.na(date))

tail(dp_2010)
table(dp_2010$age)

pen_2010 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling 2010.xlsx", sheet = "Sheet1", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(okr_section = "penticton channel") %>% 
  mutate(fork_length_cm = as.numeric(length_mm) /10) %>% 
  mutate(fecundity = as.character(fecundity)) %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f_green, fork_length_cm, dna = dna_y_n, comments, fecundity, spawn_condition, method, head_length_mm) 

dp_2010 <- bind_rows(dp_2010, pen_2010)

tail(dp_2010)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010)

#2011####
#note that the spreadsheet is dated 2012 but the data are from 2011
#there is a Deadpitch_Original (2) sheet that also has PBS ages, but these seem to be out of order in some cases 
dp_2011 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!Deadpitch Data LW 5_Mar_2012.xls", sheet = "Deadpitch_Original", skip = 6) %>% 
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  mutate(age = european_age) %>% 
  mutate(length_cm = as.numeric(length_cm)) %>% 
  select(date, location, ona_number, age, sample_quality, sex = sex_m_f, fork_length_cm = length_cm, spawned_y_n, thermal_marks, dna_y_n, age_comment = comment, comments) %>% 
  filter(!is.na(date))

tail(dp_2011)
table(dp_2011$age)

pen_2011 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling 2011 Pen Channel.xlsx", sheet = "KO Pen Ch Biosampling", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f_green, fork_length_cm = length_cm, weight_g = weight_g, comments, dna_y_n, gill_arches_y_n) 

dp_2011 <- bind_rows(dp_2011, pen_2011)

tail(dp_2011)

compare_df_cols(dp_2000, dp_2001, dp_2002, dp_2003, dp_2004, dp_2005, dp_2006, dp_2007, dp_2008, dp_2009, dp_2010, dp_2011)


#2012####
dp_2012 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Biosampling", skip = 6) %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>% 
  select(date, location, sex = sex_m_f, fork_length_cm, poh_length_cm, spawned_y_n = spawned_y_n_partial, dna_y_n, comments, vial_number = otolith_vial_number) %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(location))

ages_2012 <- read_excel("../../okanagan_data/deadpitch/2000-2012/!2012 Ok Sox Biodata correct ages.xlsx", sheet = "Aged Otos with Biodata") %>% 
  clean_names() %>% 
  select(vial_number, age = age_pbs_lab, age_ona_lab, thermal_marks) %>% 
  mutate(age = as.character(as.numeric(age)))

dp_2012 <- left_join(dp_2012, ages_2012) %>% 
  select(-vial_number)

tail(dp_2012)
table(dp_2011$age)

pen_2012 <- read_excel("../../okanagan_data/deadpitch/penticton_channel_2005-2012/KO biosampling 2012 pen chan.xlsx", sheet = "Sheet1", skip = 6) %>% 
  clean_names() %>%
  filter(!is.na(date)) %>% 
  type.convert(as.is = TRUE) %>%      
  remove_empty(c("rows", "cols")) %>% 
  mutate(date = convert_to_date(date)) %>%
  mutate(age = as.character(age)) %>% 
  mutate(okr_section = "penticton channel") %>% 
  select(date, ona_number, okr_section, age, sex = sex_m_f, poh_length_cm , thermal_marks, fork_length_cm, spawned_y_n_partial, comments, dna_y_n, fish_condition, location) 

dp_2012 <- bind_rows(dp_2012, pen_2012)

tail(dp_2012)

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
    if(file == "../../okanagan_data/deadpitch/2013_onwards/!SK Adult BioData Collection Sheet 2013 2Jun14.xlsx"){
      df_list_hold <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns", "NS", "NR"), guess_max = 1e5) %>% 
        clean_names()
      df_list <- read_excel(file, sheet = raw_data_sheet, na = c("n/a", "-", "ns", "NS", "NR"), guess_max = 1e5, skip = 1) %>% 
        clean_names()
      names(df_list) <- names(df_list_hold)
      
    } else{
      if(file == "../../okanagan_data/deadpitch/2013_onwards/SK Adult.BioData Collection Sheet 2015.xlsx"){ #because 2015 has 2 raw data sheets and the first one does not have any aging data
        
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
      df <- read_excel(file, sheet = sheet, na = c("n/a", "-"), guess_max = 1e5, .name_repair = "minimal")
      
      df <- df[,!duplicated(names(df))] %>% 
        clean_names()
      
      if(nrow(df)>0){
      
      # Exclude rows where poh length is "<24" or ">32" as these are from some sort of summary table and not from the main datasheet
      df <- df %>% filter(!(poh_length_cm %in% c("<24", ">32")))
      
      # Convert columns to the most appropriate data types
      df <- type.convert(df, as.is = TRUE)      
      return(df)
      }
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
  
  if(file == "../../okanagan_data/deadpitch/2013_onwards/SK Adult.BioData Collection Sheet 2014.xlsx"){
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
  
  if("ona_code_number" %in% names(df)){
    df <- df %>% rename(ona_number = ona_code_number)
  }
  
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
  
  if("reach" %in% names(df)){
    df$reach <- as.character(df$reach)
  }
  
  if("total_age" %in% names(df)){
    df$total_age <- as.numeric(df$total_age)
  }
  
  if("european_age" %in% names(df)){
    df$european_age <- as.character(df$european_age)
  }
  
  if("dna_grid_number" %in% names(df)){
    df$dna_grid_number <- as.character(df$dna_grid_number)
  }
  
  if("tag_number" %in% names(df)){
    df$tag_number <- as.character(df$tag_number)
  }
  
  if("kidney_number" %in% names(df)){
    df$kidney_number <- as.character(df$kidney_number)
  }

  return(df)
}

#get list of excel files in directory
file_list <- list.files(path = "../../okanagan_data/deadpitch/2013_onwards/", pattern = "*.xlsx", full.names = TRUE)

#bind all files together
df_2013_onward <- data.frame()
for(i in 1:length(file_list)){
  print(i)
  df_2013_onward <- bind_rows(df_2013_onward, read_and_clean(file_list[i]))
}

#combine 2000-2012 data with post 2012 data####
compare_df_cols(df_2013_onward, dp_2000_2012)
tabyl(df_2013_onward, sampling_event, year)


deadpitch_hold <- dp_2000_2012 %>% 
  mutate(age_ona_lab = as.character(age_ona_lab)) %>% 
  select(date, okr_section, location, ona_number, sex, dfo_age = age, ona_age = age_ona_lab, age_comment, age_sample_quality = sample_quality, fork_length_cm, poh_length_cm, weight_g, thermal_marks, species, spawned = spawned_y_n, comments) %>% 
  bind_rows(
    df_2013_onward %>% 
      filter(sampling_event == "Deadpitch") %>% #remove broodstock data because it is not random and selects for larger individuals
      select(date, okr_section, reach, location, ona_number, sex, ona_age = european_age, dfo_age = dfo_age_from_otoliths, age_comment = aging_comments, age_sample_quality, fork_length_cm, poh_length_cm, weight_g, hatchery,  spawned, comments)
  ) %>% 
  mutate(year = year(date))


#clean up combined data###
deadpitch.df <- deadpitch_hold %>% 
  mutate(okr_section = str_to_lower(okr_section),
         location = str_to_lower(location),
         sex = str_to_lower(sex),
         ona_age = as.character(ona_age)) %>% 
  mutate(hatchery = ifelse(ona_number == 104326, "natural", hatchery)) %>% #fixing mislabelled hatchery on fish that is marked as unmarked origin and aged as a kokanee 
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
    location = str_replace_all(location, "\\.", ""),  # Remove periods
    location = str_replace_all(location, "broodstock$", "broodstock site"),  # Add "site" to "broodstock" when it's missing
    location = str_replace_all(location, "^(golf - kvr|golf bridge - kvr|golf bridge - kvr pilings)$", "golf bridge - kvr bridge"),
    location = str_replace_all(location, "^(vds 15 - vds 15|vds 15 - vds 16|vds 15 - vds 17|vds 15 - vds 18|vds 15 - vds 19|vds 15 - vds 20)$", "vds 15 - vds 14"),
    location = str_replace_all(location, "ok falls - vds 17$|okanagan provincial park - vds 17", "ok falls prov park - vds 17"),
    location = str_replace_all(location, "\\bgolf(?! bridge)\\b", "golf bridge"),
    location = str_replace_all(location, "\\benhanced(?! section)\\b", "enhanced section"),
    location = str_replace_all(location, "egg take", "broodstock site"),
    location = str_replace_all(location, "bridge crossing", "bridge"),
    location = str_replace_all(location, "hwy(?! 97)\\b", "hwy 97"),
    location = str_replace_all(location, "hwy 97(?! bridge\\b)", "hwy 97 bridge"),
    location = str_replace_all(location, "$spawning grounds", "okanagan river spawning grounds"),
    location = str_replace_all(location, "okanagan$", "okanagan river"),
    location = str_replace_all(location, "osoyoos$", "osoyoos lake"),
  ) %>% 
  mutate(ona_age = as.character(as.numeric(ona_age))) %>% 
  mutate(age = as.numeric(ifelse(!is.na(dfo_age), dfo_age, ona_age))) %>% 
  mutate(age_agrees = ifelse(dfo_age == ona_age, "Y", "N")) %>%  #make column to indicate if the dfo and ona_ages agree
  mutate(age_agrees = ifelse(is.na(age_agrees), "one est.", age_agrees)) %>% 
  mutate(age_source = ifelse(!is.na(dfo_age), "DFO", ifelse(!is.na(ona_age), "ONA", NA))) %>% 
  filter(fork_length_cm < 70 | is.na(fork_length_cm)) %>%  # remove fish that are larger than largest ever recorded Sockeye - likely Chinook
  mutate(poh_length_cm = ifelse(poh_length_cm < 5 & fork_length_cm > 30, NA, poh_length_cm)) %>%  # remove impossibly small poh on fish that have fork_length > 30 cm
  mutate(fork_length_cm = ifelse(fork_length_cm < 5, NA, fork_length_cm)) %>%  # remove impossibly small fork length fish
  mutate(age = ifelse(age %in% c(0, 1), NA, age)) %>% 
  mutate(age = ifelse(age == 2 & fork_length_cm > 35, NA, age)) %>% #make age NA for weird 2007 fish that are large but labelled as 2.0s
  mutate(species_from_oto = case_when(is.wholenumber(as.numeric(age)) ~ "kokanee",
                                      !is.wholenumber(as.numeric(age)) ~ "sockeye",
                                      is.na(age) ~ NA_character_)) %>% 
  mutate(age = factor(age, levels = c(2,3,4,5,6, 1.1, 2.1, 1.2, 2.2, 1.3, 1.4), ordered =  TRUE)) %>% 
  mutate(location = ifelse(year == 2013, NA, location)) %>%  #removing location in 2013 because they are just labelled as okanagan river but this includes both lower and middle river - okr section (and reach) distinguish these
  mutate(okr_section = ifelse(!is.na(location) & location == "ok falls prov park - vds 17", "above mcintyre", okr_section))

lower_ok_river <- c("index", "north", "south", "oliver")
middle_ok_river <- c("above mcintyre")
upper_ok_river <- c("penticton channel", "skaha lake")
shingle_creek <- c("shingle creek")

lower_ok_river2 <- c("boat launch - broodstock site", 
                     "boat launch - island rd", 
                     "deer park - hwy 97 bridge", 
                     "hwy 97 bridge - boat launch", 
                     "hwy 97 bridge - beach seine # 1",
                     "hwy 97 bridge - broodstock site", 
                     "hwy 97 bridge - vds13",
                     "island rd - broodstock site", #just below boat launch
                     "mcintyre dam",
                     "okanagan river",
                     "okanagan river spawning grounds", #natural and seminatural section
                     "osoyoos lake",
                     "spawning grounds",
                     "transect 2 - hwy 97 bridge"
)
middle_ok_river2 <- c("ok falls prov park - vds 17",
                      "vds 17 - vds 16", 
                      "vds 16 - vds 15",
                      "vds 15 - vds 14")

okanagan_section <- c("equesis creek", 
                      "mission creek", 
                      "nashwito creek", 
                      "peachland creek",
                      "penticton creek",
                      "powers creek",
                      "trepanier creek",
                      "trout creek")

deadpitch.df <- deadpitch.df %>% 
  mutate(section = case_when(okr_section %in% lower_ok_river ~ "lower_ok_river",
                             okr_section %in% middle_ok_river ~ "middle_ok_river",
                             okr_section %in% upper_ok_river ~ "upper_ok_river",
                             okr_section %in% shingle_creek ~ "shingle creek",
                             TRUE ~ NA_character_)) %>% 
  mutate(section = case_when(is.na(section) & location %in% lower_ok_river2 ~ "lower_ok_river",
                             is.na(section) & location %in% middle_ok_river2 ~ "middle_ok_river",
                             is.na(section) & location %in% okanagan_section ~ "ok_lake_creeks",
                             TRUE ~ section)) 

#calculate fork lengths based on poh####
#set fork lengths to NA if they are less than poh
deadpitch.df <- deadpitch.df %>% 
  mutate(fork_length_cm = ifelse(poh_length_cm > fork_length_cm & !is.na(poh_length_cm), NA, fork_length_cm))

deadpitch.df %>% 
  ggplot(aes(x = poh_length_cm, y = fork_length_cm))+
  geom_point(aes(color = species_from_oto))+
  geom_smooth(method = 'lm')+
  geom_abline(intercept = 0, slope = 1)

length_model <- lm(fork_length_cm ~ poh_length_cm, deadpitch.df)
summary(length_model)

deadpitch.df <- deadpitch.df %>% 
  mutate(fork_length_from_poh_cm = predict(length_model, newdata = .)) %>% 
  mutate(fork_length_cm_measured = fork_length_cm) %>% 
  mutate(fork_length_imputed = ifelse(!is.na(fork_length_from_poh_cm), TRUE, FALSE)) %>% 
  mutate(fork_length_from_poh_cm = ifelse(is.na(fork_length_from_poh_cm), fork_length_cm_measured, fork_length_from_poh_cm)) %>% 
  select(-fork_length_cm)

summary(deadpitch.df$fork_length_from_poh_cm)
#remove fish with no data for fork length, ona number, age, or weight
deadpitch.df <- deadpitch.df %>% 
  filter(!is.na(fork_length_from_poh_cm) | !is.na(ona_number) | !is.na(age) | !is.na(weight_g))


#clean up spawned
tabyl(deadpitch.df$spawned)
tabyl(deadpitch.df, year, spawned)
deadpitch.df$spawned <- str_to_lower(deadpitch.df$spawned)
deadpitch.df$spawned[deadpitch.df$spawned == "n"] <- "no"
deadpitch.df$spawned[deadpitch.df$spawned == "y" | deadpitch.df$spawned == "s" | deadpitch.df$spawned == "yy"] <- "yes"
deadpitch.df$spawned[deadpitch.df$spawned == "p" | deadpitch.df$spawned == "ps"] <- "partial"
tabyl(deadpitch.df$spawned)

#clean up sex
tabyl(deadpitch.df, sex) %>%   adorn_pct_formatting(rounding = "half up", digits = 0)
tabyl(deadpitch.df, year, sex) %>% adorn_percentages("col") %>%   adorn_pct_formatting(rounding = "half up", digits = 0)

deadpitch.df <- deadpitch.df %>% 
  mutate(sex = str_replace_all(sex, "\\.|\\*|-|\\?|u|unk|imm|ko", NA_character_)) %>% 
  mutate(sex = str_replace_all(sex, "f, green|f,green|green", "f")) %>% 
  mutate(sex = str_replace_all(sex, "jack|m jack|m ko", "m"))
tabyl(deadpitch.df, year, sex)
tabyl(deadpitch.df, age, sex)

tabyl(deadpitch.df %>% filter(age == "1.1"), year, sex)

#post processing and checks####
tabyl(deadpitch.df, year)
tabyl(deadpitch.df, species_from_oto)
tabyl(deadpitch.df, year, section, species_from_oto)
tabyl(deadpitch.df, species)
tabyl(deadpitch.df, age, year)
tabyl(deadpitch.df, age_source, year)
tabyl(deadpitch.df, dfo_age, year)
tabyl(deadpitch.df, ona_age, year)
tabyl(deadpitch.df, thermal_marks)
tabyl(deadpitch.df, hatchery, year)
tabyl(deadpitch.df, location, year)
tabyl(deadpitch.df, section, year)

tabyl(deadpitch.df, location, reach)
tabyl(deadpitch.df, okr_section, section)
tabyl(deadpitch.df, section)
tabyl(deadpitch.df, location, section) %>% arrange(desc(lower_ok_river), desc(middle_ok_river), desc(shingle_creek), desc(upper_ok_river), desc(ok_lake_creeks))
tabyl(deadpitch.df, spawned, year)

deadpitch.df %>% 
  tabyl(age, year, section) %>% 
  adorn_totals(c("col"))

#checks on dataframe
summary(deadpitch.df)
str(deadpitch.df)

#check for multiples of ona_number
deadpitch <- deadpitch %>% arrange(date)

deadpitch.df %>%
  filter(!is.na(ona_number)) %>% 
  group_by(ona_number) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)

#remove duplicate fish number
deadpitch.df <- deadpitch.df[!duplicated(deadpitch.df$ona_number, fromLast = TRUE) | is.na(deadpitch.df$ona_number), ]


#plotting checks####
ggplot(deadpitch.df, aes(x = fork_length_from_poh_cm, color = species_from_oto))+
  geom_density()+
  theme_bw()+
  geom_vline(xintercept = 35)

ggplot(deadpitch.df, aes(x = species_from_oto, y = fork_length_from_poh_cm, group = species_from_oto, color = species_from_oto, shape = age_agrees))+
  geom_jitter(height = 0, width = 0.1)+
  geom_violin(fill = NA, color = 1)+
  theme_bw()+
  facet_wrap(~year)

deadpitch.df %>% 
  filter(!is.na(age)) %>% 
  ggplot(aes(x = age, y = fork_length_from_poh_cm, fill = species_from_oto))+
  geom_violin(scale = "width")+
  geom_sina(scale = "width", size = 0.6, alpha = 0.2)

deadpitch.df %>% 
  filter(!is.na(age)) %>% 
  ggplot(aes(x = year, y = fork_length_from_poh_cm, group = year))+
  geom_violin(scale = "width")+
  geom_sina(scale = "width", size = 0.6, alpha = 0.2)+
  facet_wrap(~age)

deadpitch.df %>% 
  filter(!is.na(age)) %>% 
  ggplot(aes(x = age, y = fork_length_from_poh_cm, fill = species_from_oto))+
  geom_violin(scale = "width")+
  geom_sina(scale = "width", size = 0.6, alpha = 0.2)+
  facet_grid(hatchery~sex)

deadpitch.df %>% 
  filter(!is.na(age)) %>% 
  ggplot(aes(x = sex, y = fork_length_from_poh_cm, fill = species_from_oto))+
  geom_violin(scale = "width")+
  geom_sina(scale = "width", size = 0.6, alpha = 0.2)+
  facet_grid(~age)

deadpitch.df %>% 
  ggplot(aes(x = age, y = fork_length_from_poh_cm, color = species_from_oto))+
  geom_jitter(height = 0, width = 0.25)+
  facet_wrap(~year)+
  scale_shape_manual(values = c(1, 16, 15))

deadpitch.df %>% 
  ggplot(aes(x = age, y = fork_length_from_poh_cm, color = age_source, shape = age_agrees))+
  geom_jitter(height = 0, width = 0.25)+
  facet_wrap(~year)+
  scale_shape_manual(values = c(1, 16, 15))

deadpitch.df %>% 
  ggplot(aes(x = hatchery, y = fork_length_from_poh_cm, color = species_from_oto))+
  geom_hline(yintercept = 35)+
  geom_jitter(width = 0.25, height= 0, size = 0.3)

deadpitch.df %>%
  tabyl(hatchery, species_from_oto)

deadpitch.df %>%  
  filter(species_from_oto == "sockeye", hatchery != "unknown") %>% 
  tabyl(year, hatchery, section) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

deadpitch.df %>% 
  filter(fork_length_from_poh_cm >35, hatchery != "unknown") %>% 
  tabyl(year, hatchery, section) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
  
deadpitch.df %>% 
  filter(species_from_oto == "sockeye", hatchery != "unknown") %>% 
  group_by(year, section) %>%
  summarise(prop_natural = sum(hatchery == "natural")/n(), total_fish = n()) %>% 
  mutate(type = "sockeye from otoliths") %>% 
  bind_rows(
  deadpitch.df %>% 
  filter(fork_length_from_poh_cm >35, hatchery != "unknown") %>% 
  group_by(year, section) %>%
  summarise(prop_natural = sum(hatchery == "natural")/n(), total_fish = n()) %>% 
  mutate(type = "sockeye > 35 cm")) %>% 
  ggplot(aes(x = year, y = prop_natural, color = type))+
  scale_size_binned(breaks = c(1, 10, 50, 100))+
  geom_point(aes(size = total_fish))+
  geom_line()+
  facet_wrap(~section)

deadpitch.df %>% 
  tabyl(age, year, section) %>% 
  adorn_totals(c("col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0)

deadpitch.df %>% 
  filter(species_from_oto == "sockeye") %>% 
  tabyl(age, year, section) %>% 
  adorn_totals(c("col")) %>%
  adorn_percentages("col") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0)


#write csv####
write.csv(deadpitch.df %>% 
            select(year, date, section, sub_section = okr_section, location, reach, ona_number,
                   age, dfo_age, ona_age, age_source, age_agrees, age_sample_quality, age_comment,
                   species_from_oto,
                   hatchery, thermal_marks, 
                   fork_length_from_poh_cm, fork_length_cm_measured, poh_length_cm, fork_length_imputed, 
                   weight_g, sex, spawned,
                   comments)
          , file = "../../okanagan_data/deadpitch/combined_and_cleaned/okanagan_deadpitch.csv")
