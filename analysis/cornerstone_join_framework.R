# load the joined training data & match it to the framework mapping & rework for the templates

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)


#### NOTES FOR ME ####

# "Subject IDs" refer to the mapping for the event_template doc; this is the mapping to our capabilities

# OH, IT COULD BE THAT WE DON'T ACTUALLY NEED TO BRING IN THE JOINT SPREADSHEET DATA! 
  # CLAIRE WILL BE MAPPING ALL OF THAT. USE HER SPREADSHEET. Can simply the code in the clean_and_join_lists

# eventually I will need to change the format of the date columns, use this: 
#   mutate(last_date_run = format(last_date_run, "%d/%m/%Y"))

#### LOAD THE DATA ####

# load the cleaned and joined data (run "clean_and_join_lists" first)

df_training <- read_csv(here::here("data", "cleaned", "cleaned_inspire_ienrol.csv"), lazy = FALSE) %>% 
  clean_names()

# load our mapping to the framework for ienrol
  # there is inconsistency in how the "no date" is provided, so clean that up in the second step

df_ienrol_frame <- read_excel(here::here("data", "raw", "ienrol_mapping.xlsx"),
  sheet = 'Sheet1',
#  skip = , #start at the header row
#  n_max = 250,  # last session = HDR thesis discussion group
  .name_repair = "unique",
  trim_ws = TRUE) %>% 
  clean_names() 

# fix the date information and deal with the na values [the ]

df_ienrol_frame <- df_ienrol_frame |> 
    mutate(
    last_date_run = if_else(
      tolower(trimws(last_date_run)) %in% c("no date listed", "date not listed"),
      NA_character_,
      last_date_run
    ),
    last_date_run = as.Date(
      as.numeric(last_date_run),
      origin = "1899-12-30" # don't change the origin, this is for the excel serial numbers
    ))



#### CLEAN THE DATA ####

# clean the training data (ienrol & inspire)

# turn columns into factors

cols <- c("event_id",
          "session_id",
          "event_name",
          "fan",
          "attendance_status")

df_training <- df_training %>%
  dplyr::mutate(across(all_of(cols), ~ factor(.)))


# rename the columns and split the capability number and name in the framework

df_ienrol_frame <- df_ienrol_frame |> 
  rename(c(event_id = course_code,
         event_name = course_name)) |> 
  select(c(event_id, event_name, framework_mapping, course_description)) |>
  separate (framework_mapping, c('capability_num', 'capability_name'), sep = ". ", extra = "merge", fill = "left")


# turn them into factors

cols <- c("event_id",
          "event_name",
          "capability_name",
          "capability_num") 

df_ienrol_frame <- df_ienrol_frame %>%
  dplyr::mutate(across(all_of(cols), ~ factor(.)))


#### FIX THE NAMING ISSUES FOR THE FRAMEWORK ####
  # the training data is already fixed
  # all of the exceptions are in the 'title_case_exceptions.R' script. 

source(here::here("analysis", "title_case_exceptions.R"))


# fix the formatting of the names

df_ienrol_frame <- df_ienrol_frame |> 
  mutate(event_name = str_to_title(event_name),
         event_name = str_replace_all(event_name, exceptions)) |>
  filter (event_name != "HDR Induction Seminar - Online Completion") # filter this out


# fix various issues with the framework names

df_ienrol_frame <-
  df_ienrol_frame |>
  mutate (event_name = str_replace(event_name, "^Building Research Leaders.*$",
                                   "Building Research Leaders for ECRs")) |>  # remove the days from the spreadsheet
  mutate (event_name = str_replace(event_name, "^First Nations Researchers Collective Meeting",
                                   "First Nations Researchers Collective Session")) |> # remove the days from the spreadsheet
  mutate (event_name = str_replace(event_name, "^Essence of Research Leadership.*$",
                                   "Ethos Essence of Research Leadership")) |> 
  mutate (event_name = str_replace(event_name, "^Student Research Intellectual Property Training.*$",
                                   "Student Research Intellectual Property Training")) |> 
  mutate (event_name = str_replace(event_name, "^Start Well, Stay Well.*$",
                                   "Start Well, Stay Well")) |> 
  mutate (event_name = str_replace(event_name, "^HDR Thesis Writing Discussion Group.*$",
                                   "Thesis Writing Discussion Group")) 

# update the framework names to match those already changed in the training data
  # I think the commented out ones are fixed in my exceptions code. Will need to confirm.

df_ienrol_frame <-
  df_ienrol_frame |>
  mutate (
    event_name = case_when ( #old name ~ new name
      event_name == "AI for Research and Research Leadership" ~ "AI for Research and Research Leadership: Advanced Training",    # change ienrol to match spreadsheet
      event_name == "Craft a Compelling CV Workshop [Online]" ~ "Craft a Compelling CV for Academics Workshop [Online]",
      event_name == "Defence Outreach Information Session" ~ "Department of Defence Outreach Information Session",
      event_name == "DECRA Development Workshop 3 - Near Miss" ~ "DECRA Development Workshop 3: Near Miss Applicants",
      event_name == "First Nations Researchers Collective Meeting" ~ "First Nations Researchers Collective Session",
      event_name == "3MT Practice Sessions" ~ "3MT Practice Session",
      event_name == "HDR Development Drop-in Sessions" ~ "HDR Development Drop-in Session",
#      event_name == "Researcher Mentoring Program - Launch" ~ "Researcher Mentoring Program: Launch",
#      event_name == "Researcher Mentoring Program - Roundtable #1" ~ "Researcher Mentoring Program: Mentee Roundtable #1",
#      event_name == "Researcher Mentoring Program - Roundtable #2" ~ "Researcher Mentoring Program: Mentee Roundtable #2",
#      event_name == "Researcher Mentoring Program - Roundtable #3" ~ "Researcher Mentoring Program: Mentee Roundtable #3",
#      event_name == "Researcher Mentoring Program - Mentor Networking Lunch" ~ "Researcher Mentoring Program: Mentor Networking Lunch",
#      event_name == "Diving into a MRFF Application - RDS Staff Workshop" ~ "Diving into a MRFF Application: RDS Staff Workshop",
      event_name == "Media Training for Researchers with AusSMC" ~ "Media Training for Researchers with Australian Science Media Centre",
      event_name == "NHMRC Investigator Bootcamp: Metrics and Benchmarking Masterclass" ~ "NHMRC Investigator Bootcamp: Metrics and Benchmarking",
      event_name == "Learn to Program: R" ~ "Learn to Program: R Workshop",
      event_name == "Data Manipulation and Data Visualisation in R (Combined)" ~ "Data Manipulation and Data Visualisation in R",
 #     event_name == "ResearchNow Reports - RDS Training" ~ "ResearchNow Reports: RDS Training",
      event_name == "AusSMC (Science Media Centre) Interview Bootcamp" ~ "Interview Bootcamp with Australian Science Media Centre",
#      event_name == "Biosafety Training – Storage and Transport of Biological Hazards" ~ "Biosafety Training: Storage and Transport of Biological Hazards",
#      event_name == "Industry Engagement for Researchers -  Panel Discussion" ~ "Industry Engagement for Researchers: Panel Discussion",
      event_name == "Start Well, Stay Well - Part 4" ~ "Start Well, Stay Well",
      event_name == "Flinders Research Fast Start: Researcher Induction Program" ~ "Research FastStart Induction Program",
      TRUE ~ as.character(event_name)))

# remove the duplicate codes & assign new codes as needed

df_ienrol_frame <- df_ienrol_frame |> 
  dplyr::mutate(
    event_id = case_when (
      event_name == "Industry Engagement Workshop: Persuasive Presentations" ~ "IEPP",
      TRUE ~ as.character(event_id))) |> 
  mutate(duplicate_code = case_when (event_id == "PD22." ~ "Yes",  # we have some duplicate ids that need to be removed
                                     event_id == "PD220" ~ "Yes", 
                                     event_id == "AKT" ~ "Yes",
                                     event_id == "WG01" ~ "Yes",
                                     event_id == "NHMRC" ~ "Yes",
                                     event_id == "IGIS" ~ "Yes",
                                     event_id == "MR01" ~ "Yes",
                                     event_id == "MR20" ~ "Yes",
                                     event_id == "FRPA" ~ "Yes",
                                     event_id == "IE202" ~ "Yes", # it's a duplicate for SA chief & PP
                                     event_id == "ARIS2" ~ "Yes",
                                     event_id == "20FS" ~ "Yes",
                                     event_id == "MCRF1" ~ "Yes",
                                     event_id == "MRF2" ~ "Yes",
                                     event_id == "ECF2" ~ "Yes",
                                     event_id == "ECRA1" ~ "Yes",
                                     event_id == "FFOP" ~ "Yes",
                                     event_id == "RP20" ~ "Yes",
                                     event_id == "BRL20" ~ "Yes",
                                     event_id == "BRL22" ~ "Yes",
                                     TRUE ~ as.character("No"))) |> 
  filter(duplicate_code == "No") |> 
  select(-c(duplicate_code))

# remove the mentoring ones that I've changed previously. They've been mapped to another event_id.
df_ienrol_frame <- df_ienrol_frame |> 
  filter(!event_id %in% c(
    "MP22",
    "MP221",
    "RMS19",
    "MW19"))


# add the mapping for the framework to the "subject_id" numbers (from the Cornerstone templates) 
# [breadcrumb] will need location numbers depending on the format (from cornerstone templates)


#### JOIN THE TRAINING DATA TO THE FRAMEWORK

# map the capabilities to the subject_ids from cornerstone
df_ienrol_frame <-  
  df_ienrol_frame |> 
  mutate (subject_id = case_when (capability_name == "Conduct research with integrity" ~ "555",
                                  capability_name == "Question and evaluate knowledge critically" ~ "556", 
                                  capability_name == "Apply diverse research methods" ~ "557", 
                                  capability_name == "Analyse and visualise data" ~ "558",
                                    capability_name ==  "Manage research projects effectively" ~ "559",
                                    capability_name ==  "Operate within the institutional context" ~ "560",
                                    capability_name ==  "Drive personal and career development" ~ "561",
                                    capability_name ==  "Grow research funding and strategy" ~ "562",
                                    capability_name ==  "Disseminate research findings" ~ "563",
                                    capability_name ==  "Communicate research effectively" ~ "564",
                                    capability_name ==  "Promote positive research culture" ~ "565",
                                    capability_name ==  "Lead and foster productive relationships" ~ "566",
                                    capability_name ==  "Engage in culturally responsive research" ~ "567",
                                    capability_name ==  "Collaborate across industry and community" ~ "568",
                                    capability_name ==  "Translate research into impact" ~"569",
                                    capability_name ==  "Innovate and apply research" ~ "570"))
                                          

#### JOIN TRAINING DATA WITH FRAMEWORK DATA ####

# join the training data with the mapping framework

df_join <- left_join(df_training, df_ienrol_frame, by = c("event_id", "event_name"))


#### FORMAT FOR TEMPLATES ####

# keep just the unique courses, to upload to the event template guide
  # for now, I'm keeping the capability name, but we will remove that once we're happy with it

df_event_check <- df_join |> 
  select(c(event_id, event_name, subject_id, capability_name)) |> 
  unique()



df_event_frame_check <- df_join |> 
  select(c(event_id, event_name, subject_id, capability_name)) |> 
  filter(!is.na(capability_name)) |> 
distinct(event_name, .keep_all = TRUE) 


# this will be used for the event template guide

df_event_frame <- df_join |> 
  filter(!is.na(capability_num)) |> 
  distinct(event_name, .keep_all = TRUE) |> 
  select(c(event_id, event_name, subject_id))


# keep just the unique sessions, to upload to the session template guide

df_session <- df_join |> 
  select(-c(fan, attendance_status)) |> 
  unique()

### BREADCRUMB -- WHERE AM I HERE?? ###
# I think I need to format for the other templates. 

 # this is old code -- bring in what I need.

df_ienrol_unique <- df_ienrol_codes |> 
  distinct(ienrol_id, event_name)

df_inspire_unique <- df_inspire |> 
  distinct (inspire_code, event_name)
  
df_fix_code <- full_join(
  df_ienrol_unique,
  df_inspire_unique,
  by = "event_name") |> 
  mutate(event_id = coalesce(ienrol_id, inspire_code)) |>
  relocate(inspire_code, .after = ienrol_id) |>
  relocate(event_id, .after = ienrol_id)

# now that the coalesce works, clean this up so I can match it with other data sets again using event_name

df_fix_code <- df_fix_code |> 
  select(c(event_id, event_name))

# bring together the codes with the inspire data. No need to match with ienrol because those are the 
  # original codes, but will need to rename codes to event_id.

df_inspire_codes <- left_join(
  df_inspire, df_fix_code, by = "event_name", relationship = "many-to-many") |> 
  select(-inspire_code)

df_ienrol_codes <- df_ienrol_codes |> 
  rename(event_id = ienrol_id)

# this finally works! This is the full dataset, in terms of event_ids and event_names,
  # but I need to add session_ids for the unique events. That will be the next step. 

df_inspire_ienrol <- full_join(
  df_ienrol_codes,
  df_inspire_codes,
  by = c("event_id", "event_name", "date", "fan", "attendance_status"))



#### CREATE NEW SESSION IDS FOR JOINED TIBBLE ####

# create a variable that combines the event_code and session_date

df_inspire_ienrol <- df_inspire_ienrol %>%
  mutate (unique_session = as.factor(paste0(event_id, " ", date)))

# create a new dataframe with the unique numbers for the sessions, with each new session starting at 1

df_numbered <- df_inspire_ienrol %>%
  distinct(event_id, date) %>%
  arrange(event_id, date) %>%
  group_by(event_id) %>%
  mutate(event_num = row_number())

# join these tables together & move the variables to the front

df_inspire_ienrol <- df_inspire_ienrol |> 
  left_join(df_numbered, by = c("event_id", "date")) |> 
  mutate(session_id = paste(event_id, event_num, sep = "_")) |> 
#  relocate(event_id, .before = event_name) |> 
  relocate(session_id, .before = event_name) |> 
  select(-c(unique_session, event_num))



#### JOIN THE COMPLETE DATA WITH THE FRAMEWORK MAPPING ####

df_event <- df_inspire_ienrol |> 
  select(c(event_id, event_name)) |> 
  unique()


df_event_frame <- left_join(df_event, df_frame, by = "event_name") |> 
  filter(!is.na(capability_num)) |> 
  distinct(event_name, .keep_all = TRUE) |> 
  select(-c(capability_num, capability_name, format, location))

# add the info needed for the templates

df_ienrol_codes <- df_ienrol_codes |> 
  mutate (vendor_provider = "Research Training Hub") |> 
  mutate (training_contact_email = "ResearchTrainingHub@flinders.edu.au")

#### SIMPLIFY TO UNIQUE EVENT NAMES ####

# keep just the unique courses, to check the names of the events

inspire_event <- df_inspire |> 
  select(event_name) |> 
  unique()

# compare to the ienrol frames
ienrol_event <- df_ienrol_pre |> 
  select(event_name, event_id, session_id) |> 
  unique()


# this is where I can find the mismatches between the two dataframes

setdiff(ienrol_event$event_name, inspire_event$event_name)







# keep just the unique sessions, to upload to the session template guide

df_session <- df_ienrol_pre |> 
  select(-c(fan, attendance_status)) |> 
  unique()


# keep just the unique courses, to upload to the event template guide

df_event_26 <- df_ienrol_26 |> 
  select(c(event_id, event_name)) |> 
  unique()


# join the event info with the framework
# All of the naming issues need to be resolved BEFORE this point
# this will be used for the event template guide

df_event_frame_26 <- left_join(df_event_26, df_frame, by = "event_name") |> 
  filter(!is.na(capability_num)) |> 
  distinct(event_name, .keep_all = TRUE) |> 
  select(-c(capability_num, capability_name, format, location))


# keep just the unique sessions, to upload to the session template guide

df_session_26 <- df_ienrol_26 |> 
  select(-c(fan, attendance_status)) |> 
  unique()



#### 



#### BRING IN MAPPING DATA FOR THE FRAMEWORK ####

# [breadcrumb: we will need to get the new mapping from Claire. Nothing to do yet.]





#### ADD EVENT_NUMS TO INSPIRE DATA ####

## [breadcrumb] add event_nums to inspire data

# create a variable that combines the event_code and session_date, just to double-check

df <- df %>%
  mutate (unique_session = as.factor(paste0(event_code, " ", date)))

# create a new dataframe with the unique numbers for the sessions, with each new session starting at 1

df_numbered <- df %>%
  distinct(event_code, session_date) %>%
  arrange(event_code, session_date) %>%
  group_by(event_code) %>%
  mutate(event_num = row_number())



#### WRITE THE PRE-2026 DATA ####

# for the event template

write_csv(df_event_frame_check,
          here::here("data", "cornerstone_data", "cleaned_for_event_template_for_checking.csv"))

# for the session template

write_csv(df_session,
          here::here("data", "cornerstone_data", "cleaned_for_session_template.csv"))

# for the training record

write_csv(df_ienrol_26_codes,
          here::here("data", "cornerstone_data", "cleaned_for_training_record.csv"))

#### SPLIT THE IENROL DATA BY DATE ####

# split the data for 2026 

df_ienrol_26 <- df_ienrol_codes |> 
  filter(date > "2026-01-01")


#### JOIN THE 2026 TIBBLES TOGETHER ####

# keep just the unique courses, to upload to the event template guide

df_event_26 <- df_ienrol_26 |> 
  select(c(event_id, event_name)) |> 
  unique()


# join the event info with the framework
# All of the naming issues need to be resolved BEFORE this point
# this will be used for the event template guide

df_event_frame_26 <- left_join(df_event_26, df_frame, by = "event_name") |> 
  filter(!is.na(capability_num)) |> 
  distinct(event_name, .keep_all = TRUE) |> 
  select(-c(capability_num, capability_name, format, location))


# keep just the unique sessions, to upload to the session template guide

df_session_26 <- df_ienrol_26 |> 
  select(-c(fan, attendance_status)) |> 
  unique()


#### CHECK THE FRAMEWORK DATA ####

df_half_checked <- read_excel(here::here("data", "cornerstone_data", "event_mapping_half_checked.xlsx"),
                              sheet = 'checked',
                              #  skip = , #start at the header row
                              #  n_max = 250,  # last session = HDR thesis discussion group
                              .name_repair = "unique",
                              trim_ws = TRUE) 

# now remove these from the full event_frame_check so Dani can check the new half
  # remove those that haven't been mapped yet (mainly inspire ones, but also some with issues to be sorted)

df_second_half <- anti_join(df_event_check, df_half_checked, by = "event_id") |> 
  filter(!is.na(subject_id))

# to be checked

write_csv(df_second_half,
          here::here("data", "cornerstone_data", "cleaned_for_event_template_for_checking_part2.csv"))

#### CLEAN THE ENVIRONMENT ####

rm(df_event_26, df_event_frame_26, df_ienrol_26, df_session_26)

# breadcrumb prompt for copilot: R. Use lubridate to rename the dates from YYYY-MM-DD to DD/MM/YYYY
