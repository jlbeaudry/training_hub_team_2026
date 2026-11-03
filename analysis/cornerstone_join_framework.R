# load the joined training data & match it to the framework mapping & rework for the templates

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(lubridate)


#### NOTES FOR ME ####

# "Subject IDs" refer to the mapping for the event_template doc; this is the mapping to our capabilities

# if I change the format of the date columns, use this: 
#   mutate(last_date_run = format(last_date_run, "%d/%m/%Y"))

#### LOAD THE DATA ####

# load the cleaned and joined data (run "clean_and_join_lists" first)

df_training <- read_csv(here::here("data", "cleaned", "cleaned_inspire_ienrol.csv"), lazy = FALSE) %>% 
  clean_names()

# load our mapping to the framework for ienrol
  # there is inconsistency in how the "no date" is provided, so clean that up in the second step

df_ienrol_frame <- read_excel(here::here("data", "raw", "ienrol_mapping.xlsx"),
  sheet = 'Sheet1',
  .name_repair = "unique",
  trim_ws = TRUE) %>% 
  clean_names() 

# load the inspire mapping & descriptions

df_inspire_frame <- read_excel(here::here("data", "raw", "inspire_descriptions_mapping.xlsx"),
                              sheet = 'inspire_need_mapping',
                              .name_repair = "unique",
                              trim_ws = TRUE) %>% 
  clean_names() 


# fix the date information and deal with the na values 

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


# fix the date format

df_inspire_frame <- df_inspire_frame |>
  mutate(
    last_date_run = as.Date(last_date_run)
  )


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
  separate (framework_mapping, c('capability_num', 'capability_name'), sep = ". ", extra = "merge", fill = "left") |> 
  mutate (source_list = "ienrol")


df_inspire_frame <- df_inspire_frame |> 
  rename(c(event_id = course_code,
           event_name = course_name,
           framework_mapping = mapping_to_framework, 
           course_description = course_description_if_not_in_ienrol_already)) |> 
  select(c(event_id, event_name, framework_mapping, ienrol_course_code, course_description)) |>
  separate (framework_mapping, c('capability_num', 'capability_name'), sep = ". ", extra = "merge", fill = "left") |> 
  mutate (source_list = "inspire")

# turn them into factors

cols <- c("event_id",
          "event_name",
          "capability_name",
          "capability_num") 

df_ienrol_frame <- df_ienrol_frame %>%
  dplyr::mutate(across(all_of(cols), ~ factor(.)))


df_inspire_frame <- df_inspire_frame %>%
  dplyr::mutate(across(all_of(cols), ~ factor(.)))


#### JOIN THE TWO FRAMEWORKS TOGETHER FOR IENROL & INSPIRE

df_join_frame <- full_join(df_ienrol_frame, df_inspire_frame)


# remove the courses that Dani wanted us to ignore

df_join_frame <- df_join_frame |> 
  filter(!event_id %in% c(
    "OGR293",
    "OGR294",
    "OGR287",
    "OGR382",
    "RDS235",
    "OGR439",
    "OGR424"))


# fix the issues with courses that Dani flagged, but that weren't caught originally
  # because the names are different. This renames the event_id and then pulls all the 
  # data from the ienrol code, name, etc. etc. 
  # this creates duplicates, but I'll get rid of them later when I'm updating the names. 

df_join_frame <- df_join_frame |>
  mutate(
    event_id = recode(
      event_id,
      "OGR266" = "HDR06",
      "OGR434" = "HDR07",
      "OGR414" = "HDR08",
      "OGR438" = "HDR11",
      "OGR442" = "HDR12",
      "OGR415" = "NIPHD",
      "OGR444" = "OGR322",
      "OGR383" = "SUP02", 
      "OGR100" = "PRC",
      "OGR324" = "AKN",
      "OGR325" = "BCX")
  ) |>
  left_join(
    df_join_frame |>
      filter(event_id %in% c("HDR06",
                           "HDR07",
                           "HDR08",
                           "HDR11",
                           "HDR12",
                           "NIPHD",
                           "OGR322",
                           "SUP02",
                           "PRC",
                           "AKN",
                           "BCX")) |>
      select(
        event_id,
        event_name,
        capability_num,
        capability_name,
        course_description
      ),
    by = "event_id",
    suffix = c("", "_correct")
  ) |>
  mutate(
    event_name = coalesce(event_name_correct, event_name),
    capability_num = coalesce(capability_num_correct, capability_num),
    capability_name = coalesce(capability_name_correct, capability_name),
    course_description = coalesce(course_description_correct, course_description)
  ) |>
  select(-ends_with("_correct")) |> 
  select(-ienrol_course_code)


#### FIX THE NAMING ISSUES FOR THE FRAMEWORK ####

  # the training data is already fixed
  # all of the exceptions are in the 'title_case_exceptions.R' script. 

source(here::here("analysis", "title_case_exceptions.R"))


# fix the formatting of the names

df_join_frame <- df_join_frame |> 
  mutate(event_name = str_to_title(event_name),
         event_name = str_replace_all(event_name, exceptions)) |>
  filter (event_name != "HDR Induction Seminar - Online Completion") # filter this out


# fix various issues with the framework names

df_join_frame <-
  df_join_frame |>
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
                                   "Thesis Writing Discussion Group")) |> 
  mutate (event_name = str_replace(event_name, "^Building Research Leaders.*$", 
                                   "Building Research Leaders for ECRs"))


# update the framework names to match those already changed in the training data
  # I think the commented out ones are fixed in my exceptions code. Will need to confirm.

df_join_frame <-
  df_join_frame |>
  filter(!event_name %in% c("ARC Discovery Projects: Strategic Advice for EOIs")) |> 
  mutate (
    event_name = case_when ( #old name ~ new name
      event_name == "AI for Research and Research Leadership" ~ "AI for Research and Research Leadership: Advanced Training",    # change ienrol to match spreadsheet
      event_name == "Craft a Compelling CV Workshop [Online]" ~ "Craft a Compelling CV for Academics Workshop",
      event_name == "Defence Outreach Information Session" ~ "Department of Defence Outreach Information Session",
      event_name == "DECRA Development Workshop 3: Near Miss" ~ "DECRA Development Workshop 3: Near Miss Applicants",
      event_name == "First Nations Researchers Collective Meeting" ~ "First Nations Researchers Collective Session",
      event_name == "3MT Practice Sessions" ~ "3MT Practice Session",
      event_name == "HDR Development Drop-in Sessions" ~ "HDR Development Drop-in Session",
      event_name == "Media Training for Researchers with AusSMC" ~ "Media Training for Researchers with Australian Science Media Centre",
      event_name == "NHMRC Investigator Bootcamp: Metrics and Benchmarking Masterclass" ~ "NHMRC Investigator Bootcamp: Metrics and Benchmarking",
      event_name == "Learn to Program: R" ~ "Learn to Program: R Workshop",
      event_name == "Data Manipulation and Data Visualisation in R (Combined)" ~ "Data Manipulation and Data Visualisation in R",
      event_name == "AusSMC (Science Media Centre) Interview Bootcamp" ~ "Interview Bootcamp with Australian Science Media Centre",
      event_name == "Industry Engagement for Researchers:  Panel Discussion" ~ "Industry Engagement for Researchers: Panel Discussion",
      event_name == "Start Well, Stay Well - Part 4" ~ "Start Well, Stay Well",
      event_name == "Flinders Research Fast Start: Researcher Induction Program" ~ "Research FastStart Induction Program",
      event_name == "ARC Discovery Projects: Strategic Advice for EOIs" ~ "Strategic Advice for ARC Discovery Projects",
      event_name == "Grant Writing: Narrative, Articulation & Clarity" ~ "Grant Writing: Narrative, Articulation and Clarity",
 #     event_name == "Researcher Mentoring Program: Mentor orientation" ~ "Researcher Mentoring Program: Mentor Orientation Session",
#      event_name == "Researcher Mentoring Program: Mentee Info Session" ~ "Researcher Mentoring Program: Mentee Information Session",
      TRUE ~ as.character(event_name)))


#fix the names based on the codes for the mentoring program
df_join_frame <- df_join_frame |> 
  mutate (
    event_name = case_when (
      event_id == "MP231" ~ "Researcher Mentoring Program: Mentee Information Session", 
      event_id == "MP232" ~ "Researcher Mentoring Program: Mentor Orientation Session", 
      event_id == "MP233" ~ "Researcher Mentoring Program: Mentee Roundtable #1", 
      event_id == "MP234" ~ "Researcher Mentoring Program: Mentee Roundtable #2", 
      event_id == "MP235" ~ "Researcher Mentoring Program: Mentee Roundtable #3", 
      TRUE ~ as.character(event_name)))


# remove the duplicate codes for the same event (with different codes)

df_join_frame <- df_join_frame |> 
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
                                     event_id == "BRL18" ~ "Yes",
                                     event_id == "DF22" ~ "Yes",
                                     event_id == "GWPE" ~ "Yes",
                                     event_id == "BAE" ~ "Yes",
                                     event_id == "ARIS1" ~ "Yes",
                                     event_id == "CCVO" ~ "Yes",
                                     event_id == "ETHOS" ~ "Yes",
                                     event_id == "ERL21" ~ "Yes",
                                     event_id == "BRL20" ~ "Yes",
                                     event_id == "TESTP" ~ "Yes",
                                     TRUE ~ as.character("No"))) |> 
  filter(duplicate_code == "No") |> 
  select(-c(duplicate_code)) 

# remove the mentoring ones that I've changed previously. They've been mapped to another event_id.
df_join_frame <- df_join_frame |> 
  filter(!event_id %in% c(
    "MP22",
    "MP221",
    "RMS19",
    "MW19"))



#### JOIN THE TRAINING DATA TO THE FRAMEWORK

# map the capabilities to the subject_ids from cornerstone
df_join_frame <-  
  df_join_frame |> 
  mutate (subject_id = case_when (capability_name == "Conduct research with integrity" ~ "555",
                                  capability_name == "Question and evaluate knowledge critically" ~ "556", 
                                  capability_name == "Apply diverse research methods" ~ "557", 
                                  capability_name == "Analyse and visualise data" ~ "558",
                                    capability_name ==  "Manage research projects effectively" ~ "559",
                                    capability_name ==  "Operate within the institutional context" ~ "560",
                                    capability_name ==  "Drive career development" ~ "561",
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


# remove the duplicates caused when joining inspire + ienrol data
df_join_frame <- df_join_frame |> 
  distinct(event_id, .keep_all = TRUE)


#### FIGURE OUT IF THERE ARE DOUBLE UPS IN THE DATA                                  

df_join_frame |> 
  distinct (event_name, event_id) |> 
  count(event_name) |> 
  filter (n > 1)

df_join_frame |> 
  count(event_id) |> 
  filter (n > 1)


#### JOIN TRAINING DATA WITH FRAMEWORK DATA ####

# join the training data with the mapping framework. As of Nov 3, this gives 30530
  # use the name from the joined framework, rather than from the training data

df_join <-  df_join_frame|> 
  left_join (df_training |> select (-event_name),
             by = "event_id")

# figure out if there are any NAs -- deal with them if there are

df_join |>
  filter(if_any(
    c(event_id, event_name, capability_num, capability_name, course_description),
    is.na
  ))


#### CLEAN UP FOR THE TEMPLATES ####

# I can clean these names up now. The joining works, so no need to do it any earlier. #I'm sure there will be others.

df_join <- df_join |>  
  dplyr::mutate (event_name = case_when (
              event_id == "ARC1" ~ "ARC Outreach: Special Research Initiative for Australian Society, History & Culture",
              TRUE ~ as.character(event_name)))


# add the info needed for the templates

df_join <- df_join |> 
  mutate (vendor_provider = "Research Training Hub") |> 
  mutate (training_contact_email = "ResearchTrainingHub@flinders.edu.au") |> 
  mutate (timezone = "65") |>  # as per Cornerstone instructions
  mutate (part_location = "HIST")


# make some of our events active so we can test that functionality in the system

df_join <- df_join |> 
  dplyr::mutate(
    active = case_when (
      event_id == "MP23" ~ "Active",
      event_id == "MP231" ~ "Active",
      event_id == "MP232" ~ "Active",
      event_id == "MP233" ~ "Active",
      event_id == "MP234" ~ "Active",
      event_id == "MP235" ~ "Active",
      event_id == "MP236" ~ "Active",
      event_id == "MP24" ~ "Active",
      event_id == "R101" ~ "Active",
      event_id == "HE01" ~ "Active",
      event_id == "3MT" ~ "Active",
      event_id == "HDR06" ~ "Active",
      event_id == "HDR11" ~ "Active",
      event_id == "HDR12" ~ "Active",
      event_id == "HDR08" ~ "Active",
      event_id == "TWDG" ~ "Active",
      TRUE ~ as.character("Inactive")))

# change the date column to their DD/MM/YYYY format

df_join <- df_join |> 
  mutate (date = format(ymd(date), "%d/%m/%Y"))
  
# add the date and time column that's required for the session template

df_join <- df_join |> 
  dplyr::mutate(start_date_time = paste(date, "09:00"),
                end_date_time = paste(date, "16:00")) 



#### FORMAT FOR EVENT TEMPLATE & WRITE THE DATA ####

# keep just the unique courses, to upload to the event template guide

df_event <- df_join |> 
  select(c(event_id, event_name, active, vendor_provider, subject_id, course_description, training_contact_email)) |> 
  unique()

# for the event template

write_xlsx(df_event,
           here::here("data", "cornerstone_data", "cleaned_for_event_template.xlsx"))



#### FORMAT FOR SESSION TEMPLATE & WRITE THE DATA ####

# keep just the unique courses, to upload to the event template guide


df_session <- df_join |> 
  select(c(event_id, session_id, event_name, start_date_time, end_date_time, timezone, part_location, 
           vendor_provider,training_contact_email)) |> 
  unique()

# for the session template

write_xlsx(df_session,
           here::here("data", "cornerstone_data", "cleaned_for_session_template.xlsx"))




#### FORMAT FOR TRAINING TEMPLATE & WRITE THE DATA ####

# keep just the unique courses, to upload to the event template guide


df_training_record <- df_join |> 
  select(c(event_id, session_id, event_name, date, fan, attendance_status, 
           vendor_provider,training_contact_email)) |> 
  unique()

# for the session template

write_xlsx(df_session,
           here::here("data", "cornerstone_data", "cleaned_for_training_record.xlsx"))

