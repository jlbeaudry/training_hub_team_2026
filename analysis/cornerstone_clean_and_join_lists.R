# Clean the ienrol, framework, and inspire lists & match to course codes 
  # I need to rework this and make it cleaner. Look at old version for 2026 joint spreadsheet code. 

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)


#### NOTES FOR ME ####

# "Event ID" == "Course Code"
# will need to match course codes & ienrol data based on "Course name" 
# will need to create "Session ID" for each and every course. Could be "Course_code_Event_Num" translated in
  # cornerstone language, so it's "Event ID_Event_Num". Do this later after joining with inspire.
# "Subject IDs" refer to the mapping for the event_template doc; this is the mapping to our capabilities

# breadcrumb: the stay well workshops are in the spreadsheet, but only part 4 is in ienrol. Will need to deal with that. 


#### LOAD THE DATA ####

# load data from ienrol (this contains names of Flinders attendees & names of courses). 

df_ienrol <- read_csv(here::here("data", "raw", "ienrol_all.csv"), lazy = FALSE) |>  
  clean_names() |>
  mutate (date = dmy(date))


# load the course_codes and course_names from ienrol (copied and pasted from iEnrol by Claire)

df_codes <- read_csv(here::here("data", "raw", "ienrol_course_codes.csv"), lazy = FALSE) |>  
  clean_names() |> 
  select(-page)


#### CLEAN THE DATA ####

# turn columns into factors

cols <- c("course_name",
          "status")

# clean the ienrol data [removing department & job status because not needed for import]
  # [breadcrumb: I only have the attended data now, but I should download all of it for completeness]

df_ienrol <- df_ienrol %>%
  select (c(course_name, event_num, date, fan, status)) |>
  dplyr::mutate(across(all_of(cols), ~ factor(.))) %>%
  dplyr::mutate(
    attendance_status = case_when (
      status == "attended" ~ "Attended",
      status == "didnotattend" ~ "Did not attend"
    )
  ) |>
  select(-status) |>
  filter(attendance_status %in% "Attended") 

# fix the names in both dfs

source(here::here("analysis", "title_case_exceptions.R"))

# fix ienrol & df_ienrol_codes (because it needs to contain the course_codes)

df_ienrol <- df_ienrol |> 
  mutate(event_name = str_to_title(course_name),
         event_name = str_replace_all(event_name, exceptions)) |> 
  select(-course_name)

df_codes <- df_codes |> 
  mutate(event_name = str_to_title(course_name),
         event_name = str_replace_all(event_name, exceptions)) |> 
  select(-course_name)
         

  
# fix a few of the ienrol data naming issues, otherwise they won't join with the code data

df_ienrol <- df_ienrol |>
   dplyr::mutate(
    event_name = case_when (
      event_name == "Grant Writing: Narrative, Articulation & Clarity" ~ "Grant Writing: Narrative, Articulation and Clarity",
       event_name == "Industry Engagement for Researchers:  Panel Discussion" ~ "Industry Engagement for Researchers: Panel Discussion",
       event_name == "Flinders Research Fast Start: Researcher Induction Program" ~ "Research FastStart Induction Program",
      event_name == "Craft a Compelling CV Workshop [Online]" ~ "Craft a Compelling CV for Academics Workshop",
      event_name == "Researcher Mentoring Program: Mentor orientation" ~ "Researcher Mentoring Program: Mentor Orientation Session",
      event_name == "Researcher Mentoring Program: Mentee Info Session" ~ "Researcher Mentoring Program: Mentee Information Session",
      TRUE ~ as.character(event_name))) |> 
    mutate (event_name = str_replace(event_name, "^Early Career Researcher Forum.*$",
                                         "Early Career Researcher Forum with the DVC(R)")) |>
      mutate (event_name = str_replace(event_name, "^Mid Career Researcher Forum.*$",
                                       "Mid-Career Researcher Forum with the DVC(R)")) |>
    mutate (event_name = str_replace(event_name, "^Finding Funding Opportunities: Navigating.*$",
                              "Finding Funding Opportunities: Navigating 'Research Professional'")) |>
    mutate(event_name = str_replace(event_name, "^Research Town Hall.*$", # get rid of the VC awards component; only run in 2026
                                    "Research Town Hall")) |>
    mutate(event_name = str_replace(event_name, "^Connect with ARC Grants.*$",
                                "Connect with ARC Grants: One-on-One Virtual Meeting")) |>
    mutate(event_name = str_replace(event_name, "^Biosafety Training â€“ .*$",
                                  "Biosafety Training: Storage and Transport of Biological Hazards")) |>
    mutate(event_name = str_replace(event_name, "^Flinders Research World CafÃ©.*$",
                                  "Flinders Research World Café")) |>
  mutate (event_name = str_replace(event_name, "^Essence of Research Leadership.*$",
                                   "Ethos Essence of Research Leadership")) |>
  mutate (event_name = str_replace(event_name, "^Ethos.*$",
                                   "Ethos Essence of Research Leadership")) |>
  mutate (event_name = str_replace(event_name, "^Student Research Intellectual Property Training.*$",
                                   "Student Research Intellectual Property Training")) |> 
  mutate (event_name = str_replace(event_name, "^Building Research Leaders.*$", 
                                "Building Research Leaders for ECRs")) |> 
  mutate (event_name = str_replace(event_name, "^the Flinders University Building.*$", 
                                   "Building Research Leaders for ECRs")) |> 
  mutate (event_name = str_replace(event_name, "^Animal Researcher Information Session: ResearchNow.*$", 
                                   "Animal Researcher Information Session: ResearchNow Ethics and Biosafety")) |> 
  mutate (event_name = str_replace(event_name, "^the Flinders University Researcher.*$", 
                                   "Researcher Mentoring Program: Closing Celebration"))


df_codes <- df_codes |>
   dplyr::mutate(
     event_name = case_when (
    event_name == "Grant Writing: Narrative, Articulation & Clarity" ~ "Grant Writing: Narrative, Articulation and Clarity",
#       course_name == "Animal Researcher information session" ~ "Animal Researcher Information Session",
    event_name == "Flinders Research Fast Start: Researcher Induction Program" ~ "Research FastStart Induction Program",
    event_name == "ARC Discovery Projects: Strategic Advice for EOIs" ~ "Strategic Advice for ARC Discovery Projects",
    event_name == "Building Research Leaders" ~ "Building Research Leaders for ECRs",
    event_name == "Connect with ARC Grants: One-on-one virtual meeting" ~ "Connect with ARC Grants: One-on-one Virtual Meeting",
    event_name == "Researcher Mentoring Program: Mentor orientation" ~ "Researcher Mentoring Program: Mentor Orientation Session",
    event_name == "Researcher Mentoring Program: Mentee Info Session" ~ "Researcher Mentoring Program: Mentee Information Session",
#       course_name == "Biosafety Training – Storage and Transport of Biological Hazards" ~ "Biosafety Training: Storage and Transport of Biological Hazards",
       TRUE ~ as.character(event_name) 
    )
  ) |>
  mutate (event_name = str_replace(event_name, "^Student Research Intellectual Property Training.*$",
                                   "Student Research Intellectual Property Training")) |>
  mutate (event_name = str_replace(event_name, "^Finding Funding Opportunities: Navigating.*$",
                                   "Finding Funding Opportunities: Navigating 'Research Professional'")) |>
  mutate (event_name = str_replace(event_name, "^Student Research Intellectual Property Training.*$",
                                   "Student Research Intellectual Property Training")) |> 
  mutate (event_name = str_replace(event_name, "^Animal Researcher Information Session: ResearchNow.*$", 
                                   "Animal Researcher Information Session: ResearchNow Ethics and Biosafety")) |> 
  mutate (event_name = str_replace(event_name, "^Building Research Leaders.*$", 
                                   "Building Research Leaders for ECRs")) |> 
  mutate (event_name = str_replace(event_name, "^the Flinders University Building.*$", 
                                   "Building Research Leaders for ECRs")) |> 
  mutate (event_name = str_replace(event_name, "^the Flinders University Researcher.*$", 
                                   "Researcher Mentoring Program: Closing Celebration"))


# rename course_name in codes & remove the duplicates & assign new codes as needed

df_codes <- df_codes |> 
  dplyr::mutate(
    course_code = case_when (
      event_name == "Industry Engagement Workshop: Persuasive Presentations" ~ "IEPP",
      event_name == "Strategic Advice for ARC Discovery Projects" ~ "ADPIS",
      TRUE ~ as.character(course_code))) |> 
  mutate(duplicate_code = case_when (course_code == "PD22." ~ "Yes",  # we have some duplicate ids that need to be removed
                                     course_code == "PD220" ~ "Yes", 
                                     course_code == "AKT" ~ "Yes",
                                     course_code == "WG01" ~ "Yes",
                                     course_code == "NHMRC" ~ "Yes",
                                     course_code == "IGIS" ~ "Yes",
                                     course_code == "MR01" ~ "Yes",
                                     course_code == "MR20" ~ "Yes",
                                     course_code == "FRPA" ~ "Yes",
                                     course_code == "IE202" ~ "Yes", # it's a duplicate for SA chief & PP
                                     course_code == "ARIS2" ~ "Yes",
                                     course_code == "20FS" ~ "Yes",
                                     course_code == "MCRF1" ~ "Yes",
                                     course_code == "MRF2" ~ "Yes",
                                     course_code == "ECF2" ~ "Yes",
                                     course_code == "ECRA1" ~ "Yes",
                                     course_code == "FFOP" ~ "Yes",
                                     course_code == "RP20" ~ "Yes",
                                     course_code == "BRL22" ~ "Yes",
                                     course_code == "BRL18" ~ "Yes",
                                     course_code == "ETHOS" ~ "Yes",
                                     course_code == "ETHOS" ~ "Yes",
                                     course_code == "ERL21" ~ "Yes",
                                     course_code == "DF22" ~ "Yes",
                                     course_code == "GWPE" ~ "Yes",
                                     course_code == "BAE" ~ "Yes",
                                     course_code == "ARIS1" ~ "Yes",
                                     course_code == "RMS18" ~ "Yes",
                                     course_code == "CCVO" ~ "Yes",
                                     course_code == "ETHOS" ~ "Yes",
                                     course_code == "ERL21" ~ "Yes",
                                     course_code == "BRL20" ~ "Yes",
                                     TRUE ~ as.character("No"))) |> 
  filter(duplicate_code == "No") |> 
  select(-duplicate_code)

#### JOIN THE IENROL & CODES ####

# This needs to happen before I start fixing the event_names because the event names of 
  # both of these tibbles match already. 
  # I haven't split by year either because the codes apply to all ienrol sessions.
  # No manipulation should happen before they are joined. 

# join the ienrol dataframes together so we have the course_code for the event_names
# create a session_id column that combines the course_code with the event_num. Do this later once
# combined with inspire data
# this keeps all attendance data, which we will need for the training record. 

df_ienrol_codes <- left_join (df_ienrol, df_codes, relationship = "many-to-many") |> 
  relocate(course_code, .before = event_name) |> 
  rename (event_id = course_code)  # rename so it matches cornerstone

# remove courses that were only online        

df_ienrol <- df_ienrol |> 
  filter (event_name != "HDR Induction Seminar - Online Completion") # filter this out 


# if there are more rows in the join than original ienrol, this is the code to check what's happening

df_ienrol_codes |> 
  distinct (event_name, event_id) |> 
  count(event_name) |> 
  filter (n > 1)

# check for any courses without event_ids

df_ienrol_codes |> filter(is.na(event_id)) |> 
  distinct(event_id, event_name)


#### CLEAN THE MENTORING PROGRAM  DATA ####

# iEnrol had some messy data for the mentoring program for 2019 & 2022. We need to 
# filter it out, clean it up, and then rejoin it to the ienrol data
# there is duplication in the MP22 data for some reason. Need to delete half of them before I remove the event_num


# separate them on their own to clean them up
df_mentor <- df_ienrol_codes |> 
  filter(event_id %in% c(
    "MP22",
    "MP221",
    "RMS19", 
    "MW19")) |>
  filter (event_num < 8)


# remove them from the df_ienrol tibble
df_ienrol_codes <- df_ienrol_codes |> 
  filter(!event_id %in% c(
          "MP22",
          "MP221",
          "RMS19",
          "MW19"))

# fix the names
df_mentor <- df_mentor |> 
  mutate (
    event_name = case_when (
      date == "2019-02-20" ~ "Researcher Mentoring Program: Mentee Information Session",
      date == "2019-02-21" ~ "Researcher Mentoring Program: Mentor Orientation Session",
      date == "2019-04-17" ~ "Researcher Mentoring Program: Mentee Roundtable #1",
      date == "2019-07-02"~ "Researcher Mentoring Program: Mentee Roundtable #2",
      date == "2019-09-17"~ "Researcher Mentoring Program: Mentee Roundtable #3",
      date == "2022-02-17" ~ "Researcher Mentoring Program: Mentee Information Session",
      date == "2022-02-23" ~ "Researcher Mentoring Program: Mentee Information Session",
      date == "2022-03-09" ~ "Researcher Mentoring Program: Launch",
      date == "2022-05-04"~  "Researcher Mentoring Program: Mentee Roundtable #1",
      date == "2022-08-16"~ "Researcher Mentoring Program: Mentee Roundtable #2",
      date == "2022-10-20"~ "Researcher Mentoring Program: Mentee Roundtable #3",
      date == "2022-11-23"~ "Researcher Mentoring Program: Closing Celebration",
      date == "2022-02-24" ~ "Researcher Mentoring Program: Mentor Orientation Session",
      date == "2019-04-09" ~ "Researcher Mentoring Program: Mentor Orientation Session",
      TRUE ~ as.character(event_name)))

#fix the codes
df_mentor <- df_mentor |> 
  mutate (
    event_id = case_when (
      event_name == "Researcher Mentoring Program: Mentee Information Session" ~ "MP231",
      event_name == "Researcher Mentoring Program: Mentor Orientation Session" ~ "MP232",
      event_name == "Researcher Mentoring Program: Mentee Roundtable #1" ~ "MP233",
      event_name == "Researcher Mentoring Program: Mentee Roundtable #2" ~ "MP234",
      event_name == "Researcher Mentoring Program: Mentee Roundtable #3" ~ "MP235",
      event_name == "Researcher Mentoring Program: Launch" ~ "MP23",
      event_name == "Researcher Mentoring Program: Closing Celebration" ~ "MP236",
      event_name == "Researcher Mentoring Program: Mentor Networking Lunch" ~ "MP24",
      TRUE ~ as.character(event_id)))
      
# rejoin it with the main df

df_ienrol_codes <- full_join (df_ienrol_codes, df_mentor) |> 
  select(-event_num) # now remove event_num (I used to remove it earlier, but need it here)

rm(df_mentor)

#### FIX NAMING ISSUES FOR IENROL (df_ienrol_codes) 

# fix the ienrol names to have better names
        
df_ienrol_codes <-
  df_ienrol_codes |>
        mutate (
          event_name = case_when ( #old name ~ new name
            event_name == "AI for Research and Research Leadership" ~ "AI for Research and Research Leadership: Advanced Training",    
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
            event_name == "Start Well, Stay Well - Part 4" ~ "Start Well, Stay Well",
            TRUE ~ as.character(event_name))) |> 
  rename(ienrol_id = event_id)
        
                                           
#### WORK WITH THE INSPIRE DATA ####

#### LOAD DATA ####

# load the inspire report from 2019 to 2025

df_inspire <- read_excel(here::here("data", "raw", "inspire_all.xlsx"),
                         sheet = 'Attendance',
                         #      n_max = 250,  # last session = HDR thesis discussion group
                         .name_repair = "unique",
                         trim_ws = TRUE) %>% 
  clean_names()  |> 
  filter(!event_code %in% c( # removing these as per Dani's advice re: counted elsewhere
    "OGR293",
    "OGR294",
    "OGR287",
    "OGR382",
    "RDS235",
    "OGR439",
    "OGR424",
    "OGR110")) # duplicate data entry of same session attendees

  

#### CLEAN THE INSPIRE DATA ####

# clean the inspire data first and then later I'll create new event_nums and session_ids because
# these are based on the ienrol data

df_inspire <- df_inspire |> 
  rename(c(event_name = event_title, 
           fan = username,
           inspire_code = event_code)) |> 
  mutate(dt = lubridate::ymd_hms(session_start_date),
         date = lubridate::as_date(dt)) |> 
  relocate(date, .after = session_start_date) |> 
  select (c(event_name, inspire_code, date, fan, attendance_type)) |> 
  dplyr::mutate(attendance_status = case_when (attendance_type == "ATTENDED" ~ "Attended")) |> 
  select (-attendance_type)

# turn them into factors
cols <- c("event_name",
          "attendance_status", 
          "fan")

df_inspire <- df_inspire |> 
dplyr::mutate(across(all_of(cols), ~factor(.)))

  
#### FIX NAMING ISSUES BETWEEN IENROL (df_ienrol) AND INSPIRE

# There are a lot of naming issues between the two datasets. I've already cleaned
  # the ienrol data, so now I just need to clean the inspire data.

# first start by fixing the captilisation for the session titles, noting that we need some exceptions
# The between words needs to be treated differently, see code below

# fix inspire spreadsheet, using the exceptions string from above

df_inspire <- df_inspire |> 
  mutate(event_name = str_to_title(event_name),
         event_name = str_replace_all(event_name, exceptions)) |> 
  mutate(event_name = case_when (inspire_code == "OGR360" ~ "So, you are graduating your PhD in a pandemic -- what is next?",
                                 inspire_code == "OGR423" ~ "Tiny Habits for Research Success",
                                 TRUE ~ as.character(event_name))) |> 
  filter(event_name != "HDR Online Induction")

#### CHECK THAT ALL DUPLICATES ARE REMOVED FROM INSPIRE DATA ####

inspire_dup <- df_inspire |>
  count(inspire_code, fan, date, event_name) |>
  filter(n > 1)

rm (inspire_dup)

# remove rows that are duplicates due to mapping to different colleges during moves.

df_inspire <- df_inspire |>
  arrange(desc(date)) |>
  distinct(inspire_code, fan, date, .keep_all = TRUE)


#### JOIN INSPIRE DATA TO IENROL CODE DATA ####

# I need to force the inspire data to use the ienrol code. So I need to do that
  # first and then rejoin this with the df_ienrol data AND then join with df_ienrol_codes


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

# as of Oct 27, this should give us 30739 rows. 


#### RECODE THE SELECTED INSPIRE ONES TO MATCH THE REVISED INSPIRE FRAMEWORK MAPPING

# fix the issues with courses that Dani flagged, but that weren't caught originally
# because the names are different. This renames the event_id and then pulls all the 
# data from the ienrol code, name, etc. etc. 

# this is flagging that we have duplicated inspire data it seems. Asking Dani to check; we can't move forward

df_inspire_ienrol2 <- df_inspire_ienrol |>
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
      "OGR325" = "BCX"
    )
  ) |>
  left_join(
    df_inspire_ienrol |>
      filter(event_id %in% c(
        "HDR06", "HDR07", "HDR08", "HDR11", "HDR12",
        "NIPHD", "OGR322", "SUP02", "PRC", "AKN", "BCX"
      )) |>
      select(
        event_id,
        date,
        fan,
        event_name,
        attendance_status
      ),
    by = c("event_id", "fan", "date"),
    suffix = c("", "_correct"),
    relationship = "many-to-one"
  ) |>
  mutate(
    event_name = coalesce(event_name_correct, event_name),
    attendance_status = coalesce(
      attendance_status_correct,
      attendance_status
    )
  ) |>
  select(-ends_with("_correct"))

# check if any have a NA for the event_id -- this flags any naming issues
df_inspire_ienrol |> filter(is.na(event_id))

# check if there are any duplicates. Lots in inspire data to sort out. 
df_inspire_ienrol |>
    count(event_id, fan, date) |>
    filter(n > 1)

df_inspire_ienrol3 <- df_inspire_ienrol |>
  arrange(desc(date)) |>
  distinct(event_id, fan, date, .keep_all = TRUE)

# we have two duplicates in our data because we had two sessions on one day in the NT, with same name


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


#### WRITE THE CLEANED DATA ####

# the full data set, pre-mapped

write_csv(df_inspire_ienrol,
          here::here("data", "cleaned", "cleaned_inspire_ienrol.csv"))

# write the inspire data that needs to be checked. 

write_csv(inspire_dup,
          here::here("data", "cleaned", "duplicated_inspire_data.csv"))
