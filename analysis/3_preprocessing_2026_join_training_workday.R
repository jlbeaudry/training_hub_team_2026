# 3. Match ienrol data + workday data

#### LIBRARY ####

# Function to load or install packages
load_or_install <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# Required packages 
packages <- c("here"
              , "tidyverse"
              , "readxl"
              , "janitor"
              , "stringr"
              , "lubridate"
              , "readxl"
              , "writexl"
)


# Load or install the packages
load_or_install(packages)

rm(packages)

# if you need captioner
# install.packages("devtools")
# devtools::install_github("adletaw/captioner")

#### LOAD & CLEAN IENROL DATA ####

df_ienrol <- read_csv(here::here("data", "cleaned", "cleaned_ienrol.csv"), lazy = FALSE) %>% 
  clean_names()


# maybe we'll need the rest of this code [breadcrumb]
df_ienrol <- df_ienrol %>% 
 # separate (name, into = c('surname', 'first_name'), sep = ", ") %>% # split name 
  mutate (name = paste(first_name, last_name, sep = " ")) %>% 
  relocate (name, .before = last_name) %>% 
  select (-c(last_name, first_name))

# need to add in an email column

df_ienrol <- df_ienrol %>% 
  mutate (email = paste(fan, "@flinders.edu.au", sep = ""))

#### LOAD & CLEAN WORKDAY DATA ####

# load data and match the names to the ienrol report

#rn_names = c("name", "staff_type", "employed_as", "email")
wd_names = c("worker", "job_family_group", "job_family", "email_primary_work")

# workthru this [breadcrumb]

wd = read_excel(here::here("data", "raw", "workday_all_research_staff.xlsx"), 
                skip = 1) %>% 
  clean_names() %>%
  mutate(worker_original_hire_date = ymd(worker_original_hire_date), # use lubridate to turn the date into a date format
         worker_latest_hire_date = ymd(worker_latest_hire_date),
         current_position_filled_date = ymd(current_position_filled_effective_date),
         data_source = "Workday") %>% 
  rename_at(all_of(wd_names), ~ rn_names) %>% 
  mutate (list_date = ymd("2026-03-16")) %>% # add the date the Workday report was run
  select(-c(current_position_filled_effective_date, end_employment_date))



# need to filter out casuals and limit to the first date of last FastStart month


wd <- wd %>% 
  mutate(position_worker_type = as.factor(position_worker_type)) %>% 
  filter (!position_worker_type %in% "Casual") %>% 
  filter (worker_latest_hire_date > "2025-11-01") %>%  # the last faststart was in November
  mutate (mismatch_dates = as.numeric(worker_latest_hire_date - worker_original_hire_date)) %>% 
  relocate (mismatch_dates, .before = position_id) %>% 
  filter (mismatch_dates == 0)


# need to pull the College data from the supervisory_org column

wd <- wd %>%
  mutate(organisational_unit = case_when(str_detect(supervisory_organization, "MPH") ~ "College of Medicine and Public Health",
                                         str_detect(supervisory_organization, "FHMRI") ~ "College of Medicine and Public Health",
                                         str_detect(supervisory_organization, "NHS") ~ "College of Nursing and Health Sciences", 
                                         str_detect(supervisory_organization, "S&E") ~ "College of Science and Engineering",
                                         str_detect(supervisory_organization, "CSE") ~ "College of Science and Engineering",
                                         str_detect(supervisory_organization, "SE") ~ "College of Science and Engineering",
                                         str_detect(supervisory_organization, "EPSW") ~ "College of Education, Psychology and Social Work", 
                                         str_detect(supervisory_organization, "BGL") ~ "College of Business, Government and Law",
                                         str_detect(supervisory_organization, "HASS") ~ "College of Humanities, Arts and Social Sciences", 
                                         str_detect(supervisory_organization, "DVCR") ~ "Deputy Vice-Chancellor (Research)", 
                                         str_detect(supervisory_organization, "RDS") ~ "Deputy Vice-Chancellor (Research)", 
                                         str_detect(supervisory_organization, "Raymond Chan") ~ "Deputy Vice-Chancellor (Research)", 
                                         str_detect(supervisory_organization, "MRFF") ~ "Deputy Vice-Chancellor (Research)", 
                                         TRUE ~ as.character("Other"))) %>% 
  relocate (organisational_unit, .after = supervisory_organization)



#### JOIN THE DATAFRAMES TOGETHER ####

# join them, remove variables we don't need, remove the doubles

join <- full_join (rn, wd) %>% 
  select(-c(created_date, position_id, position_worker_type, job_category, job_profile, 
            compensation_grade_profile, current_position_filled_date, mismatch_dates, 
            worker_original_hire_date, worker_latest_hire_date, position_title, 
            supervisory_organization))

# remove folks who were in both datasets, keep just the RN one

join <- join %>% 
  distinct (name, .keep_all = TRUE)



#### WRITE THE DATA ####

write_xlsx(join,
           here::here("data", "cleaned", "combined_new_starter_list.xlsx"))



#### JOIN WORKDAY & IENROL DATA ####