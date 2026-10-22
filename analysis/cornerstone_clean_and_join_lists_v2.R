# Clean the ienrol list & match to course codes & match it to the framework mapping in our joint spreadsheet
  # the names are really messy and don't match between the ienrol and our joint spreadsheet
  # I am trying out copilot's suggested approach rather than continuing to do this manually. 
  # do not rename to event_name until after things are matched. 

# So, this worked really well. The problem was that some of them were so very different that 
  # I couldn't map them easily. 
  # I am going to continue to work with the code so I understand all of the steps in case it helps in the future. 

# NO THIS ISN'T QUITE WORKING. I'M ABADONING IT FOR NOW! 

library(here)
library(tidyverse)
library(janitor)
library(readxl)
library(fuzzyjoin)


#### NOTES FOR ME ####

# "Event ID" == "Course Code"
# will need to match course codes & ienrol data based on "Course name" 
# will need to create "Session ID" for each and every course. Could be "Course_code_Event_Num" translated in
# cornerstone language, so it's "Event ID_Event_Num"
# "Subject IDs" refer to the mapping for the event_template doc; this is the mapping to our capabilities

# breadcrumb: decisions need to be made about the lab notebook sessions in ienrol--they are not in our joint spreadsheet.
# breacrumb: the stay well workshops are in the spreadsheet, but only part 4 is in ienrol. Will need to deal with that. 
# breadcrubme: the "HDR Induction Seminar - Online Completion" is in ienrol but not our spreadsheet



#### LOAD DATA ####

# load data from ienrol (this contains names of Flinders attendees & names of courses)

df_ienrol <- read_csv(here::here("data", "raw", "2026_ienrol_data.csv"), lazy = FALSE) %>% 
  clean_names()


# load the course_codes and course_names from ienrol (copied and pasted from iEnrol)

df_codes <- read_csv(here::here("data", "raw", "ienrol_course_codes.csv"), lazy = FALSE) |>  
  clean_names() |> 
  select(-page)

# load our joint training spreadsheet that has the mapping to the framework 

df_frame <- read_excel(here::here("data", "raw", "training_hub_joint.xlsx"),
                       sheet = 'Joint 2026 program',
                       skip = 5, #start at the header row
                       n_max = 236,  # last session = HDR thesis discussion group
                       .name_repair = "unique",
                       trim_ws = TRUE) %>% 
  clean_names() 

#### CLEAN THE DATA ####

# turn columns into factors

cols <- c("course_name",
          "event_num",
          "status")

# clean the ienrol data [removing department & job status because not needed for import]

df_ienrol <- df_ienrol %>%
  select (c(course_name, event_num, date, first_name, last_name, fan, status)) |> 
  dplyr::mutate(across(all_of(cols), ~factor(.))) %>% 
  dplyr::mutate(attendance_status = case_when (status == "attended" ~ "Attended", 
                                               status == "didnotattend" ~ "Did not attend")) |> 
  select(-status) |> 
  filter(attendance_status %in% "Attended") 


# clean the framework data

df_frame <- df_frame |> 
  select(c(capability, session_title, format)) |> 
  separate (capability, c('capability_num', 'capability_name'), sep = ". ", extra = "merge", fill = "left") # split capability 


# add the mapping for the framework to the "subject_id" numbers (from the Cornerstone templates) 
# and location numbers depending on the format (from cornerstone templates)



#### JOIN THE IENROL & CODES FIRST ####

# This needs to happen before I start fixing the event_names because the event names of both of these tibbles match already. 
# No manipulation should happen before they are joined. 

# join the ienrol dataframes together so we have the course_code for the event_names
# create a session_id column that combines the course_code with the event_num
# this keep all attendance data, which we will need for the training record

df_full <- left_join (df_ienrol, df_codes) |> 
  mutate(session_id = paste(course_code, event_num, sep = "_")) |> 
  relocate(course_code, .before = course_name) |> 
  relocate(session_id, .before = course_name) |>  
  rename (event_id = course_code) |>  # rename so it matches cornerstone
  select(-event_num) # we don't need this anymore

# add the info needed for the templates

df_full <- df_full |> 
  mutate (vendor_provider = "Research Training Hub") |> 
  mutate (training_contact_email = "ResearchTrainingHub@flinders.edu.au")




#### FIX NAMING ISSUES BETWEEN IENROL (df_full) AND JOINT SPREADSHEET
# this all comes from copilot. We end up matching on stripped and lowercase 
  # titles, but then we'll ultimately use the 'proper' titles when we decide what matches. 

# Create a reusable normalising function

normalise_title <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bhdr supervision:\\s*", "") |>
    str_replace_all("\\bhdr\\b", "") |>
    str_replace_all("\\bworkshops\\b", "workshop") |>
    str_replace_all("\\bsessions\\b", "session") |>
    str_replace_all("\\binformation\\b", "info") |>
    str_replace_all("\\bonline\\b", "") |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

# keep original titles and add matching keys

ienrol_lookup <- df_full |>
  select(event_id, session_id, course_name, date, vendor_provider, training_contact_email) |> 
  distinct() |> 
  mutate(
    ienrol_key = normalise_title(course_name)
  )


frame_lookup <- df_frame |>
  select(capability_num, capability_name, session_title, format) |> 
  distinct() |> 
  mutate(
    frame_key = normalise_title(session_title)
  )


# Generate every possible pairing and calculate similarity. Jaro-Winkler gives a distance between 0 and 1
  # 0 is identical; smaller values indicate closer matches; larger values indicate weaker matches. 

match_candidates <- crossing(
  ienrol_lookup,
  frame_lookup
) |>
  mutate(
    distance = stringdist::stringdist(
      ienrol_key,
      frame_key,
      method = "jw"
    )
  ) |>
  group_by(session_id) |>
  arrange(distance, .by_group = TRUE) |>
  mutate(
    match_rank = row_number(),
    second_distance = nth(distance, 2),
    margin = second_distance - distance
  ) |>
  ungroup()

# keep the best match for each course

best_matches <- match_candidates |>
  filter(match_rank == 1) |>
  mutate(
    match_status = case_when(
      distance == 0 ~ "Exact",
      distance <= 0.10 & margin >= 0.03 ~ "High-confidence fuzzy",
      distance <= 0.18 & margin >= 0.02 ~ "Probable fuzzy",
      TRUE ~ "Review"
    )
  ) |>
  select(
    session_id,
    event_id,
    course_name,
    session_title,
    distance,
    margin,
    match_status
  )

# look at only non-identical matches

review <- best_matches |>
  filter(match_status != "Exact")



# create a table of all best matches

best_matches <- match_candidates |>
  filter(match_rank == 1) |>
  select(
    event_id,
    course_name,
    session_title,
    distance
  ) |>
  arrange(distance) |> 
  mutate(
    approved_frame_name = case_when (
         distance > 0 ~ "", 
         TRUE ~ as.character(course_name)))

# then export it so I can manually review it & assign the best name to the ones with issues
write_csv(best_matches, 
          here::here("data", "cornerstone_data", "best_matches.csv"))


# now read back in the reviewed matches

accepted_matches <- read_csv(
  here::here("data", "cornerstone_data", "best_matches_reviewed.csv")
)

join <- df_full |>
    left_join(
    accepted_matches,
    by = c(
      "event_id"
    )
  ) 


  left_join(
    df_frame,
    by = c(
      "event_name" = "session_title"
    )
  )

#### ISSUES REMAINING ####

# maybe seven sessions are included in the framework, but not in ienrol. i need to explore those later. 

# issues <- anti_join(
#   frame_lookup,
#   accepted_matches,
#   by = c("session_title" = "session_title"))
# 
# 
# framework_not_matched <- anti_join(
#   df_frame |> distinct(event_name),
#   accepted_matches |> select(frame_name),
#   by = c("event_name" = "frame_name")
# )
# 
# framework_not_matched
