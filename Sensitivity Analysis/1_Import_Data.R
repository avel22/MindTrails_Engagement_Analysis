### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### Purpose of this script is to import the data 
### downloaded from the osf public component (https://osf.io/s8v3h/)
### and perform some further cleaning and filtering steps

## Zenodo Citation for Data Cleaning Script: 
## Eberle, Jeremy W., Baee, Sonia, Behan, Henry C., Baglione, Anna N., 
## Boukhechba, Mehdi, Funk, Daniel H., Barnes, Laura E., & Teachman, Bethany A. 
## (2022). TeachmanLab/MT-Data-CalmThinkingStudy: 1.0.0 (v1.0.0). 
## Zenodo. https://doi.org/10.5281/zenodo.6149366

## Github for Data Cleaning Script:
## https://github.com/jwe4ec/MT-Data-CalmThinkingStudy

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools)

#functions
source(here("0_Functions.R"))

#--------------------------------------------------------------------------------#
# importing the data ----
#--------------------------------------------------------------------------------#
int_cln_data_dir <- here("Data_Sensitivity_Analysis","intermediate_clean")
file.names <- list.files(int_cln_data_dir, pattern = "\\.csv$", full.names = FALSE)

check_relevant_files(file.names)

#remove extension from file name, create new array with just file name
table.names <- file_path_sans_ext(file.names)

#read in each data file, stores it in a list
dat <- lapply(paste0(int_cln_data_dir, "/", file.names), read.csv)

#name components on the list based on file name
names(dat) <- table.names

# ---------------------------------------------------------------------------- #
# Convert system-generated time stamps back to POSIXct data type ----
# From 5_import_clean_data.R by Jeremy W. Eberle
# https://github.com/jwe4ec/MT-Data-CalmThinkingStudy
# ---------------------------------------------------------------------------- #

# As stated and coded in data cleaning script 5_import_clean_data.R by Jeremy W. Eberle:
# "System-generated time stamps were outputted as characters by "4_clean_data.R". 
# They need to be converted back to POSIXct data types (with "tz = 'UTC'" for 
# user-provided "return_date_as_POSIXct" of "return_intention" table and "tz 
# = 'EST'" for all system-generated timestamps)."
for (i in 1:length(dat)) {
  POSIXct_colnames <- c(names(dat[[i]])[grep("as_POSIXct", names(dat[[i]]))],
                        "system_date_time_earliest",
                        "system_date_time_latest")
  
  for (j in 1:length(POSIXct_colnames)) {
    # Strip timezone from character vector
    
    dat[[i]][, POSIXct_colnames[j]] <- sub(" UTC| EST", "", 
                                           dat[[i]][, POSIXct_colnames[j]])
    
    # Convert character vector to POSIXct, specifying timezone
    
    if (names(dat[i]) == "return_intention" & 
        POSIXct_colnames[j] == "return_date_as_POSIXct") {
      dat[[i]][, POSIXct_colnames[j]] <- as.POSIXct(dat[[i]][, POSIXct_colnames[j]],
                                                    format = "%Y-%m-%d %H:%M:%S",
                                                    tz = "UTC")
    } else {
      dat[[i]][, POSIXct_colnames[j]] <- as.POSIXct(dat[[i]][, POSIXct_colnames[j]],
                                                    format = "%Y-%m-%d %H:%M:%S",
                                                    tz = "EST")
    }
  }
}

#--------------------------------------------------------------------------------#
# Exclude participants indicated by "exclude_analysis" in "participant" tables ----
# From data cleaning script 4_clean_data.R line 2036: "Of the 1748 who did enroll, 6 participants should be excluded from analysis 
# because they have more than two sets of unique values on DASS-21-AS items"
# repeated DASS-21-AS screening
#--------------------------------------------------------------------------------#
p.ids.exclude1 <- dat$participant %>% filter(exclude_analysis == 1) %>% select(participant_id) %>% pull()

#--------------------------------------------------------------------------------#
# Exclude participants who did not get to the the first training session ----
# In other words, did not complete the preAffect assessment in session 1
# (example: those who received coaching)
#--------------------------------------------------------------------------------#
# get participants IDs for those who did complete the preAffect assessment in session 1
# considering that most of our engagement metrics are related to training sessions
# we set the cut at those who reached training session 1
# task log table
#get participants who completed the preAffect assessment in session 1
p.1.session <- dat$task_log %>% filter(session_only == "firstSession", tag == "pre", task_name == "Affect") %>% 
  select(participant_id) %>% pull()
#get participant ids who did not complete the preAffect assessment in session 1
p.ids.exclude2 <- dat$participant %>% select(participant_id) %>% filter(!(participant_id %in% p.1.session)) %>% pull()

#assessment table
p.1.session.affect <- dat$affect %>% filter(session_only == "firstSession", tag == "pre") %>% select(participant_id) %>% pull()

p.ids.exclude2.affect <- dat$participant %>% select(participant_id) %>% filter(!(participant_id %in% p.1.session.affect)) %>% pull()

#check to make sure that both participant_ids in task log and affect table match
#returns TRUE which means that both sets of participant_ids are the same
dplyr::setequal(p.ids.exclude2, p.ids.exclude2.affect)



#--------------------------------------------------------------------------------#
# Exclude participants that were assigned to conditions that are not being analyzed ----
#--------------------------------------------------------------------------------#
#coaching participants (HR_COACH) received a different type of intervention that is not comparable to those who only received CBM-I training
#other MT researchers have taken a similar approach of removing coaching participants when answering their research question
#Exclude psychoeducation (CONTROL) since we are only focused on participants who received CBM-I training
#Exclude NONE condition, these are participants who dropped out of the study before they could be randomized into a condition
#For the purpose of our analysis focus on participants who received CBM-I training


#Check participants 176 396 390 403 which were identified to have some issues in condition assignment
# From Jeremy Eberle: 
# "
#participant_ids 176 396 390 403 are labeled as conditioning = NONE in Angular training 
## from data cleaning script "
#"
#   For participant 176, whose condition is "NONE" in "angular_training" table but 
#   "TRAINING" in "study" table at the first training session, Henry Behan said on 
#   9/7/2021 that he thinks the "angular_training" table did not pick up their 
#   assigned condition for some reason. They did receive CBM-I training during the 
#   first session. However, they did not complete the training session, which Henry 
#   Behan said on 9/8/2021 is likely due to attrition (vs. a programming error).

#   For participants 390 and 396, Changes/Issues log on 5/21/2019 says that these
#   participant IDs were missing from the "dass" table and thus manually inputted
#   there. These participants were in "NONE" at "firstSession" (where they received 
#   CBM-I training) and "LR_TRAINING" starting at "secondSession"

#   For participant 403, condition is also "NONE" at "firstSession" (where they 
#   received CBM-I training) and "LR_TRAINING" starting at "secondSession".
# "

#conditions to exclude
conditions.exclude <- c("CONTROL","HR_COACH", "NONE")

#list of participants assigned to the conditions that are being excluded
p.ids.exclude.condition <- dat$study %>% filter(conditioning %in% conditions.exclude) %>% select(participant_id) %>% pull()

# participants not in conditions being excluded
p.check.condition <- dat$study %>% filter(participant_id %in% c(176,396,390,403))
c(176,396,390,403) %in% p.ids.exclude.condition

#--------------------------------------------------------------------------------#
# Exclude participants ----
#--------------------------------------------------------------------------------#
exclude.p.ids <- unique(c(p.ids.exclude1,p.ids.exclude2,p.ids.exclude.condition))

#remove participants data from all the tables
#the only table that does not have the participant_id column is condition_assignment_settings
#as noted in the 4_clean_data.R line 1165 "condition_assignment_settings" table, for which "participant_id" is irrelevant, is retained only for Calm Thinking study"
dat.2 <- lapply(dat, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids))} else x)


#--------------------------------------------------------------------------------#
# Identify participants who completed preAffect,first training scenario, first training session ----
#--------------------------------------------------------------------------------#
#From Jeremy Eberle's code:
#https://github.com/jwe4ec/fy7e6/blob/develop/code/02_compute_completion.R
#   For the following participants, "task_log" says they completed training at the 
#   given session, but "angular_training" lacks full training data

#     firstSession
#       No data at all in "angular_training" = 602
#       0  scenarios in "angular_training"   = 406, 465
#       10 scenarios in "angular_training"   = 639
#       39 scenarios in "angular_training"   = 384, 1496
#     secondSession  
#       25 scenarios in "angular_training"   = 639
#       30 scenarios in "angular_training"   = 779
#       37 scenarios in "angular_training"   = 768
#       39 scenarios in "angular_training"   = 429, 666, 916, 1756
#     thirdSession  
#       5  scenarios in "angular_training"   = 832
#       10 scenarios in "angular_training"   = 779
#       13 scenarios in "angular_training"   = 639
#       16 scenarios in "angular_training"   = 1659
#       39 scenarios in "angular_training"   = 429, 572
#     fourthSession  
#       36 scenarios in "angular_training"   = 714
#     fifthSession  
#       4  scenarios in "angular_training"   = 832

#completed first session preAffect
p.1.preAffect <- dat.2$task_log %>% filter(session_only == "firstSession", tag == "pre", task_name == "Affect") %>% 
  select(participant_id) %>% pull()

#completed first session introduction
angular_intro <- dat.2$angular%>% filter(session_and_task_info == "firstSession", (stimulus_name == "intro" | stimulus_name == "intro1")) %>%  group_by(participant_id) %>% slice(1)
p.1.first.intro <- angular_intro %>% select(participant_id) %>% pull()

#has an entry in angular table for the first session
# angular_first <- dat.2$angular%>% filter(session_and_task_info == "firstSession") %>%  group_by(participant_id) %>% slice(1)
# p.1.first <- angular_first %>% select(participant_id) %>% pull()
#p.1.first[!(p.1.first %in% p.1.first.intro)]

#completed first session first CBM-I scenario exercise Spotting a neighbor
angular_scenario<- dat.2$angular %>% filter(step_title == "scenario",stimulus == "Spotting a neighbor") %>% group_by(participant_id) %>% slice(1)
p.1.first.scenario <- angular_scenario %>% select(participant_id) %>% pull()

#completed first session postAffect measure
p.1.postAffect <- dat.2$task_log %>% filter(session_only == "firstSession", tag == "post", task_name == "Affect") %>% 
  select(participant_id) %>% pull()

#add columns in participant table indicating whether participants are included in the above cut offs
dat.2$participant <- dat.2$participant %>% mutate(comp_preAffect = ifelse(participant_id %in% p.1.preAffect,1,0),
                                                  comp_intro =  ifelse(participant_id %in% p.1.first.intro,1,0),
                                                  comp_1_scenario = ifelse(participant_id %in% p.1.first.scenario,1,0),
                                                  comp_1_scenario_602 = ifelse(participant_id %in% p.1.first.scenario | participant_id == 602,1,0),
                                                  comp_postAffect = ifelse(participant_id %in% p.1.postAffect,1,0))


View(dat.2$participant)

#--------------------------------------------------------------------------------#
# Factor session_only columns and apply levels ----
#--------------------------------------------------------------------------------#
#session names
session.names <- c("Eligibility","preTest","firstSession","secondSession","thirdSession","fourthSession","fifthSession","PostFollowUp")
#specify column of interest
col.interest<- c("session_only")

#factor session_only column in all tables that include it and apply levels based on session order and presence
dat.3 <- lapply(dat.2, function(df) {
  column.present <- colnames(df) %in% col.interest #check to see if column of interest is in df
  if (any(column.present)){# if it is then
    present.sessions <- unique(df[column.present]) %>% pull() #extract unique values from column of interest and store it as a vector
    levels.session <- session.names[session.names %in% present.sessions] #from session names get session values that are present in column of interest, ensure that order is maintained
    df[column.present] <- lapply(df[column.present],  factor,levels = levels.session) #turn to factor and use levels based on session presence
  }
  df
})

# ---------------------------------------------------------------------------- #
# Filter all tables based on participant cut-offs ----
# ---------------------------------------------------------------------------- #

exclude.p.ids.preAffect <- dat.3$participant %>% filter(comp_preAffect == 0) %>% select(participant_id) %>% pull()

exclude.p.ids.intro <- dat.3$participant %>% filter(comp_intro == 0) %>% select(participant_id) %>% pull()

exclude.p.ids.first.scenario <-dat.3$participant %>% filter(comp_1_scenario == 0) %>% select(participant_id) %>% pull()

exclude.p.ids.first.scenario.602 <-dat.3$participant %>% filter(comp_1_scenario_602 == 0) %>% select(participant_id) %>% pull()

exclude.p.ids.postAffect <- dat.3$participant %>% filter(comp_postAffect == 0) %>% select(participant_id) %>% pull()


#remove participants data from all the tables
#the only table that does not have the participant_id column is condition_assignment_settings
#as noted in the 4_clean_data.R line 1165 "condition_assignment_settings" table, for which "participant_id" is irrelevant, is retained only for Calm Thinking study"
dat.3.preAffect <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids.preAffect))} else x)

dat.3.intro <-lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids.intro))} else x)
  
dat.3.first.scenario <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids.first.scenario))} else x)
  
dat.3.first.scenario.602 <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids.first.scenario.602))} else x)
  
dat.3.postAffect <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  !(participant_id %in% exclude.p.ids.postAffect))} else x)
  

#count participants in each dat participant table

length(dat.3$participant$participant_id)
length(dat.3.preAffect$participant$participant_id)
length(dat.3.intro$participant$participant_id)
length(dat.3.first.scenario$participant$participant_id)
length(dat.3.first.scenario.602$participant$participant_id)
length(dat.3.postAffect$participant$participant_id)

length(dat.3$study$participant_id)
length(dat.3.preAffect$study$participant_id)
length(dat.3.intro$study$participant_id)
length(dat.3.first.scenario$study$participant_id)
length(dat.3.first.scenario.602$study$participant_id)
length(dat.3.postAffect$study$participant_id)

# ---------------------------------------------------------------------------- #
# Save as RData file to be used in next step ----
# ---------------------------------------------------------------------------- #
#store in a list

dat.names <- c("dat","dat.2","dat.3","dat.3.preAffect","dat.3.intro","dat.3.first.scenario","dat.3.first.scenario.602","dat.3.postAffect")

dat.files <- list(dat,dat.2,dat.3,dat.3.preAffect,dat.3.intro,dat.3.first.scenario,dat.3.first.scenario.602,dat.3.postAffect)

names(dat.files)<- dat.names

save(dat,dat.2,dat.3,dat.3.preAffect,dat.3.intro,dat.3.first.scenario,dat.3.first.scenario.602,dat.3.postAffect, file = here("Data_Sensitivity_Analysis","1_Import_Data.RData"))

save(dat.files, file = here("Data_Sensitivity_Analysis","1_Import_Data_Dat_List.RData"))

save(dat.3, file = here("Data_Sensitivity_Analysis","1_Import_Data_Dat3.RData"))








