### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### Script is adapted from the completion rate script written by Jeremy Eberle. https://github.com/jwe4ec/fy7e6/blob/main/code/2_compute_completion.R
### Purpose of this script is to calculate participant's completion rate

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools,reshape2,lubridate,hash)

source(here("0_Functions.R"))

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
#load RData from 1_Import_Data.R
load(here("Data_Sensitivity_Analysis","1_Import_Data.RData"))


dat.used <- dat.3


#--------------------------------------------------------------------------------#
# variables
#--------------------------------------------------------------------------------#
session.names <- c("Eligibility","preTest","firstSession","secondSession","thirdSession","fourthSession","fifthSession","PostFollowUp")


# ---------------------------------------------------------------------------- #
# Identify training completion per "task_log" table ----
# ---------------------------------------------------------------------------- #

# Identify participants who completed training at each session per "task_log" table
comp_s1_train_ids <- dat.used$task_log$participant_id[dat.used$task_log$task_name == "1"]
comp_s2_train_ids <- dat.used$task_log$participant_id[dat.used$task_log$task_name == "2"]
comp_s3_train_ids <- dat.used$task_log$participant_id[dat.used$task_log$task_name == "3"]
comp_s4_train_ids <- dat.used$task_log$participant_id[dat.used$task_log$task_name == "4"]
comp_s5_train_ids <- dat.used$task_log$participant_id[dat.used$task_log$task_name == "5"]

comp_train_ids <- list(comp_s1_train_ids = comp_s1_train_ids,
                       comp_s2_train_ids = comp_s2_train_ids,
                       comp_s3_train_ids = comp_s3_train_ids,
                       comp_s4_train_ids = comp_s4_train_ids,
                       comp_s5_train_ids = comp_s5_train_ids)

# ---------------------------------------------------------------------------- #
# Compare training completion in "task_log" vs. "angular_training" in CBM-I ----
# ---------------------------------------------------------------------------- #

# To estimate number of scenarios completed per session in "angular_training" table,
# count number of unique "step_index" values for "step_title" of "scenario"

## participant_ids 176 396 390 403 are labeled as conditioning = NONE in Angular training 
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
#"

## include NONE condition for now for those particular cases mentioned above
cbm_conditions <-  c("TRAINING", "LR_TRAINING", "HR_NO_COACH","NONE")

cbm_ids <- 
  unique(dat.used$angular_training[dat.used$angular_training$conditioning %in% cbm_conditions, 
                              "participant_id"])
#tidyverse way
#cbm_ids2 <- dat.used$angular_training %>% select(participant_id) %>% unique() %>% pull()
# check to see if both lists are the same
#all(cbm_ids ==cbm_ids2)


train_session <- paste0(c("first", "second", "third", "fourth", "fifth"), "Session")

output_cbm <- data.frame(participant_id = rep(cbm_ids, each = length(train_session)),
                         session = rep(train_session, length(cbm_ids)),
                         n_unq_step_index_for_scenarios = NA)

#dplyr way could replace for loop and is faster
# output_cbm2 <- dat.used$angular_training %>% 
#                       select(participant_id, session_and_task_info,step_title,step_index) %>% 
#                         filter(step_title == "scenario") %>%
#                           group_by(participant_id,session_and_task_info) %>%
#                             summarise(count = n_distinct(step_index)) %>% ungroup()


for (i in 1:length(cbm_ids)) {
  for (j in 1:length(train_session)) {
    output_cbm[output_cbm$participant_id == cbm_ids[i] & output_cbm$session == train_session[j],
               "n_unq_step_index_for_scenarios"] <- 
      length(unique(dat.used$angular_training[dat.used$angular_training$step_title == "scenario" &
                                           dat.used$angular_training$participant_id == cbm_ids[i] &
                                           dat.used$angular_training$session_and_task_info == train_session[j],
                                         "step_index"]))
  }
}


## check to see that data is the same
#output_check <- output_cbm %>% left_join(output_cbm2, by = c("participant_id","session" = "session_and_task_info"))
#output_check$count <- replace_na(output_check$count,0)

#check that columns are the same
#all(output_check$n_unq_step_index_for_scenarios==output_check$count)

# Compare participants who completed training per "task_log" table against those
# who completed 40 scenarios per "angular_training" (i.e., have at least 40 unique
# values of "step_index")

comp_train_ids_cbm_diff <- data.frame(session = train_session,
                                      comp_train_ids_in_tl_not_at = NA,
                                      comp_train_ids_in_at_not_tl = NA)

#try dplyr way
# at_40 <- lapply(unique(output_cbm$session),function(x) output_cbm %>% filter(session == x, n_unq_step_index_for_scenarios>=40) %>% select(participant_id) %>% pull())
# comp_train_ids
# 
# a <-dplyr::setdiff(at_40[[1]],comp_train_ids[[1]])
# diff1 <- lapply(1:length(train_session), function(i) setdiff(at_40[[i]],comp_train_ids[[i]]))
# diff2 <- lapply(1:length(train_session), function(i) setdiff(comp_train_ids[[i]],at_40[[i]]))
# 
# comp_train_ids_cbm_diff2 <- comp_train_ids_cbm_diff
# comp_train_ids_cbm_diff2 <- lapply(1:length(train_session), )



for (i in 1:length(train_session)) {
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_tl_not_at"] <-
    paste(setdiff(dat.used$study$participant_id[dat.used$study$participant_id %in% comp_train_ids[[i]] &
                                             dat.used$study$conditioning %in% cbm_conditions],
                  output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios >= 40]),
          collapse = ", ")
  comp_train_ids_cbm_diff[comp_train_ids_cbm_diff$session == train_session[i],
                          "comp_train_ids_in_at_not_tl"] <-
    paste(setdiff(output_cbm$participant_id[output_cbm$session == train_session[i] &
                                              output_cbm$n_unq_step_index_for_scenarios >= 40], 
                  dat.used$study$participant_id[dat.used$study$participant_id %in% comp_train_ids[[i]] &
                                             dat.used$study$conditioning %in% cbm_conditions]),
          collapse = ", ")
}

comp_train_ids_cbm_diff

# ---------------------------------------------------------------------------- #
# Investigate discrepancies in "task_log" vs. "angular_training" in CBM-I conditions ----
# ---------------------------------------------------------------------------- #

# 1. Investigate discrepancies in "comp_train_ids_in_tl_not_at" of "comp_train_ids_cbm_diff"

#   For participants where training completion in "task_log" table is not accompanied 
#   by full training data in "angular_training" table, list number of unique "step_index" 
#   values for "step_title" of "scenario" in "angular_training" table at Sessions 1-5.
#   Note: Participants without any data in "angular_training" will not be listed.

comp_train_ids_cbm_in_tl_not_at <- 
  paste(comp_train_ids_cbm_diff$comp_train_ids_in_tl_not_at, collapse = ", ")
comp_train_ids_cbm_in_tl_not_at <- 
  sort(unique(as.integer(unlist(strsplit(comp_train_ids_cbm_in_tl_not_at, split = ", ")))))

cbm_diff_report <- output_cbm[output_cbm$participant_id %in% comp_train_ids_cbm_in_tl_not_at, ]
cbm_diff_report <- cbm_diff_report[order(cbm_diff_report$participant_id), ]

# Adapting for Jeremy's comments to include Ps of interest for this analysis:

#   For the following participants, "task_log" says they completed training at the 
#   given session, but "angular_training" lacks full training data

#     firstSession
#       No data at all in "angular_training" = 602
#       10 scenarios in "angular_training"   = 639
#       39 scenarios in "angular_training"   = 384, 1496
#     secondSession  
#       25 scenarios in "angular_training"   = 639
#       30 scenarios in "angular_training"   = 779
#       37 scenarios in "angular_training"   = 768
#       39 scenarios in "angular_training"   = 429, 666, 916, 1756
#     thirdSession  
#       10 scenarios in "angular_training"   = 779
#       13 scenarios in "angular_training"   = 639
#       39 scenarios in "angular_training"   = 429, 572

#   On 2/3/2022, Henry Behan compared "date_completed" timestamps in "task_log" for 
#   9 rows of the CBM-I participants where we have "angular_training" data for fewer 
#   than 20 scenarios and determined that enough time elapsed between the "preAffect" 
#   entry in "task_log" and the "1", "2", "3", "4", or "5" entry in "task_log" such
#   that the participant could have completed the training. We think the participant's 
#   online session may have timed out, at which point when they pressed the button to 
#   complete the training, they received the "1", "2", "3", "4", or "5" entry with a 
#   "date_completed" in "task_log" but were redirected to log in again, resulting in 
#   a loss of some or all of their "angular_training" data for the session. (A similar 
#   issue occurred for "js_psych_trial" table training data in Future Thinking study.)

#   Thus, we will consider these cases of training completion in the section "Compute
#   indicator of training completion by session" below.

# 2. Investigate discrepancies in "comp_train_ids_in_at_not_tl" of "comp_train_ids_cbm_diff"

#   For the following participants, inspection of "angular_training" reveals that 
#   they completed at least 40 scenarios at a given session ("firstSession" for 1189
#   and 1161, but "task_log" does not indicate that they 
#   completed training at that session.

# View(dat.used$angular_training[dat.used$angular_training$participant_id == 1189, ])
# View(dat.used$angular_training[dat.used$angular_training$participant_id == 1161, ])
# 
# View(dat.used$task_log[dat.used$task_log$participant_id == 1189, ])
# View(dat.used$task_log[dat.used$task_log$participant_id == 1161, ])

#   Unclear why this occurred, but we will consider these cases of training completion
#   in the section "Compute indicator of training completion by session" below.


# ---------------------------------------------------------------------------- #
# Compute indicator of training completion by session ----
# ---------------------------------------------------------------------------- #

# Create "completion" table where "compl_session_train" indicates whether participant
# completed a given session's training (1 = yes, 0 = no, NA = session has no training)

participant_ids <- unique(dat.used$participant$participant_id)
sessions <- session.names

completion <- data.frame(
  participant_id = rep(participant_ids, each = length(sessions)),
  session_only = rep(sessions, length(participant_ids)),
  compl_session_train = NA
)

completion$session_only <- factor(completion$session_only, levels = sessions)

# Compute "compl_session_train". Based on sections "Investigate discrepancies in 
# 'task_log' vs. 'angular_training' in CBM-I conditions [Psychoeducation]" above,
# assume "task_log" entries indicate training completion even if "angular_training" 
# lacks some or all corresponding training data at a given session.

task_log_train <- dat.used$task_log[dat.used$task_log$session_only %in% train_session &
                                 dat.used$task_log$task_name %in% 1:5, ]

task_log_train$session_only <- factor(task_log_train$session_only,
                                      levels = train_session)

task_log_train <- task_log_train[order(task_log_train$participant_id,
                                       task_log_train$session_only), ]

task_log_train <- task_log_train[, c("participant_id", "session_only", "task_name")]

#   If multiple unexpected rows are present, remove duplicated rows. However, no
#   unexpected rows are present for training entries in "task_log".

sum(task_log_train[duplicated(task_log_train), ]) == 0

#   Check whether training entries in "task_name" correspond to "session_only"

for (i in 1:length(train_session)) {
  if(sum(task_log_train[task_log_train$session_only == train_session[i] &
                        task_log_train$task_name != i, ]) == 0) {
    print(paste0("TRUE for ", train_session[i]))
  } else {
    print(paste0("FALSE for ", train_session[i]))
  }
}

#   Check for consecutive training entries in "task_name" across "session_only"
#   values within a given participant (to check for skipped entries)

task_log_train$task_name <- as.integer(task_log_train$task_name)

task_log_train <- task_log_train %>%
  group_by(participant_id) %>%
  mutate(task_name_diff = task_name - lag(task_name))

table(task_log_train$task_name_diff, task_log_train$session_only, useNA = "always")

#   Investigate cases of nonconsecutive training entries in "task_name".

nonconsec_task_name_ids <- 
  unique(task_log_train$participant_id[!is.na(task_log_train$task_name_diff) &
                                         task_log_train$task_name_diff != 1])

#   Use "task_name" entries to compute "compl_session_train"

completion <- merge(completion, task_log_train, 
                    by = c("participant_id", "session_only"),
                    all.x = TRUE)

completion$compl_session_train[completion$session_only %in% train_session] <- 0
completion$compl_session_train[!is.na(completion$task_name)] <- 1

# Clean "compl_session_train" based on section "Investigate discrepancies in 'task_log' 
# vs. 'angular_training' in CBM-I conditions" above

completion$compl_session_train[completion$participant_id %in% c(1189, 1161) &
                                 completion$session_only == "firstSession"] <- 1

# Remove unneeded columns
completion$task_name <- NULL
completion$task_name_diff <- NULL

# remove Eligibility, preTest, and PostFollowUp

completion2 <- completion %>% filter(!(session_only %in% c("Eligibility","preTest","PostFollowUp")))

# pivot wide
completionWide <- pivot_wider(completion2,names_from = "session_only",values_from = "compl_session_train")

#total number of training sessions completed
completionWide <- completionWide %>% rowwise() %>% mutate(trainingCompleted = sum(c_across(all_of(train_session))))
trainingCompletion <- completionWide

#--------------------------------------------------------------------------------#
# Create assessment table from individual tables
#--------------------------------------------------------------------------------#
#columns of interest
columns <- c("participant_id","session_only","date_as_POSIXct","system_date_time_earliest","system_date_time_latest","n_rows","tag")

#create assessment table
assessments.table2 <- createAssessmentTable(dat.used,columns)

assessments.table2$session_only <- factor(assessments.table2$session_only,levels = session.names)


#calculate number of total assessments
#number of total assessments per session
n_session_task <- assessments.table2 %>% select(session_only,assessment_name) %>% unique() %>% group_by(session_only) %>% summarise(n = n())

n_session_task

#1 Eligibility       1
#2 preTest          12
#3 firstSession      7
#4 secondSession     3
#5 thirdSession     13
#6 fourthSession     3
#7 fifthSession     15
#8 PostFollowUp      9

#check that it matches with task log
#RR is split into two, in order to get a full count for that assessment we are considering completion for RR entry
tasks_assessments <-  assessments.table2 %>% select(session_only,assessment_name) %>% unique() %>% group_by(session_only)
tasks_tl <- dat.used$task_log %>% select(session_only,task_name) %>% filter(!(task_name %in% c(1:5,"Covid19","SESSION_COMPLETE"))) %>%  unique() %>% group_by(session_only)


n_session_task_tl <- dat.used$task_log %>% select(session_only,tag, task_name) %>% filter(!(task_name %in% c(1:5,"Covid19","SESSION_COMPLETE", "recognitionRatings"))) %>%
  unite(c("tag","task_name"), col = "assessment_name", na.rm=TRUE, sep = "") %>% unique() %>% group_by(session_only) %>% summarise(n = n())

n_session_task_tl

#1 Eligibility       1
#2 preTest          12
#3 firstSession      7
#4 secondSession     3
#5 thirdSession     13
#6 fourthSession     3
#7 fifthSession     15
#8 PostFollowUp      9

#total number of assessments (63)
n_assessments <- sum(n_session_task$n)

#number of training sessions
n_sessions <- 5

#group by participant_id and arrange by date_as_POSIXct
assessments.table2 <- assessments.table2 %>% group_by(participant_id) %>% arrange(date_as_POSIXct)

#add count
assessments.table2$count <- 1

#pivot wide
assessments.wide <-assessments.table2 %>% select(participant_id, session_only,assessment_name,count) %>%  pivot_wider(names_from = "assessment_name",values_from = "count")

assessments.wide <- assessments.wide %>% rowwise() %>% mutate(totalAssessmentsCompleted = sum(c_across(all_of(unique(assessments.table2$assessment_name)))))

assessments.completion <- assessments.wide %>% select(participant_id,session_only,totalAssessmentsCompleted) %>% group_by(participant_id,session_only) %>% summarise(assessmentsCompleted = sum(totalAssessmentsCompleted)) %>% ungroup()

#participants with two eligibility entries
#[1]  177  214  389  456  462  472  477  499  508  545  547  556
#[13]  571  618  641  897  951 1007 1009 1021 1068 1069 1122 1143
#[25] 1234 1239 1272 1276 1391 1417 1464 1716 1908 1920 1943 1948
#[37] 1961 1979 1991 1997
p_elig_2 <- assessments.completion %>% filter(session_only == "Eligibility",assessmentsCompleted>1) %>% select(participant_id) %>%  pull()

#for the purpose of the completion analysis we will count as 1 rather than 2 given that not every participant had to do a second eligibility screener
#as part of the intervention tasks
assessments.completion$assessmentsCompleted[assessments.completion$participant_id %in% p_elig_2 & assessments.completion$session_only == "Eligibility"] <- 1

assessments.completion2 <- assessments.completion %>% group_by(participant_id) %>% summarise(completed = sum(assessmentsCompleted))

#participant 1762 has two entries for ReturnIntention in the firstSession both in the task_log table and the return_intention table, when calculating completion rate subtract 1 from total number of completed assessments
assessments.completion2 <- assessments.completion2 %>% mutate(assessmentCompleted = ifelse(participant_id==1762, completed -1, completed))
total_sessions_assessments <- n_sessions + n_assessments


#merge with training completion

training_assessment_completion <- trainingCompletion %>% left_join(select(assessments.completion2, c("participant_id","assessmentCompleted")), by = "participant_id") %>% mutate(completionRate = (trainingCompleted+assessmentCompleted)/total_sessions_assessments)

#density of completion rate distribution
#bimodal distribution
plot(density(training_assessment_completion$completionRate))

# use this to double check that if last assessment for a given session was completed then total number of assessments completed for that session should be equal to the number of assessments in that session
#
# ---------------------------------------------------------------------------- #
# Compute indicator of assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Note: Given that indicator of task completion is not needed for Calm Thinking 
# main outcomes paper, it is not checked for cleaning needs here. By contrast,
# the indicators of training and assessment completion are checked and cleaned.

# Compute "compl_session_all_task" to indicate whether participant completed a 
# given session's tasks (1 = yes, 0 = no). Below, this is done based on whether
# the "task_name" for the participant's latest "date_completed_as_POSIXct" time
# stamp in "task_log" at a given session matches the last expected "task_name"
# for that session. Thus, this assumes that if the participant's final task is
# the last expected task for the session, then they did all the session's tasks.

compute_compl_session_all_task <- function(dat) {
  last_exp_task_name_by_session <- hash::hash()
  last_exp_task_name_by_session[["Eligibility"]] <- "DASS21_AS"
  last_exp_task_name_by_session[["preTest"]] <- "TechnologyUse"
  last_exp_task_name_by_session[["firstSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["secondSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["thirdSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["fourthSession"]] <- "ReturnIntention"
  last_exp_task_name_by_session[["fifthSession"]] <- "AssessingProgram"
  last_exp_task_name_by_session[["PostFollowUp"]] <- "HelpSeeking"
  
  latest_task_compl_df <- tibble()
  
  
  for (session in keys(last_exp_task_name_by_session)) {
    tmp <- group_by(filter(dat.used$task_log, session_only == session), 
                    participant_id, session_only) %>%
      summarise(latest_date_completed = max(date_completed_as_POSIXct), 
                latest_task_name = task_name[which.max(date_completed_as_POSIXct)], 
                .groups = "drop")
    
    tmp$compl_session_all_task <- NA
    tmp$compl_session_all_task[tmp$latest_task_name == 
                                 last_exp_task_name_by_session[[session]]] <- 1
    tmp$compl_session_all_task[tmp$latest_task_name != 
                                 last_exp_task_name_by_session[[session]]] <- 0
    
    latest_task_compl_df <- bind_rows(latest_task_compl_df, tmp)
  }
  
  return(latest_task_compl_df)
}

latest_task_compl_df <- compute_compl_session_all_task(dat.used)



# ---------------------------------------------------------------------------- #
# Compute indicator of assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Compute "compl_session_assess" to indicate whether participant completed a 
# given session's assessment (1 = yes, 0 = no). Below, this is done based on
# whether the last expected "task_name" for a given session's assessment is 
# present in "task_log" at that session. Thus, this assumes that if this task
# is present, then the participant did all the session's assessment tasks.

compute_compl_session_assess <- function(dat) {
  last_exp_assess_task_name_by_session <- hash::hash()
  last_exp_assess_task_name_by_session[["Eligibility"]] <- "DASS21_AS"
  last_exp_assess_task_name_by_session[["preTest"]] <- "TechnologyUse"
  last_exp_assess_task_name_by_session[["firstSession"]] <- "CoachPrompt"
  last_exp_assess_task_name_by_session[["secondSession"]] <- "OA"
  last_exp_assess_task_name_by_session[["thirdSession"]] <- "Mechanisms"
  last_exp_assess_task_name_by_session[["fourthSession"]] <- "OA"
  last_exp_assess_task_name_by_session[["fifthSession"]] <- "AssessingProgram"
  last_exp_assess_task_name_by_session[["PostFollowUp"]] <- "HelpSeeking"
  
  compl_session_assess_df <- tibble()
  
  for (session in keys(last_exp_assess_task_name_by_session)) {
    tmp <- group_by(filter(dat.used$task_log, session_only == session), 
                    participant_id) %>% 
      mutate(compl_session_assess = 
               ifelse(task_name == last_exp_assess_task_name_by_session[[session]], 1, 0))
    
    tmp2 <- tmp[c("participant_id", "session_only", "task_name", "compl_session_assess")]
    
    tmp3 <- filter(tmp2, compl_session_assess == 1)
    names(tmp3)[names(tmp3) == "task_name"] <- "last_exp_assess_task_name"
    
    compl_session_assess_df <- bind_rows(compl_session_assess_df, tmp3)
  }
  
  return(compl_session_assess_df)
}

compl_session_assess_df <- compute_compl_session_assess(dat.used)

# Remove duplicated rows

compl_session_assess_df <- compl_session_assess_df[!duplicated(compl_session_assess_df), ]



# ---------------------------------------------------------------------------- #
# Compare task and assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Note: Given that this comparison is not needed for Calm Thinking main outcomes 
# paper, it is based on the uncleaned task completion indicator and the as yet
# uncleaned assessment completion indicator (which is cleaned in section below)

# The following participants did not finish the last expected task of a given
# session but finished the last expected assessment task of that session

latest_task_compl_df2 <- merge(latest_task_compl_df, compl_session_assess_df,
                               by = c("participant_id", "session_only"), all.x = TRUE)

# ---------------------------------------------------------------------------- #
# Clean assessment completion by session ----
# ---------------------------------------------------------------------------- #

# Check for discrepancies between a participant having a "compl_session_assess"
# value of 1 at a given session and appearing in the corresponding last expected
# assessment table at that session.

check_last_exp_assess_task_name_vs_tbl <- function(dat, compl_session_assess_df) {
  last_exp_assess_tbl_by_session <- hash::hash()
  last_exp_assess_tbl_by_session[["Eligibility"]] <- "dass21_as"
  last_exp_assess_tbl_by_session[["preTest"]] <- "technology_use"
  last_exp_assess_tbl_by_session[["firstSession"]] <- "coach_prompt"
  last_exp_assess_tbl_by_session[["secondSession"]] <- "oa"
  last_exp_assess_tbl_by_session[["thirdSession"]] <- "mechanisms"
  last_exp_assess_tbl_by_session[["fourthSession"]] <- "oa"
  last_exp_assess_tbl_by_session[["fifthSession"]] <- "assessing_program"
  last_exp_assess_tbl_by_session[["PostFollowUp"]] <- "help_seeking"
  
  for (session in keys(last_exp_assess_tbl_by_session)) {
    tmp <- filter(compl_session_assess_df, 
                  compl_session_assess == 1, session_only == session)
    
    assess_tbl_name <- last_exp_assess_tbl_by_session[[session]]
    assess_tbl <- dat[[assess_tbl_name]]
    tmp2 <- filter(assess_tbl, session_only == session)
    
    # Ignore "participant_id" of NA in "dass21_as" table at "Eligibility" as
    # these reflect screening attempts of non-enrolled participants
    if (assess_tbl_name == "dass21_as") {
      tmp2 <- filter(tmp2, !is.na(participant_id))
    }
    
    ids_in_tmp_not_tmp2 <- setdiff(tmp$participant_id, tmp2$participant_id)
    ids_in_tmp2_not_tmp <- setdiff(tmp2$participant_id, tmp$participant_id)
    
    if (length(ids_in_tmp_not_tmp2) == 0 & length(ids_in_tmp2_not_tmp) == 0) {
      print(paste0("At session '", session, "', ",
                   "no discrepancy between 'compl_session_assess' and table '",
                   assess_tbl_name, "'"))
    } else if (length(ids_in_tmp_not_tmp2) != 0) {
      print(paste0("At session '", session, "', ",
                   "'participant_id' ", ids_in_tmp_not_tmp2,
                   " in 'compl_session_assess' but not in table '", assess_tbl_name, "'"))
    } else if (length(ids_in_tmp2_not_tmp) != 0) {
      print(paste0("At session '", session, "', ",
                   "'participant_id' ", ids_in_tmp2_not_tmp,
                   " in table '", assess_tbl_name, "' but not in 'compl_session_assess'"))
    }
  }
}




# At "Eligibility", "participant_id" 383 is in "dass21_as" table but not in
# "compl_session_assess". We will correct this in "compl_session_assess" below.

check_last_exp_assess_task_name_vs_tbl(dat.used, compl_session_assess_df)

# Check for consecutive assessment completion across "session_only" values within 
# a given participant (to check for skipped entries)

compl_session_assess_df$session_only <- factor(compl_session_assess_df$session_only,
                                               levels = sessions)

compl_session_assess_df <- compl_session_assess_df[order(compl_session_assess_df$participant_id,
                                                         compl_session_assess_df$session_only), ]

compl_session_assess_df$session_only_int <- as.integer(compl_session_assess_df$session_only)

compl_session_assess_df <- compl_session_assess_df %>%
  group_by(participant_id) %>%
  mutate(session_only_int_diff = session_only_int - lag(session_only_int))

table(compl_session_assess_df$session_only_int_diff,
      compl_session_assess_df$session_only, useNA = "always")

#   Investigate cases of nonconsecutive assessment completion entries

#     As found above, participant 383 lacks "compl_session_assess" at "Eligibility",
#     resulting in NA for "session_only_int_diff" at "preTest". We will correct this below.

# training completion does include the eligibility entry since it used the actual assessment table and not the task log, hence issue not present in the calculation for completion rate

non_eligibility_na_session_only_int_ids <-
  unique(compl_session_assess_df$participant_id[is.na(compl_session_assess_df$session_only_int_diff) &
                                                  compl_session_assess_df$session_only != "Eligibility"])

non_eligibility_na_session_only_int_ids == 383

# Add "compl_session_assess" to "completion"

completion <- merge(completion, compl_session_assess_df, 
                    by = c("participant_id", "session_only"), all.x = TRUE)

completion$compl_session_assess[is.na(completion$compl_session_assess)] <- 0

# Clean "compl_session_assess" based on result of running above function
# "check_last_exp_assess_task_name_vs_tbl"

completion$compl_session_assess[completion$participant_id == 383 &
                                  completion$session_only == "Eligibility"] <- 1

# Remove unneeded columns

completion$last_exp_assess_task_name <- NULL
completion$session_only_int <- NULL
completion$session_only_int_diff <- NULL

# ---------------------------------------------------------------------------- #
# Check to see that completion matches
# ---------------------------------------------------------------------------- #

check_completion <- latest_task_compl_df2 %>% select(participant_id,session_only,compl_session_all_task,compl_session_assess) %>% left_join(assessments.completion, by = c("participant_id","session_only"))

check_completion <- check_completion %>% left_join(n_session_task, by = c("session_only"))

check_completion2 <- check_completion %>% mutate(check = ifelse(compl_session_all_task ==1 & compl_session_assess==1, assessmentsCompleted==n , NA)) %>% arrange(participant_id)

#It is important to note that it could be the case that a participant had multiple entries for the same assessment that would add to the completion count, but that would not necessarily mean that they completed the given module. A more in depth check would be required to 100% determine that this is not the case. We will move forward with training assessment completion. 

final_completion <- training_assessment_completion %>% select(participant_id,trainingCompleted,assessmentCompleted,completionRate)

# ---------------------------------------------------------------------------- #
# Save table ----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps


save(final_completion,completion,check_completion2, file = here("Data_Sensitivity_Analysis","2_1_Engagement_Metrics_Completion_Rate.RData"))

