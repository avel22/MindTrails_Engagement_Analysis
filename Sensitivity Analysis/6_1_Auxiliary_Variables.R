### Adapted from script written by: Jeremy Eberle
### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### January-December 2022
### The purpose of this script is to find auxiliary variables for data imputation


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,DescTools)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Data","RData","4_1_Demographic_Characteristics_627.RData"))
load(here("Data","RData","3_Clustering_627.RData"))
load(here("Data","RData","2_1_Engagement_Metrics_Completion_Rate.RData"))

#--------------------------------------------------------------------------------#
# variables ----
#--------------------------------------------------------------------------------#
p_ids <- dat.3.demographics.cleaned.627$participant %>% select(participant_id) %>% pull()

dat.3 <- dat.3.demographics.cleaned.627

sessions <- c("preTest","firstSession","secondSession","thirdSession","fourthSession","fifthSession","PostFollowUp")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Compute indicator for missing a given session's assessment
completion<- completion %>% filter(participant_id %in% p_ids)
completion$miss_session_assess <- NA
completion$miss_session_assess[completion$compl_session_assess == 1] <- 0
completion$miss_session_assess[completion$compl_session_assess == 0] <- 1

# Add condition

completion <- merge(completion, dat.3$study[, c("participant_id", "conditioning")],
                    by = "participant_id", all.x = TRUE)

# Restrict to all sample

completion_all_697 <- completion

# Collapse "Eligibility" and "preTest" into "baseline" given that no analysis
# variable was assessed at both time points. To do so, remove "Eligibility"
# rows (because ITT participants had to complete "Eligibility" and "preTest")
# and rename "preTest" to "baseline"

completion_all_697 <- completion_all_697[completion_all_697$session_only != "Eligibility", ]
completion_all_697$session_only <- recode_factor(completion_all_697$session_only, preTest = "baseline")

compl_itt <- completion_all_697
# ---------------------------------------------------------------------------- #
# Compute proportion of missing assessments across time points ----
# ---------------------------------------------------------------------------- #

tmp_ag <- aggregate(miss_session_assess ~ participant_id,
                    compl_itt,
                    FUN = sum)

names(tmp_ag)[names(tmp_ag) == "miss_session_assess"] <- "miss_session_assess_sum"

tmp_ag$miss_session_assess_prop <- tmp_ag$miss_session_assess_sum / 7

compl_itt <- merge(compl_itt, tmp_ag, by = "participant_id", all.x = TRUE)


# ---------------------------------------------------------------------------- #
# Add potential auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Extract demographic variables

tmp_dem <- dat.3$demographics[, c("participant_id", "age","education", "employment_stat",
                                  "ethnicity", "gender", "marital_stat", "race_col",
                                  "country", "country_col","income")]

# After inspection of differences in mean proportion of missing assessments (see
# below), it was decided to collapse "gender" levels further

tmp_dem$gender_col <- as.character(tmp_dem$gender)

tmp_dem$gender_col[tmp_dem$gender_col %in%
                     c("Transgender", "Transgender Female", "Transgender Male",
                       "Other")] <- "Transgender/Other"

#prefer not to answer coded as NA
tmp_dem$gender_col[tmp_dem$gender_col %in%
                     c("Prefer not to answer")] <- NA



tmp_dem$gender_col <- factor(tmp_dem$gender_col,
                             levels = c("Female", "Male", "Transgender/Other"))

tmp_dem$gender_col_na <- tmp_dem$gender_col
tmp_dem$gender_col <- addNA(tmp_dem$gender_col)


tmp_dem$gender_col2 <- as.character(tmp_dem$gender)
tmp_dem$gender_col2[tmp_dem$gender_col2 %in%
                      c("Transgender", "Transgender Female", "Transgender Male",
                        "Other","Prefer not to answer")] <- "Transgender/Other/Prefer not to answer"

# Extract device. Note that "device" is not recorded at "Eligibility" or at "task_name" 
# of "SESSION_COMPLETE"; remove these rows.

tmp_tl <- dat.3$task_log[, c("participant_id", "session_only", "device", "task_name")]

tmp_tl <- tmp_tl[tmp_tl$session_only != "Eligibility" & 
                   tmp_tl$task_name != "SESSION_COMPLETE", ]

# Compute time-invariant "device_col" representing device types used throughout
# study (where "multiple types" is more than one type)

tmp_tl_unq <- unique(tmp_tl[, c("participant_id", "device")])

n_devices <- tmp_tl_unq %>%
  group_by(across("participant_id")) %>%
  summarise(count=n()) %>%
  as.data.frame()

names(n_devices)[names(n_devices) == "count"] <- "n_devices"

tmp_tl_unq <- merge(tmp_tl_unq, n_devices, 
                    by = c("participant_id"), all.x = TRUE)

tmp_tl_unq$device_col <- tmp_tl_unq$device
tmp_tl_unq$device_col[tmp_tl_unq$n_devices > 1] <- "multiple types"

tmp_tl_unq$device_col <-
  factor(tmp_tl_unq$device_col,
         levels = c("desktop", "tablet", "mobile", "multiple types"))

tmp_tl_unq2 <- unique(tmp_tl_unq[, c("participant_id", "n_devices", "device_col")])

# After inspection of differences in mean proportion of missing assessments (see
# below), it was decided to collapse "device_col" further into a binary variable

tmp_tl_unq2$device_col_bin <- as.character(tmp_tl_unq2$device_col)

tmp_tl_unq2$device_col_bin[tmp_tl_unq2$device_col_bin %in%
                             c("desktop", "tablet", "mobile")] <- "one type"

tmp_tl_unq2$device_col_bin <- factor(tmp_tl_unq2$device_col_bin,
                                     levels = c("one type", "multiple types"))


# Add extracted variables

compl_itt <- merge(compl_itt, tmp_dem, by = "participant_id", all.x = TRUE)
compl_itt <- merge(compl_itt, tmp_tl_unq2, by = "participant_id", all.x = TRUE)

# Sort by "participant_id" and "session_only"

sessions <- c("baseline",
              paste0(c("first", "second", "third", "fourth", "fifth"), "Session"),
              "PostFollowUp")

compl_itt$session_only <- factor(compl_itt$session_only, levels = sessions)

compl_itt <- compl_itt[order(compl_itt$participant_id, compl_itt$session_only), ]

# ---------------------------------------------------------------------------- #
# Create data frame for time-invariant auxiliary variables ----
# ---------------------------------------------------------------------------- #

time_varying_cols <- c("session_only", "compl_session_train", "compl_session_assess",
                       "miss_session_assess")

compl_itt_iv <- compl_itt[, names(compl_itt)[!(names(compl_itt) %in% time_varying_cols)]]

compl_itt_iv <- unique(compl_itt_iv)


# ---------------------------------------------------------------------------- #
# Search for time-invariant categorical and ordinal auxiliary variables ----
# ---------------------------------------------------------------------------- #

# Per consult with Cynthia Tong on 2/22/22, define function to compute mean 
# proportion of missing assessments across time points within each level of the 
# potential auxiliary variable (treat ordinal variables as categorical)

compute_desc_by_level <- function(df) {
  # Compute count, mean, and standard deviation
  
  vars <- c("gender", "gender_col", "race_col", "ethnicity", "country_col", 
            "education", "employment_stat", "marital_stat", 
            "device_col", "device_col_bin","income")
  
  var_labels <- c("Gender", "Gender (Collapsed)", "Race", "Ethnicity", "Country", 
                  "Education", "Employment Status", "Marital Status", 
                  "Device (Collapsed)", "Device (Binary)","Income")
  
  res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <-     table(df[, vars[i]])
    ag_mean <- aggregate(df$miss_session_assess_prop, list(df[, vars[i]]), 
                         FUN = mean, drop = FALSE)
    ag_sd <-   aggregate(df$miss_session_assess_prop, list(df[, vars[i]]), 
                         FUN = sd, drop = FALSE)
    
    var_res <- rbind(data.frame(label = var_labels[i],
                                n     = NA,
                                M     = NA,
                                SD    = NA),
                     data.frame(label = names(tbl),
                                n     = as.numeric(tbl),
                                M     = round(ag_mean$x, 2),
                                SD    = round(ag_sd$x, 2)))
    
    res <- rbind(res, var_res)
  }
  
  return(res)
}

# Run function for each analysis sample, compute size of each sample, combine 
# results into table, and export table

fct_res_itt_unrestricted <- compute_desc_by_level(compl_itt_iv)

nrow(compl_itt_iv) == 627

write.csv(fct_res_itt_unrestricted,
          file = here("Tables","6_1_potential_cat_ord_aux_vars.csv"),
          row.names = FALSE)

# Inspect tables using Cynthia Tong's guidance on 4/1/22: For very small differences 
# in mean proportion of missing assessments or proportion of missing income between 
# levels of a potential auxiliary variable, exclude the variable from missing data 
# handling. For clear differences (i.e., difference > .2 between two levels that each 
# have > 200 participants), include the variable. If unsure (i.e., a difference > .1 
# between two levels that each have > 200 participants), test differences and include 
# variable if differences are significant; otherwise, exclude it.

# On 4/1/22, Cynthia Tong, Katie Daniel, and Jeremy Eberle deemed variables except
# gender and device to have very small differences for "miss_session_assess_prop". 
# Device was deemed to have clear differences; however, Cynthia advised that its 
# levels be collapsed into a binary variable ("device_col_bin" above) to promote 
# model convergence. We were unsure re gender and decided to collapse sparse levels 
# (above) and test differences (below).

# Define function to test differences. For differences in mean proportion of missing
# assessments, use one-way ANOVA as long as sample sizes are sufficiently large (i.e., 
# 2-9 groups with >= 15 participants per group) see 
# https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/anova/how-to/kruskal-wallis-test/before-you-start/data-considerations/).
# For differences in proportion of missing values for income, use logistic regression.


test_diffs <- function(df, model, outcome, aux_var) {
  fml <- as.formula(paste0(outcome, " ~ ", aux_var))
  
  n_aux_var_levels <- length(levels(df[, aux_var]))
  
  if (model == "lm") {
    lm_mod <- lm(fml, df)
    print(summary(lm_mod))
    print(anova(lm_mod))
  } else if (model == "glm") {
    glm_mod <- glm(fml, df, family = "binomial")
    print(summary(glm_mod))
    print(wald.test(Sigma = vcov(glm_mod), b = coef(glm_mod), Terms = 2:n_aux_var_levels))
  }
}

sink_test_diffs <- function(itt_unrestricted_df,
                            model, outcome, aux_var) {
  sink(file = paste0(here("Tables"),"/","6_1_",outcome, "_", aux_var, "_diffs.txt"))
  
  cat("Analyzed Sample:", "\n", "\n")
  test_diffs(itt_unrestricted_df, model, outcome, aux_var)
  cat("\n")
  cat("--------------------", "\n", "\n")
  sink()
}

# Given significant differences, include "gender_col" in missing data handling for
# longitudinal outcomes

sink_test_diffs(compl_itt_iv,
                "lm", "miss_session_assess_prop", "gender_col_na")
# ---------------------------------------------------------------------------- #
# Save Auxiliary variables  ----
# ---------------------------------------------------------------------------- #
#age,gender,device

aux_vars <- compl_itt_iv %>% select(participant_id,age,gender,gender_col,gender_col2,device_col,device_col_bin)

save(aux_vars, file = here("Data","RData","6_1_aux_vars.RData"))
