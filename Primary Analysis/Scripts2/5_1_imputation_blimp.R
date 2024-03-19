### Adapting Jeremy Eberle's script
### https://github.com/jwe4ec/jp5ws/blob/main/Syntax/15_multiple_imputation_linear_spline.R
### Eberle, J. W., Boukhechba, M., Sun, J., Zhang, D., Funk, D., Barnes, L., & Teachman, B. (2022, January 13). Shifting Episodic Prediction With Online Cognitive Bias Modification: A Randomized Controlled Trial. Retrieved from osf.io/jp5ws

### Modified by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is impute missing data. We will test out two approached one using the jomo package in R and a second using the Blimp software. 

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,here,mitml,ggplot2)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Scripts2","Data","outcomes_df_for_imputation.RData"))
load(here("Scripts2","Data","aux_vars.RData"))

outcomes.scores.df %>% select(participant_id) %>% unique() %>% nrow()

#join aux variables
outcomes.scores.df2 <- outcomes.scores.df %>% left_join(aux_vars, by = c("participant_id"))


#check 

averages <- outcomes.scores.df2 %>%
  group_by(session_only, engagement_cluster) %>%
  summarise(
    mean_OA = mean(OA_MeanScore, na.rm = TRUE),
    mean_BBSIQ = mean(BBSIQ_MeanScore, na.rm = TRUE),
    mean_RR_POS_BIAS = mean(RR_POS_BIAS_MeanScore, na.rm = TRUE),
    mean_RR_NEG_BIAS = mean(RR_NEG_BIAS_MeanScore, na.rm = TRUE),
    mean_DASS21 = mean(DASS21_MeanScore, na.rm = TRUE)
  )

#--------------------------------------------------------------------------------#
# MLM LME4 Syntax ----
#--------------------------------------------------------------------------------#

#http://eshinjolly.com/pymer4/rfx_cheatsheet.html

# #Random intercepts only
# (1 | Group)
# 
# #Random slopes only
# (0 + Variable | Group)
# 
# #Random intercepts and slopes (and their correlation)
# (Variable | Group)
# 
# #Random intercepts and slopes (without their correlation)
# (1 | Group) + (0 + Variable | Group)
# 
# #Same as above but will not separate factors (see: https://rdrr.io/cran/lme4/man/expandDoubleVerts.html)
# (Variable || Group)
# 
# #Random intercept and slope for more than one variable (and their correlations)
# (Variable_1 + Variable_2 | Group)

# Correlated random intercept and slope:
#m3 <- lmer(Reaction ~ Days + (1 + Days|Subject), data = sleepstudy)

#--------------------------------------------------------------------------------#
# Fully Bayesian model-based imputation with BLIMP setup ---- 
#--------------------------------------------------------------------------------#
#### Modification 0, credibility missing values, gender missing values 3 categories, no age
blimp_outcomes_mod_0 <-outcomes.scores.df2
blimp_outcomes_mod_0 <- blimp_outcomes_mod_0 %>% dplyr::select(-cred_online_imp,-age,-gender,-device_col,-gender_col2)



#Change gender to numeric
blimp_outcomes_mod_0$gender_col<-factor(blimp_outcomes_mod_0$gender_col, levels = c("Female","Male","Transgender/Other"))
blimp_outcomes_mod_0$gender_col_num<- as.numeric(blimp_outcomes_mod_0$gender_col)

#2 missing values
table(blimp_outcomes_mod_0$gender_col_num,useNA = "ifany")/7
#4 missing values for credibility online
table(blimp_outcomes_mod_0$cred_online_na,useNA = "ifany")/7

#Change device to numeric
#One Type = 1, Multiple Types = 0
blimp_outcomes_mod_0 <- blimp_outcomes_mod_0 %>% mutate(isOneType = ifelse(device_col_bin == "one type",1,0))

#select variables of interest
colnames(blimp_outcomes_mod_0)
blimp_outcomes_mod_0 <- blimp_outcomes_mod_0 %>% select(-gender_col,-device_col_bin,-session_only)
colnames(blimp_outcomes_mod_0)

#replace NA values to 999 for Blimp
blimp_outcomes_mod_0[is.na(blimp_outcomes_mod_0)]<-999

#analyze structure and print summary
str(blimp_outcomes_mod_0)
summary(blimp_outcomes_mod_0)
colnames(blimp_outcomes_mod_0)

#--------------------------------------------------------------------------------#
# Fully Bayesian model-based imputation outcome structure for Blimp ---- 
#--------------------------------------------------------------------------------#
#### Create outcome structures for blimp
#tell which modification
blimp_modification <- blimp_outcomes_mod_0
#create df for each outcome
session_outcomes$time <- 0:6
outcomes_loc <- grep("*_MeanScore",colnames(blimp_modification))
outcomes_names <- colnames(blimp_modification[outcomes_loc])

blimp_outcome_structure <- function(blimp_outcomes,outcomes_names, names_position, session_outcomes){
  blimp_outcome <- blimp_outcomes %>% select(-outcomes_names[names_position]) %>% filter(time %in% session_outcomes)
  #blimp_outcome$timev1 <- rep(0:(length(session_outcomes)-1),times = length(unique(blimp_outcome$participant_id)))
  #blimp_outcome <- blimp_outcome %>% mutate(timev1 = ifelse(timev1 == max(timev1),max(timev1)-1,timev1))
  #blimp_outcome$timev2 <- blimp_outcome$time2
  
  return(blimp_outcome)
}

#OA
blimp_oa <- blimp_outcome_structure(blimp_modification,outcomes_names,-1,session_outcomes$time[session_outcomes$OA==1])
view(blimp_oa)
#BBSIQ
blimp_bbsiq <- blimp_outcome_structure(blimp_modification,outcomes_names,-2,session_outcomes$time[session_outcomes$BBSIQ==1])
view(blimp_bbsiq)
#RR POS BIAS
blimp_rr_pos_bias <-  blimp_outcome_structure(blimp_modification,outcomes_names,-3,session_outcomes$time[session_outcomes$RR_POS_BIAS==1])
view(blimp_rr_pos_bias)
#RR NEG BIAS
blimp_rr_neg_bias <-  blimp_outcome_structure(blimp_modification,outcomes_names,-4,session_outcomes$time[session_outcomes$RR_NEG_BIAS==1])
view(blimp_rr_neg_bias)
#DASS21
blimp_dass21 <- blimp_outcome_structure(blimp_modification,outcomes_names,-5,session_outcomes$time[session_outcomes$DASS21==1])
view(blimp_dass21)
#save table as csv file to be used in Blimp

mod <- 0

#OASIS
write.table(blimp_oa,here("Scripts2","Data","imputation",paste0("MOD",as.character(mod)),paste0("blimp_imputation_oa_mod",as.character(mod),".csv")),row.names = F,col.names = F)

#DASS21
write.table(blimp_dass21,here("Scripts2","Data","imputation",paste0("MOD",as.character(mod)),paste0("blimp_imputation_dass21_mod",as.character(mod),".csv")),row.names = F,col.names = F)

#BBSIQ
write.table(blimp_bbsiq,here("Scripts2","Data","imputation",paste0("MOD",as.character(mod)),paste0("blimp_imputation_bbsiq_mod",as.character(mod),".csv")),row.names = F,col.names = F)

#RR POSITIVE BIAS
write.table(blimp_rr_pos_bias,here("Scripts2","Data","imputation",paste0("MOD",as.character(mod)),paste0("blimp_imputation_rr_pos_mod",as.character(mod),".csv")),row.names = F,col.names = F)

#RR NEGATIVE BIAS
write.table(blimp_rr_neg_bias,here("Scripts2","Data","imputation",paste0("MOD",as.character(mod)),paste0("blimp_imputation_rr_neg_mod",as.character(mod),".csv")),row.names = F,col.names = F)

