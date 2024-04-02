### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is input imputed data and save as mitml object


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer,lme4,psycho,afex,nlme,optimx,emmeans,gtsummary)

#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Data_Sensitivity_Analysis","5_outcomes_df_for_imputation.RData"))
session_outcomes$time <- 0:6

#imputed outcomes
mod0_columns <- c("imp_num","participant_id","time","t1","t2","outcome","cred_on","eng_cluster","gender_num","isOneType")



#OA
oa_imputed <- read.csv(here("Data_Sensitivity_Analysis","imputation","Blimp","OASIS","Actual_Imputation","OA_imps_mod0.csv"),header = F)
colnames(oa_imputed) <- mod0_columns
oa_imputed <- oa_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
oa_imputed$session_only <- oa_imputed$sessions

#DASS21
dass21_imputed <- read.csv(here("Data_Sensitivity_Analysis","imputation","Blimp","DASS21","Actual_Imputation","DASS21_imps_mod0.csv"),header = F)
colnames(dass21_imputed) <- mod0_columns
dass21_imputed <- dass21_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
dass21_imputed$session_only <- dass21_imputed$sessions

#RR_NEG
rr_neg_imputed <- read.csv(here("Data_Sensitivity_Analysis","imputation","Blimp","RR_NEG_BIAS","Actual_Imputation","RR_NEG_BIAS_imps_mod0.csv"),header = F)
colnames(rr_neg_imputed) <- mod0_columns
rr_neg_imputed <- rr_neg_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_neg_imputed$session_only <- rr_neg_imputed$sessions

#RR_POS
rr_pos_imputed <-  read.csv(here("Data_Sensitivity_Analysis","imputation","Blimp","RR_POS_BIAS","Actual_Imputation","RR_POS_BIAS_imps_mod0.csv"),header = F)
colnames(rr_pos_imputed) <- mod0_columns
rr_pos_imputed <- rr_pos_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_pos_imputed$session_only <- rr_pos_imputed$sessions

#BBSIQ
bbsiq_imputed <- read.csv(here("Data_Sensitivity_Analysis","imputation","Blimp","BBSIQ","Actual_Imputation","BBSIQ_imps_mod0.csv"),header = F)
colnames(bbsiq_imputed) <- mod0_columns
bbsiq_imputed <- bbsiq_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
bbsiq_imputed$session_only <- bbsiq_imputed$sessions



#convert to mitml list
mitml_list_oa <- as.mitml.list(split(oa_imputed,oa_imputed$imp_num))
mitml_list_dass21 <- as.mitml.list(split(dass21_imputed,dass21_imputed$imp_num))
mitml_list_rr_neg <- as.mitml.list(split(rr_neg_imputed,rr_neg_imputed$imp_num))
mitml_list_rr_pos <- as.mitml.list(split(rr_pos_imputed,rr_pos_imputed$imp_num))
mitml_list_bbsiq <- as.mitml.list(split(bbsiq_imputed,bbsiq_imputed$imp_num))


#save mitml lists
save.image(here("Data_Sensitivity_Analysis","6_3_imputed_outcomes_mitml_lists.RData"))

