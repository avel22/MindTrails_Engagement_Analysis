### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to combined all engagement metrics into one dataframe

#--------------------------------------------------------------------------------#
# loading the libraries
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools,reshape2,lubridate,hash,caret)

#--------------------------------------------------------------------------------#
# loading the engagement metrics
#--------------------------------------------------------------------------------#
load(here("Scripts2","Data_Primary_Analysis","2_Calm_2.RData"))
load(here("Scripts2","Data_Primary_Analysis","completionRate.RData"))
load(here("Scripts2","Data_Primary_Analysis","E_T_1_Winsor_Complete.RData"))


#--------------------------------------------------------------------------------#
# participant filtering -----
#--------------------------------------------------------------------------------#

#filter for participants who started the first scenario of angular training
#629 participants
p_ids_scenario <- dat.3$participant %>% filter(comp1scenario== 1) %>% select(participant_id) %>% pull()

#--------------------------------------------------------------------------------#
# combined data into one dataframe
#--------------------------------------------------------------------------------#
#select columns of interest, exclude participants 776 and 169
#engagement_metrics_completion <- final_completion %>% select(participant_id,completionRate) %>% filter(participant_id %in% p_ids_scenario,!(participant_id %in% c(776,169)))
engagement_metrics_completion <- final_completion %>% select(participant_id,completionRate) %>% filter(!(participant_id %in% c(776,169)))


engagement_metrics_time <- E_T_FINAL_NA_1 %>% filter(!(participant_id %in% c(776,169)))

engagement_metrics <- engagement_metrics_completion %>% left_join(engagement_metrics_time, by = c("participant_id"))


E_M_1 <- engagement_metrics

# E_IMP <- engagement_metrics %>% select(participant_id, completionRate,avgLogInsRounded,T1_days_time_between_sessions_winsor_cluster,T2_sec_mean_scenario_rt_across_sessions_3_imp_median,grep("T4_imp_",names(engagement_metrics)),finalOutlierScore)
# ---------------------------------------------------------------------------- #
# Save data as RData ----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps
save(E_M_1, file = here("Scripts2","Data_Primary_Analysis","E_M_1.RData"))
