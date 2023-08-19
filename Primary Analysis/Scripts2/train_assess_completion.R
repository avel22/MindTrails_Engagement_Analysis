### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### Compute session assessment


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,DescTools)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Scripts2","Data2","dat.3.Rdata"))

load(here("Scripts2","Data2","participant_cluster.RData"))

#(here("Scripts2","Data2","E_M2_1_Winsor.RData"))
load(here("Scripts2","Data2","Test699","E_M2_1_Winsor.RData"))

load(here("Scripts2","Data2","completionRate.RData"))

#load data from scrip 1_importing_data.R
load(here("Scripts2","Data2","2_Calm.RData"))


#--------------------------------------------------------------------------------#
# training and assessment ----
#--------------------------------------------------------------------------------#

study <- dat.3$study

task <- dat.3$task_log
completion2<- completion %>% left_join(select(study, participant_id, conditioning), by = c("participant_id"))
train_assess <- completion %>% group_by(session_only) %>% summarise(train = sum(compl_session_train) , assess =sum(compl_session_assess))

train_assess_study <- completion2 %>% group_by(session_only,conditioning) %>% summarise(train = sum(compl_session_train) , assess =sum(compl_session_assess))

completion3 <- completion %>% filter(!(participant_id %in% c(776,169)))

train_assess_sample <- completion3 %>% group_by(session_only) %>% summarise(train = sum(compl_session_train) , assess =sum(compl_session_assess))



