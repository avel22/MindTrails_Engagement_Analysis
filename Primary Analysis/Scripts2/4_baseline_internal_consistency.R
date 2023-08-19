### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to structure and analyze psychosocial outcomes of interest
## DASS-21-21,OASIS,BBSIQ,RR


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#

load(here("Scripts2","Data2","Test699","outcomes_df_for_imputation.RData"))

#--------------------------------------------------------------------------------#
# outcomes at baseline ----
#--------------------------------------------------------------------------------#

outcomes.scores.baseline <- outcomes.scores.df %>% 
  select(participant_id,
         session_only,
         colnames(outcomes.scores.df[grep("_MeanScore", colnames(outcomes.scores.df))])) %>% 
  filter(session_only == "preTest")

#--------------------------------------------------------------------------------#
# Compute Cronbach's alpha ---- ----
#--------------------------------------------------------------------------------#

#OASIS
oa_baseline <- oa %>% filter(session_only =="preTest") %>% select(score_cols)

#DASS21AS
dass21_baseline <- oa %>% filter(session_only =="Eligbility") %>% select(score_cols)

#RR NEG BIAS

#RR POS BIAS

#BBSIQ

# Positive expectancybias

## Mean of "shortRest", "settleIn", "consideredAdvancement", "financiallySecure"

psych::alpha(x.baseline.ITT[, c("shortRest", 
                                "settleIn", 
                                "consideredAdvancement", 
                                "financiallySecure")])$total$std.alpha


psych::alpha(x.baseline.Completers[, c("shortRest", 
                                       "settleIn", 
                                       "consideredAdvancement", 
                                       "financiallySecure")])$total$std.alpha


