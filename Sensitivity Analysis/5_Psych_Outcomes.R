### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### January-December 2022
### The purpose of this script is to structure and analyze psychosocial outcomes of interest
## DASS-21-21,OASIS,BBSIQ,RR


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,stargazer,mice)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#

load(here("Data","RData","4_1_Demographic_Characteristics_627.RData"))
load(here("Data","RData","3_Clustering_627.RData"))
# load(here("Scripts2","Data2","2_Calm.RData"))
# 
# load(here("Scripts2","Data2","participant_cluster.RData"))
# 
# #(here("Scripts2","Data2","E_M2_1_Winsor.RData"))
# load(here("Scripts2","Data2","Test699","E_M2_1_Winsor.RData"))
# 
# load(here("Scripts2","Data2","completionRate.RData"))

#--------------------------------------------------------------------------------#
# functions ----
#--------------------------------------------------------------------------------#
missingData <- function(outcome_df,score_cols,missing_df,outcomeName){
  #show level of data missingness in outcome dataframe
  missing.data <- mice::md.pattern(outcome_df[score_cols],rotate.names = T)
  #retrieve number of files with missing data across columns of interest
  num.entries.missing <- missing.data[nrow(missing.data),ncol(missing.data)]
  #calculate total number of entries
  num.entries <- nrow(outcome_df)*ncol(outcome_df[score_cols])
  #calculate percentage missing
  missing.percentage <- round((num.entries.missing/num.entries)*100,digits = 3)
  #add information in tibble
  missing.summary <- tibble(outcome = outcomeName, missingPercent = missing.percentage)
  #add tibble to dataframe with missing outcome data summary
  missing <- bind_rows(missing_df,missing.summary)
  return(missing)
}


outcomeScore <- function(outcome_df,score_cols,scoreType,missing,outcomeName) {
  #select columns of interest for the outcome score calculation
  outcome <- outcome_df %>% dplyr::select(participant_id,session_only,score_cols)
  
  #sessions were the outcome was assessed present in the outcome table
  sessions <- outcome$session_only %>% unique()
  
  #replace 555 (Prefer not to answer) values with NA
  outcome[score_cols] <- outcome[score_cols] %>% replace(.== 555, NA)
  
  #count number of NAs per entry accross columns
  outcome$countNA <- apply(is.na(outcome[score_cols]), 1, sum)
  
  missing.summary <- missingData(outcome_df = outcome,score_cols = score_cols,missing_df = missing,outcomeName = outcomeName)
  
  # if(scoreType == "sumScore"){
  #   #if all rows are NAs then keep as NA, else sum scores using available data
  #   outcome <-outcome %>% mutate(measureSumScore = ifelse(countNA == length(score_cols), NA, rowSums(select(., all_of(score_cols)), na.rm = T)))
  #   
  # }else{}
  outcome <-outcome %>% mutate(measureMeanScore = ifelse(countNA == length(score_cols), NA, round(rowMeans(dplyr::select(., all_of(score_cols)),na.rm = T),digits = 3)))
  
  #select columns of interest for final dataframe
  outcome.short <- outcome %>% dplyr::select(participant_id,session_only,measureMeanScore)
  
  #create dataframe in long format for MLM
  outcome.long <- tibble(participant_id = rep(p_ids, each = length(sessions)), session_only = rep(sessions,times = length(p_ids)), time = rep(0:(length(sessions)-1),times = length(p_ids)))
  
  #join outcome score data
  outcome.long <- outcome.long %>% left_join(outcome.short,by = c("participant_id","session_only"))
  
  
  df.list <- list(missing.summary,outcome.long)
  
  return(df.list)
}

session_columns <- function(outcome_df, session_column = "session_only"){
  session_col <-  rlang::sym(session_column)
  cols <- outcome_df %>% select(!!session_column) %>% unique() %>% pull()
  col_names <- as.character(cols)
  return(col_names)
}

#--------------------------------------------------------------------------------#
# variables ----
#--------------------------------------------------------------------------------#
p_ids <- dat.3.demographics.cleaned.627$participant %>% dplyr::select(participant_id) %>% pull()

dat.3 <- dat.3.demographics.cleaned.627

sessions <- c("preTest","firstSession","secondSession","thirdSession","fourthSession","fifthSession","PostFollowUp")

missing_data_df <- tibble()

outcomes.scores.list <- list()

#dataframe to store all variables to be used for imputation
outcomes.scores.df <- tibble(participant_id = rep(p_ids, each = length(sessions)), session_only = rep(sessions, times = length(p_ids)), time = rep(0:6, times = length(p_ids)))

#create time1 and time2 components
outcomes.scores.df <- outcomes.scores.df %>% mutate(time1 = ifelse(time == 6, 5,time), time2 = ifelse(time !=6, 0,1))

#tibble to store on which sessions do outcomes appear
session_outcomes <- tibble(sessions = sessions)

#factor sesssion_only to set order
outcomes.scores.df$session_only<- factor(outcomes.scores.df$session_only,levels = sessions)

#--------------------------------------------------------------------------------#
# oa ----
#--------------------------------------------------------------------------------#
#From Jeremy:
#For OASIS, take the mean of available items: "axf", "axs", "avo", "wrk", "soc".
score_cols <- c("axf", "axs", "avo", "wrk", "soc")

#oa table
oa <- dat.3$oa

#call function to get missing data and score
oa.info <- outcomeScore(outcome_df = oa,score_cols = score_cols,scoreType = "meanScore",missing = missing_data_df,outcomeName = "oa")

#missing data
missing_data_df <- oa.info[[1]]
#score
score.oa <- oa.info[[2]]
#rename score column
colnames(score.oa)[grep("measureMeanScore",names(score.oa))] <- "OA_MeanScore"
#add to outcomes.scores.df
outcomes.scores.df <- outcomes.scores.df %>% left_join(select(score.oa,participant_id,session_only,OA_MeanScore), by = c("participant_id","session_only"))

#see in which sessions is outcome assessed
oa.sessions <- session_columns(oa)
session_outcomes <- session_outcomes %>% mutate(OA = ifelse(sessions %in% oa.sessions, 1 , 0))

#--------------------------------------------------------------------------------#
# bbsiq ----
#--------------------------------------------------------------------------------#
#From Jeremy:
# My interpretation of this is that you took the mean of all the items below to form a single negative interpretation bias score, but you might check the code for the R34 main outcomes paper.
# Items for negative interpretation bias for internal events = "breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart", "heart_wrong", "confused_outofmind", "dizzy_ill"
# Items for negative interpretation bias for external events = "visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent", "jolt_burglar", "party_boring", "urgent_died"
#bbsiq table
bbsiq <- dat.3$bbsiq
#negative interpretation bias internal columns
neg_int_bias_int_events_cols <-c("breath_suffocate", "vision_illness", "lightheaded_faint", "chest_heart", "heart_wrong", "confused_outofmind", "dizzy_ill")
#negative interpretation bias external columns
neg_int_bias_ext_events_cols <-c("visitors_bored", "shop_irritating", "smoke_house", "friend_incompetent", "jolt_burglar", "party_boring", "urgent_died")
#combine both types of negative interpretation bias
single_neg_int_bias_cols <- c(neg_int_bias_int_events_cols,neg_int_bias_ext_events_cols)
#call function
bbsiq.info <- outcomeScore(outcome_df = bbsiq,score_cols = single_neg_int_bias_cols,scoreType = "meanScore",missing = missing_data_df,outcomeName = "bbsiq")
#missing data
missing_data_df <- bbsiq.info[[1]]
#bbsiq score
score.bbsiq <-bbsiq.info[[2]]
#rename score column
colnames(score.bbsiq)[grep("measureMeanScore",names(score.bbsiq))] <- "BBSIQ_MeanScore"
#add to outcomes.scores.df
outcomes.scores.df <- outcomes.scores.df %>% left_join(select(score.bbsiq,participant_id,session_only,BBSIQ_MeanScore), by = c("participant_id","session_only"))

#see in which session is outcome assessed
bbsiq.sessions <- session_columns(bbsiq)
session_outcomes <- session_outcomes %>% mutate(BBSIQ = ifelse(sessions %in% bbsiq.sessions, 1 , 0))

#--------------------------------------------------------------------------------#
# rr ----
#--------------------------------------------------------------------------------#
#From Jeremy:
# For the R34 main outcomes paper, Ji et al. (2021) says "Endorsements of threat-relevant negative disambiguated interpretations [i.e., (b)] were averaged across items to form the negative interpretation bias, and endorsements of threat-relevant positive/benign disambiguated interpretations [i.e., (a)] were averaged across items to form the positive interpretation bias score."

#rr table
rr <- dat.3$rr

#positive interpretation bias potential threat
#Items for positive interpretation bias of potential threat = "blood_test_ps", "elevator_ps", "job_ps", "lunch_ps", "meeting_friend_ps", "noise_ps", "scrape_ps", "shopping_ps", "wedding_ps"
rr_pos_int_bias_potentialThreat_cols <- colnames(rr[grep("_ps",names(rr))])

#negative interpretation bias potential threat
#Items for negative interpretation bias for potential threat = "blood_test_ns", "elevator_ns", "job_ns", "lunch_ns", "meeting_friend_ns", "noise_ns", "scrape_ns", "shopping_ns", "wedding_ns"
rr_neg_int_bias_potentialThreat_cols <- colnames(rr[grep("_ns",names(rr))])

#positive interpretation bias unrelated threat
#Items for positive interpretation bias unrelated to threat = "blood_test_pf", "elevator_pf", "job_pf", "lunch_pf", "meeting_friend_pf", "noise_pf", "scrape_pf", "shopping_pf", "wedding_pf"
rr_pos_int_bias_unrelatedThreat_cols <- colnames(rr[grep("_pf",names(rr))])

#negative interpretation bias unrelated threat
#Items for negative interpretation bias unrelated to threat = "blood_test_nf", "elevator_nf", "job_nf", "lunch_nf", "meeting_friend_nf", "noise_nf", "scrape_nf", "shopping_nf", "wedding_nf"
rr_neg_int_bias_unrelatedThreat_cols <- colnames(rr[grep("_nf",names(rr))])


#positive interpretation bias potential threat
rr.pos.int.bias.potentialThreat.info <- outcomeScore(outcome_df = rr,score_cols = rr_pos_int_bias_potentialThreat_cols,scoreType = "meanScore",missing = missing_data_df,outcomeName = "rr.pos.int.bias.potentialThreat")

missing_data_df <- rr.pos.int.bias.potentialThreat.info[[1]]

score.pos.int.bias.potentialThreat <- rr.pos.int.bias.potentialThreat.info[[2]]

colnames(score.pos.int.bias.potentialThreat)[grep("measureMeanScore",names(score.pos.int.bias.potentialThreat))] <- "RR_POS_BIAS_MeanScore"

outcomes.scores.df <- outcomes.scores.df %>% left_join(select(score.pos.int.bias.potentialThreat,participant_id,session_only,RR_POS_BIAS_MeanScore), by = c("participant_id","session_only"))

#negative interpretation bias potential threat
rr.neg.int.bias.potentialThreat.info <-outcomeScore(outcome_df = rr,score_cols = rr_neg_int_bias_potentialThreat_cols,scoreType = "meanScore",missing = missing_data_df,outcomeName = "rr.neg.int.bias.potentialThreat")

missing_data_df <- rr.neg.int.bias.potentialThreat.info[[1]]

score.neg.int.bias.potentialThreat<- rr.neg.int.bias.potentialThreat.info[[2]]

colnames(score.neg.int.bias.potentialThreat)[grep("measureMeanScore",names(score.neg.int.bias.potentialThreat))] <- "RR_NEG_BIAS_MeanScore"

outcomes.scores.df <- outcomes.scores.df %>% left_join(select(score.neg.int.bias.potentialThreat,participant_id,session_only,RR_NEG_BIAS_MeanScore), by = c("participant_id","session_only"))

#see in which session is outcome assessed
rr.sessions <- session_columns(rr)
session_outcomes <- session_outcomes %>% mutate(RR_POS_BIAS = ifelse(sessions %in% rr.sessions, 1 , 0))
session_outcomes <- session_outcomes %>% mutate(RR_NEG_BIAS = ifelse(sessions %in% rr.sessions, 1 , 0))

#--------------------------------------------------------------------------------#
# dass21_as ----
#--------------------------------------------------------------------------------#
#From Jeremy:
#take the mean of available items: "bre", "dry", "hea", "pan", "sca", "tre", "wor"
#analyze the mean score
#participants who have more than two entries for dass21_as in eligibility have a mean score of their number of entries represented in dass21_as_total_anal
dass21_as <- dat.3$dass21_as %>% filter(!is.na(participant_id)) %>% group_by(participant_id,session_only) %>% slice_tail() %>% ungroup()

#use dass21_as_total_anal/7 to get mean score per question
dass21_as2 <- dass21_as %>% select(participant_id, session_only,dass21_as_total_anal) %>% mutate(DASS21_MeanScore = round(dass21_as_total_anal/7,digits =3))

#select columns of interest for final dataframe
dass21_as.short <- dass21_as2 %>% select(participant_id,session_only,DASS21_MeanScore)

#to facilitate the joining process, change Eligibility to PreTest
dass21_as.short <- dass21_as.short %>% mutate(session_only2 = ifelse(session_only == "Eligibility", "preTest", as.character(session_only)))


outcomes.scores.df <- outcomes.scores.df %>% left_join(select(dass21_as.short,participant_id,session_only2,DASS21_MeanScore), by = c("participant_id","session_only"= "session_only2"))


#see in which session is outcome assessed
dass21.sessions <- session_columns(dass21_as.short,session_column = "session_only2")
session_outcomes <- session_outcomes %>% mutate(DASS21 = ifelse(sessions %in% dass21.sessions, 1 , 0))

#missing data item level including all eligibility attempts
#dass21_as
dass21_as3 <- dass21_as
dass21_as_col <- c("bre","dry","hea","pan","sca","tre","wor")
#replace 555 (Prefer not to answer) values with NA
dass21_as3[dass21_as_col] <- dass21_as3[dass21_as_col] %>% replace(.== 555, NA)

missing_data_df_2 <- missingData(outcome_df = dass21_as3,score_cols = dass21_as_col,missing_df = missing_data_df,outcomeName = "DASS21")

#--------------------------------------------------------------------------------#
# Compute baseline internal consistency using Cronbach's alpha ---- ----
#--------------------------------------------------------------------------------#
#OASIS
cronbach_measures <- tibble()

cronbach_calc <- function(baseline_df,cronbach_measures,outcome){
  #cronbach values
  cronbach <- psych::alpha(baseline_df)
  #raw alpha
  raw_alpha <- cronbach[["total"]][["raw_alpha"]]
  #std alpha
  std_alpha <- cronbach[["total"]][["std.alpha"]]
  #lower CI
  low_ci_95 <- cronbach[["feldt"]][["lower.ci"]][["raw_alpha"]]
  #higher CI
  up_ci_95 <- cronbach[["feldt"]][["upper.ci"]][["raw_alpha"]]
  
  
  cronbach_df = tibble(outcome = outcome, raw_alpha = raw_alpha,std_alpha = std_alpha,low_ci_95=low_ci_95,up_ci_95=up_ci_95 )
  
  cronbach_measures = rbind(cronbach_df,cronbach_measures)
}

#OASIS
baseline_oa <- oa %>% filter(session_only =="preTest") %>% select(all_of(score_cols)) %>% replace(.== 555, NA)
#DASS21AS
baseline_dass21 <- dass21_as %>% filter(session_only =="Eligibility") %>% select(all_of(dass21_as_col)) %>% replace(.== 555, NA)
#BBSIQ
baseline_bbsiq <- bbsiq %>%  filter(session_only =="preTest") %>% select(all_of(single_neg_int_bias_cols)) %>% replace(.== 555, NA)
#RR NEG BIAS
baseline_rr_neg_bias <- rr %>%  filter(session_only =="preTest") %>% select(all_of(rr_neg_int_bias_potentialThreat_cols)) %>% replace(.== 555, NA)
#RR POS BIAS
baseline_rr_pos_bias <-rr %>%  filter(session_only =="preTest") %>% select(all_of(rr_pos_int_bias_potentialThreat_cols)) %>% replace(.== 555, NA)

cronbach_measures <- cronbach_calc(baseline_oa,cronbach_measures,"OASIS")
cronbach_measures <- cronbach_calc(baseline_dass21,cronbach_measures,"DASS21")
cronbach_measures <- cronbach_calc(baseline_bbsiq,cronbach_measures,"BBSIQ")
cronbach_measures <- cronbach_calc(baseline_rr_neg_bias,cronbach_measures,"RR_NEG_BIAS")
cronbach_measures <- cronbach_calc(baseline_rr_pos_bias,cronbach_measures,"RR_POS_BIAS")

cronbach_measures_rounded <- cronbach_measures %>% 
  mutate_if(is.numeric, round,digits = 3)

sink(here("Tables","5_cronbach.txt"))
cronbach_measures
sink()

sink(here("Tables","5_cronbach_rounded.txt"))
cronbach_measures_rounded
sink()

gdata::write.fwf(cronbach_measures_rounded,file=here("Tables","5_cronbach_rounded_table.txt"),sep="\t", quote=F, rownames=T)

#--------------------------------------------------------------------------------#
# visualize outcomes using density plot ----
#--------------------------------------------------------------------------------#
ggplot(score.oa,aes(x = OA_MeanScore))+geom_density()+facet_wrap(~session_only)

ggplot(dass21_as.short,aes(x = DASS21_MeanScore))+geom_density()+facet_wrap(~session_only)

ggplot(score.bbsiq,aes(x = BBSIQ_MeanScore))+geom_density()+facet_wrap(~session_only)

ggplot(score.neg.int.bias.potentialThreat,aes(x = RR_NEG_BIAS_MeanScore))+geom_density()+facet_wrap(~session_only)

ggplot(score.pos.int.bias.potentialThreat,aes(x = RR_POS_BIAS_MeanScore))+geom_density()+facet_wrap(~session_only)

#--------------------------------------------------------------------------------#
# exploratory analysis of outcome sores ----
#--------------------------------------------------------------------------------#
#Lattice plot by participant outcome score over sessions for the first 30 participants
theme.1 <- theme(axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 plot.title=element_text(hjust=.9,face="italic",size=12))


analyze_outcomes <- function(outcome_df, outcome_scores_df,participant_sample,outcome_name){
  col <-  rlang::sym(outcome_name)
  
  
  outcome_sessions <- outcome_df %>% select(session_only) %>% unique() %>% pull()
  
  outcome_scores <- outcome_scores_df %>% filter(session_only %in% outcome_sessions)
  outcome_scores_short <- outcome_scores %>% filter(participant_id %in% participant_sample)
  
  
  a <- ggplot(outcome_scores_short, aes(x = time, y = !!col)) + 
    geom_line(aes(group = participant_id), color = "dark grey") + 
    geom_smooth(aes(group = 1), color = "black", size = 1) + 
    theme.1 + 
    labs(x = "Session", y = outcome_name) 
  
  print(a)
  
  # b <- ggplot(outcome_scores_short, aes(x = time, y = !!col)) + 
  #   geom_line(aes(group = participant_id), color = "dark grey") + 
  #   facet_grid(.~engagement_cluster)+
  #   geom_smooth(aes(group = 1), color = "black", size = 1) + 
  #   theme.1 + 
  #   labs(x = "Session", y = outcome_name) 
  # print(b)
}

#randomly select 100 participants to analyze the overall trend of the outcomes
set.seed(1234)
participant_ids_sample <- sample(p_ids,100)
participant_ids_sample

#OA decline from time 0 to time 2, increase from 2 to 3, constant from 3 to 6
analyze_outcomes(oa,outcomes.scores.df,participant_ids_sample,"OA_MeanScore")

#DASS decrease from time 0 to time 6
analyze_outcomes(dass21_as,outcomes.scores.df,participant_ids_sample,"DASS21_MeanScore")

#BBSIQ decline from time 0 to time 3, slight increase from time 3 to time 6
analyze_outcomes(bbsiq,outcomes.scores.df,participant_ids_sample,"BBSIQ_MeanScore")

#RR Positive Bias increase from time 0 to time 3, slight decrease from  time 3 to 6
analyze_outcomes(rr,outcomes.scores.df,participant_ids_sample,"RR_POS_BIAS_MeanScore")

#RR Negative Bias decrease from time 0 time 3, slight increase from time 3 to time 5, decrease from time 5 to time 6
analyze_outcomes(rr,outcomes.scores.df,participant_ids_sample,"RR_NEG_BIAS_MeanScore")

#--------------------------------------------------------------------------------#
# missing data for outcome scores ----
#--------------------------------------------------------------------------------#
#factor sesssion_only to set order
outcomes.scores.df$session_only<- factor(outcomes.scores.df$session_only,levels = sessions)

outcomes.scores.df %>% filter(session_only %in% session_outcomes$sessions[session_outcomes$OA==1],) %>% select(session_only, OA_MeanScore) %>% 
  group_by(session_only) %>%
  dplyr::summarise_each(funs(sum(is.na(.))/length(.)))

outcomes.scores.df %>% filter(session_only %in% session_outcomes$sessions[session_outcomes$DASS21==1],) %>% select(session_only, DASS21_MeanScore) %>% 
  group_by(session_only) %>%
  dplyr::summarise_each(funs(sum(is.na(.))/length(.)))

outcomes.scores.df %>% filter(session_only %in% session_outcomes$sessions[session_outcomes$BBSIQ==1],) %>% select(session_only, BBSIQ_MeanScore) %>% 
  group_by(session_only) %>%
  dplyr::summarise_each(funs(sum(is.na(.))/length(.)))

#participant 420 answered 555 for all RR questions in the preTest session
outcomes.scores.df %>% filter(session_only %in% session_outcomes$sessions[session_outcomes$RR_NEG_BIAS==1],) %>% select(session_only, RR_NEG_BIAS_MeanScore) %>% 
  group_by(session_only) %>%
  dplyr::summarise_each(funs(sum(is.na(.))/length(.)))

outcomes.scores.df %>% filter(session_only %in% session_outcomes$sessions[session_outcomes$RR_POS_BIAS==1],) %>% select(session_only, RR_POS_BIAS_MeanScore) %>% 
  group_by(session_only) %>%
  dplyr::summarise_each(funs(sum(is.na(.))/length(.)))


#--------------------------------------------------------------------------------#
# credibility ----
#--------------------------------------------------------------------------------#
credibility <- dat.3$credibility

#How confident are you that an online training program will reduce your anxiety?
credibility_online <- credibility  %>% select(participant_id,session_only,confident_online)

#only 4 participants out of 698 selected prefer not to answer, replace value with NA, impute by the median which is 2
credibility_online <-credibility_online %>% replace(.== 555, NA) %>% mutate(confident_online2 = ifelse(is.na(confident_online),median(confident_online,na.rm = T),confident_online))

#count of NAs, participants with NA values 
table(credibility_online$confident_online,useNA = "ifany")

#credibility with NA
cred_online_na <- credibility_online %>% select(confident_online) %>% pull()

#credibility with imputed values
cred_online_imp <-  credibility_online %>% select(confident_online2) %>% pull()

#add to outcomes df
outcomes.scores.df$cred_online_na<- rep(cred_online_na, each = length(sessions))
outcomes.scores.df$cred_online_imp<- rep(cred_online_imp, each = length(sessions))

#check to see presence of NA values
table(outcomes.scores.df$cred_online_na,useNA = "ifany")
table(outcomes.scores.df$cred_online_imp,useNA = "ifany")


#visualize credibility bar plot
ggplot(credibility_online,aes(x = confident_online2))+geom_bar()

#--------------------------------------------------------------------------------#
# engagement clusters ----
#--------------------------------------------------------------------------------#
engagement_cluster_group <- participant_cluster %>% select(cluster) %>% pull()

table(participant_cluster$cluster)

outcomes.scores.df$engagement_cluster <- rep(engagement_cluster_group, each = length(sessions))


#--------------------------------------------------------------------------------#
# Raw Means and Standard Deviations of Outcomes for analyzed sample and clusters ----
#--------------------------------------------------------------------------------#
outcomes.scores.df.2 <- outcomes.scores.df %>% select(participant_id,session_only,colnames(outcomes.scores.df[grep("_MeanScore", colnames(outcomes.scores.df))]),engagement_cluster)

#table to store raw means and sd for each of the outcomes
summary_df <- tibble()

raw_outcomes <- function(outcomes.scores.df, outcomeColumn, outcomeName, summary.df){
  outcomeColumn <- rlang::sym(outcomeColumn)
  
  summary_outcome <- outcomes.scores.df %>% group_by(session_only) %>% 
    summarise(n = n()-sum(is.na(!!outcomeColumn)), 
              M = round(mean(!!outcomeColumn,na.rm = T),2), 
              SD = round(sd(!!outcomeColumn, na.rm = T),2)) %>% 
    ungroup()
  summary_outcome$outcome <- outcomeName
  
  summary.df <- rbind(summary.df,summary_outcome)
  
  return(summary.df)
}

summary_df <- raw_outcomes(outcomes.scores.df.2,"OA_MeanScore","OASIS",summary_df)
summary_df <- raw_outcomes(outcomes.scores.df.2,"DASS21_MeanScore","DASS21",summary_df)
summary_df <- raw_outcomes(outcomes.scores.df.2,"BBSIQ_MeanScore","Negative Bias (BBSIQ)",summary_df)
summary_df <- raw_outcomes(outcomes.scores.df.2,"RR_NEG_BIAS_MeanScore","Negative Bias (RR)",summary_df)
summary_df <- raw_outcomes(outcomes.scores.df.2,"RR_POS_BIAS_MeanScore","Positive Bias (RR)",summary_df)


#filter for timepoints where measures were not assessed
summary_df <- summary_df %>% filter(n != 0)
#reorder columns
summary_df <- summary_df %>% select(outcome,colnames(summary_df[-5]))



#repeat for engagement cluster 1 
outcomes.scores.df.2.1 <- outcomes.scores.df.2 %>% filter(engagement_cluster == 1)

#table to store raw means and sd for each of the outcomes
summary_df_1 <- tibble()

summary_df_1 <- raw_outcomes(outcomes.scores.df.2.1,"OA_MeanScore","OASIS",summary_df_1)
summary_df_1 <- raw_outcomes(outcomes.scores.df.2.1,"DASS21_MeanScore","DASS21",summary_df_1)
summary_df_1 <- raw_outcomes(outcomes.scores.df.2.1,"BBSIQ_MeanScore","Negative Bias (BBSIQ)",summary_df_1)
summary_df_1 <- raw_outcomes(outcomes.scores.df.2.1,"RR_NEG_BIAS_MeanScore","Negative Bias (RR)",summary_df_1)
summary_df_1 <- raw_outcomes(outcomes.scores.df.2.1,"RR_POS_BIAS_MeanScore","Positive Bias (RR)",summary_df_1)



#filter for timepoints where measures were not assessed
summary_df_1 <- summary_df_1 %>% filter(n != 0)
#reorder columns
summary_df_1 <- summary_df_1 %>% select(outcome,colnames(summary_df_1[-5]))


#repeat for engagement cluster 2
outcomes.scores.df.2.2 <- outcomes.scores.df.2 %>% filter(engagement_cluster == 2)

#table to store raw means and sd for each of the outcomes
summary_df_2 <- tibble()

summary_df_2 <- raw_outcomes(outcomes.scores.df.2.2,"OA_MeanScore","OASIS",summary_df_2)
summary_df_2 <- raw_outcomes(outcomes.scores.df.2.2,"DASS21_MeanScore","DASS21",summary_df_2)
summary_df_2 <- raw_outcomes(outcomes.scores.df.2.2,"BBSIQ_MeanScore","Negative Bias (BBSIQ)",summary_df_2)
summary_df_2 <- raw_outcomes(outcomes.scores.df.2.2,"RR_NEG_BIAS_MeanScore","Negative Bias (RR)",summary_df_2)
summary_df_2 <- raw_outcomes(outcomes.scores.df.2.2,"RR_POS_BIAS_MeanScore","Positive Bias (RR)",summary_df_2)


#filter for timepoints where measures were not assessed
summary_df_2 <- summary_df_2 %>% filter(n != 0)
#reorder columns
summary_df_2 <- summary_df_2 %>% select(outcome,colnames(summary_df_2[-5]))

#bind columns
summary_df_final <- cbind(summary_df,select(summary_df_1,-session_only,-outcome),select(summary_df_2,-session_only,-outcome))

#export as latex table 
sink(here("Tables","5_desc_stats_outcomes_scores_v2.txt"))
stargazer(summary_df_final, type = "latex", title="Descriptive statistics of target outcomes over time for analyzed sample broken down by engagement clusters", digits=2, summary = F,rownames = F)
sink()


# ---------------------------------------------------------------------------- #
# Compute proportions of scale-level missing data ----
# ---------------------------------------------------------------------------- #
prop_scale_missing <- summary_df %>% group_by(outcome) %>% summarise(prop_missing = round((sum(n)/(n()*697))*100,2))



# ---------------------------------------------------------------------------- #
# Save data as RData ----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps
save(outcomes.scores.df,session_outcomes, file = here("Data","RData" ,"5_outcomes_df_for_imputation.RData"))











