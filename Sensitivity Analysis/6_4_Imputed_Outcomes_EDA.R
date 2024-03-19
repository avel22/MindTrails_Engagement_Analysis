### Adapting Jeremy Eberle's script
### https://github.com/jwe4ec/jp5ws/blob/main/Syntax/16_longitudinal_analyses.R
### Eberle, J. W., Boukhechba, M., Sun, J., Zhang, D., Funk, D., Barnes, L., & Teachman, B. (2022, January 13). Shifting Episodic Prediction With Online Cognitive Bias Modification: A Randomized Controlled Trial. Retrieved from osf.io/jp5ws

### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is to calculate distribution of imputed data and analyze proportion of values that are below and above the possible values
### We did not run this script for the sensitivity analysis. 
#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer)

#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Data","5_outcomes_df_for_imputation.RData"))
session_outcomes$time <- 0:6

#imputed outcomes
mod0_columns <- c("imp_num","participant_id","time","t1","t2","outcome","cred_on","eng_cluster","gender_num","isOneType")



#OA
oa_imputed <- read.csv(here("Data","imputation","Blimp","OASIS","ActualImputation","OA_imps_mod0.csv"),header = F)
colnames(oa_imputed) <- mod0_columns
oa_imputed <- oa_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
oa_imputed$session_only <- oa_imputed$sessions

#DASS21
dass21_imputed <- read.csv(here("Data","imputation","Blimp","DASS21","ActualImputation","DASS21_imps_mod0.csv"),header = F)
colnames(dass21_imputed) <- mod0_columns
dass21_imputed <- dass21_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
dass21_imputed$session_only <- dass21_imputed$sessions

#RR_NEG
rr_neg_imputed <- read.csv(here("Data","imputation","Blimp","RR_NEG_BIAS","ActualImputation","RR_NEG_imps_mod0.csv"),header = F)
colnames(rr_neg_imputed) <- mod0_columns
rr_neg_imputed <- rr_neg_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_neg_imputed$session_only <- rr_neg_imputed$sessions

#RR_POS
rr_pos_imputed <- read.csv(here("Data","imputation","Blimp","RR_POS_BIAS","ActualImputation","RR_POS_imps_mod0.csv"),header = F)
colnames(rr_pos_imputed) <- mod0_columns
rr_pos_imputed <- rr_pos_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_pos_imputed$session_only <- rr_pos_imputed$sessions

#BBSIQ
bbsiq_imputed <- read.csv(here("Data","imputation","Blimp","BBSIQ","ActualImputation","BBSIQ_imps_mod0.csv"),header = F)
colnames(bbsiq_imputed) <- mod0_columns
bbsiq_imputed <- bbsiq_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
bbsiq_imputed$session_only <- bbsiq_imputed$sessions



#convert to mitml list
mitml_list_oa <- as.mitml.list(split(oa_imputed,oa_imputed$imp_num))
mitml_list_dass21 <- as.mitml.list(split(dass21_imputed,dass21_imputed$imp_num))
mitml_list_rr_neg <- as.mitml.list(split(rr_neg_imputed,rr_neg_imputed$imp_num))
mitml_list_rr_pos <- as.mitml.list(split(rr_pos_imputed,rr_pos_imputed$imp_num))
mitml_list_bbsiq <- as.mitml.list(split(bbsiq_imputed,bbsiq_imputed$imp_num))



#--------------------------------------------------------------------------------#
# functions ----
#--------------------------------------------------------------------------------#
# Function 1: for a given outcome, for each imputed dataset, for each session calculate the stats for values below and above the scoring scale
imputation_range_analysis <- function(imputationList,path,scaleMin,ScaleMax,sessions){
  plotlist = list()
  #outcome <- rlang::sym(outcome)
  #empty tibble to store values for imputation range analysis
  imp_range_analysis <- tibble()
  #for each imputed dataset calculate values below and above scale range
  for(i in 1:length(imputationList)){
    imp_df <- imputationList[[i]]
    imp_df <- imp_df %>% select(participant_id,session_only,outcome)
    imp_df$session_only <- factor(imp_df$session_only, levels = sessions)
    imp_range <- imp_df %>% group_by(session_only) %>% 
      mutate(
        k = i,
        isBelow = ifelse(outcome < scaleMin, 1, 0) ,
        isAbove = ifelse(outcome > ScaleMax, 1, 0)
      ) %>% 
      summarise(
        imp = mean(k) ,
        countBelow = sum(isBelow),
        countAbove = sum(isAbove),
        minBelow = min(outcome),
        maxAbove =max(outcome),
        perc_below = countBelow/n(),
        perc_above =countAbove/n()
      ) %>% ungroup()
    
    
    
    imp_range_analysis<- rbind(imp_range_analysis,imp_range)
    
    
  }
  
  imp_output <-
    imp_range_analysis %>% 
    group_by(session_only) %>% 
    summarise(
      meanPercBelow = round(mean(perc_below) * 100,3),
      meanCountBelow = mean(countBelow) ,
      meanMinBelow = round(mean(minBelow), 3),
      absMin = min(minBelow),
      meanPercAbove = round(mean(perc_above) * 100,3),
      meanCountAbove = mean(countAbove) ,
      meanMaxAbove = round(mean(maxAbove), 3) ,
      absMax = max(maxAbove)
    )
  
  return(imp_output)
}

# Function 2: Use Jeremy's function to check that values above match and plot overlapping histograms
histbytime <- function(data, path, outcome,outcome_label, xmin, xmax, ymin, ymax, scalemin, scalemax,sessions) {
  sink(file = paste0(path, ".txt"))
  
  x <- data
  
  print(paste("Dataset:", deparse(substitute(data))), quote = FALSE)
  print(paste0("Outcome: ", outcome), quote = FALSE)
  print(paste0("Minimum plausible scale value: ", scalemin), quote = FALSE)
  print(paste0("Maximum plausible scale value: ", scalemax), quote = FALSE)
  cat("\n")
  
  pdf(paste0(path, ".pdf"), width = 9, height = 7)
  
  par(new = FALSE)
  par(mfrow = c(2, 3))
  
  for (k in unique(sessions)) {
    n <- nrow(x[[1]][x[[1]]$session_only == k, ])
    iabove <- rep(0, length(x))
    ibelow <- rep(0, length(x))
    imax <- rep(0, length(x))
    imin <- rep(0, length(x))
    outrange <- data.frame(iabove, ibelow, imax, imin)
    
    for (i in 1:length(x)) {
      outrange[i, "ibelow"] <- sum(x[[i]][x[[i]]$session_only == k, outcome] < scalemin)
      outrange[i, "iabove"] <- sum(x[[i]][x[[i]]$session_only == k, outcome] > scalemax)
      outrange[i, "imin"] <- min(x[[i]][x[[i]]$session_only == k, outcome])
      outrange[i, "imax"] <- max(x[[i]][x[[i]]$session_only == k, outcome])
      
      hist(x[[i]][x[[i]]$session_only == k, outcome],
           main = paste0("session = ", k),
           breaks = c(xmin:xmax),
           right = FALSE,
           xlim = range(xmin, xmax),
           ylim = range(ymin, ymax),
           xlab = outcome_label,
           col = "white")
      
      par(new = TRUE)
    }
    
    print(paste0("For session_int = ", k, ":"), quote = FALSE)
    cat("\n")
    print(paste0("Mean number of imputed values below plausible range = ", 
                 mean(outrange$ibelow)), quote = FALSE)
    print(paste0("Mean percent of imputed values below plausible range = ", 
                 round(mean((outrange$ibelow/n)*100), digits = 3), "%"), quote = FALSE)
    print(paste0("Mean minimum imputed value = ", 
                 round(mean(outrange$imin), digits = 3)), quote = FALSE)
    print(paste0("Absolute minimum imputed value = ", 
                 round(min(outrange$imin), digits = 3)), quote = FALSE)
    cat("\n")
    print(paste0("Mean number of imputed values above plausible range = ", 
                 mean(outrange$iabove)), quote = FALSE)
    print(paste0("Mean percent of imputed values above plausible range = ", 
                 round(mean((outrange$iabove/n)*100), digits = 3), "%"), quote = FALSE)
    print(paste0("Mean maximum imputed value = ", 
                 round(mean(outrange$imax), digits = 3)), quote = FALSE)
    print(paste0("Absolute maximum imputed value = ",
                 round(max(outrange$imax), digits = 3)), quote = FALSE)
    cat("\n")
    
    abline(v = scalemin)
    abline(v = scalemax)
    par(new = FALSE)
  }
  dev.off()
  par(mfrow = c(1,1))
  
  sink()
}

histbytime_before_imp <- function(data, path, outcome,outcome_label, xmin, xmax, ymin, ymax, scalemin, scalemax,sessions) {
  #outcome <- rlang::sym(outcome)
  x <- data
  
  
  
  pdf(paste0(path, ".pdf"), width = 9, height = 7)
  
  par(new = FALSE)
  par(mfrow = c(2, 3))
  
  for (k in unique(sessions)) {
    session_data <- x[x$session_only == k, outcome]
    session_data <- session_data %>% pull()
    
    
    hist(as.numeric(session_data),
         main = paste0("session = ", k),
         breaks = c(xmin:xmax),
         right = FALSE,
         xlim = range(xmin, xmax),
         ylim = range(ymin, ymax),
         xlab = outcome_label,
         col = "white")
    
    abline(v = scalemin)
    abline(v = scalemax)
    par(new = FALSE)
    
  }
  
  
  
  
  dev.off()
  par(mfrow = c(1,1))
}

#--------------------------------------------------------------------------------#
# Data Distributions Before Imputations ----
#--------------------------------------------------------------------------------#
histbytime_before_imp(data = dplyr::select(outcomes.scores.df,session_only,OA_MeanScore), 
                      path = here("Figures","6_4_BeforeImputationDistribution_OA"),outcome = "OA_MeanScore", 
                      outcome_label = "OASIS Mean Score Scale",
                      xmin = -4,xmax =  8,ymin =  0,ymax =  500,scalemin =  0,scalemax =  4,
                      sessions = session_outcomes$sessions[session_outcomes$OA==1])

histbytime_before_imp(data = dplyr::select(outcomes.scores.df,session_only,DASS21_MeanScore), 
                      path = here("Figures","6_4_BeforeImputationDistribution_DASS21"),outcome = "DASS21_MeanScore", 
                      outcome_label = "DASS21 Mean Score Scale",
                      xmin = -4,xmax =  8,ymin =  0,ymax =  500,scalemin =  0,scalemax =  3,
                      sessions = session_outcomes$sessions[session_outcomes$DASS21==1])

histbytime_before_imp(data = dplyr::select(outcomes.scores.df,session_only,BBSIQ_MeanScore), 
                      path = here("Figures","6_4_BeforeImputationDistribution_BBSIQ"),outcome = "BBSIQ_MeanScore", 
                      outcome_label = "BBSIQ Mean Score Scale",
                      xmin = -4,xmax =  8,ymin =  0,ymax =  500,scalemin =  0,scalemax =  4,
                      sessions = session_outcomes$sessions[session_outcomes$BBSIQ==1])

histbytime_before_imp(data = dplyr::select(outcomes.scores.df,session_only,RR_NEG_BIAS_MeanScore), 
                      path = here("Figures","6_4_BeforeImputationDistribution_RR_NEG_BIAS"),outcome = "RR_NEG_BIAS_MeanScore", 
                      outcome_label = "RR Negative Bias Mean Score Scale",
                      xmin = -4,xmax =  8,ymin =  0,ymax =  500,scalemin =  1,scalemax =  4,
                      sessions = session_outcomes$sessions[session_outcomes$RR_NEG_BIAS==1])

histbytime_before_imp(data = dplyr::select(outcomes.scores.df,session_only,RR_POS_BIAS_MeanScore), 
                      path = here("Figures","6_4_BeforeImputationDistribution_RR_POS_BIAS"),outcome = "RR_POS_BIAS_MeanScore", 
                      outcome_label = "RR Positive Bias Mean Score Scale",
                      xmin = -4,xmax =  8,ymin =  0,ymax =  500,scalemin =  1,scalemax =  4,
                      sessions = session_outcomes$sessions[session_outcomes$RR_POS_BIAS==1])


#--------------------------------------------------------------------------------#
# Imputed Data Distributions and Out of Range Analysis Function 1 ----
#--------------------------------------------------------------------------------#
#OA min of 0, max of 4
range_OA <- imputation_range_analysis(imputationList = mitml_list_oa,scaleMin = 0,ScaleMax = 4, sessions = session_outcomes$sessions[session_outcomes$OA==1])

range_OA$outcome <- c("OASIS", rep("", nrow(range_OA)-1))
range_OA$scale <- c("[0,4]", rep("", nrow(range_OA)-1))

#DASS21 min of 0, max of 3
range_DASS21 <- imputation_range_analysis(imputationList = mitml_list_dass21,scaleMin = 0,ScaleMax = 3, sessions = session_outcomes$sessions[session_outcomes$DASS21==1])

range_DASS21$outcome <- c("DASS21", rep("", nrow(range_DASS21)-1))
range_DASS21$scale <- c("[0,3]", rep("", nrow(range_DASS21)-1))

#BBSIQ min of 0, max of 4
range_BBSIQ <- imputation_range_analysis(imputationList = mitml_list_bbsiq,scaleMin = 0,ScaleMax = 4, sessions = session_outcomes$sessions[session_outcomes$BBSIQ==1])

range_BBSIQ$outcome <- c("BBSIQ", rep("", nrow(range_BBSIQ)-1))
range_BBSIQ$scale <- c("[0,4]", rep("", nrow(range_BBSIQ)-1))


#RR_POS_BIAS min of 1, max of 4
range_RR_POS_BIAS <- imputation_range_analysis(imputationList = mitml_list_rr_pos,scaleMin = 1,ScaleMax = 4, sessions = session_outcomes$sessions[session_outcomes$RR_POS_BIAS==1])

range_RR_POS_BIAS$outcome <- c("RR Positive Bias", rep("", nrow(range_RR_POS_BIAS)-1))
range_RR_POS_BIAS$scale <- c("[1,4]", rep("", nrow(range_RR_POS_BIAS)-1))

#RR_NEG_BIAS min of 1, max of 4
range_RR_NEG_BIAS <- imputation_range_analysis(imputationList = mitml_list_rr_neg,scaleMin = 1,ScaleMax = 4, sessions = session_outcomes$sessions[session_outcomes$RR_NEG_BIAS==1])

range_RR_NEG_BIAS$outcome <- c("RR Negative Bias", rep("", nrow(range_RR_NEG_BIAS)-1))
range_RR_NEG_BIAS$scale <- c("[1,4]", rep("", nrow(range_RR_NEG_BIAS)-1))

extreme_scores<- rbind(range_OA,range_DASS21,range_BBSIQ,range_RR_NEG_BIAS,range_RR_POS_BIAS)

extreme_scores$session_only <- recode_factor(extreme_scores$session_only, preTest = "Baseline" , firstSession = "Session 1",secondSession= "Session 2",thirdSession = "Session 3",fourthSession = "Session 4",fifthSession = "Session 5",PostFollowUp = "Post Follow-Up")

extreme_scores <- extreme_scores %>% select(outcome,session_only,scale, colnames(extreme_scores[-c(1,10,11)])) %>% select(-meanCountBelow,-meanCountAbove)

extreme_scores$session_only<- as.character(extreme_scores$session_only)

round_format <- function(var,roundNum){
  formatC(round(var,roundNum),format = "f",digits = roundNum)
}

extreme_scores[c(-1,-2,-3)] <- lapply(extreme_scores[c(-1,-2,-3)], round_format, roundNum = 2)

sink(here("Tables","6_4_extreme_scores.txt"))
stargazer(extreme_scores, type = "latex", title="Scale out of bounds values across the 100 imputed datasets for the analyzed sample", digits=2, summary = F,rownames = F)
sink()

#--------------------------------------------------------------------------------#
# Imputed Data Distributions and Out of Range Analysis Function 2 ----
#--------------------------------------------------------------------------------#
#OA min of 0, max of 4
histbytime(mitml_list_oa, 
           here("Figures","6_4_ImputationDistribution_OA"), 
           "outcome",outcome_label = "OASIS Mean Score Scale", -4, 8, 0, 500, 0, 4,session_outcomes$sessions[session_outcomes$OA==1])

#DASS21 min of 0, max of 3
histbytime(mitml_list_dass21, 
           here("Figures","6_4_ImputationDistribution_DASS21"), 
           "outcome",outcome_label = "DASS21 Mean Score Scale", -4, 8, 0, 500, 0, 3,session_outcomes$sessions[session_outcomes$DASS21==1])

#BBSIQ min of 0, max of 4
histbytime(mitml_list_bbsiq, 
           here("Figures","6_4_ImputationDistribution_BBSIQ"), 
           "outcome",outcome_label = "BBSIQ Mean Score Scale", -4, 8, 0, 500, 0, 4,session_outcomes$sessions[session_outcomes$BBSIQ==1])

#RR_POS_BIAS min of 1, max of 4
histbytime(mitml_list_rr_pos, 
           here("Figures","6_4_ImputationDistribution_RR_POS_BIAS"), 
           "outcome",outcome_label = "RR Positive Bias Mean Score Scale", -4, 8, 0, 500, 1, 4,session_outcomes$sessions[session_outcomes$RR_POS_BIAS==1])

#RR_NEG_BIAS min of 1, max of 4
histbytime(mitml_list_rr_neg, 
           here("Figures","6_4_ImputationDistribution_RR_NEG_BIAS"), 
           "outcome",outcome_label = "RR Negative Bias Mean Score Scale", -4, 8, 0, 500, 1, 4,session_outcomes$sessions[session_outcomes$RR_NEG_BIAS==1])


