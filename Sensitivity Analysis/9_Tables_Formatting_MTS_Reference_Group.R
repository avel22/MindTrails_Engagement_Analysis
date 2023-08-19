### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to print out the tables for the model results


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer,lme4,psycho,afex,nlme,optimx,emmeans,gtsummary)


#--------------------------------------------------------------------------------#
# TABLE FOR LONGITUDINAL MLM PIECEWISE MODELING RESULTS AND SIMPLE TIME EFFECTS ----
#--------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Data","RData","7_2_simple_time_effects_results_MTS_Reference_Group.RData"))

# ---------------------------------------------------------------------------- #
# Define create_re_var_cov() ----
# ---------------------------------------------------------------------------- #

# Define function to create random effects variance-covariance matrix from 
# extra.pars output of testEstimates function

create_re_var_cov <- function(pooled, n_random_effects) {
  var_first <- 1
  var_last <- n_random_effects
  var <- pooled$extra.pars[1:var_last]
  cov_first <- n_random_effects + 1
  cov_last <- length(pooled$extra.pars) - 2
  cov <- pooled$extra.pars[cov_first:cov_last]
  
  names <- unlist(lapply(strsplit(labels(pooled$extra.pars)[[1]][1:var_last], 
                                  split = "~~", 
                                  fixed = TRUE),
                         function(x) x[1]))
  
  var_cov <- matrix(NA, ncol = length(var), nrow = length(var))
  rownames(var_cov) <- names
  colnames(var_cov) <- names
  diag(var_cov) <- var
  var_cov[upper.tri(var_cov)] <- cov
  var_cov[lower.tri(var_cov)] <- t(var_cov)[lower.tri(t(var_cov))]
  
  return(var_cov)
}

# ---------------------------------------------------------------------------- #
# Define create_results_list() ----
# ---------------------------------------------------------------------------- #
#Function from Jeremy W. Eberle 
# https://github.com/jwe4ec/yn3z6/blob/main/syntax/00_define_functions.R

# Define function to create list of results for a model, including random
# effects correlation matrix and confidence intervals for fixed effects

create_results_list <- function(modelList, pooled, n_random_effects) {
  if (modelList[1] == "Model did not converge" & is.na(pooled[1])) {
    re_var_cov <- NA
    re_cor <- NA
    ci <- NA
  } else {
    re_var_cov <- create_re_var_cov(pooled, n_random_effects)
    re_cor <- cov2cor(re_var_cov)
    ci <- confint(pooled)
  }
  results_list <- list("modelList" = modelList,
                       "pooled" = pooled,
                       "re_var_cov" = re_var_cov,
                       "re_cor" = re_cor,
                       "ci" = ci)
  
  return(results_list)
}


# ---------------------------------------------------------------------------- #
# Define create_results_table() ----
# ---------------------------------------------------------------------------- #
create_results_table <- function(modelList, pooled, outcome) {
  round_format <- function(var,roundNum){
    formatC(round(var,roundNum),format = "f",digits = roundNum)
  }
  #row names for fixed and random effects
  row_names_fixed <- c("Intercept", "time$_{TR}$", "time$_{FU}$", "Less Time Spent", "Training confidence", 
                       "Less Time Spent $\times$ time$_{TR}$", "Less Time Spent $\times$ time$_{FU}$")
  
  row_names_random <- c("1. Intercept","2. time$_{TR}$", "3. time$_{FU}$", "Residual", "","","")
  
  #fixed effects table
  #fixed effect model values
  fixed_output_table_values<- as.data.frame(pooled[["estimates"]])
  # compute confidence intervals
  fixed_ci <- confint.mitml.testEstimates(pooled)
  #bind values with CI
  fixed_output_table <- cbind(fixed_output_table_values,fixed_ci)
  #format CI 
  fixed_output_table$CI <- paste0("[",formatC(round(fixed_output_table$`2.5 %`,2),format = "f",digits = 2),", ",formatC(round(fixed_output_table$`97.5 %`,2),format = "f",digits = 2),"]")
  #remove columns not of interest
  fixed_output_table2 <- fixed_output_table %>% select(-`2.5 %`,-`97.5 %`,-RIV,-FMI) 
  #round to two decimals
  fixed_output_table3 <- fixed_output_table2 %>% mutate_at(vars(-`P(>|t|)`,-CI), funs(round(., 2)))
  #round p values to 3 decimals
  fixed_output_table3$`P(>|t|)` <- formatC(round(fixed_output_table3$`P(>|t|)`,3),format = "f",digits = 3)
  # format b(SE) columns
  fixed_output_table3$"b(SE)" <- paste0(round_format(fixed_output_table3$Estimate,2),"(",fixed_output_table3$Std.Error,")")
  #define outcome
  fixed_output_table3$Outcome <- c(outcome, rep("",6))
  #set fixed effect rows
  fixed_output_table3$FixedEffect <- row_names_fixed
  #reorder columns
  fixed_output_table4 <- fixed_output_table3 %>% select(Outcome,FixedEffect,"b(SE)",CI,df,t.value,`P(>|t|)`)
  
  #random effects table
  #values
  random_output_table_values <- as.data.frame(pooled[["extra.pars"]])
  random_output_table_values_cor <- as.data.frame(create_results_list(modelList,pooled,3)[["re_cor"]])
  
  
  
  #variance
  var_int <- round_format(random_output_table_values[1,1],2)
  var_time1 <-round_format(random_output_table_values[2,1],2)
  var_time2 <-round_format(random_output_table_values[3,1],2)
  #covariance
  cov_time1_int <-round_format(random_output_table_values[4,1],2)
  cov_time2_int <-round_format(random_output_table_values[5,1],2)
  cov_time1_time2 <-round_format(random_output_table_values[6,1],2)
  #correlation
  cor_time1_int <-round_format(random_output_table_values_cor[2,1],2)
  cor_time2_int <-round_format(random_output_table_values_cor[3,1],2)
  cor_time1_time2 <- round_format(random_output_table_values_cor[3,2],2)
  #residual
  residual <- round_format(random_output_table_values[7,1],2)
  
  #set random effects rows
  random_output_table <- tibble(RandomEffect = row_names_random)
  #build variance covariance matrix
  random_output_table$var_res <- c(var_int,var_time1,var_time2,residual, rep("",3))
  random_output_table$cov_1 <- c("-",cov_time1_int,cov_time2_int,rep("",4))
  random_output_table$cov_2 <- c("","-",cov_time1_time2,rep("",4))
  random_output_table$cov_3 <- c("","","-",rep("",4))
  random_output_table$cor_1 <- c("-",cor_time1_int,cor_time2_int,rep("",4))
  random_output_table$cor_2 <- c("","-",cor_time1_time2,rep("",4))
  random_output_table$cor_3 <- c("","","-",rep("",4))
  
  #bind fixed and random effects
  fixed_random_table <- cbind(fixed_output_table4,random_output_table)
  
  fixed_random_table <- fixed_random_table %>% mutate(across(everything(), as.character))
  
  #convert all columns to character
  
  return(fixed_random_table)
}

# ---------------------------------------------------------------------------- #
# Define create_results_table_simple_time_effects() ----
# ---------------------------------------------------------------------------- #
create_results_table_simple <- function(modelList, pooled, outcome) {
  round_format <- function(var,roundNum){
    formatC(round(var,roundNum),format = "f",digits = roundNum)
  }
  #row names for fixed and random effects
  row_names_fixed <- c("Intercept", "time$_{TR}$", "time$_{FU}$", "Training confidence")
  
  row_names_random <- c("1. Intercept","2. time$_{TR}$", "3. time$_{FU}$", "Residual")
  
  #fixed effects table
  #fixed effect model values
  fixed_output_table_values<- as.data.frame(pooled[["estimates"]])
  # compute confidence intervals
  fixed_ci <- confint.mitml.testEstimates(pooled)
  #bind values with CI
  fixed_output_table <- cbind(fixed_output_table_values,fixed_ci)
  #format CI 
  fixed_output_table$CI <- paste0("[",formatC(round(fixed_output_table$`2.5 %`,2),format = "f",digits = 2),", ",formatC(round(fixed_output_table$`97.5 %`,2),format = "f",digits = 2),"]")
  #remove columns not of interest
  fixed_output_table2 <- fixed_output_table %>% select(-`2.5 %`,-`97.5 %`,-RIV,-FMI) 
  #round to two decimals
  fixed_output_table3 <- fixed_output_table2 %>% mutate_at(vars(-`P(>|t|)`,-CI), funs(round(., 2)))
  #round p values to 3 decimals
  fixed_output_table3$`P(>|t|)` <- formatC(round(fixed_output_table3$`P(>|t|)`,3),format = "f",digits = 3)
  # format b(SE) columns
  fixed_output_table3$"b(SE)" <- paste0(round_format(fixed_output_table3$Estimate,2),"(",fixed_output_table3$Std.Error,")")
  #define outcome
  fixed_output_table3$Outcome <- c(outcome, rep("",3))
  #set fixed effect rows
  fixed_output_table3$FixedEffect <- row_names_fixed
  #reorder columns
  fixed_output_table4 <- fixed_output_table3 %>% select(Outcome,FixedEffect,"b(SE)",CI,df,t.value,`P(>|t|)`)
  
  #random effects table
  #values
  random_output_table_values <- as.data.frame(pooled[["extra.pars"]])
  random_output_table_values_cor <- as.data.frame(create_results_list(modelList,pooled,3)[["re_cor"]])
  
  
  
  #variance
  var_int <- round_format(random_output_table_values[1,1],2)
  var_time1 <-round_format(random_output_table_values[2,1],2)
  var_time2 <-round_format(random_output_table_values[3,1],2)
  #covariance
  cov_time1_int <-round_format(random_output_table_values[4,1],2)
  cov_time2_int <-round_format(random_output_table_values[5,1],2)
  cov_time1_time2 <-round_format(random_output_table_values[6,1],2)
  #correlation
  cor_time1_int <-round_format(random_output_table_values_cor[2,1],2)
  cor_time2_int <-round_format(random_output_table_values_cor[3,1],2)
  cor_time1_time2 <- round_format(random_output_table_values_cor[3,2],2)
  #residual
  residual <- round_format(random_output_table_values[7,1],2)
  
  #set random effects rows
  random_output_table <- tibble(RandomEffect = row_names_random)
  #build variance covariance matrix
  random_output_table$var_res <- c(var_int,var_time1,var_time2,residual)
  random_output_table$cov_1 <- c("-",cov_time1_int,cov_time2_int,"")
  random_output_table$cov_2 <- c("","-",cov_time1_time2,"")
  random_output_table$cov_3 <- c("","","-","")
  random_output_table$cor_1 <- c("-",cor_time1_int,cor_time2_int,"")
  random_output_table$cor_2 <- c("","-",cor_time1_time2,"")
  random_output_table$cor_3 <- c("","","-","")
  
  #bind fixed and random effects
  fixed_random_table <- cbind(fixed_output_table4,random_output_table)
  
  fixed_random_table <- fixed_random_table %>% mutate(across(everything(), as.character))
  
  #convert all columns to character
  
  return(fixed_random_table)
}

#result table
#OASIS: modelList_oa_lme_mod1,pooled_oa_lme_mod1
fixed_random_table_oa <- create_results_table(modelList_oa_lme,pooled_oa_lme,"OASIS")

#DASS21: modelList_dass21_lme_mod1, pooled_dass21_lme_mod1
fixed_random_table_dass21 <- create_results_table(modelList_dass21_lme,pooled_dass21_lme,"DASS21")

#BBSIQ: modelList_bbsiq_lme_mod1, pooled_bbsiq_lme_mod1
fixed_random_table_bbsiq <- create_results_table(modelList_bbsiq_lme,pooled_bbsiq_lme,"BBSIQ")

#RR NEG BIAS: modelList_rr_neg_lme, pooled_rr_neg_lme
fixed_random_table_rr_neg <- create_results_table(modelList_rr_neg_lme,pooled_rr_neg_lme,"RR Negative Bias")

#RR POS BIAS: modelList_rr_pos_lme_mod1 , pooled_rr_pos_lme_mod1
fixed_random_table_rr_pos <- create_results_table(modelList_rr_pos_lme,pooled_rr_pos_lme,"RR Positive Bias")

#row bind all the tables

outcomes_fixed_random_table <- rbind(fixed_random_table_oa,fixed_random_table_dass21,fixed_random_table_bbsiq,fixed_random_table_rr_neg,fixed_random_table_rr_pos)

#order columns for table
outcomes_fixed_random_table <- outcomes_fixed_random_table %>% select(Outcome,FixedEffect,`b(SE)`,t.value,df,`P(>|t|)`,CI,RandomEffect,var_res,cov_1,cov_2,cov_3,cor_1,cor_2,cor_3)

outcomes_fixed_random_table_cor <- outcomes_fixed_random_table %>% select(-cov_1,-cov_2,-cov_3)



str(outcomes_fixed_random_table_cor)

outcomes_fixed_random_table_cor_formatting = outcomes_fixed_random_table_cor

# Formatting the P(>|t|) column
outcomes_fixed_random_table_cor_formatting$`P(>|t|)` <- sapply(outcomes_fixed_random_table_cor_formatting$`P(>|t|)`, function(p) {
  p_num <- as.numeric(p)
  if (p_num < 0.001) {
    return("\\textless{}.001***")
  } else if (p_num < 0.01) {
    return(paste0(substr(p, 2, nchar(p)), "**"))
  } else if (p_num < 0.05) {
    return(paste0(substr(p, 2, nchar(p)), "*"))
  } else {
    return(substr(p, 2, nchar(p)))
  }
})

# Formatting the t.value column
outcomes_fixed_random_table_cor_formatting$`t.value` <- sprintf("%.2f", as.numeric(outcomes_fixed_random_table_cor_formatting$`t.value`))

# Formatting the df column
outcomes_fixed_random_table_cor_formatting$df <- sprintf("%0.2f", as.numeric(outcomes_fixed_random_table_cor_formatting$df))
outcomes_fixed_random_table_cor_formatting$df <- format(as.numeric(outcomes_fixed_random_table_cor_formatting$df), big.mark = ",", decimal.mark = ".", scientific = FALSE)



sink(here("Tables","9_outcomes_fixed_random_table_MTS_Reference_Group.txt"))
stargazer(outcomes_fixed_random_table_cor_formatting, type = "latex", title="Result from pooled longitudinal multilevel model for each of the outcomes", digits=2, summary = F,rownames = F)
sink()


#results table simple effects
#BBSIQ: modelList_bbsiq_lme_mod1, pooled_bbsiq_lme_mod1
fixed_random_table_bbsiq_cluster_1 <- create_results_table_simple(modelList_bbsiq_lme_e_cluster_1,pooled_bbsiq_lme_e_cluster_1,"BBSIQ More Time Spent")

fixed_random_table_bbsiq_cluster_2 <- create_results_table_simple(modelList_bbsiq_lme_e_cluster_2,pooled_bbsiq_lme_e_cluster_2,"BBSIQ Less Time Spent")


#RR NEG BIAS: modelList_rr_neg_lme, pooled_rr_neg_lme
fixed_random_table_rr_neg_cluster_1 <- create_results_table_simple(modelList_rr_neg_lme_e_cluster_1,pooled_rr_neg_lme_e_cluster_1,"RR Negative Bias More Time Spent")

fixed_random_table_rr_neg_cluster_2 <- create_results_table_simple(modelList_rr_neg_lme_e_cluster_2,pooled_rr_neg_lme_e_cluster_2,"RR Negative Bias Less Time Spent")


outcomes_fixed_random_simple_table <- rbind(fixed_random_table_bbsiq_cluster_2,fixed_random_table_bbsiq_cluster_1,fixed_random_table_rr_neg_cluster_2,fixed_random_table_rr_neg_cluster_1)

#order columns for table
outcomes_fixed_random_simple_table <- outcomes_fixed_random_simple_table %>% select(Outcome,FixedEffect,`b(SE)`,t.value,df,`P(>|t|)`,CI,RandomEffect,var_res,cov_1,cov_2,cov_3,cor_1,cor_2,cor_3)

outcomes_fixed_random_simple_table_cor <- outcomes_fixed_random_simple_table %>% select(-cov_1,-cov_2,-cov_3)

outcomes_fixed_random_simple_table_cor_no_random <- outcomes_fixed_random_simple_table_cor %>%  select(-RandomEffect,-var_res,-cor_1,-cor_2,-cor_3)

outcomes_fixed_random_simple_table_cor_no_random_formatting = outcomes_fixed_random_simple_table_cor_no_random

outcomes_fixed_random_simple_table_cor_no_random_formatting$`P(>|t|)` <- sapply(outcomes_fixed_random_simple_table_cor_no_random_formatting$`P(>|t|)`, function(p) {
  p_num <- as.numeric(p)
  if (p_num < 0.001) {
    return("\\textless{}.001***")
  } else if (p_num < 0.01) {
    return(paste0(substr(p, 2, nchar(p)), "**"))
  } else if (p_num < 0.05) {
    return(paste0(substr(p, 2, nchar(p)), "*"))
  } else {
    return(substr(p, 2, nchar(p)))
  }
})

# Formatting the t.value column
outcomes_fixed_random_simple_table_cor_no_random_formatting$`t.value` <- sprintf("%.2f", as.numeric(outcomes_fixed_random_simple_table_cor_no_random_formatting$`t.value`))

# Formatting the df column
outcomes_fixed_random_simple_table_cor_no_random_formatting$df <- sprintf("%0.2f", as.numeric(outcomes_fixed_random_simple_table_cor_no_random_formatting$df))
outcomes_fixed_random_simple_table_cor_no_random_formatting$df <- format(as.numeric(outcomes_fixed_random_simple_table_cor_no_random_formatting$df), big.mark = ",", decimal.mark = ".", scientific = FALSE)


sink(here("Tables","9_outcomes_fixed_simple_table_MTS_Reference_Group.txt"))
stargazer(outcomes_fixed_random_simple_table_cor_no_random_formatting, type = "latex", title="Simple effects of time from pooled longitudinal multilevel model for each of the outcomes", digits=2, summary = F,rownames = F)
sink()

save.image(file = here("Data","RData","9_Tables_Formatting_MTS_Reference_Group.RData"))



# ---------------------------------------------------------------------------- #
# correlations ----
# ---------------------------------------------------------------------------- #

#https://rpubs.com/yjunechoe/correlationsLMEM

fixed_random_table_oa_corr <- create_results_list(modelList_oa_lme,pooled_oa_lme,3)

fixed_random_table_dass21_corr <- create_results_list(modelList_dass21_lme,pooled_dass21_lme,3)


