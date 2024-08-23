### Written by: Ángel Vela and Jeremy Eberle
### Engagement Paper
### University of Virginia
### August 2024
### The purpose of this script is to compute the sd in order to calculate the effect sizes GMA d

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml)


#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Scripts2","Data_Primary_Analysis","outcomes_df_for_imputation.RData"))
session_outcomes$time <- 0:6

#imputed outcomes
mod0_columns <- c("imp_num","participant_id","time","t1","t2","outcome","cred_on","eng_cluster","gender_num","isOneType")



#OA
oa_imputed <- read.csv(here("Scripts2","Data_Primary_Analysis","imputation","MOD0","OASIS","ActualImputation","OA_imps_mod0.csv"),header = F)
colnames(oa_imputed) <- mod0_columns
oa_imputed <- oa_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
oa_imputed$session_only <- oa_imputed$sessions

#DASS21
dass21_imputed <- read.csv(here("Scripts2","Data_Primary_Analysis","imputation","MOD0","DASS21","ActualImputation","DASS21_imps_mod0.csv"),header = F)
colnames(dass21_imputed) <- mod0_columns
dass21_imputed <- dass21_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
dass21_imputed$session_only <- dass21_imputed$sessions

#RR_NEG
rr_neg_imputed <- read.csv(here("Scripts2","Data_Primary_Analysis","imputation","MOD0","RR_NEG_BIAS","ActualImputation","RR_NEG_imps_mod0.csv"),header = F)
colnames(rr_neg_imputed) <- mod0_columns
rr_neg_imputed <- rr_neg_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_neg_imputed$session_only <- rr_neg_imputed$sessions

#RR_POS
rr_pos_imputed <- read.csv(here("Scripts2","Data_Primary_Analysis","imputation","MOD0","RR_POS_BIAS","ActualImputation","RR_POS_imps_mod0.csv"),header = F)
colnames(rr_pos_imputed) <- mod0_columns
rr_pos_imputed <- rr_pos_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_pos_imputed$session_only <- rr_pos_imputed$sessions

#BBSIQ
bbsiq_imputed <- read.csv(here("Scripts2","Data_Primary_Analysis","imputation","MOD0","BBSIQ","ActualImputation","BBSIQ_imps_mod0.csv"),header = F)
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
#  C. Steps for Computing Each Group’s SD at Baseline and Pooled SD Across Groups at Baseline ----
#--------------------------------------------------------------------------------#


# Before we can compute effect sizes, we need to compute the SD in each group 
# and the pooled SD across groups. The steps are as follows:

# 1. For each imputed dataset:
#    a. Compute each of the two group’s SD at baseline and store them as “sd_grp1” and “sd_grp2”.

# Function to compute SD for each group at baseline
compute_sd_baseline <- function(imputed_data, cluster_var = "eng_cluster", time_var = "time", outcome_var = "outcome", baseline_time = 0) {
  
  # Filter data for baseline time point
  baseline_data <- subset(imputed_data, imputed_data[[time_var]] == baseline_time)
  
  # Compute SD for each group (eng_cluster)
  sd_grp1 <- sd(baseline_data[baseline_data[[cluster_var]] == 1, outcome_var], na.rm = TRUE)
  sd_grp2 <- sd(baseline_data[baseline_data[[cluster_var]] == 2, outcome_var], na.rm = TRUE)

  # Return the results as a named list
  return(list(sd_grp1 = sd_grp1, sd_grp2 = sd_grp2))
}

# Documentation and efficiency considerations:
# 1. The function `compute_sd_baseline` is general and can be reused for any imputed dataset with the same structure.
# 2. Using `lapply` efficiently applies the function across all imputed datasets in mitml_list
# 3. The standard deviation calculations are done within a filtered subset of the data, ensuring we only compute SD at the baseline time point.
# 4. NA values are handled by setting `na.rm = TRUE` to ensure that missing values do not affect the SD computation. Nonetheless, there should be no missing values.
# 5. The results are saved as lists of named elements, making it easy to access `sd_grp1` and `sd_grp2` for each imputed dataset.

# Apply the function to each imputed dataset and save results
sd_results_oa <- lapply(mitml_list_oa, compute_sd_baseline)
sd_results_dass21 <- lapply(mitml_list_dass21, compute_sd_baseline)
sd_results_rr_neg <- lapply(mitml_list_rr_neg, compute_sd_baseline)
sd_results_rr_pos <- lapply(mitml_list_rr_pos, compute_sd_baseline)
sd_results_bbsiq <- lapply(mitml_list_bbsiq, compute_sd_baseline)

# Function to compute sample sizes for each group at baseline
compute_n_baseline <- function(imputed_data, cluster_var = "eng_cluster", time_var = "time", baseline_time = 0) {
  
  # Filter data for baseline time point
  baseline_data <- subset(imputed_data, imputed_data[[time_var]] == baseline_time)
  
  # Compute sample sizes for each group (eng_cluster)
  n_grp1 <- sum(baseline_data[[cluster_var]] == 1, na.rm = TRUE)
  n_grp2 <- sum(baseline_data[[cluster_var]] == 2, na.rm = TRUE)
  
  # Return the results as a named list
  return(list(n_grp1 = n_grp1, n_grp2 = n_grp2))
}

# Apply the function to each imputed dataset and save results
n_results_oa <- lapply(mitml_list_oa, compute_n_baseline)
n_results_dass21 <- lapply(mitml_list_dass21, compute_n_baseline)
n_results_rr_neg <- lapply(mitml_list_rr_neg, compute_n_baseline)
n_results_rr_pos <- lapply(mitml_list_rr_pos, compute_n_baseline)
n_results_bbsiq <- lapply(mitml_list_bbsiq, compute_n_baseline)

# Documentation and efficiency considerations:
# 1. The function `compute_n_baseline` is designed to calculate the sample sizes for each group at baseline.
# 2. Using `lapply` efficiently applies the function across all imputed datasets.
# 3. The results are saved as lists of named elements, making it easy to access `n_grp1` and `n_grp2` for each imputed dataset.

#    b. Using the groups’ SDs and their sample sizes “n_grp1” and “n_grp2” at baseline, 
#       compute the pooled SD across groups using the formula:
#       sd_pooled = sqrt(((n_grp1 - 1)*sd_grp1^2 + (n_grp2 - 1)*sd_grp2^2) / (n_grp1 + n_grp2 - 2))

# Function to compute pooled SD across groups at baseline
compute_pooled_sd <- function(sd_results, n_results) {
  
  # Extract values for SDs and sample sizes
  sd_grp1 <- sd_results$sd_grp1
  print(sd_grp1)
  sd_grp2 <- sd_results$sd_grp2
  print(sd_grp2)
  n_grp1 <- n_results$n_grp1
  print(n_grp1)
  n_grp2 <- n_results$n_grp2
  print(n_grp2)

  # Compute the pooled SD across groups
  sd_pooled <- sqrt(((n_grp1 - 1) * sd_grp1^2 + (n_grp2 - 1) * sd_grp2^2) / (n_grp1 + n_grp2 - 2))
  
  # Return the pooled SD
  return(sd_pooled)
}

# Apply the function to each imputed dataset and save results
pooled_sd_results_oa <- mapply(compute_pooled_sd, sd_results_oa, n_results_oa, SIMPLIFY = TRUE)
pooled_sd_results_dass21 <- mapply(compute_pooled_sd, sd_results_dass21, n_results_dass21, SIMPLIFY = TRUE)
pooled_sd_results_rr_neg <- mapply(compute_pooled_sd, sd_results_rr_neg, n_results_rr_neg, SIMPLIFY = TRUE)
pooled_sd_results_rr_pos <- mapply(compute_pooled_sd, sd_results_rr_pos, n_results_rr_pos, SIMPLIFY = TRUE)
pooled_sd_results_bbsiq <- mapply(compute_pooled_sd, sd_results_bbsiq, n_results_bbsiq, SIMPLIFY = TRUE)

# Documentation and efficiency considerations:
# 1. The function `compute_pooled_sd` computes the pooled standard deviation across groups at baseline using the formula provided.
# 2. `mapply` is used to efficiently apply the function to paired lists of SD and sample size results.
# 3. The output is a vector of pooled SD values for each imputed dataset.



# 2. Pool all of the “sd_grp1”, “sd_grp2”, and “sd_pooled” values across imputed datasets 
#    at baseline (time 0) by taking the average for each set of values:
#    a. sd_grp1_final = mean of sd_grp1 values across imputed datasets
#    b. sd_grp2_final = mean of sd_grp2 values across imputed datasets
#    c. sd_pooled_final = mean of sd_pooled values across imputed datasets

# Function to pool (average) the SD values across imputed datasets
compute_final_sd <- function(sd_values_list) {
  # Calculate the mean of the values across the imputed datasets
  sd_final <- mean(sd_values_list, na.rm = TRUE)
  
  # Return the final pooled SD
  return(sd_final)
}

# Compute the final averaged SD values across imputed datasets
sd_grp1_final_oa <- compute_final_sd(sapply(sd_results_oa, function(x) x$sd_grp1))
sd_grp2_final_oa <- compute_final_sd(sapply(sd_results_oa, function(x) x$sd_grp2))
sd_pooled_final_oa <- compute_final_sd(pooled_sd_results_oa)

sd_grp1_final_dass21 <- compute_final_sd(sapply(sd_results_dass21, function(x) x$sd_grp1))
sd_grp2_final_dass21 <- compute_final_sd(sapply(sd_results_dass21, function(x) x$sd_grp2))
sd_pooled_final_dass21 <- compute_final_sd(pooled_sd_results_dass21)

sd_grp1_final_rr_neg <- compute_final_sd(sapply(sd_results_rr_neg, function(x) x$sd_grp1))
sd_grp2_final_rr_neg <- compute_final_sd(sapply(sd_results_rr_neg, function(x) x$sd_grp2))
sd_pooled_final_rr_neg <- compute_final_sd(pooled_sd_results_rr_neg)

sd_grp1_final_rr_pos <- compute_final_sd(sapply(sd_results_rr_pos, function(x) x$sd_grp1))
sd_grp2_final_rr_pos <- compute_final_sd(sapply(sd_results_rr_pos, function(x) x$sd_grp2))
sd_pooled_final_rr_pos <- compute_final_sd(pooled_sd_results_rr_pos)

sd_grp1_final_bbsiq <- compute_final_sd(sapply(sd_results_bbsiq, function(x) x$sd_grp1))
sd_grp2_final_bbsiq <- compute_final_sd(sapply(sd_results_bbsiq, function(x) x$sd_grp2))
sd_pooled_final_bbsiq <- compute_final_sd(pooled_sd_results_bbsiq)

# Documentation and efficiency considerations:
# 1. The function `compute_final_sd` calculates the mean of a vector of SD values across imputed datasets.
# 2. `sapply` is used to extract the SD values (sd_grp1, sd_grp2) from the list of results for each imputed dataset.
# 3. The final pooled SD values are computed by averaging the SDs across all imputed datasets.
# 4. The output includes the final averaged SD for each group (sd_grp1_final, sd_grp2_final) and the pooled SD (sd_pooled_final).

# Specify the file name where you want to save the variables
save_file <- here("Scripts2","Data_Primary_Analysis","5_5_final_sd_values.RData")

# Save the variables to the file
save(sd_grp1_final_oa, sd_grp2_final_oa, sd_pooled_final_oa,
     sd_grp1_final_dass21, sd_grp2_final_dass21, sd_pooled_final_dass21,
     sd_grp1_final_rr_neg, sd_grp2_final_rr_neg, sd_pooled_final_rr_neg,
     sd_grp1_final_rr_pos, sd_grp2_final_rr_pos, sd_pooled_final_rr_pos,
     sd_grp1_final_bbsiq, sd_grp2_final_bbsiq, sd_pooled_final_bbsiq,
     file = save_file)

# Documentation:
# 1. The `save` function is used to store the specified variables into an RData file.
# 2. The file `final_sd_values.RData` can be loaded later to retrieve these variables.
