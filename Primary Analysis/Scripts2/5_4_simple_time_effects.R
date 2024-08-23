### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to calcualte simple time effects for significant interaction terms

#--------------------------------------------------------------------------------#
# Probing significant interactions ----
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer,lme4,psycho,afex,nlme,optimx,emmeans,gtsummary)

#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Scripts2","Data_Primary_Analysis","pooledResults7LME.RData"))


#--------------------------------------------------------------------------------#
# Probing significant interactions ----
#--------------------------------------------------------------------------------#
#See differences in slopes for negative interpretation bias for significant interactions and BBSIQ

#filter for engagement cluster
filter_imps_cluster <- function(imputationList,  cluster){
  for(i in 1:length(imputationList)){
    #filter for cluster
    imputationList[[i]] <- imputationList[[i]] %>% filter(eng_cluster == cluster)
  }
  return(imputationList)
}


#create rr_neg_bias with clusters
mitml_list_rr_neg_e_cluster_1 <- filter_imps_cluster(mitml_list_rr_neg_2, 1)
mitml_list_rr_neg_e_cluster_2 <- filter_imps_cluster(mitml_list_rr_neg_2, 2)



df_eng_clust_1 <- c( 1156,1156,1156,384)
df_eng_clust_2 <- c( 931,931,931,3019)


modelList_rr_neg_lme_e_cluster_1 <-
  with(mitml_list_rr_neg_e_cluster_1,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))

pooled_rr_neg_lme_e_cluster_1<- testEstimates(modelList_rr_neg_lme_e_cluster_1, extra.pars = TRUE, df.com = df_eng_clust_1)

pooled_rr_neg_lme_e_cluster_1
conf_int_rr_neg_grp1 <- confint.mitml.testEstimates(pooled_rr_neg_lme_e_cluster_1)



modelList_rr_neg_lme_e_cluster_2 <-
  with(mitml_list_rr_neg_e_cluster_2,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))

pooled_rr_neg_lme_e_cluster_2<- testEstimates(modelList_rr_neg_lme_e_cluster_2, extra.pars = TRUE, df.com = df_eng_clust_2)
pooled_rr_neg_lme_e_cluster_2
conf_int_rr_neg_grp2<- confint.mitml.testEstimates(pooled_rr_neg_lme_e_cluster_2)



#create bbsiq
#create bbsiq  with clusters
mitml_list_bbsiq_e_cluster_1 <- filter_imps_cluster(mitml_list_bbsiq_2, 1)
mitml_list_bbsiq_e_cluster_2 <- filter_imps_cluster(mitml_list_bbsiq_2, 2)

# #check df as if data was complete
# model_clust1 <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
#     random = ~ t1 + t2 | participant_id,
#     data = mitml_list_bbsiq_e_cluster_1[[1]],
#     control = lmeControl(opt = c("optim")),
#     method = "REML")
# 
# 
# model_clust1[["fixDF"]][["terms"]]
# # 
# model_clust2_bbsiq <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
#                     random = ~ t1 + t2 | participant_id,
#                     data = mitml_list_bbsiq_e_cluster_2[[1]],
#                     control = lmeControl(opt = c("optim"),
#                                          msMaxIter = 1e9,
#                                          niterEM = 1000),
#                     method = "REML")
# 
# model_clust2_bbsiq[["fixDF"]][["terms"]]


df_eng_clust_1 <- c( 1156,1156,1156,384)
df_eng_clust_2 <- c( 931,931,931,3019)


modelList_bbsiq_lme_e_cluster_1 <-
  with(mitml_list_bbsiq_e_cluster_1,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_e_cluster_1<- testEstimates(modelList_bbsiq_lme_e_cluster_1, extra.pars = TRUE, df.com = df_eng_clust_1)

pooled_bbsiq_lme_e_cluster_1
conf_int_bbsiq_grp1 <- confint.mitml.testEstimates(pooled_bbsiq_lme_e_cluster_1)


modelList_bbsiq_lme_e_cluster_2 <-
  with(mitml_list_bbsiq_e_cluster_2,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_e_cluster_2<- testEstimates(modelList_bbsiq_lme_e_cluster_2, extra.pars = TRUE, df.com = df_eng_clust_2)
pooled_bbsiq_lme_e_cluster_2
conf_int_bbsiq_grp2 <- confint.mitml.testEstimates(pooled_bbsiq_lme_e_cluster_2)

# Specify the file name where you want to save the variables
save_file <-here("Scripts2","Data_Primary_Analysis","5_4_pooled_results_and_confidence_intervals_bbsiq_rr_neg.RData")

# Save the specified variables to the file
save(
  # Pooled results and confidence intervals for rr_neg
  pooled_rr_neg_lme_e_cluster_1, conf_int_rr_neg_grp1,
  pooled_rr_neg_lme_e_cluster_2, conf_int_rr_neg_grp2,
  
  # Pooled results and confidence intervals for bbsiq
  pooled_bbsiq_lme_e_cluster_1, conf_int_bbsiq_grp1,
  pooled_bbsiq_lme_e_cluster_2, conf_int_bbsiq_grp2,
  
  file = save_file
)

# E. Formulas for Within-Group Effect Size

# To compute the within-group effect size, we need each group’s SD (referred to as “sd_grp_final” 
# in the formulas below, but this will be replaced with “sd_grp1_final” or “sd_grp2_final” 
# depending on the group) and the beta estimates from restructured Table 4. 

# Let’s refer to the beta estimates as follows:
# - b_time_tr = beta estimate for the simple effect of timeTR within the given group 
#               (called “timeTR” in restructured Table 4)
# - b_time_fu = beta estimate for the simple effect of timeFU within the given group 
#               (called “timeFU” in restructured Table 4)

# With this information, we can compute the within-group effect sizes at Session 5 and follow-up:

# Formula to compute within-group effect size at Session 5:
# wth_d_s5 = (b_time_tr*5 + b_time_fu*0) / sd_grp_final

# Formula to compute within-group effect size at follow-up:
# wth_d_fu = (b_time_tr*5 + b_time_fu*1) / sd_grp_final

# Function to compute within-group effect sizes at Session 5 and follow-up
# Load the saved variables from the RData file
load(here("Scripts2","Data_Primary_Analysis","5_5_final_sd_values.RData"))

# Now, the variables (e.g., sd_grp1_final_oa, sd_grp2_final_oa, etc.) are available for use

compute_within_group_effect_size <- function(b_time_tr, b_time_fu, sd_grp_final) {
  
  # Compute within-group effect size at Session 5
  wth_d_s5 <- (b_time_tr * 5 + b_time_fu * 0) / sd_grp_final
  
  # Compute within-group effect size at follow-up
  wth_d_fu <- (b_time_tr * 5 + b_time_fu * 1) / sd_grp_final
  
  # Return the results as a named list
  return(list(wth_d_s5 = wth_d_s5, wth_d_fu = wth_d_fu))
}

# bbsiq
b_time_tr_grp1_bbsiq <- pooled_bbsiq_lme_e_cluster_1$estimates[2]

b_time_fu_grp1_bbsiq <- pooled_bbsiq_lme_e_cluster_1$estimates[3]

b_time_tr_grp2_bbsiq <- pooled_bbsiq_lme_e_cluster_2$estimates[2]
  
b_time_fu_grp2_bbsiq <- pooled_bbsiq_lme_e_cluster_2$estimates[3]
  
within_group_effect_size_grp1_bbsiq <- compute_within_group_effect_size(b_time_tr_grp1_bbsiq, b_time_fu_grp1_bbsiq, sd_grp1_final_bbsiq)

within_group_effect_size_grp2_bbsiq <- compute_within_group_effect_size(b_time_tr_grp2_bbsiq, b_time_fu_grp2_bbsiq, sd_grp2_final_bbsiq)


# rr neg

b_time_tr_grp1_rr_neg <- pooled_rr_neg_lme_e_cluster_1$estimates[2]

b_time_fu_grp1_rr_neg <- pooled_rr_neg_lme_e_cluster_1$estimates[3]

b_time_tr_grp2_rr_neg <- pooled_rr_neg_lme_e_cluster_2$estimates[2]

b_time_fu_grp2_rr_neg <- pooled_rr_neg_lme_e_cluster_2$estimates[3]

within_group_effect_size_grp1_rr_neg <- compute_within_group_effect_size(b_time_tr_grp1_rr_neg,b_time_fu_grp1_rr_neg,sd_grp1_final_rr_neg)

within_group_effect_size_grp2_rr_neg <- compute_within_group_effect_size(b_time_tr_grp2_rr_neg,b_time_fu_grp2_rr_neg,sd_grp2_final_rr_neg)


# Specify the file name where you want to save the variables
save_file <- here("Scripts2","Data_Primary_Analysis","5_4_within_group_effect_sizes.RData")

# Save the specified variables to the file
save(
  # bbsiq variables
  b_time_tr_grp1_bbsiq, b_time_fu_grp1_bbsiq, b_time_tr_grp2_bbsiq, b_time_fu_grp2_bbsiq,
  within_group_effect_size_grp1_bbsiq, within_group_effect_size_grp2_bbsiq,
  
  # rr neg variables
  b_time_tr_grp1_rr_neg, b_time_fu_grp1_rr_neg, b_time_tr_grp2_rr_neg, b_time_fu_grp2_rr_neg,
  within_group_effect_size_grp1_rr_neg, within_group_effect_size_grp2_rr_neg,
  
  file = save_file
)

# Documentation:
# 1. The `save` function is used to store the specified variables into an RData file.
# 2. The file `within_group_effect_sizes.RData` can be loaded later to retrieve these variables.


# Documentation and efficiency considerations:
# 1. The function `compute_within_group_effect_size` calculates the within-group effect sizes at Session 5 and follow-up.
# 2. Inputs are the beta estimates (`b_time_tr`, `b_time_fu`) and the final SD for the group (`sd_grp_final`).
# 3. The results are returned as a list containing the effect sizes at Session 5 (`wth_d_s5`) and follow-up (`wth_d_fu`).
