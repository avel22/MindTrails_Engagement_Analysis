### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is to calculate simple time effects for significant interaction terms

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
load(here("Data_Sensitivity_Analysis","7_1_mitml_pooled_results_2.RData"))


#--------------------------------------------------------------------------------#
# Probing significant interactions ----
#--------------------------------------------------------------------------------#
#See differences in slopes for negative interpretation bias for significant interactions


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

# #check df as if data was complete
model_clust1 <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
                    random = ~ t1 + t2 | participant_id,
                    data = mitml_list_rr_neg_e_cluster_1[[1]],
                    control = lmeControl(opt = c("optim"),
                                         msMaxIter = 1e9,
                                         niterEM = 1000),
                    method = "REML")
model_clust1[["fixDF"]][["terms"]]
#
model_clust2 <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
                    random = ~ t1 + t2 | participant_id,
                    data = mitml_list_rr_neg_e_cluster_2[[1]],
                    control = lmeControl(opt = c("optim"),
                                         msMaxIter = 1e9,
                                         niterEM = 1000),
                    method = "REML")

model_clust2[["fixDF"]][["terms"]]


df_eng_clust_1 <- c( 910,910,910,302)
df_eng_clust_2 <- c( 967,967,967,321)


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


#create bbsiq
#create bbsiq  with clusters
mitml_list_bbsiq_e_cluster_1 <- filter_imps_cluster(mitml_list_bbsiq_2, 1)
mitml_list_bbsiq_e_cluster_2 <- filter_imps_cluster(mitml_list_bbsiq_2, 2)

#check df as if data was complete
model_clust1_bbsiq <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
                    random = ~ t1 + t2 | participant_id,
                    data = mitml_list_bbsiq_e_cluster_1[[1]],
                    control = lmeControl(opt = c("optim")),
                    method = "REML")


model_clust1_bbsiq[["fixDF"]][["terms"]]
#
model_clust2_bbsiq <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
                          random = ~ t1 + t2 | participant_id,
                          data = mitml_list_bbsiq_e_cluster_2[[1]],
                          control = lmeControl(opt = c("optim"),
                                               msMaxIter = 1e9,
                                               niterEM = 1000),
                          method = "REML")

model_clust2_bbsiq[["fixDF"]][["terms"]]


df_eng_clust_1 <- c( 910,910,910,302)
df_eng_clust_2 <- c( 967,967,967,321)


modelList_bbsiq_lme_e_cluster_1 <-
  with(mitml_list_bbsiq_e_cluster_1,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_e_cluster_1<- testEstimates(modelList_bbsiq_lme_e_cluster_1, extra.pars = TRUE, df.com = df_eng_clust_1)
pooled_bbsiq_lme_e_cluster_1


modelList_bbsiq_lme_e_cluster_2 <-
  with(mitml_list_bbsiq_e_cluster_2,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_e_cluster_2<- testEstimates(modelList_bbsiq_lme_e_cluster_2, extra.pars = TRUE, df.com = df_eng_clust_2)
pooled_bbsiq_lme_e_cluster_2

save.image(file = here("Data_Sensitivity_Analysis","7_2_simple_time_effects_results.RData"))
