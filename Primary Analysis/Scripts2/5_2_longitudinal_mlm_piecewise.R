### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to run the MLM analysis on each of the imputed datasets and pool the results


#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer,lme4,psycho,afex,nlme,optimx,emmeans,gtsummary)

#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Scripts2","Data","outcomes_df_for_imputation.RData"))
session_outcomes$time <- 0:6

#imputed outcomes
mod0_columns <- c("imp_num","participant_id","time","t1","t2","outcome","cred_on","eng_cluster","gender_num","isOneType")



#OA
oa_imputed <- read.csv(here("Scripts2","Data","imputation","MOD0","OASIS","ActualImputation","OA_imps_mod0.csv"),header = F)
colnames(oa_imputed) <- mod0_columns
oa_imputed <- oa_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
oa_imputed$session_only <- oa_imputed$sessions

#DASS21
dass21_imputed <- read.csv(here("Scripts2","Data","imputation","MOD0","DASS21","ActualImputation","DASS21_imps_mod0.csv"),header = F)
colnames(dass21_imputed) <- mod0_columns
dass21_imputed <- dass21_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
dass21_imputed$session_only <- dass21_imputed$sessions

#RR_NEG
rr_neg_imputed <- read.csv(here("Scripts2","Data","imputation","MOD0","RR_NEG_BIAS","ActualImputation","RR_NEG_imps_mod0.csv"),header = F)
colnames(rr_neg_imputed) <- mod0_columns
rr_neg_imputed <- rr_neg_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_neg_imputed$session_only <- rr_neg_imputed$sessions

#RR_POS
rr_pos_imputed <- read.csv(here("Scripts2","Data","imputation","MOD0","RR_POS_BIAS","ActualImputation","RR_POS_imps_mod0.csv"),header = F)
colnames(rr_pos_imputed) <- mod0_columns
rr_pos_imputed <- rr_pos_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
rr_pos_imputed$session_only <- rr_pos_imputed$sessions

#BBSIQ
bbsiq_imputed <- read.csv(here("Scripts2","Data","imputation","MOD0","BBSIQ","ActualImputation","BBSIQ_imps_mod0.csv"),header = F)
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
# Analyzing and pooling imputed datasets ----
#--------------------------------------------------------------------------------#
#grand mean centering credibility online for each pooled data
mods_on_imps <- function(imputationList){
  for(i in 1:length(imputationList)){
    #center credibility online
    imputationList[[i]] <- imputationList[[i]] %>% mutate(cred_on_gmc =cred_on-mean(cred_on))
    #factor engagement cluster
    imputationList[[i]]$eng_cluster <- as.factor(imputationList[[i]]$eng_cluster)

    #sum coding engagement cluster and factoring
    imputationList[[i]] <- imputationList[[i]] %>% mutate(eng_cluster_sum_code = ifelse(eng_cluster == 1,-1,1))

  }
 return(imputationList)
}

mitml_list_oa_2 <- mods_on_imps(mitml_list_oa)
mitml_list_dass21_2 <-mods_on_imps(mitml_list_dass21)
mitml_list_rr_neg_2 <-mods_on_imps(mitml_list_rr_neg)
mitml_list_rr_pos_2 <-mods_on_imps(mitml_list_rr_pos)
mitml_list_bbsiq_2 <-mods_on_imps(mitml_list_bbsiq)

# 
# mitml_list_rr_neg_3 <-mods_on_imps(mitml_list_rr_neg)
# 
# View(mitml_list_rr_neg_3[[1]])
# 
# contrasts(as.factor(mitml_list_rr_neg_3[[1]]$eng_cluster_sum_code))
# 
# imp1 <- mitml_list_rr_neg_3[[1]]
# contrasts(imp1$eng_cluster) <- rbind(-1, 1)
# colnames(contrasts(imp1$eng_cluster)) <- levels(imp1$eng_cluster)[8]
# 
# lme4::lmer(outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc +(t1 + t2 | participant_id),data = imp1,control = lmerControl(optimizer = "nlminbwrap",optCtrl = list(maxfun= 1e9)))

#set df as though data were complete
#model with 7 timepoints (OASIS outcome)
# df_test2 <- lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#     random = ~ t1 + t2 | participant_id,
#     data = mitml_list_oa_2[[1]],
#     control = lmeControl(opt = c("optim")),
#     method = "REML")

df_7_t <- c(4178,4178,4178,694,694,4178,4178)


#model with 4 timespoints (DASS21, RR_NEG_BIAS, RR_POS_BIAS, BBSIQ)

# df_test <- lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#     random = ~ t1 + t2 | participant_id,
#     data = mitml_list_rr_pos_2[[1]],
#     control = lmeControl(opt = c("optim")),
#     method = "REML")

df_4_t <- c(2087,2087,2087,694,694,2087,2087)


#OA
#MOD0, no control parameter modifications
#Converged, no warning messages
modelList_oa_lme_mod0 <- 
  with(mitml_list_oa_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           method = "REML"))
pooled_oa_lme_mod0<- testEstimates(modelList_oa_lme_mod0, extra.pars = TRUE, df.com = df_7_t)
pooled_oa_lme_mod0


#MOD1, control parameter modification control = lmeControl(opt = c("optim")
#Converged, no warning messages
modelList_oa_lme_mod1 <- 
  with(mitml_list_oa_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_oa_lme_mod1<- testEstimates(modelList_oa_lme_mod1, extra.pars = TRUE, df.com = df_7_t)
pooled_oa_lme_mod1

#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
#Converged, no warning messages
modelList_oa_lme_mod2 <- 
  with(mitml_list_oa_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9),
           method = "REML"))

pooled_oa_lme_mod2<- testEstimates(modelList_oa_lme_mod2, extra.pars = TRUE, df.com = df_7_t)
pooled_oa_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
#Converged, no warning messages
modelList_oa_lme <- 
  with(mitml_list_oa_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))
pooled_oa_lme<- testEstimates(modelList_oa_lme, extra.pars = TRUE, df.com = df_7_t)
pooled_oa_lme


#DASS21
#MOD0, no control parameter modifications
# Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#                        nlminb problem, convergence error code = 1
#                      message = iteration limit reached without convergence (10)
# modelList_dass21 <- 
#   with(mitml_list_dass21_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster +      cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            method = "REML"))
# pooled <- testEstimates(modelList_dass21, extra.pars = TRUE, df.com = df_4_t)

#MOD1 control parameter modification control = lmeControl(opt = c("optim")
modelList_dass21_lme_mod1 <-
  with(mitml_list_dass21_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))
pooled_dass21_lme_mod1 <- testEstimates(modelList_dass21_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
pooled_dass21_lme_mod1

#MOD1_1, control parameter modification control = lmeControl(msMaxIter = 1e9)
# Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#                        nlminb problem, convergence error code = 1
#                      message = function evaluation limit reached without convergence (9)
# modelList_dass21 <-
#   with(mitml_list_dass21_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster +      cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(msMaxIter = 1e9),
#            method = "REML"))
# pooled <- testEstimates(modelList_dass21, extra.pars = TRUE, df.com = df_4_t)

#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
modelList_dass21_lme_mod2 <-
  with(mitml_list_dass21_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9),
           method = "REML"))
pooled_dass21_lme_mod2 <- testEstimates(modelList_dass21_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
pooled_dass21_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_dass21_lme <-
  with(mitml_list_dass21_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))
pooled_dass21_lme <- testEstimates(modelList_dass21_lme, extra.pars = TRUE, df.com = df_4_t)
pooled_dass21_lme

#RR_NEG_BIAS
#MOD0, no control parameter modifications
# Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#                        nlminb problem, convergence error code = 1
#                      message = iteration limit reached without convergence (10)
# modelList_rr_neg <-
#   with(mitml_list_rr_neg_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster +      cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#           
#            method = "REML"))
# pooled_rr_neg<- testEstimates(modelList_dass21, extra.pars = TRUE, df.com = df_4_t)

#MOD1, control parameter modification control = lmeControl(opt = c("optim")
# Error in MEestimate(lmeSt, grps) : 
#   Singularity in backsolve at level 0, block 1
# modelList_rr_neg <-
#   with(mitml_list_rr_neg_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster +      cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim")),
#            method = "REML"))
# pooled_rr_neg<- testEstimates(modelList_dass21, extra.pars = TRUE, df.com = df_4_t)

#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
# Error in MEestimate(lmeSt, grps) : 
#   Singularity in backsolve at level 0, block 1
# modelList_rr_neg_lme <-
#   with(mitml_list_rr_neg_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster +      cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim"),msMaxIter = 1e9),
#            method = "REML"))
# pooled_rr_neg<- testEstimates(modelList_rr_neg_lme, extra.pars = TRUE, df.com = df_4_t)

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_rr_neg_lme <-
  with(mitml_list_rr_neg_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))

pooled_rr_neg_lme<- testEstimates(modelList_rr_neg_lme, extra.pars = TRUE, df.com = df_4_t)
pooled_rr_neg_lme

#MOD4, control parameter modification niterEM = 1000)
# Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#                        nlminb problem, convergence error code = 1
#                      message = iteration limit reached without convergence (10)

# modelList_rr_neg_lme_mod4 <-
#   with(mitml_list_rr_neg_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(niterEM = 1000),
#            method = "REML"))
# pooled_rr_neg_lme_mod4<- testEstimates(modelList_rr_neg_lme_mod4, extra.pars = TRUE, df.com = df_4_t)
# modelList_rr_neg_lme_mod4



#RR_POS_BIAS
#Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#nlminb problem, convergence error code = 1
#message = iteration limit reached without convergence (10)
#MOD0, no control parameter modifications
# modelList_rr_pos_lme_mod0 <-
#   with(mitml_list_rr_pos_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            method = "REML"))

#MOD1, control parameter modification control = lmeControl(opt = c("optim")
#Converged
modelList_rr_pos_lme_mod1 <-
  with(mitml_list_rr_pos_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_rr_pos_lme_mod1<- testEstimates(modelList_rr_pos_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
pooled_rr_pos_lme_mod1
#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
modelList_rr_pos_lme_mod2 <-
  with(mitml_list_rr_pos_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9),
           method = "REML"))

pooled_rr_pos_lme_mod2<- testEstimates(modelList_rr_pos_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
pooled_rr_pos_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_rr_pos_lme <-
  with(mitml_list_rr_pos_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))

pooled_rr_pos_lme<- testEstimates(modelList_rr_pos_lme, extra.pars = TRUE, df.com = df_4_t)
pooled_rr_pos_lme

#BBSIQ
#MOD0, no control parameter modifications
#Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#nlminb problem, convergence error code = 1
#message = iteration limit reached without convergence (10)
# modelList_bbsiq_lme_mod0 <-
#   with(mitml_list_bbsiq_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            method = "REML"))

#MOD1, control parameter modification control = lmeControl(opt = c("optim")
modelList_bbsiq_lme_mod1 <-
  with(mitml_list_bbsiq_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_mod1<- testEstimates(modelList_bbsiq_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
pooled_bbsiq_lme_mod1


#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
#Warning message:
#In UseMethod("depth") :
#  no applicable method for 'depth' applied to an object of class "NULL"
#Re run and message did not appear
modelList_bbsiq_lme_mod2 <-
  with(mitml_list_bbsiq_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9),
           method = "REML"))

pooled_bbsiq_lme_mod2<- testEstimates(modelList_bbsiq_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
pooled_bbsiq_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_bbsiq_lme <-
  with(mitml_list_bbsiq_2,
       lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim"),
                                msMaxIter = 1e9,
                                niterEM = 1000),
           method = "REML"))

pooled_bbsiq_lme<- testEstimates(modelList_bbsiq_lme, extra.pars = TRUE, df.com = df_4_t)
pooled_bbsiq_lme


# Error in lme.formula(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster +  : 
#                        nlminb problem, convergence error code = 1
#                      message = iteration limit reached without convergence (10)
#MOD4 control parameter modification niterEM = 1000)
# modelList_bbsiq_lme_mod4 <-
#   with(mitml_list_bbsiq_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(niterEM = 1000),
#            method = "REML"))
# pooled_bbsiq_lme_mod4<- testEstimates(modelList_bbsiq_lme_mod4, extra.pars = TRUE, df.com = df_4_t)
# pooled_bbsiq_lme_mod4


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
# model_clust1 <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
#     random = ~ t1 + t2 | participant_id,
#     data = mitml_list_rr_neg_e_cluster_1[[1]],
#     control = lmeControl(opt = c("optim"),
#                          msMaxIter = 1e9,
#                          niterEM = 1000),
#     method = "REML")
# model_clust1[["fixDF"]][["terms"]]
# # 
# model_clust2 <- lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
#                     random = ~ t1 + t2 | participant_id,
#                     data = mitml_list_rr_neg_e_cluster_2[[1]],
#                     control = lmeControl(opt = c("optim"),
#                                          msMaxIter = 1e9,
#                                          niterEM = 1000),
#                     method = "REML")
# 
# model_clust2[["fixDF"]][["terms"]]


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


modelList_bbsiq_lme_e_cluster_2 <-
  with(mitml_list_bbsiq_e_cluster_2,
       lme(fixed = outcome ~ t1 + t2 + cred_on_gmc ,
           random = ~ t1 + t2 | participant_id,
           control = lmeControl(opt = c("optim")),
           method = "REML"))

pooled_bbsiq_lme_e_cluster_2<- testEstimates(modelList_bbsiq_lme_e_cluster_2, extra.pars = TRUE, df.com = df_eng_clust_2)
pooled_bbsiq_lme_e_cluster_2

save.image(file = here("Scripts2","Data","pooledResults7LME.RData"))
