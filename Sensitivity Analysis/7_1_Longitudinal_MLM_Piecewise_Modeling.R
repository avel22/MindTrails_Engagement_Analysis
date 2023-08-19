### Written by: Ángel Vela and Jeremy Eberle
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to run the MLM analysis on each of the imputed datasets and pool the results
#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,here,mitml,ggplot2)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Data","RData","6_3_imputed_outcomes_mitml_lists.RData"))

#--------------------------------------------------------------------------------#
# Analyzing and pooling imputed datasets ----
#--------------------------------------------------------------------------------#
#add label,  1=="More Time Spent", 2 == "Less Time Spent"

#grand mean centering credibility online for each pooled data
mods_on_imps <- function(imputationList){
  for(i in 1:length(imputationList)){
    #center credibility online
    imputationList[[i]] <- imputationList[[i]] %>% mutate(cred_on_gmc =cred_on-mean(cred_on))
    #add label,  1=="More Time Spent", 2 == "Less Time Spent"
    imputationList[[i]]<- imputationList[[i]] %>% mutate(eng_label = ifelse(eng_cluster==1, "More Time Spent","Less Time Spent"))
    
    #factor engagement cluster
    imputationList[[i]]$eng_cluster <- as.factor(imputationList[[i]]$eng_cluster)
  }
  
  #relevel so that less time spent "2" is reference group
  imputationList[[i]] <- within(imputationList[[i]], eng_cluster <- relevel(eng_cluster, ref = 2))
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
df_test2 <- lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
    random = ~ t1 + t2 | participant_id,
    data = mitml_list_oa_2[[1]],
    control = lmeControl(opt = c("optim")),
    method = "REML")
df_test2[["fixDF"]][["X"]]
df_7_t <- c(3758,3758,3758,624,624,3758,3758)


#model with 4 timespoints (DASS21, RR_NEG_BIAS, RR_POS_BIAS, BBSIQ)

df_test <- lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
    random = ~ t1 + t2 | participant_id,
    data = mitml_list_rr_neg_2[[1]],
    control = lmeControl(opt = c("optim")),
    method = "REML")

df_test[["fixDF"]][["X"]]

df_4_t <- c(1877,1877,1877,624,624,1877,1877)


#OA
#MOD0, no control parameter modifications
#Converged, no warning messages
# modelList_oa_lme_mod0 <- 
#   with(mitml_list_oa_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            method = "REML"))
# pooled_oa_lme_mod0<- testEstimates(modelList_oa_lme_mod0, extra.pars = TRUE, df.com = df_7_t)
# pooled_oa_lme_mod0


#MOD1, control parameter modification control = lmeControl(opt = c("optim")
#Converged, no warning messages
# modelList_oa_lme_mod1 <- 
#   with(mitml_list_oa_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim")),
#            method = "REML"))
# 
# pooled_oa_lme_mod1<- testEstimates(modelList_oa_lme_mod1, extra.pars = TRUE, df.com = df_7_t)
# pooled_oa_lme_mod1

#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
#Converged, no warning messages
# modelList_oa_lme_mod2 <- 
#   with(mitml_list_oa_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim"),
#                                 msMaxIter = 1e9),
#            method = "REML"))
# 
# pooled_oa_lme_mod2<- testEstimates(modelList_oa_lme_mod2, extra.pars = TRUE, df.com = df_7_t)
# pooled_oa_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
#Converged, no warning messages
modelList_oa_lme <- 
  with(mitml_list_oa_2,
       lme(fixed = outcome ~ t1 + t2 + eng_label + t1 * eng_label + t2 * eng_label + cred_on_gmc ,
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
# modelList_dass21_lme_mod1 <-
#   with(mitml_list_dass21_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim")),
#            method = "REML"))
# pooled_dass21_lme_mod1 <- testEstimates(modelList_dass21_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
# pooled_dass21_lme_mod1

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
# modelList_dass21_lme_mod2 <-
#   with(mitml_list_dass21_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim"),
#                                 msMaxIter = 1e9),
#            method = "REML"))
# pooled_dass21_lme_mod2 <- testEstimates(modelList_dass21_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
# pooled_dass21_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_dass21_lme <-
  with(mitml_list_dass21_2,
       lme(fixed = outcome ~ t1 + t2 + eng_label + t1 * eng_label + t2 * eng_label + cred_on_gmc ,
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
       lme(fixed = outcome ~ t1 + t2 + eng_label + t1 * eng_label + t2 * eng_label + cred_on_gmc ,
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
# modelList_rr_pos_lme_mod1 <-
#   with(mitml_list_rr_pos_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim")),
#            method = "REML"))
# 
# pooled_rr_pos_lme_mod1<- testEstimates(modelList_rr_pos_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
# pooled_rr_pos_lme_mod1
#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
# modelList_rr_pos_lme_mod2 <-
#   with(mitml_list_rr_pos_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim"),
#                                 msMaxIter = 1e9),
#            method = "REML"))
# 
# pooled_rr_pos_lme_mod2<- testEstimates(modelList_rr_pos_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
# pooled_rr_pos_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_rr_pos_lme <-
  with(mitml_list_rr_pos_2,
       lme(fixed = outcome ~ t1 + t2 + eng_label + t1 * eng_label + t2 * eng_label + cred_on_gmc ,
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
# modelList_bbsiq_lme_mod1 <-
#   with(mitml_list_bbsiq_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim")),
#            method = "REML"))
# 
# pooled_bbsiq_lme_mod1<- testEstimates(modelList_bbsiq_lme_mod1, extra.pars = TRUE, df.com = df_4_t)
# pooled_bbsiq_lme_mod1


#MOD2, control parameter modification control = lmeControl(opt = c("optim"),msMaxIter = 1e9)
#Warning message:
#In UseMethod("depth") :
#  no applicable method for 'depth' applied to an object of class "NULL"
#Re run and message did not appear
# modelList_bbsiq_lme_mod2 <-
#   with(mitml_list_bbsiq_2,
#        lme(fixed = outcome ~ t1 + t2 + eng_cluster + t1 * eng_cluster + t2 * eng_cluster + cred_on_gmc ,
#            random = ~ t1 + t2 | participant_id,
#            control = lmeControl(opt = c("optim"),
#                                 msMaxIter = 1e9),
#            method = "REML"))
# 
# pooled_bbsiq_lme_mod2<- testEstimates(modelList_bbsiq_lme_mod2, extra.pars = TRUE, df.com = df_4_t)
# pooled_bbsiq_lme_mod2

#MOD3, control parameter modification control = lmeControl(opt = c("optim"), msMaxIter = 1e9, niterEM = 1000)
modelList_bbsiq_lme <-
  with(mitml_list_bbsiq_2,
       lme(fixed = outcome ~ t1 + t2 + eng_label + t1 * eng_label + t2 * eng_label + cred_on_gmc ,
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


save.image(file = here("Data","RData","7_1_mitml_pooled_results_2.RData"))