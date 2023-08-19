#check to make sure that cluster 1 is lees time spent

#load participant clustering from May 1 2022
load(here("Scripts2","Data2","participant_cluster.RData"))

eng_df_cluster.m <- melt(as.data.frame(cluster_summary),id.vars=c('participant_id',"cluster"), measure.vars=colnames(cluster_summary[,-c(1,2,3,4,5,19)]))

ggplot(eng_df_cluster.m, aes(log(value), variable))+geom_boxplot(aes(fill = as.factor(cluster)))

table(participant_cluster$cluster)


#session outcomes
load(here("Scripts2","Data2","Test699","outcomes_df_for_imputation.RData"))
session_outcomes$time <- 0:6

#imputed outcomes
mod0_columns <- c("imp_num","participant_id","time","t1","t2","outcome","cred_on","eng_cluster","gender_num","isOneType")



#OA
oa_imputed <- read.csv(here("Scripts2","Data2","BLIMP_IMPUTATION","MOD0","OASIS","ActualImputation","OA_imps_mod0.csv"),header = F)
colnames(oa_imputed) <- mod0_columns
oa_imputed <- oa_imputed  %>% left_join(select(session_outcomes,time,sessions), by ="time")
oa_imputed$session_only <- oa_imputed$sessions

oa_imputed_1 <- oa_imputed %>% filter(imp_num == 1) %>% select(participant_id,eng_cluster) %>% distinct(participant_id,eng_cluster)

cluster_check <- oa_imputed_1 %>% left_join(participant_cluster,by = c("participant_id"))

table(cluster_check$eng_cluster)
table(cluster_check$cluster)

cluster_check$check <- cluster_check$cluster==cluster_check$eng_cluster


final_check <- cluster_summary %>% left_join(select(oa_imputed_1,participant_id,eng_cluster),by = c("participant_id"))

table(final_check$cluster)
table(final_check$eng_cluster)

eng_df_cluster.m.2 <- melt(as.data.frame(final_check),id.vars=c('participant_id',"eng_cluster"), measure.vars=colnames(final_check[,-c(1,2,3,4,5,19,20)]))

ggplot(eng_df_cluster.m.2, aes(log(value), variable))+geom_boxplot(aes(fill = as.factor(eng_cluster)))

table(participant_cluster$cluster)

