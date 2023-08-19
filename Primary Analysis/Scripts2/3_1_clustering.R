### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to cluster participants based on engagement metrics and generate engagement labels


#--------------------------------------------------------------------------------
# loading libraries ----
#--------------------------------------------------------------------------------
#libraries 
pacman::p_load(tidyverse,here,Hmisc,cluster,factoextra,mclust,mixtools,Rtsne,NbClust,reshape2,dendextend,clValid,cowplot,rstatix, hrbrthemes,viridis)

#--------------------------------------------------------------------------------
# loading the engagement metrics  ----
#--------------------------------------------------------------------------------
load(here("Scripts2","Data2","Test699","E_M2_1_Winsor.RData"))

eng_met <- engagement_metrics_winsor

#--------------------------------------------------------------------------------
# modifying engagement metrics for transformation  ----
#--------------------------------------------------------------------------------
#add 0.001 to features that have 0 values to preform log transformation
eng_met$T2_sec_mean_scenario_rt_across_sessions_winsor <- eng_met$T2_sec_mean_scenario_rt_across_sessions_winsor+0.001
eng_met$T3_min_time_spent_lemon_winsor <- eng_met$T3_min_time_spent_lemon_winsor+0.001
eng_met$T3_2_min_time_spent_imagine_winsor <- eng_met$T3_2_min_time_spent_imagine_winsor+0.001

#select columns of interest
eng_num <- eng_met %>% select(participant_id, completionRate,T2_sec_mean_scenario_rt_across_sessions_winsor,T3_min_time_spent_lemon_winsor,T3_2_min_time_spent_imagine_winsor,grep("T4_",names(eng_met))) %>% select(-T4_min_mean_assessment_time_bbsiq_winsor)

colnames(eng_num)

#apply log transformation
eng_num_log <- eng_num
eng_num_log[-c(1,2)] <- lapply(eng_num_log[-c(1,2)],log)
#eng_num_log[-c(1)] <- lapply(eng_num_log[-c(1)],log)



#scable variables by subtracting the mean and dividing by the standard deviation
eng_num_log_scale <- eng_num_log
eng_num_log_scale[-1]<- scale(eng_num_log_scale[-1])

#take a look at summary statistics of orginial data
summary(eng_num[-1])

#take a look at histograms for origina data, log data, and log scaled data
hist.data.frame(eng_num[-1])
hist.data.frame(eng_num_log[-1])
hist.data.frame(eng_num_log_scale[-1])
dev.off()

#--------------------------------------------------------------------------------
# Cluster Validation ----
#--------------------------------------------------------------------------------
#https://www.jstatsoft.org/article/view/v025i04
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7932

eng_df <- as.data.frame(eng_num_log_scale[-1])

# hierarchical = agnes
clmethods <- c("kmeans","pam","hierarchical")
# Compute internal validation measures
#Minimize connectivity, maximize both Dunn index and the silhouette width
set.seed(123)
internal_cluster <- clValid(eng_df, nClust = 2:4, clMethods = clmethods,
                            validation = "internal",maxitems = 700,
                            metric = "euclidean", method = "ward")
#summary results
summary(internal_cluster)


#plot graphs
png(here("Scripts2","Tables_Figures","Figures","internal_validation_cluster.png"), units="in", width=9, height=5, res=300)
op <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(internal_cluster, legend = FALSE)
plot(nClusters(internal_cluster), measures(internal_cluster, "Dunn")[, , 1], type = "n", axes = F, xlab = "", ylab = "")
legend("center", clusterMethods(internal_cluster), col = 1:4, lty = 1:4,pch = paste(1:4))
par(op)
dev.off()


# Compute stability validation measures
# measures should be minimized
set.seed(123)
stability_cluster <- clValid(eng_df, nClust = 2:4, clMethods = clmethods,
                             validation = "stability",maxitems = 700,
                             metric = "euclidean")

#summary results
optimalScores(stability_cluster)
summary(stability_cluster)

#plot graphs
png(here("Scripts2","Tables_Figures","Figures","stability_validation_cluster.png"), units="in", width=9, height=5, res=300)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(stability_cluster, measure = c("APN", "AD", "ADM","FOM"), legend = FALSE)
#plot(nClusters(stability_cluster), measures(stability_cluster, "APN")[, , 1], type = "n",axes = F, xlab = "", ylab = "")
par(op)
legend("center", clusterMethods(stability_cluster), col = 1:9, lty = 1:9,pch = paste(1:9))
dev.off()


#--------------------------------------------------------------------------------
# clustering ----
#--------------------------------------------------------------------------------
#df to store participant ids and cluster assignment
p_clusters <- tibble(participant_id = eng_num$participant_id)

#Links
#https://rpkgs.datanovia.com/factoextra/reference/fviz_cluster.html


#--------------------------------------------------------------------------------
# partitioning around mediods (PAM) 2,3,4 clusters ----
#--------------------------------------------------------------------------------
pam_model <- function(k,eng_transformed,eng_original){
  
  pam_eng = cluster::pam(eng_transformed, k = k, metric = "euclidean")
  
  pam_summary <- eng_original %>%
    cbind(cluster = pam_eng$clustering) %>%
    group_by(cluster) %>%
    do(cluster_summary = summary(.))
  
  pam_box <- eng_original %>%
    cbind(cluster = pam_eng$clustering) %>%
    group_by(cluster)
  
  
  print(table(pam_box$cluster))
  
  info <- list(pam_eng = pam_eng,pam_summary = pam_summary,pam_box = pam_box)
  
  return(info)
}

#create pam clusters
pam_model_2 <-pam_model(2,eng_transformed = eng_df,eng_original = eng_num)
p_clusters$pam_2 <- pam_model_2$pam_box$cluster

pam_model_3 <-pam_model(3,eng_transformed = eng_df,eng_original = eng_num)
p_clusters$pam_3 <- pam_model_3$pam_box$cluster

pam_model_4 <-pam_model(4,eng_transformed = eng_df,eng_original = eng_num)
p_clusters$pam_4 <- pam_model_4$pam_box$cluster

#visualize clusters
p1_pam <- fviz_cluster(object = pam_model_2$pam_eng, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("PAM clustering with 2 clusters") 
p2_pam <- fviz_cluster(object = pam_model_3$pam_eng, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("PAM clustering with 3 clusters") 
p3_pam <- fviz_cluster(object = pam_model_4$pam_eng, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("PAM clustering with 4 clusters")

cowplot::plot_grid(p1_pam, p2_pam, p3_pam)
dev.off()

#--------------------------------------------------------------------------------
# kmeans 2,3,4 clusters ----
#--------------------------------------------------------------------------------
kmean_model <- function(eng_transformed,k){
  kmeans <- kmeans(eng_transformed, centers = k, 
         algorithm = "Lloyd",iter.max = 30)
  print(table(kmeans$cluster))
  
  return(kmeans)
}

#create kmeans clusters
km2 <- kmean_model(eng_num_log_scale[-1], 2)
p_clusters$km_2 <-km2$cluster
km3 <- kmean_model(eng_num_log_scale[-1], 3)
p_clusters$km_3 <- km3$cluster
km4 <- kmean_model(eng_num_log_scale[-1], 4)
p_clusters$km_4 <- km4$cluster

#visualize clusters
p1_km <- fviz_cluster(km2, data = eng_num_log_scale[-1], ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("K-means clustering with 2 clusters") 
p2_km <- fviz_cluster(km3, data = eng_num_log_scale[-1], ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("K-means clustering with 3 clusters")
p3_km <- fviz_cluster(km4, data = eng_num_log_scale[-1], ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("K-means clustering with 4 clusters")

cowplot::plot_grid(p1_km, p2_km, p3_km)
dev.off()


#determine optimal number of clusters
nb <- NbClust(eng_num_log_scale[-1], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)
#--------------------------------------------------------------------------------
# hierarchical clustering 2,3,4 clusters ----
#--------------------------------------------------------------------------------
hclust_model <- function(eng_transformed,k){
  # Copmutes hclust and cuts the tree
  hc.cut <- hcut(eng_transformed, k = k,hc_func = c("hclust"), hc_method = "ward.D2",hc_metric = "euclidean")
  
  print(table(hc.cut$cluster))
  
  return(hc.cut)
}

#create hclust clusters
hclust2 <- hclust_model(eng_num_log_scale[-1], 2)
p_clusters$hclust_2 <-hclust2$cluster

hclust3 <- hclust_model(eng_num_log_scale[-1], 3)
p_clusters$hclust_3 <- hclust3$cluster

hclust4 <- hclust_model(eng_num_log_scale[-1], 4)
p_clusters$hclust_4 <- hclust4$cluster

# Visualize dendrogram
# p1_hclust_dend <- fviz_dend(hclust2, show_labels = FALSE, rect = TRUE)
# p2_hclust_dend <- fviz_dend(hclust3, show_labels = FALSE, rect = TRUE)
# p3_hclust_dend <-fviz_dend(hclust4, show_labels = FALSE, rect = TRUE)
# 
# cowplot::plot_grid(p1_hclust_dend, p2_hclust_dend, p3_hclust_dend)
# dev.off()


#visualize clusters
p1_hclust <- fviz_cluster(hclust2, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("Hierarchical clustering with 2 clusters") 
p2_hclust <- fviz_cluster(hclust3, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("Hierarchical clustering with 3 clusters")
p3_hclust <- fviz_cluster(hclust4, ellipse.type = "convex",geom="point",palette = "Set2") + theme_minimal() + ggtitle("Hierarchical clustering with 4 clusters")

cowplot::plot_grid(p1_hclust, p2_hclust, p3_hclust)
dev.off()


#--------------------------------------------------------------------------------
# visualize cluster by method ----
#--------------------------------------------------------------------------------
#2 clusters
cowplot::plot_grid(p1_hclust, p1_km, p1_pam)
dev.off()
#3 clusters
cowplot::plot_grid(p2_hclust, p2_km, p2_pam)
dev.off()
#4 clusters
cowplot::plot_grid(p3_hclust, p3_km, p3_pam)
dev.off()

#plot graphs
png(here("Scripts2","Tables_Figures","Figures","2_clusters_visual.png"), units="in", width=9, height=5, res=300)
cowplot::plot_grid(p1_hclust, p1_km, p1_pam)
dev.off()

png(here("Scripts2","Tables_Figures","Figures","3_clusters_visual.png"), units="in", width=9, height=5, res=300)
cowplot::plot_grid(p2_hclust, p2_km, p2_pam)
dev.off()

png(here("Scripts2","Tables_Figures","Figures","4_clusters_visual.png"), units="in", width=9, height=5, res=300)
cowplot::plot_grid(p3_hclust, p3_km, p3_pam)
dev.off()

#--------------------------------------------------------------------------------
# Visualize Silhouette Information from Clustering ----
#--------------------------------------------------------------------------------

# Visualize silhouhette information

#sil_km2 <- silhouette(km2$cluster, dist(eng_num_log_scale[-1]))

#par(mfrow=c(3,1))
#plot_sil_km2 <- fviz_silhouette(sil_km2)
#plot_sil_km2[["labels"]][["title"]] <- "Clusters silhouette plot for K-means \n Average silhouette width: 0.25"

#plot_sil_pam2 <- fviz_silhouette(pam_model_2$pam_eng)
#plot_sil_km2[["labels"]][["title"]] <- "Clusters silhouette plot for PAM \n Average silhouette width: 0.24"

#fviz_silhouette(hclust2)



#--------------------------------------------------------------------------------
# Visualize features ----
#--------------------------------------------------------------------------------
chosen_cluster <- km2

#load participant clustering from May 1 2022
load(here("Scripts2","Data2","participant_cluster.RData"))


#add clustering to engagement features
eng_df_cluster <- eng_num %>%
  cbind(cluster = participant_cluster$cluster) %>%
  group_by(cluster)

#fctor cluster variable
eng_df_cluster$cluster <- as.factor(eng_df_cluster$cluster)

#add label,  1=="Less Time Spent", 2 == "More Time Spent"
eng_df_cluster<- eng_df_cluster %>% mutate(cluster_label = ifelse(cluster==1, "Less Time Spent","More Time Spent")) 

#factor
eng_df_cluster$cluster_label <- as.factor(eng_df_cluster$cluster_label)


#Cluster count
table(eng_df_cluster$cluster_label)
#Cluster count
table(eng_df_cluster$cluster)

#Label cluster as more time spent vs less time spent 

#Completion Rate
library(ggridges)

ggplot(eng_df_cluster,aes(x= completionRate, fill = cluster_label))+geom_histogram()+facet_wrap(~as.factor(cluster_label))

ggplot(eng_df_cluster, aes(x=completionRate, fill=cluster_label)) + geom_density(alpha=.75)+labs(title="Density distribution of the completion rate",x="Completion Rate", y = "Density", fill = "Engagement Group") + scale_fill_brewer(palette="Dark2")+
  guides(fill = guide_legend(reverse = TRUE))

# Color by quantiles
plot_comp_dist <- ggplot(eng_df_cluster, aes(x = completionRate, y = cluster_label, fill = factor(stat(quantile)))) +
  ggridges::geom_density_ridges(aes(fill = cluster_label)) +
  scale_fill_brewer(palette="Dark2")+ 
  theme(legend.position = "none")+
  labs(title="Density distribution of completion rate",x="Completion Rate", y = "Engagement Group")#+
  #xlim(0, 1)

plot_comp_dist

#scenarios
#ggplot(eng_df_cluster, aes(x = T2_sec_mean_scenario_rt_across_sessions_winsor, fill = as.factor(cluster_label)))+geom_boxplot()

#fct_rev to reverse order in y axis
plot_scenarios <- ggplot(eng_df_cluster, aes(x=T2_sec_mean_scenario_rt_across_sessions_winsor, y=cluster_label, fill=as.factor(cluster_label))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  theme(legend.position="none")+
  labs(title="Average time spent on scenarios across sessions",x="Average time spent (seconds)", y = "Engagement Group") +scale_fill_brewer(palette="Dark2")

plot_scenarios

#lemon exercise
#ggplot(eng_df_cluster, aes(x = T3_min_time_spent_lemon_winsor, fill = as.factor(cluster)))+geom_boxplot()
plot_lemon <- ggplot(eng_df_cluster, aes(x=T3_min_time_spent_lemon_winsor, y=cluster_label, fill=as.factor(cluster_label))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  theme(legend.position="none")+
  labs(title="Time spent on imagery practice exercise",x="Time spent (minutes)", y = "Engagement Group") + scale_fill_brewer(palette="Dark2")

plot_lemon


#Imagine exercise
#ggplot(eng_df_cluster, aes(x = T3_2_min_time_spent_imagine_winsor, fill = as.factor(cluster)))+geom_boxplot()
plot_imagine <- ggplot(eng_df_cluster, aes(x=T3_2_min_time_spent_imagine_winsor, y=cluster_label, fill=as.factor(cluster_label))) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  theme(legend.position="none")+
  labs(title="Time spent on the anxiety imagery prime exercise",x="Time spent (minutes)", y = "Engagement Group") + scale_fill_brewer(palette="Dark2")


plot_imagine

ggpubr::ggarrange(plot_scenarios,plot_lemon,plot_imagine,ncol = 2, nrow = 2)

#Measures
eng_df_cluster_m <- eng_df_cluster %>% select(participant_id,cluster,cluster_label,colnames(eng_df_cluster[grep("T4_",names(eng_df_cluster))]))

mod_measure_names <- colnames(eng_df_cluster[grep("T4_",names(eng_df_cluster))])
mod_measure_names1 <- gsub("T4_min_mean_assessment_time*.", "",mod_measure_names)  
mod_measures_names2 <- gsub("_winsor","",mod_measure_names1)
mod_measures_names3 <- gsub("_"," ",mod_measures_names2)

measure_names <- c("Anxiety Identity","Anxiety Triggers","Comorbid","Credibility","DASS-21 AS","Demographics","Mechanisms","Mental Health History","OASIS","Pre-Affect","RR","Technology Use","Wellness")

colnames(eng_df_cluster_m) <- c("participant_id","cluster","cluster_label",measure_names)

eng_df_cluster.m <- melt(as.data.frame(eng_df_cluster_m),id.vars=c('participant_id',"cluster_label","cluster"), measure.vars=colnames(eng_df_cluster_m[,-c(1,2,3)]))

ggplot(eng_df_cluster.m, aes(log(value), variable))+geom_boxplot(aes(fill = cluster))+guides(color = guide_legend(reverse=TRUE))

plot_measures <- ggplot(eng_df_cluster.m, aes(x=log(value), y=variable, fill=cluster_label)) + 
  geom_boxplot(aes(fill = cluster_label))+
  labs(title="Log of time spent on measures",x="log(time spent (minutes))", y = "Measure", fill = "Engagement Group") + scale_fill_brewer(palette="Dark2")+
  guides(fill = guide_legend(reverse = TRUE))
plot_measures


#Density distribution
plot_measures_distributions <- ggplot(eng_df_cluster.m, aes(x=log(value), fill=cluster_label)) + geom_density(alpha=.75)+facet_wrap(~variable,scales = "free")+ labs(title="Density distribution of the log of time spent on measures",x="log(time spent (minutes))", y = "Density", fill = "Engagement Group") + scale_fill_brewer(palette="Dark2")+
  guides(fill = guide_legend(reverse = TRUE))

plot_measures_distributions

#save images

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","training_lemon.png"), units="in", width=9, height=5, res=300)
plot_lemon
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","training_scenarios.png"), units="in", width=9, height=5, res=300)
plot_scenarios
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","training_imagine.png"), units="in", width=9, height=5, res=300)
plot_imagine
dev.off()



png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","time_training_box2.png"), units="in", width=9, height=5, res=300)
ggpubr::ggarrange(plot_scenarios,plot_lemon,plot_imagine,ncol = 2, nrow = 2)
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","time_measures_box.png"), units="in", width=9, height=5, res=300)
plot_measures
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","time_measures_distributions.png"), units="in", width=9, height=5, res=300)
plot_measures_distributions
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","completion_rate_distributions_orig.png"), units="in", width=9, height=5, res=300)
plot_comp_dist
dev.off()

#--------------------------------------------------------------------------------
# Wilcoxon rank-sum tests for 2 groups ----
#--------------------------------------------------------------------------------
#Ho: Median engagement feature value for cluster 1 = Median engagement feature value for cluster 2

wilcox_features <- tibble()

wilcox_function <- function(eng_df_test,feature,wilcox_features){
  print(feature)
  test <- wilcox.test(feature ~ cluster, data =eng_df_test)
  print(test)
  wilcox_df <- tibble(feature = feature, W = test$statistic, pvalue = round(test$p.value,4))
  wilcox_features <- rbind(wilcox_features,wilcox_df)
  return(wilcox_features)
}

for(col in colnames(eng_df_cluster[c(-1,-19,-20)])){
  eng_wilcox_df <- eng_df_cluster
  eng_wilcox_df <- eng_df_cluster %>% select(col,cluster)
  feature <- colnames(eng_wilcox_df[1])
  colnames(eng_wilcox_df) <- c("feature","cluster")
  wilcox_features <- wilcox_function(eng_wilcox_df, feature,wilcox_features)
}

wilcox_features_table <- wilcox_features


features <- c("Completion Rate", "Average time spent on scenarios across sessions","Time spent on lemon exercise","Time spent on the anxiety imagery prime exercise",measure_names)

wilcox_features_table$feature_name <- c("Completion Rate", "Average time spent on scenarios across sessions","Time spent on lemon exercise","Time spent on the anxiety imagery prime exercise",measure_names)

#--------------------------------------------------------------------------------
# Differences in the engagement measurements between the high- and low-engagement groups  ----
#--------------------------------------------------------------------------------
summary_table <- tibble(feature = features)

#engagement group
summary_cluster_median <- eng_df_cluster[c(-1,-19)] %>% group_by(cluster_label) %>% summarise_all(list(median = median)) %>% ungroup()

summary_cluster_median_t <-as.data.frame(t(summary_cluster_median))
colnames(summary_cluster_median_t)<- c("medianLess","medianMore")


#all participants
#summary_cluster_median_all <- eng_df_cluster[c(-1,-19,-20)] %>% summarise_all(list(median = median))
#summary_cluster_median_all_t <-as.data.frame(t(summary_cluster_median_all))


#engagement group
summary_cluster_IQR <- eng_df_cluster[c(-1,-19)] %>% group_by(cluster_label) %>% summarise_all(list(IQR = IQR))
summary_cluster_IQR_t <- as.data.frame(t(summary_cluster_IQR))
colnames(summary_cluster_IQR_t)<- c("IQRLess","IQRMore")

#all participants


summary_table <- cbind(summary_table,summary_cluster_median_t[-1,])
summary_table <- cbind(summary_table,summary_cluster_IQR_t[-1,])
rownames(summary_table) <- NULL

round_format <- function(var,roundNum){
  formatC(round(var,roundNum),format = "f",digits = roundNum)
}

summary_table[-1] <- lapply(summary_table[-1],as.numeric)

summary_table[-1] <- lapply(summary_table[-1],round_format,roundNum = 2)

summary_table<- cbind(summary_table,select(wilcox_features,W,pvalue))

#buid latex table

eng_features_table <-tibble(Engagement_Metric =summary_table$feature )

eng_features_table$LessMedianIQR <- paste0(summary_table$medianLess, " (", summary_table$IQRLess,")")

eng_features_table$MoreMedianIQR <- paste0(summary_table$medianMore, " (", summary_table$IQRMore,")")


eng_features_table$W <-summary_table$W

eng_features_table$pValue <-as.character(summary_table$pvalue)

eng_features_table$pValue[eng_features_table$pValue == "0"] <- "<0.001"


#save as latex table 

sink(here("Scripts2","Tables_Figures","Tables","eng_stats_wilcoxon.txt"))
stargazer(eng_features_table, type = "latex", title="Descriptive statistics of engagement metrics by groups", digits=2, summary = F,rownames = F)
sink()




#--------------------------------------------------------------------------------
# Kruskal-Wallis test by rank for more than 2 groups ----
#--------------------------------------------------------------------------------
#http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
#http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
chosen_cluster2 <- km3
eng_df_cluster2 <- eng_num %>%
  cbind(cluster = chosen_cluster2$cluster) %>%
  group_by(cluster)


kruskal_features <- tibble()

kruskal_function <- function(eng_df_test,feature,kruskal_features){
  print(feature)
  test <- kruskal.test(feature ~ cluster, data =eng_df_test)
  print(test)
  kruskal_df <- tibble(feature = feature, W = test$statistic, pvalue = round(test$p.value,4))
  kruskal_features <- rbind(kruskal_features,kruskal_df)
  return(kruskal_features)
}


pair_wilcox_function <- function(eng_df_test,feature,kruskal_features){
  print(feature)
  test <- pairwise.wilcox.test(x = eng_df_test$feature, g =eng_df_test$cluster, p.adjust.method = "BH")
  print(test)
}


for(col in colnames(eng_df_cluster2[c(-1,-19)])){
  eng_kruskal_df <- eng_df_cluster2
  eng_kruskal_df <- eng_df_cluster2 %>% select(col,cluster)
  feature <- colnames(eng_kruskal_df[1])
  colnames(eng_kruskal_df) <- c("feature","cluster")
  kruskal_features <- kruskal_function(eng_kruskal_df, feature,kruskal_features)
  pair_wilcox_function(eng_kruskal_df, feature,kruskal_features)
}


# ---------------------------------------------------------------------------- #
# Save data as RData ----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps
#dataframe to save with clustering assignment
participant_cluster <- eng_df_cluster %>% select(participant_id,cluster)
cluster_summary <- eng_df_cluster

save(participant_cluster,cluster_summary, file = here("Scripts2","Data2","participant_cluster.RData"))
