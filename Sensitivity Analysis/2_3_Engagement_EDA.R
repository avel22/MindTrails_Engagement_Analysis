### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is to combined all engagement metrics into one dataframe and preform EDA

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools,reshape2,lubridate,hash,caret,corrplot,Hmisc,outliers)

#--------------------------------------------------------------------------------#
# loading the data  ----
#--------------------------------------------------------------------------------#
#clean the environment
rm(list=ls())

load(here("Data","1_Import_Data_Dat3.RData"))
load(here("Data","2_1_Engagement_Metrics_Completion_Rate.RData"))
load(here("Data","2_2_Engagement_Metrics_Time_Spent.RData"))

#--------------------------------------------------------------------------------#
# combined data into one dataframe  ----
#--------------------------------------------------------------------------------#
#select columns of interest, exclude participants 776 and 169 since they were considered as outliers
engagement_metrics_completion <- final_completion %>% select(participant_id,completionRate) %>% filter(!(participant_id %in% c(776,169)))


engagement_metrics_time <- E_T_FINAL_NA_1 %>% filter(!(participant_id %in% c(776,169)))

engagement_metrics <- engagement_metrics_completion %>% left_join(engagement_metrics_time, by = c("participant_id"))

#--------------------------------------------------------------------------------#
# engagement metrics EDA  ----
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# summary of engagement metrics  ----
#--------------------------------------------------------------------------------#
summary(engagement_metrics)

engagement_metrics_winsor_cols <- grep("winsor",names(engagement_metrics))

engagement_metrics_winsor <- engagement_metrics %>% select(participant_id,completionRate,colnames(engagement_metrics[engagement_metrics_winsor_cols]))



#participant 602 completed the first training session but did not have any information in angular training
#for lemon and imagine exercise impute by the median
engagement_metrics_winsor$T3_min_time_spent_lemon_winsor[which(engagement_metrics_winsor$participant_id == 602)]<-median(engagement_metrics_winsor$T3_min_time_spent_lemon_winsor,na.rm = T)

engagement_metrics_winsor$T3_2_min_time_spent_imagine_winsor[which(engagement_metrics_winsor$participant_id == 602)]<-median(engagement_metrics_winsor$T3_2_min_time_spent_imagine_winsor,na.rm = T)


#--------------------------------------------------------------------------------#
# na count  ----
#--------------------------------------------------------------------------------#
apply( engagement_metrics_winsor , 2 , function(x) sum ( is.na(x) ) )
#columns that have NAs include time for invidual scenario and time between sessions
#Should we replaces those values with 0 since they did not complete any scenarios? Is it really a 0?

mice::md.pattern(engagement_metrics_winsor,rotate.names = T)

#time between session set to 200 to represent those who never returned to the study
engagement_metrics_winsor$T1_days_mean_time_between_sessions_winsor[is.na(engagement_metrics_winsor$T1_days_mean_time_between_sessions_winsor)] <- 200

engagement_metrics_winsor[is.na(engagement_metrics_winsor)]<-0

#--------------------------------------------------------------------------------#
# correlation plot  ----
#--------------------------------------------------------------------------------#
corr_table <- cor(engagement_metrics_winsor[,-1])
corr_table2 <- corr_table

#changed column names for visual display in correlation plot
colnames(corr_table2) <- 1:length(colnames(corr_table))


#correlation plot to visualize correlation matrix between variables
corrplot(corr_table2, is.corr = T, method = "number")

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(corr_table, cutoff = 0.7)
# print indexes of highly correlated attributes that we might want to remove
print(highlyCorrelated)
#remove highly correlated columns if any
corr_table_df <- as.data.frame(corr_table)
remove_cols <- colnames(corr_table_df[highlyCorrelated])

engagement_metrics2<- engagement_metrics_winsor %>% select(-all_of(remove_cols))

corr_table_df[ corr_table_df<0.7] <- ""

#--------------------------------------------------------------------------------#
# low variance  ----
#--------------------------------------------------------------------------------#
#https://jtr13.github.io/cc20/data-preprocessing-and-feature-engineering-in-r.html
# Identify near zero variance predictors to remove
remove_cols_2 <- caret::nearZeroVar(engagement_metrics2, names = TRUE, 
                                  freqCut = 2, uniqueCut = 10)
#remove avgLogInsRounded since it was identified as having low variance
engagement_metrics3 <- engagement_metrics2 %>% select(-all_of(remove_cols_2))

#--------------------------------------------------------------------------------#
# boxplots  ----
#--------------------------------------------------------------------------------#
#reshape dataframe
engagement_metrics3.m <- melt(as.data.frame(engagement_metrics_winsor),id.vars='participant_id', measure.vars=colnames(engagement_metrics_winsor[,-1]))
#plot boxplot
ggplot(engagement_metrics3.m, aes(value))+geom_boxplot()+facet_wrap(~variable,scales = "free")

# ---------------------------------------------------------------------------- #
# Save data as RData ----
# ---------------------------------------------------------------------------- #
engagement_metrics_final <- engagement_metrics3
#save as RData file to be used in later steps
save(engagement_metrics,engagement_metrics_winsor,engagement_metrics_final, file = here("Data","2_3_Engagement_EDA.RData"))
