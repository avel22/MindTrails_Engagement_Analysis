### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022


#--------------------------------------------------------------------------------#
# loading the libraries
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools,reshape2,lubridate,hash,caret,corrplot,Hmisc,outliers)

#--------------------------------------------------------------------------------#
# loading the engagement metrics
#--------------------------------------------------------------------------------#
load(here("Scripts2","Data_Primary_Analysis","E_M_1.RData"))

#--------------------------------------------------------------------------------#
# summary of engagement metrics
#--------------------------------------------------------------------------------#
engagement_metrics <- E_M_1

summary(engagement_metrics)

engagement_metrics_winsor_cols <- grep("winsor",names(engagement_metrics))

engagement_metrics_winsor <- engagement_metrics %>% select(participant_id,completionRate,colnames(engagement_metrics[engagement_metrics_winsor_cols]))



#participant 602 completed the first training session but did not have any information in angular training
#for for lemon and imagine exercise impute by the median
engagement_metrics_winsor$T3_min_time_spent_lemon_winsor[which(engagement_metrics_winsor$participant_id == 602)]<-median(engagement_metrics_winsor$T3_min_time_spent_lemon_winsor,na.rm = T)

engagement_metrics_winsor$T3_2_min_time_spent_imagine_winsor[which(engagement_metrics_winsor$participant_id == 602)]<-median(engagement_metrics_winsor$T3_2_min_time_spent_imagine_winsor,na.rm = T)


#--------------------------------------------------------------------------------#
# na count
#--------------------------------------------------------------------------------#
apply( engagement_metrics_winsor , 2 , function(x) sum ( is.na(x) ) )
#columns that have NAs include time for invidual scenario and time between sessions
#Should we replaces those values with 0 since they did not complete any scenarios? Is it really a 0?

mice::md.pattern(engagement_metrics_winsor,rotate.names = T)

#time between session set to 200 to represent those who never returned to the study
engagement_metrics_winsor$T1_days_mean_time_between_sessions_winsor[is.na(engagement_metrics_winsor$T1_days_mean_time_between_sessions_winsor)] <- 200

engagement_metrics_winsor[is.na(engagement_metrics_winsor)]<-0

#--------------------------------------------------------------------------------#
# correlation plot
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
# low variance
#--------------------------------------------------------------------------------#
#https://jtr13.github.io/cc20/data-preprocessing-and-feature-engineering-in-r.html
# Identify near zero variance predictors to remove
remove_cols <- caret::nearZeroVar(engagement_metrics2, names = TRUE, 
                           freqCut = 2, uniqueCut = 10)
#remove avgLogInsRounded since it was identified as having low variance
engagement_metrics3 <- engagement_metrics2 %>% select(-all_of(remove_cols))
#--------------------------------------------------------------------------------#
# boxplots
#--------------------------------------------------------------------------------#
#reshape dataframe
engagement_metrics3.m <- melt(as.data.frame(engagement_metrics_winsor),id.vars='participant_id', measure.vars=colnames(engagement_metrics_winsor[,-1]))
#plot boxplot
ggplot(engagement_metrics3.m, aes(value))+geom_boxplot()+facet_wrap(~variable,scales = "free")

# ---------------------------------------------------------------------------- #
# Save data as RData ----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps
save(engagement_metrics_winsor, file = here("Scripts2","Data_Primary_Analysis","E_M2_1_Winsor.RData"))
