### Written by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to calculate the average time spent in a scenario, training session, assessment, questionnaire

#--------------------------------------------------------------------------------#
# loading the libraries -----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,tools,reshape2,lubridate,hash,caret,datawizard,psych,rlang)

#--------------------------------------------------------------------------------#
# source functions -----
#--------------------------------------------------------------------------------#
source(here("Scripts2","functions.R"))

#--------------------------------------------------------------------------------#
# loading the data -----
#--------------------------------------------------------------------------------#
#load data from scrip 1_importing_data.R
load(here("Scripts2","Data_Primary_Analysis","2_Calm_2.RData"))

#--------------------------------------------------------------------------------#
# variables -----
#--------------------------------------------------------------------------------#
training.session.order <- c("firstSession","secondSession","thirdSession","fourthSession","fifthSession")


#--------------------------------------------------------------------------------#
# participant filtering -----
#--------------------------------------------------------------------------------#

#filter for participants who started the first scenario of angular training
#629 participants
#p_ids_scenario <- dat.3$participant %>% filter(comp1scenario== 1) %>% select(participant_id) %>% pull()

#remove participants data from all the tables
#the only table that does not have the participant_id column is condition_assignment_settings
#as noted in the 4_clean_data.R line 1165 "condition_assignment_settings" table, for which "participant_id" is irrelevant, is retained only for Calm Thinking study"
#dat.3 <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  (participant_id %in% p_ids_scenario))} else x)

#--------------------------------------------------------------------------------#
# engagement time dataframe -----
#--------------------------------------------------------------------------------#
#dataframe that we will be adding engagement time variables to
engagement.time <- data.frame(participant_id = dat.3$participant$participant_id)

#--------------------------------------------------------------------------------#
# T_1: Time passed between training session modules "T1" in days -----
#--------------------------------------------------------------------------------#
#***********************************************************
#
#		T_1 Part 1: Calculating time between sessions -----
#
#***********************************************************
# Date difference between last completed entry in session X and first completed entry in session X+1 (X in 1,2,3,4)
task.log <- dat.3$task_log

#get first and last entries per participant per session
#focused on sessions that include training, so exclude Eligibility, preTest, and PostFollowUp
tl.first.last <-
  task.log %>% filter(!(session_only %in% c("Eligibility", "preTest", "PostFollowUp"))) %>%
  group_by(participant_id, session_only) %>%
  slice(c(1, n())) %>%
  ungroup()

#select columns of interest
tl.first.last2 <-
  tl.first.last %>% select(
    participant_id,
    session_only,
    tag,
    task_name,
    date_completed_as_POSIXct,
    n_unq_item_rows
  )

#calculate the first and last entry in a given session for a participant
tl.first.last.3 <-
  tl.first.last %>% select(participant_id,
                           session_only,
                           task_name,
                           date_completed_as_POSIXct) %>% 
                    group_by(participant_id, session_only) %>% 
                    summarise(
                             first = min(date_completed_as_POSIXct),
                             last = max(date_completed_as_POSIXct)
                           ) %>% 
                    ungroup()

#calculate date difference between first entry in session X+1 and last entry in session X
#note that 5 days need to pass in order to have access to the next session
tl.first.last.3 <-
  tl.first.last.3 %>% group_by(participant_id) %>%  
                      mutate(
                              dayDifference = c(NA, tail(first, -1) - head(last,-1)),
                              dayDifferenceAdjusted = c(NA, tail(first, -1) - head(last,-1)),
                              sessions_period = c(NA, paste(head(session_only,-1), tail(session_only, -1), sep = "_"))
                            )

#widen the data
engagement.time.between.sessions <-
  tl.first.last.3 %>% 
  select(participant_id, dayDifferenceAdjusted, sessions_period) %>% 
  pivot_wider(names_from = "sessions_period", values_from = "dayDifferenceAdjusted") %>% 
  select(-"NA")

#count how many na values are present between sessions and replace NAs with 0
engagement.time.between.sessions <-
  engagement.time.between.sessions  %>% 
  rowwise() %>%
  mutate(count0 = sum(is.na(cur_data()))) %>%
  #replace(is.na(.), 0) %>%
  ungroup()

#get between session column names
sessions.between <- grep( "Session" , names(engagement.time.between.sessions))

#calculate the average time between sessions in days accounting for number of entries
engagement.time.between.sessions  <-
  engagement.time.between.sessions %>% 
  rowwise() %>%
  mutate(T1_days_mean_time_between_sessions = case_when(count0 == 4 ~ 0,
                                                  TRUE ~ sum(c_across(all_of(sessions.between)),na.rm =T)/(length(sessions.between) - count0))
         )

engagement.time.between.sessions$T1_days_mean_time_between_sessions[engagement.time.between.sessions$T1_days_mean_time_between_sessions ==0]<-NA

summary(engagement.time.between.sessions)

#add "time_between" to column name
colnames(engagement.time.between.sessions)[sessions.between] <- paste("T1_days_time_between",colnames(engagement.time.between.sessions[sessions.between]),sep = "_")


#outlier detection
outlier_T1_p <- engagement.time.between.sessions2 %>% ungroup() %>% filter(!is.na(T1_days_mean_time_between_sessions))

outlier_T1_p<- outlier_T1_p %>% select(participant_id,T1_days_mean_time_between_sessions) %>% filter(abs(T1_days_mean_time_between_sessions-stats::median(T1_days_mean_time_between_sessions,na.rm = T))/stats::mad(T1_days_mean_time_between_sessions,na.rm = T)>3.0) 

T1_outliers <-outlier_T1_p


#data summary and visualizations
#https://stefvanbuuren.name/fimd/missing-data-pattern.html
#monotone pattern of missingness
#300 participants who did not get to the second sessions, therefore there average time between sessions will be NA, replace with large extreme value to represent that they dropped out of the study
engagement.time.between.sessions2 <- engagement.time.between.sessions %>% select(-count0)

mice::md.pattern(engagement.time.between.sessions2,rotate.names = T)

#summary statistics
summary(engagement.time.between.sessions2[-1])

#reshape dataframe for data visualizations
engagement.time.between.sessions.m <- melt(as.data.frame(engagement.time.between.sessions2),id.vars='participant_id', measure.vars=colnames(engagement.time.between.sessions2[-1]))

#plot boxplot+violin plot
#identify outliers from visual, for example a participant's time between second and third session is 188 days.
ggplot(engagement.time.between.sessions.m, aes(value, variable))+geom_violin(trim=TRUE,fill="gray")+geom_boxplot(width=0.1)+
  theme_classic()

#histograms
#data is rights skewed
ggplot(engagement.time.between.sessions.m,aes(value))+geom_histogram()+facet_wrap(~variable)

#density plots
ggplot(engagement.time.between.sessions.m, aes(x = value))+geom_density()+facet_wrap(~variable, scales = "free")+
  theme_classic()

#***********************************************************
#
#		T_1 Part 2: Winsor Capping Time Data -----
#
#***********************************************************
#let's try capping the data at the 99th percentile to handle extreme outliers
#calculate the 99% value per session
eng.time.between.sessions.winsor <- engagement.time.between.sessions[-ncol(engagement.time.between.sessions)]

winsor.columns <- grep("Session", names(eng.time.between.sessions.winsor))

eng.time.between.sessions.winsor[winsor.columns] <- lapply(eng.time.between.sessions.winsor[winsor.columns],winsor,trim = 0.01, na.rm = T)

eng.time.between.sessions.winsor  <-
  eng.time.between.sessions.winsor %>% 
  rowwise() %>%
  mutate(T1_days_mean_time_between_sessions_winsor = sum(c_across(all_of(winsor.columns)),na.rm =T)/(length(winsor.columns) - count0))%>% select(-count0)

eng.time.between.sessions.winsor$T1_days_mean_time_between_sessions_winsor[is.nan(eng.time.between.sessions.winsor$T1_days_mean_time_between_sessions_winsor)]<-NA

summary(eng.time.between.sessions.winsor)

#reshape dataframe
eng.time.between.sessions.winsor.m <- melt(as.data.frame(eng.time.between.sessions.winsor),id.vars='participant_id', measure.vars=colnames(eng.time.between.sessions.winsor[-1]))

#summary stats
summary(eng.time.between.sessions.winsor)

#plot boxplot+violin plot
ggplot(eng.time.between.sessions.winsor.m, aes(value, variable))+geom_violin(trim=TRUE,fill="gray")+geom_boxplot(width=0.1)+
  theme_classic()

#histograms
ggplot(eng.time.between.sessions.winsor.m,aes(value))+geom_histogram()+facet_wrap(~variable)


#density
ggplot(eng.time.between.sessions.winsor.m, aes(x = value))+geom_density()+facet_wrap(~variable)+
  theme_classic()




#***********************************************************
#
#		T_1 Part 4: Add to time engagement dataframe -----
#
#***********************************************************

E_T1 <- eng.time.between.sessions.winsor %>% select(participant_id,T1_days_mean_time_between_sessions_winsor)

E_T1 <- cbind(E_T1, T1_days_mean_time_between_sessions = engagement.time.between.sessions2$T1_days_mean_time_between_sessions)

#add to engagement.time dataframe
engagement.time1 <- engagement.time %>% left_join(E_T1, by = c("participant_id"))

#--------------------------------------------------------------------------------#
# T_2: time spent on individual training scenario T2 in seconds -----
#--------------------------------------------------------------------------------#
#For R01, each training session has 40 scenarios
#Data about the training scenarios is stored in the angular training table
#Calculate the average time spent per scenario across sessions
#From conversations with Henry, use reaction time column (rt) to calculate time spent on scenario
#Based on the codebooks, rt is "Total reaction time of user action(s) to get correct response."
#Value for rt is originally in milliseconds, convert to seconds for easier interpretability
#As mentioned by Jeremy Eberle, there are some participants where task log says that they completed a training session, but do not have 40 scenarios in angular training table do to some system issue shown below
#We will still calculate the average based on the number of scenarios present in angular training

#     firstSession
#       No data at all in "angular_training" = 602
#       0  scenarios in "angular_training"   = 406, 465
#       10 scenarios in "angular_training"   = 639
#       39 scenarios in "angular_training"   = 384, 1496
#     secondSession  
#       25 scenarios in "angular_training"   = 639
#       30 scenarios in "angular_training"   = 779
#       37 scenarios in "angular_training"   = 768
#       39 scenarios in "angular_training"   = 429, 666, 916, 1756
#     thirdSession  
#       5  scenarios in "angular_training"   = 832
#       10 scenarios in "angular_training"   = 779
#       13 scenarios in "angular_training"   = 639
#       16 scenarios in "angular_training"   = 1659
#       39 scenarios in "angular_training"   = 429, 572
#     fourthSession  
#       36 scenarios in "angular_training"   = 714
#     fifthSession  
#       4  scenarios in "angular_training"   = 832


angular <- dat.3$angular_training

#remove columns not of interest and filter for scenario step_title
#remove Recognition Ratings from session_and_task_info column
angular.scenario <-angular %>% 
                   select(-c(X, id, date, time_on_page, button_pressed, correct, device)) %>% 
                   filter(step_title == "scenario",session_and_task_info != "Recognition Ratings")

#before we analyze rt, let's check that scenarios appear once per session
#Henry confirmed that scenarios do not repeat within sessions, so a participant will see 40 unique scenarios in a given session if they complete that training session
#Even if they did not complete the entire training session, they would still see the scenarios that they did complete only once
#Let's check that the trial_type Intro appears once indicating that the scenario was completed once in a session
#session_index is not the best indicator of session since there are cases where it does not change to the corresponding session, use session_and_task_info instead

angular.scenario.intro <-angular.scenario %>% 
                        filter(trial_type == "Intro") %>%  
                        group_by(participant_id,
                                 session_and_task_info,
                                 stimulus_name,
                                 trial_type) %>% 
                        summarise(count = n()) %>% 
                        filter(count > 1) %>% 
                        ungroup()

#There are 711 instances where a given scenario trial_type Intro appears more than once in a session
#For example, participant 1231 has 4 entries of stimulus_name silent and trial_type Intro when there should only be one
#This could be because the participant did not complete the training session in one sitting or there was a system error

#check to see if there are duplicated entries for intro
duplicated.intro.check <- duplicated(angular.scenario[,c("participant_id","session_and_task_info","stimulus_name","trial_type","date_as_POSIXct")])

duplicated.intro.check2 <- angular.scenario[duplicated.intro.check,]

#3 particpants identified. The scenarios had two missing letter components that were entered with the same data time stamp, but are not duplicates.



# Seeing that there are no duplicate entries, average the rt for those repeated scenarios in a given session and specify number of repeated scenarios.
angular.scenario.fix2 <- angular.scenario %>%
                           group_by(participant_id,
                                    session_and_task_info,
                                    stimulus,
                                    stimulus_name,
                                    trial_type) %>%
                           mutate(rt_mean = mean(rt),n_count = n()) %>% 
                           filter(row_number() == n()) %>% 
                           ungroup()

#select columns of interest
angular.scenario.short <-
  angular.scenario.fix2 %>% 
  select(participant_id, session_and_task_info, stimulus_name, rt_mean)



#each scenario consists of at least two parts, sum those parts to get total time spent in a scenario
angular.scenario.group <-
  angular.scenario.short %>% 
  group_by(participant_id, session_and_task_info, stimulus_name) %>% 
  summarise(
    sum.rt = sum(rt_mean),
    sum.rt.sec = sum(rt_mean) / 1000,
    sum.rt.min = (sum(rt_mean) / 1000) / 60
  ) %>% 
  ungroup()


ggplot(angular.scenario.group,aes(x = sum.rt.min))+geom_boxplot()

angular.scenario.group.count <-  angular.scenario.group %>% group_by(participant_id, session_and_task_info) %>% summarise(scenario_count = n()) %>% ungroup()

angular.scenario.group.count2 <- angular.scenario.group.count %>% group_by(participant_id) %>% summarise(total_scenario_count = sum(scenario_count))


#Given that we have extremely high outliers, such as participant 521 who has an rt of 1045 min for scenario, metro, we will use the median absolute deviation over standard deviation 
#identify outlier scenarios within a participant's training session scenarios using median absolute deviation

t2.outlier.scenarios2 <- angular.scenario.group %>% group_by(participant_id,session_and_task_info) %>% filter(abs((sum.rt-stats::median(sum.rt)))/stats::mad(sum.rt)>3.0) %>% ungroup()

t2.outlier.scenarios.count2 <- t2.outlier.scenarios2 %>% group_by(participant_id) %>% summarise(count = n())

ggplot(t2.outlier.scenarios.count2,aes(x = count)) + geom_bar()


#join outlier count with total count

scenario_count <- angular.scenario.group.count2 %>% left_join(t2.outlier.scenarios.count2)

scenario_count <- scenario_count %>% mutate(propOutlier = count/total_scenario_count)


#remove those outlier scenarios for the participants
#after removing outliers we still have around 69,000 entries
angular.scenario.group2 <- angular.scenario.group %>% anti_join(t2.outlier.scenarios2, by = c("participant_id","session_and_task_info","stimulus_name"))

ggplot(angular.scenario.group2, aes(x = sum.rt.sec))+geom_boxplot()

#Participant 989 and 639 have averages above 200 seconds per scenario, let's take a closer look and decide if we should keep their scenario entries
#angular.scenario.short.ps.look <- angular.scenario.short %>% filter(participant_id %in% c(989,639))
#given that participant 989 has only two scenarios out of which one was not even completed and the other took 23 minutes to complete, remove entries for this participant and consider as if they had not completed any scenarios
#participant 639 seems to be taking longer in the intro part of the scenario and completed 119 scenarios (three training sessions), so we will keep the entries 

angular.scenario.group2 <- angular.scenario.group2 %>% filter(participant_id != 989)

#calculate the mean scenario time per session
angular.scenario.rt <-
  angular.scenario.group2 %>% 
  group_by(participant_id, session_and_task_info) %>% 
  summarise(T2_milisec_mean_scenario_rt = mean(sum.rt),T2_sec_mean_scenario_rt = mean(sum.rt.sec),max_scenario_min = max(sum.rt.min),scenario_count = n()) %>% ungroup()

#total scenario count per participant
angular.scenario.count <- angular.scenario.rt %>% group_by(participant_id) %>% summarise(total_scenario_count = sum(scenario_count))

ggplot(angular.scenario.count,aes(total_scenario_count))+geom_histogram()

#widen the data
angular.scenario.rt.wide <-
  angular.scenario.rt %>% select(participant_id,
                                 session_and_task_info,
                                 T2_sec_mean_scenario_rt) %>%
  group_by(participant_id) %>% 
  mutate(T2_sec_mean_scenario_rt_across_sessions = mean(T2_sec_mean_scenario_rt)) %>% 
  ungroup() %>% 
  arrange(match(session_and_task_info, training.session.order)) %>%
  pivot_wider(names_from = "session_and_task_info", values_from = "T2_sec_mean_scenario_rt")#%>%
  #replace(is.na(.), 0) 

#add "time" to column name
rename.columns <-  grep( "Session" , names(angular.scenario.rt.wide))
colnames(angular.scenario.rt.wide)[rename.columns] <- paste("T2_sec_mean_scenario_rt",colnames(angular.scenario.rt.wide[rename.columns]),sep = "_")


#stats and visuals
mice::md.pattern(angular.scenario.rt.wide,rotate.names = T)

#summary statistics
summary(angular.scenario.rt.wide[-1])

#reshape dataframe for data visualizations
angular.scenario.rt.wide.m <- melt(as.data.frame(angular.scenario.rt.wide),id.vars='participant_id', measure.vars=colnames(angular.scenario.rt.wide[-1]))

#plot boxplot+violin plot
#identify outliers from visual, for example a participant's time between second and third session is 188 days.
ggplot(angular.scenario.rt.wide.m, aes(value, variable))+geom_violin(trim=TRUE,fill="gray")+geom_boxplot(width=0.1)+
  theme_classic()

#histograms
#data is rights skewed
ggplot(angular.scenario.rt.wide.m,aes(value))+geom_histogram()+facet_wrap(~variable)

#density plots
ggplot(angular.scenario.rt.wide.m, aes(x = value))+geom_density()+facet_wrap(~variable, scales = "free")+
  theme_classic()

#outlier detection
outlier_T2_p <- angular.scenario.rt.wide %>% ungroup() %>% filter(!is.na(T2_sec_mean_scenario_rt_across_sessions))

outlier_T2_p<- outlier_T2_p %>% select(participant_id,T2_sec_mean_scenario_rt_across_sessions) %>% filter(abs(T2_sec_mean_scenario_rt_across_sessions-stats::median(T2_sec_mean_scenario_rt_across_sessions,na.rm = T))/stats::mad(T2_sec_mean_scenario_rt_across_sessions,na.rm = T)>3.0) 

T2_outliers <-outlier_T2_p

#winsorize
winsor.columns.scenario <- colnames(angular.scenario.rt.wide[grep("Session",colnames(angular.scenario.rt.wide))])
angular.scenario.rt.wide.winsor <- angular.scenario.rt.wide
angular.scenario.rt.wide.winsor[winsor.columns.scenario] <- lapply(angular.scenario.rt.wide.winsor[winsor.columns.scenario],DescTools::Winsorize,probs = c(0.01,0.99), na.rm = T)


angular.scenario.rt.wide.winsor <- angular.scenario.rt.wide.winsor %>% select(-T2_sec_mean_scenario_rt_across_sessions) %>% mutate(T2_sec_mean_scenario_rt_across_sessions_winsor = rowMeans(angular.scenario.rt.wide.winsor[winsor.columns.scenario],na.rm = T))


colnames(angular.scenario.rt.wide.winsor)[grep("Session",colnames(angular.scenario.rt.wide.winsor))] <- paste0(colnames(angular.scenario.rt.wide.winsor[winsor.columns.scenario]),"_winsor")


#reshape dataframe for data visualizations
angular.scenario.rt.wide.winsor.m <- melt(as.data.frame(angular.scenario.rt.wide.winsor),id.vars='participant_id', measure.vars=colnames(angular.scenario.rt.wide.winsor[-1]))

#plot boxplot+violin plot
#identify outliers from visual, for example a participant's time between second and third session is 188 days.
ggplot(angular.scenario.rt.wide.winsor.m, aes(value, variable))+geom_violin(trim=TRUE,fill="gray")+geom_boxplot(width=0.1)+
  theme_classic()

#histograms
#data is rights skewed
ggplot(angular.scenario.rt.wide.winsor.m,aes(value))+geom_histogram()+facet_wrap(~variable)

#density plots
ggplot(angular.scenario.rt.wide.winsor.m, aes(x = value))+geom_density()+facet_wrap(~variable, scales = "free")+
  theme_classic()


#add to engagement metrics
#Participant 602 has not data on angular training despite having finished first session
#Impute value by median of the first session
angular.scenario.rt.wide$T2_sec_mean_scenario_rt_across_sessions[which(angular.scenario.rt.wide$participant_id == 602)]<-median(angular.scenario.rt.wide$T2_sec_mean_scenario_rt_firstSession)

angular.scenario.rt.wide.winsor$T2_sec_mean_scenario_rt_firstSession_winsor[which(angular.scenario.rt.wide.winsor$participant_id == 602)]<-median(angular.scenario.rt.wide.winsor$T2_sec_mean_scenario_rt_firstSession_winsor)


E_T2 <- angular.scenario.rt.wide %>% select(participant_id,T2_sec_mean_scenario_rt_across_sessions)

E_T2$T2_sec_mean_scenario_rt_across_sessions_winsor <- angular.scenario.rt.wide.winsor$T2_sec_mean_scenario_rt_across_sessions_winsor


p_602 <- data.frame(participant_id = 602, T2_sec_mean_scenario_rt_across_sessions =median(angular.scenario.rt.wide$T2_sec_mean_scenario_rt_firstSession), T2_sec_mean_scenario_rt_across_sessions_winsor=median(angular.scenario.rt.wide.winsor$T2_sec_mean_scenario_rt_firstSession_winsor))

E_T2_2 <- rbind(E_T2,p_602)
#add to engagement.time dataframe
engagement.time2 <- engagement.time1 %>% left_join(E_T2, by = c("participant_id"))


#--------------------------------------------------------------------------------#
# T_3: time spent on lemon exercise -----
#--------------------------------------------------------------------------------#

#check for stimulus_name containing "le"
angular_lemon <- dat.3$angular_training %>% filter(str_starts(stimulus_name,"le[1-8]"))

#lemon exercise consists 8 lemon prompts, check that participants have at most 8 entries in the first session
angular_lemon_check <- angular_lemon %>% group_by(participant_id) %>% summarise(count = n())

#17 participants have less that 8 entries for lemon exercise
nrow(angular_lemon_check %>% filter(count <8))
#133 participants have more than 8 entries for lemon exercises
nrow(angular_lemon_check %>% filter(count >8))

#check to see if there are duplicated entries for lemon
duplicated.intro.check.lemon <- duplicated(angular_lemon[,c("participant_id","session_and_task_info","stimulus_name","date_as_POSIXct")])
#no duplicates
duplicated.intro.check2.lemon <- angular_lemon[duplicated.intro.check.lemon,]


#calculate the mean time per lemon exercise
angular_lemon_2 <-
  angular_lemon %>% 
  group_by(participant_id, stimulus_name) %>% 
  summarise(T3_milisec_mean_lemon_rt = mean(rt),T3_sec_mean_lemon_rt = mean(rt)/1000,lemon_count = n()) %>% ungroup()

angular_lemon_check2 <- angular_lemon_2 %>% group_by(participant_id) %>% summarise(count = n())

angular_lemon_rt <- angular_lemon_2 %>% group_by(participant_id) %>% summarise(T3_sec_time_spent_lemon = sum(T3_sec_mean_lemon_rt), lemon_count = n())

angular_lemon_rt$T3_min_time_spent_lemon <- angular_lemon_rt$T3_sec_time_spent_lemon/60


ggplot(angular_lemon_rt, aes(T3_min_time_spent_lemon))+geom_boxplot()

#winsorize
angular_lemon_rt_winsor <- angular_lemon_rt
angular_lemon_rt_winsor$T3_min_time_spent_lemon_winsor <- DescTools::Winsorize(angular_lemon_rt_winsor$T3_min_time_spent_lemon,probs = c(0.01,0.99), na.rm = T)




#outlier detection
outlier_T3_p <- angular_lemon_rt %>% ungroup() %>% filter(!is.na(T3_min_time_spent_lemon))

outlier_T3_p<- outlier_T3_p %>% select(participant_id,T3_min_time_spent_lemon) %>% filter(abs(T3_min_time_spent_lemon-stats::median(T3_min_time_spent_lemon,na.rm = T))/stats::mad(T3_min_time_spent_lemon,na.rm = T)>3.0) 

T3_outliers <-outlier_T3_p



E_T3 <- angular_lemon_rt %>% select(participant_id,T3_min_time_spent_lemon)

E_T3$T3_min_time_spent_lemon_winsor<-angular_lemon_rt_winsor$T3_min_time_spent_lemon_winsor

#add to engagement.time dataframe
engagement.time3 <- engagement.time2 %>% left_join(E_T3, by = c("participant_id"))

#--------------------------------------------------------------------------------#
# T_3_2: time spent on use your imagination-----
#--------------------------------------------------------------------------------#

#filter values for Use Your Imagination, only appears in the firstSession
angular_imagine<- dat.3$angular_training %>% filter(step_title == "Use Your Imagination")

#use your imagination, most participants have 12 entries
angular_imagine_check <- angular_imagine %>% group_by(participant_id) %>% summarise(count = n())

#use your imagination, session_counter
angular_imagine_check_2 <- angular_imagine %>% group_by(participant_id,session_counter) %>% summarise(count = n())

#4 participants have less than 12 entries
nrow(angular_imagine_check %>% filter(count <12))
#107 participants have more than 12 entries
nrow(angular_imagine_check %>% filter(count >12))

#check to see if there are duplicated entries for imagine
duplicated.intro.check.imagine<- duplicated(angular_imagine[,c("participant_id","session_and_task_info","stimulus_name","date_as_POSIXct")])
#no duplicates
duplicated.intro.check2.imagine <- angular_imagine[duplicated.intro.check.imagine,]


angular_imagine_2 <- angular_imagine %>% filter(trial_type == "page")
#calculate the mean time imagine entry
angular_imagine_3 <-
  angular_imagine_2 %>% 
  group_by(participant_id, session_counter) %>% 
  summarise(T3_2_milisec_mean_imagine_rt = mean(rt),T3_2_sec_mean_imagine_rt = mean(rt)/1000,imagine_count = n()) %>% ungroup()

angular_imagine_rt <- angular_imagine_3 %>% group_by(participant_id) %>% summarise(T3_2_sec_time_spent_imagine = sum(T3_2_sec_mean_imagine_rt), imagine_count = n())

angular_imagine_rt$T3_2_min_time_spent_imagine<- angular_imagine_rt$T3_2_sec_time_spent_imagine/60


ggplot(angular_imagine_rt, aes(T3_2_min_time_spent_imagine))+geom_boxplot()



#winsorize
angular_imagine_rt_winsor <- angular_imagine_rt
angular_imagine_rt_winsor$T3_2_min_time_spent_imagine_winsor <- DescTools::Winsorize(angular_imagine_rt_winsor$T3_2_min_time_spent_imagine,probs = c(0.01,0.99), na.rm = T)

ggplot(angular_imagine_rt_winsor, aes(T3_2_min_time_spent_imagine_winsor))+geom_boxplot()



#outlier detection
outlier_T3_2_p <- angular_imagine_rt %>% ungroup() %>% filter(!is.na(T3_2_min_time_spent_imagine))

outlier_T3_2_p<- outlier_T3_2_p %>% select(participant_id,T3_2_min_time_spent_imagine) %>% filter(abs(T3_2_min_time_spent_imagine-stats::median(T3_2_min_time_spent_imagine,na.rm = T))/stats::mad(T3_2_min_time_spent_imagine,na.rm = T)>3.0) 

T3_2_outliers <-outlier_T3_2_p



E_T3_2 <- angular_imagine_rt %>% select(participant_id,T3_2_min_time_spent_imagine)

E_T3_2$T3_2_min_time_spent_imagine_winsor<-angular_imagine_rt_winsor$T3_2_min_time_spent_imagine_winsor

#add to engagement.time dataframe
engagement.time3.2 <- engagement.time3 %>% left_join(E_T3_2, by = c("participant_id"))

mice::md.pattern(engagement.time3.2,rotate.names = T)

#--------------------------------------------------------------------------------#
# time spent on set of assessments given in eligibility, preTest, and session 1
#--------------------------------------------------------------------------------#
#let's create the assessment table
columns <- c("participant_id","session_only","date_as_POSIXct","system_date_time_earliest","system_date_time_latest","time_on_page","time_on_page_mean","n_rows","tag")

assessments.table <- createAssessmentTable(dat.3,columns)

assessment.elig.pretest <- assessments.table %>% filter(session_only %in% c("Eligibility", "preTest")) %>% select(assessment_name) %>% unique() %>% pull()
  
assessment.1 <-assessments.table %>% filter(session_only %in% c("firstSession")) %>% select(assessment_name) %>% unique() %>% pull()

assessments.analyze <- unique(c(assessment.elig.pretest,assessment.1))

assessments.analyze

#Exclude:
#mental_health_history: 4 questions
#cc 2 questions
#demographics 10 questions
#technology_use 4 questions
#coach_prompt 4 questions exclude
#preaffect 1 question
#postaffect 1 question
#anxiety_identity 1 question
#comorbid 5 questions
#Wellness 9 questions
#Mechanisms 6 questions
#session_review: 4 questions asking about setting while completing study, include
#return_intention: 2 questions, could include as a metric itself

#Include:
#dass21_as: 7 questions, outcome being analyzed
#oa: 5 question, outcome being analyzed
#rr: 36 questions, outcome being analyzed
#bbsiq: 14 situations, 42 questions, outcome being analyzed
#anxiety_triggers: 6 questions
#credibility 2 questions

assessments.include <- c("dass21_as","oa","rr","bbsiq","anxiety_triggers","credibility")

assessments.time <- assessments.table %>% filter(assessment_name %in% assessments.include)

#we limited assessments to only those where particpanst had at least one entry. Exclude the following. 
assessments.exclude <- c("cc","session_review","return_intention","postaffect","help_seeking","evaluation","coach_prompt","cc","assessing_program")

assessments.time<- assessments.table %>% filter(!(assessment_name %in% assessments.exclude))

#not all tables have time_on_page_mean, so if they, do use that time, otherwise, use time_on_page
assessments.time2 <-assessments.time %>% mutate(time_min_on_page_final = case_when(
                                                                                    is.na(time_on_page_mean) ~ round(time_on_page/60,digits = 3),
                                                                                    TRUE ~ round(time_on_page_mean/60,digits = 3)
                                                                                  ))


#calculate the mean assessment time for each assessment
assessment.time3 <- assessments.time2 %>% 
                         select(participant_id,
                                assessment_name,
                                time_min_on_page_final) %>% 
                         group_by(participant_id,assessment_name) %>%
                         mutate(mean_assessment_time = mean(time_min_on_page_final), sd_assessment_time = sd(time_min_on_page_final), count = n()) %>% slice_tail() 


#keep only one entry per participant per assessment
assessment.time.4 <- assessment.time3 %>% select(participant_id,assessment_name,mean_assessment_time) %>%  slice_tail()

# pivot wide so that each column is the mean time for assessment X
assessment.time.wide <-
  assessment.time.4 %>%  pivot_wider(names_from = "assessment_name",
                                     values_from = "mean_assessment_time",
                                     names_glue = "T4_min_{.value}_{assessment_name}") 
#%>%
 #                        replace(is.na(.), 0) #replace NA values with 0 since if a participant did not complete an assessment at least once then their time spent on that assessment is 0

#count NAs
apply( assessment.time.wide , 2 , function(x) sum ( is.na(x) ) )

#reshape dataframe
assessment.time.wide.m <- melt(as.data.frame(assessment.time.wide),id.vars='participant_id', measure.vars=colnames(assessment.time.wide[-1]))


#plot boxplot+violin plot
ggplot(assessment.time.wide.m, aes(value, variable))+geom_boxplot()+
  theme_classic()

ggplot(assessment.time.wide.m, aes(x = value))+geom_density()+facet_wrap(~variable)+
  theme_classic()

ggplot(assessment.time.wide.m, aes(x = value))+geom_density()+facet_wrap(~variable,scales = "free")+
  theme_classic()

ggplot(assessment.time.wide, aes(x = T4_min_mean_assessment_time_credibility))+geom_density()



#winsorize data
assessment.time.wide.winsor <- assessment.time.wide
winsor.columns.assessments <- grep("T4_",colnames(assessment.time.wide.winsor))
assessment.time.wide.winsor[winsor.columns.assessments] <- lapply(assessment.time.wide.winsor[winsor.columns.assessments],DescTools::Winsorize,probs = c(0.01,0.99), na.rm = T)

colnames(assessment.time.wide.winsor)[winsor.columns.assessments] <- paste0(colnames(assessment.time.wide.winsor[winsor.columns.assessments]),"_winsor")


#reshape dataframe
assessment.time.wide.winsor.m <- melt(as.data.frame(assessment.time.wide.winsor),id.vars='participant_id', measure.vars=colnames(assessment.time.wide.winsor[-1]))


#plot boxplot+violin plot
ggplot(assessment.time.wide.winsor.m, aes(value, variable))+geom_boxplot()+
  theme_classic()

ggplot(assessment.time.wide.winsor.m, aes(x = value))+geom_density()+facet_wrap(~variable)+
  theme_classic()

ggplot(assessment.time.wide.winsor.m, aes(x = value))+geom_density()+facet_wrap(~variable,scales = "free")+
  theme_classic()



T4_assessments <- tibble(participant_id = assessment.time.wide$participant_id)  
columns <- grep("T4_min", names(assessment.time.wide))
columnnames <- colnames(assessment.time.wide[columns])
for (col in columnnames){
  assessment.time.wide2 <- assessment.time.wide %>% select(participant_id,col) %>% ungroup()
  T4_assessments <- outlierCalculation(assessment.time.wide2,col,T4_assessments)
  
}


T4_outliers <- T4_assessments %>% select(participant_id, grep("outlierScore",names(T4_assessments)))

T4_outliers<- T4_outliers %>% mutate(totalOutlierScore =  rowSums(across( grep("outlierScore",names(T4_outliers))))) %>% filter(totalOutlierScore>0) %>% arrange(desc(totalOutlierScore))

T4_outliers_ps <- T4_outliers %>% filter(totalOutlierScore >4) %>%  select(participant_id) %>% unique() %>% pull()

#plot boxplot and highlight outlier point
ggplot(assessment.time.wide.m, aes(value, variable))+geom_boxplot()+geom_point(                               # add the highlight points
  data=subset(assessment.time.wide.m, participant_id %in% T4_outliers_ps ), 
  aes(x=value, y=variable), 
  color="red", size=3
)+
  theme_classic()


E_T4 <- cbind(assessment.time.wide,assessment.time.wide.winsor[-1])

  
#add to engagement.time dataframe
engagement.time4 <- engagement.time3.2 %>% left_join(E_T4, by = c("participant_id"))

# ---------------------------------------------------------------------------- #
# outliers -----
# ---------------------------------------------------------------------------- #
T4_outliers <- T4_assessments %>% select(participant_id, grep("outlierScore",names(T4_assessments))) %>% ungroup()
T1_outliers_p_ids <- T1_outliers %>% select(participant_id) %>% unique() %>% pull()
T2_outliers_p_ids <- T2_outliers %>% select(participant_id) %>% unique() %>% pull()
T3_outliers_p_ids <- T3_outliers %>% select(participant_id) %>% unique() %>% pull()
T3_2_outliers_p_ids <- T3_2_outliers %>% select(participant_id) %>% unique() %>% pull()


outliers <- T4_outliers
outliers<- outliers %>% mutate(outlierScore_T1 = ifelse(participant_id %in% T1_outliers_p_ids,1,0),outlierScore_T2 = ifelse(participant_id %in% T2_outliers_p_ids,1,0),outlierScore_T3 = ifelse(participant_id %in% T3_outliers_p_ids,1,0),outlierScore_T3_2 = ifelse(participant_id %in% T3_2_outliers_p_ids,1,0))
  
outliers <- outliers %>% mutate(totalOutlierScore =  rowSums(outliers[-1])) %>% filter(totalOutlierScore>0) %>% arrange(desc(totalOutlierScore))
outliers$prop <- outliers$totalOutlierScore/19


engagement.time4 <- engagement.time4 %>% left_join(select(outliers,participant_id,totalOutlierScore))


engagement.time4$totalOutlierScore[is.na(engagement.time4$totalOutlierScore)]<-0

#776 and 169 were flagged as outliers in more than 14 features, remove from analysis

# ---------------------------------------------------------------------------- #
# Save table -----
# ---------------------------------------------------------------------------- #
#save as RData file to be used in later steps
#replace NAs with 0
E_T_FINAL_NA_1 <- engagement.time4
E_T_FINAL_1 <- engagement.time4 %>% replace(is.na(.), 0) 
save(E_T_FINAL_1,E_T_FINAL_NA_1, file = here("Scripts2","Data_Primary_Analysis","E_T_1_Winsor_Complete.RData"))





