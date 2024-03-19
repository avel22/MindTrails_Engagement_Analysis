### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is to contrast cluster group membership on a series of variables to further describe how the clusters differ from one another.

### We will be conducting statistical tests that include independent samples t-test for continuous and normal variables, Wilcoxon-Mann Whitney test for ordinal or non-normal continuous variables, and chi-square or Fisher's exact test for categorical variables.

### Based on Bethany Teachman's recommendation the variables that we will be focusing are:
### Demographics:
# Age
# Sex/gender identity
# Race
# Ethnicity
# Education

### Mental health:
# DASS-anxiety baseline score
# OASIS baseline score

### Interpretation bias:
# RR - negative threat relevant bias score
# RR - positive threat relevant bias score
# BBSIQ - negative bias score
# 
### Credibility:
# Credibility Online

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,stargazer,forcats)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Scripts2","Data","2_Calm_2.RData"))

load(here("Scripts2","Data","participant_cluster.RData"))

load(here("Scripts2","Data","dem_tbl.RData"))

load(here("Scripts2","Data","outcomes_df_for_imputation.RData"))


#check 

averages <- outcomes.scores.df %>%
  group_by(session_only, engagement_cluster) %>%
  summarise(
    mean_OA = mean(OA_MeanScore, na.rm = TRUE),
    mean_BBSIQ = mean(BBSIQ_MeanScore, na.rm = TRUE),
    mean_RR_POS_BIAS = mean(RR_POS_BIAS_MeanScore, na.rm = TRUE),
    mean_RR_NEG_BIAS = mean(RR_NEG_BIAS_MeanScore, na.rm = TRUE),
    mean_DASS21 = mean(DASS21_MeanScore, na.rm = TRUE)
  )

#--------------------------------------------------------------------------------#
# Demographics ----
#--------------------------------------------------------------------------------#
# Age
# Sex/gender identity
# Race
# Ethnicity
# Education
colnames(dem_tbl)
dem_char_compar <- dem_tbl %>% select("participant_id","cluster","age","gender","race_col","ethnicity","education")

# 1. T-test for age
# First check for assumptions like normality
# Using the Shapiro-Wilk test for normality for each cluster
shapiro_test_cluster1 <- shapiro.test(dem_char_compar$age[dem_char_compar$cluster == "1"])
shapiro_test_cluster1$p.value
shapiro_test_cluster2 <- shapiro.test(dem_char_compar$age[dem_char_compar$cluster == "2"])
shapiro_test_cluster2$p.value

# If both p-values from the Shapiro-Wilk test are greater than 0.05, we can assume normality.
#Since both values are less than 0.05 the data significantly deviates from a normal distribution
# 1. Mann-Whitney U test for age (since it's not normally distributed)
age_mw <- wilcox.test(age ~ cluster, data = dem_char_compar)

# 2. Chi-squared test for gender
# Transform the gender column
#convert prefer not to answer to NA
dem_char_compar$gender[dem_char_compar$gender == "Prefer not to answer"] <- NA

dem_char_compar <- dem_char_compar %>% mutate(genderGrp = fct_collapse(gender,
                              `Male` = c("Male"),
                              `Female` = c("Female"),
                              `Transgender/Other` = c("Transgender", "Transgender Female", "Transgender Male", "Other")))

#refactor grouped column
dem_char_compar$genderGrp = factor(dem_char_compar$genderGrp )


gender_table <- table(dem_char_compar$genderGrp, dem_char_compar$cluster)
gender_table

# Conduct chi-squared test on combined table
gender_chisq <- chisq.test(gender_table)

# Since no value is less than 5 we can use the Chi Squared Test
gender_chisq$expected


# 3. Fisher test for race_col
dem_char_compar$race_col[dem_char_compar$race_col == "Prefer not to answer"] <- NA
dem_char_compar$race_col = factor(dem_char_compar$race_col )

race_table <- table(dem_char_compar$race_col, dem_char_compar$cluster)
race_table

race_chisq <- chisq.test(race_table)
race_chisq
race_chisq$expected

race_fisher <- fisher.test(race_table,simulate.p.value = TRUE, B = 10000000)
race_fisher

# 4. Chi-squared test for ethnicity
dem_char_compar$ethnicity[dem_char_compar$ethnicity == "Prefer not to answer"] <- NA
dem_char_compar$ethnicity[dem_char_compar$ethnicity == "Unknown"] <- NA
dem_char_compar$ethnicity = factor(dem_char_compar$ethnicity )
ethnicity_table <- table(dem_char_compar$ethnicity, dem_char_compar$cluster)
ethnicity_table
ethnicity_chisq <- chisq.test(ethnicity_table)
ethnicity_chisq$expected

# 5. Mann-Whitney U test for education
# To simplify conceptual overlap for creating an ordered variable, group
# "Elementary School," "Junior High," and "Some High School" into "Not High School
# Graduate"; group "Associate's Degree" and "Some College" into "Some College"; 
# rename "Bachelor's Degree" as "College Graduate"; and group "J.D.," "M.B.A.," 
# "M.D.," "Master's Degree," "Other Advanced Degree," and "Ph.D." into 
# "Advanced degree."
levels(dem_char_compar$education)
# Transform the education column
dem_char_compar <- dem_char_compar %>%
  mutate(educationGrp = fct_collapse(education,
                              `Not High School Graduate` = c("Junior High", "Some High School"),
                              `Some College` = c("Associate's Degree", "Some College"),
                              `College Graduate` = "Bachelor's Degree",
                              `Advanced degree` = c("J.D.", "M.B.A.", "M.D.", "Master's Degree", 
                                                    "Other Advanced Degree", "Ph.D.")))

levels(dem_char_compar$educationGrp)

dem_char_compar$educationGrp[dem_char_compar$educationGrp == "Prefer not to answer"] <- NA
dem_char_compar$educationGrp[dem_char_compar$educationGrp == "Unknown"] <- NA


# First, ensure the factor levels are ordered correctly, then conduct the test
dem_char_compar$educationGrp = factor(dem_char_compar$educationGrp, ordered = TRUE)
levels(dem_char_compar$educationGrp)

# Convert the ordered factor levels of 'education' to numeric values
dem_char_compar$education_numeric <- as.numeric(dem_char_compar$educationGrp)
education_mw <- wilcox.test(education_numeric ~ cluster, data = dem_char_compar)

educationGrp_table = table(dem_char_compar$educationGrp, dem_char_compar$cluster)
educationGrp_table

# Format Results from test

results_dem_tests <- data.frame(
  Demographic = c("Age","Gender","Race","Ethnicity","Education"),
  Test = c("Wilcoxon rank-sum test", "Chi-squared", "Fisher’s Exact Test", "Chi-squared", "Wilcoxon rank-sum test"),
  Statistic = c(
    age_mw$statistic,
    gender_chisq$statistic,
    NA,
    ethnicity_chisq$statistic,
    education_mw$statistic
  ),
  p = c(
    age_mw$p.value,
    gender_chisq$p.value,
    race_chisq$p.value,
    ethnicity_chisq$p.value,
    education_mw$p.value
  )
)

results_dem_tests

# Function to format p-values
format_pvalue <- function(p) {
  if (p < 0.001) {
    return("<0.001***")
  } else if (p < 0.01) {
    return(sprintf("%.3f**", p))
  } else if (p < 0.05) {
    return(sprintf("%.3f*", p))
  } else {
    return(sprintf("%.3f", p))
  }
}

# Function to format the Statistic column
format_statistic <- function(statistic) {
  formatted_statistic <- formatC(statistic, format = "f", digits = 1, big.mark = ",")
  return(formatted_statistic)
}


# Apply the function to the Statistic column
results_dem_tests$Statistic <- sapply(results_dem_tests$Statistic, format_statistic)

# Adjust the p_value column
results_dem_tests$p <- sapply(results_dem_tests$p, format_pvalue)


results_dem_tests


#--------------------------------------------------------------------------------#
# Assessments----
#--------------------------------------------------------------------------------#
# Factor the engagement_cluster column
outcomes.scores.df$engagement_cluster <- factor(outcomes.scores.df$engagement_cluster, levels = c(1, 2))

#--------------------------------------------------------------------------------#
# Mental health----
#--------------------------------------------------------------------------------#
# DASS-anxiety baseline score
dass_outcomes = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,DASS21_MeanScore)
dass_bl = dass_outcomes %>% filter(session_only == "preTest")
# OASIS baseline score
oasis_outcomes = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,OA_MeanScore)
oasis_bl = oasis_outcomes %>% filter(session_only == "preTest")
#--------------------------------------------------------------------------------#
# Interpretation bias ----
#--------------------------------------------------------------------------------#
# RR - negative threat relevant bias score
rr_neg_outcomes = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,RR_NEG_BIAS_MeanScore)
rr_neg_bl = rr_neg_outcomes %>% filter(session_only == "preTest")

rrnegbiasmeanscore <- rr_neg_bl %>%
  group_by(engagement_cluster) %>%
  summarise(
    mean_RR_NEG_BIAS = mean(RR_NEG_BIAS_MeanScore, na.rm = TRUE),
    sd_RR_NEG_BIAS = sd(RR_NEG_BIAS_MeanScore, na.rm = TRUE)
  )




colnames(rr_neg_bl)
# RR - positive threat relevant bias score
rr_pos_outcomes = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,RR_POS_BIAS_MeanScore)
rr_pos_bl = rr_pos_outcomes %>% filter(session_only == "preTest")

rrposbiasmeanscore <- rr_pos_bl %>%
  group_by(engagement_cluster) %>%
  summarise(
    mean_RR_pos_BIAS = mean(RR_POS_BIAS_MeanScore, na.rm = TRUE),
    sd_RR_pos_BIAS = sd(RR_POS_BIAS_MeanScore, na.rm = TRUE)
  )

# BBSIQ - negative bias score
bbsiq_outcomes = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,BBSIQ_MeanScore)
bbsiq_bl = bbsiq_outcomes %>% filter(session_only == "preTest")

#--------------------------------------------------------------------------------#
# Credibility Online----
#--------------------------------------------------------------------------------#
#How confident are you that an online training program will reduce your anxiety?
cred_online = outcomes.scores.df %>% select(participant_id,engagement_cluster,session_only,cred_online_imp)
cred_online_bl = cred_online %>% filter(session_only == "preTest")


# The comparison function remains mostly unchanged
compare_means <- function(dataframe, score_column) {
  # Print the number of NA values in the score column
  na_count <- sum(is.na(dataframe[[score_column]]))
  cat(paste("Number of NA values in", score_column, ":", na_count, "\n"))
  # Remove rows with NA in the score column
  dataframe <- dataframe[!is.na(dataframe[[score_column]]), ]
  
  # Ensure the score column is numeric
  dataframe[[score_column]] <- as.numeric(dataframe[[score_column]])
  
  
  shapiro_test_cluster1 <- shapiro.test(dataframe[[score_column]][dataframe$engagement_cluster == "1"])
  shapiro_test_cluster2 <- shapiro.test(dataframe[[score_column]][dataframe$engagement_cluster == "2"])
  
  if (shapiro_test_cluster1$p.value > 0.05 && shapiro_test_cluster2$p.value > 0.05) {
    test_result <- t.test(dataframe[[score_column]] ~ dataframe$engagement_cluster)
    test_name <- "Two-sample t-test"
  } else {
    test_result <- wilcox.test(dataframe[[score_column]] ~ dataframe$engagement_cluster)
    test_name <- "Wilcoxon rank-sum test"
  }
  
  return(list(test_name = test_name, test_result = test_result))
}

# List of dataframes and their corresponding score columns
dfs <- list(dass_bl = "DASS21_MeanScore", 
            oasis_bl = "OA_MeanScore", 
            rr_neg_bl = "RR_NEG_BIAS_MeanScore",
            bbsiq_bl = "BBSIQ_MeanScore",
            rr_pos_bl = "RR_POS_BIAS_MeanScore",
            cred_online_bl = "cred_online_imp"
            )

# Initialize an empty results dataframe
results_assessments <- data.frame(
  Score = character(),
  Test = character(),
  Statistic = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each dataframe and compile results
for (df_name in names(dfs)) {
  test_output <- compare_means(get(df_name), dfs[[df_name]])
  results_assessments <- rbind(results_assessments, data.frame(
    Score = dfs[[df_name]],
    Test = test_output$test_name,
    Statistic = test_output$test_result$statistic,
    p = test_output$test_result$p.value
  ))
}

results_assessments$p <- sapply(results_assessments$p, format_pvalue)
results_assessments$Statistic <- sapply(results_assessments$Statistic, format_statistic)

results_assessments

results_dem_tests



#--------------------------------------------------------------------------------#
# PHQ-2 and AUDIT-C----
#--------------------------------------------------------------------------------#
#Yep, for baseline depression symptoms (PHQ-2) take the mean of available "depressed" and "pleasure" items in the "comorbid" table.

#And yes, while you're at it, might as 

#I think we've already handled unexpected multiple entries, but just make sure that each participant has only one row at baseline (let me know if not).


#776 and 169 were flagged as outliers in more than 14 features, remove from analysis

comorbid = dat.3$comorbid %>% filter(!participant_id %in% c(776, 169))

# join cluster ID
comorbid_2 = left_join(comorbid,participant_cluster, by = "participant_id")
comorbid_2 = comorbid_2 %>% rename("engagement_cluster" = "cluster")

#baseline results
comorbid_bl = comorbid_2 %>% filter(session_only == "preTest") 

#baseline depression symptoms (PHQ-2) take the mean of available "depressed" and "pleasure" items in the "comorbid" table.
comorbid_bl_phq_2 = comorbid_bl %>% select("participant_id","engagement_cluster","session_only","depressed" , "pleasure" )

#The other data cleaning steps would be to confirm the response ranges make sense (1 to 4 for PHQ-2 items and 0 to 4 for AUDIT-C items) and replace 555 ("prefer not to answer") with NA before computing the means.
summary(comorbid_bl_phq_2)
#There are some 555 answers, change to NA
comorbid_bl_phq_2$depressed[comorbid_bl_phq_2$depressed == 555] <- NA
comorbid_bl_phq_2$pleasure[comorbid_bl_phq_2$pleasure == 555] <- NA

#4 NAs for depressed, 2 NAs for pleasure
summary(comorbid_bl_phq_2)

#compute the mean ignoring NAs
comorbid_bl_phq_2 <- comorbid_bl_phq_2 %>%
  mutate(mean_phq_2 = (depressed + pleasure) / 2)


#audit-c is the mean of available "how_often", "number_of_drinks", and "six_or_more" in the "comorbid" table.
comorbid_bl_audit_c = comorbid_bl %>% select("participant_id","engagement_cluster","session_only","how_often", "number_of_drinks", "six_or_more" )

#The other data cleaning steps would be to confirm the response ranges make sense (1 to 4 for PHQ-2 items and 0 to 4 for AUDIT-C items) and replace 555 ("prefer not to answer") with NA before computing the means.
summary(comorbid_bl_audit_c)
#There are some 555 answers, change to NA
comorbid_bl_audit_c$how_often[comorbid_bl_audit_c$how_often == 555] <- NA
comorbid_bl_audit_c$number_of_drinks[comorbid_bl_audit_c$number_of_drinks == 555] <- NA
comorbid_bl_audit_c$six_or_more[comorbid_bl_audit_c$six_or_more == 555] <- NA


#1 NAs for how_often, 5 NAs for number_of_drinks, 1 NA for six_or_more
summary(comorbid_bl_audit_c)

#compute the mean ignoring NAs
comorbid_bl_audit_c <- comorbid_bl_audit_c %>%
  mutate(mean_audit_c = (how_often + number_of_drinks + six_or_more ) / 3)


#statistical tests

# List of dataframes and their corresponding score columns
dfs_phq_acid <- list(comorbid_bl_phq_2 = "mean_phq_2", 
            comorbid_bl_audit_c = "mean_audit_c"
)

# Initialize an empty results dataframe
results_phq_acid <- data.frame(
  Score = character(),
  Test = character(),
  Statistic = numeric(),
  p = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each dataframe and compile results
for (df_name in names(dfs_phq_acid)) {
  test_output <- compare_means(get(df_name), dfs_phq_acid[[df_name]])
  results_phq_acid <- rbind(results_phq_acid, data.frame(
    Score = dfs_phq_acid[[df_name]],
    Test = test_output$test_name,
    Statistic = test_output$test_result$statistic,
    p = test_output$test_result$p.value
  ))
}

results_phq_acid$p <- sapply(results_phq_acid$p, format_pvalue)
results_phq_acid$Statistic <- sapply(results_phq_acid$Statistic, format_statistic)

results_phq_acid
