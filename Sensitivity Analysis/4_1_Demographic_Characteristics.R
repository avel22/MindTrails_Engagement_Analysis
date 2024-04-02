### Adapted from Jeremy Eberle's code
### https://github.com/jwe4ec/fy7e6/blob/develop/code/04_further_clean_demog_data.R
### Written by: Ángel Vela and Jeremy Eberle
### MT Engagement Analysis
### University of Virginia
### August 2023
### The purpose of this script is to get demographic characteristics of sample being analyzed

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,stargazer)

#--------------------------------------------------------------------------------#
# loading the data ----
#--------------------------------------------------------------------------------#
load(here("Data_Sensitivity_Analysis","1_Import_Data_Dat3.RData"))
load(here("Data_Sensitivity_Analysis","3_Clustering_627.RData"))

#--------------------------------------------------------------------------------#
# variables ----
#--------------------------------------------------------------------------------#
p_ids <- cluster_summary %>% select(participant_id)%>% pull()

dat.3 <- lapply(dat.3, function(x) if ("participant_id" %in% colnames(x)) {subset(x,  (participant_id %in% p_ids))} else x)

sessions <- c("preTest","firstSession","secondSession","thirdSession","fourthSession","fifthSession","PostFollowUp")

dat <- dat.3
# ---------------------------------------------------------------------------- #
# Recode "Prefer not to answer" in categorical and ordinal variables ----
# ---------------------------------------------------------------------------- #

pna <- "Prefer not to answer"

# Recode 555 as "Prefer not to answer". 555 in "birth_year" (integer variable)
# is recoded after "age" is computed below.

target_vars <- c("education", "employment_stat", "ethnicity", "gender", "income",
                 "marital_stat")
dat$demographics[, target_vars][dat$demographics[, target_vars] == 555] <- pna

dat$demographics_race$race[dat$demographics_race$race == 555] <- pna

# Recode "NoAnswer" as "Prefer not to answer"

dat$demographics$country[dat$demographics$country == "NoAnswer"] <- pna

# ---------------------------------------------------------------------------- #
# Compute age ----
# ---------------------------------------------------------------------------- #

# Compute age (use NA where "birth_year" is 555 ["prefer not to answer"])

dat$demographics$age <- NA

for (i in 1:nrow(dat$demographics)) {
  if (dat$demographics$birth_year[i] != 555) {
    dat$demographics$age[i] <- 
      as.integer(format(dat$demographics$date_as_POSIXct[i], "%Y")) - 
      dat$demographics$birth_year[i]
  }
}

# Participants range from 18 to 75 years of age, which is reasonable

range(dat$demographics$age, na.rm = TRUE)

# All NAs in "age" are due to "birth_year" of 555

all(dat$demographics$birth_year[is.na(dat$demographics$age)] == 555)

# Recode 555 in "birth_year" to "Prefer not to answer"

dat$demographics$birth_year[dat$demographics$birth_year == 555] <- pna


# ---------------------------------------------------------------------------- #
# Clean gender ----
# ---------------------------------------------------------------------------- #

# Reorder levels and add "Transgender Female". Codebook indicates that on 8/5/2019, 
# "Transgender" option was replaced with "Transgender Male" and "Transgender Female".

dat$demographics$gender <- 
  factor(dat$demographics$gender,
         levels = c("Female", "Male", 
                    "Transgender", "Transgender Female", "Transgender Male",
                    "Other", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean ethnicity ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$ethnicity <- 
  factor(dat$demographics$ethnicity,
         levels = c("Hispanic or Latino", "Not Hispanic or Latino", 
                    "Unknown", "Prefer not to answer"))


# ---------------------------------------------------------------------------- #
# Clean education ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$education <-
  factor(dat$demographics$education,
         levels = c("Junior High", "Some High School", "High School Graduate", 
                    "Some College", "Associate's Degree", "Bachelor's Degree",
                    "Some Graduate School", "Master's Degree", "M.B.A.", "J.D.", 
                    "M.D.", "Ph.D.", "Other Advanced Degree",
                    "Prefer not to answer"))


# ---------------------------------------------------------------------------- #
# Clean employment status ----
# ---------------------------------------------------------------------------- #

# Recode "Homemaker"

dat$demographics$employment_stat[dat$demographics$employment_stat ==
                                   "Homemaker/keeping house or raising children full-time"] <-
  "Homemaker"

# Reorder levels

dat$demographics$employment_stat <- 
  factor(dat$demographics$employment_stat,
         levels = c("Student", "Homemaker", "Unemployed or laid off", "Looking for work",
                    "Working part-time", "Working full-time", "Retired", "Other",
                    "Unknown", "Prefer not to answer"))


# ---------------------------------------------------------------------------- #
# Clean income ----
# ---------------------------------------------------------------------------- #

# Recode "Don't know"

dat$demographics$income[dat$demographics$income == "Don't know"] <- "Unknown"

# Reorder levels

dat$demographics$income <-
  factor(dat$demographics$income,
         levels = c("Less than $5,000", "$5,000 through $11,999", 
                    "$12,000 through $15,999", "$16,000 through $24,999", 
                    "$25,000 through $34,999", "$35,000 through $49,999",
                    "$50,000 through $74,999", "$75,000 through $99,999",
                    "$100,000 through $149,999", "$150,000 through $199,999",
                    "$200,000 through $249,999", "$250,000 or greater",
                    "Unknown", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean marital status ----
# ---------------------------------------------------------------------------- #

# Reorder levels

dat$demographics$marital_stat <-
  factor(dat$demographics$marital_stat,
         levels = c("Single", "Single,dating", "Single,engaged", "Single,marriagelike",
                    "Married", "civilunion", "Separated", "Divorced", "Widow/widower",
                    "Other", "Prefer not to answer"))


# ---------------------------------------------------------------------------- #
# Clean race ----
# ---------------------------------------------------------------------------- #

# No duplicated responses exist

nrow(dat$demographics_race[duplicated(dat$demographics_race[, c("participant_id", 
                                                                "race")]), ]) == 0

# Compute number of responses per "participant_id"

race_ag <- aggregate(race ~ participant_id,
                     dat$demographics_race,
                     length)
names(race_ag)[names(race_ag) == "race"] <- "race_cnt"

dat$demographics_race <- merge(dat$demographics_race, race_ag,
                               by = "participant_id", all.x = TRUE)

# Define "race_col", collapsing cases of more than one race

dat$demographics_race$race_col <- NA

for (i in 1:nrow(dat$demographics_race)) {
  if (dat$demographics_race$race_cnt[i] == 1) {
    dat$demographics_race$race_col[i] <- dat$demographics_race$race[i]
  } else if (dat$demographics_race$race_cnt[i] > 1) {
    dat$demographics_race$race_col[i] <- "More than one race"
  }
}


dat$demographics_race <- dat$demographics_race[dat$demographics_race$participant_id != 1992, ]





# Add "race_col" to "demographics" table

dat$demographics <- merge(dat$demographics,
                          unique(dat$demographics_race[, c("participant_id", 
                                                           "race_col")]),
                          by = "participant_id",
                          all.x = TRUE)

# Reorder levels

dat$demographics$race_col <-
  factor(dat$demographics$race_col,
         levels = c("American Indian/Alaska Native", "Black/African origin",
                    "East Asian", "Native Hawaiian/Pacific Islander", "South Asian",
                    "White/European origin", "More than one race", "Other or Unknown", 
                    "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Clean country ----
# ---------------------------------------------------------------------------- #

# Recode "�.land Islands"

dat$demographics$country[dat$demographics$country == "�.land Islands"] <- "�land Islands"

# Define desired levels order (decreasing frequency ending with "prefer not to answer")

country_levels <- names(sort(table(dat$demographics$country), decreasing = TRUE))
country_levels <- c(country_levels[country_levels != pna], pna)

# Reorder levels of "country"

dat$demographics$country <- factor(dat$demographics$country, levels = country_levels)

# Define "country_col", collapsing countries with fewer than 5 participants into "Other"

top_countries <- 
  names(table(dat$demographics$country)[as.numeric(table(dat$demographics$country)) > 5])

all(top_countries == c("United States", "Australia", "Canada", "United Kingdom"))

dat$demographics$country_col <- NA

for (i in 1:nrow(dat$demographics)) {
  if (as.character(dat$demographics$country)[i] %in% top_countries) {
    dat$demographics$country_col[i] <- as.character(dat$demographics$country)[i]
  } else if (as.character(dat$demographics$country)[i] == pna) {
    dat$demographics$country_col[i] <- pna
  } else {
    dat$demographics$country_col[i] <- "Other"
  }
}

# Reorder levels of "country_col"

dat$demographics$country_col <-
  factor(dat$demographics$country_col,
         levels = c(top_countries, "Other", pna))

# ---------------------------------------------------------------------------- #
# Save cleaned data ----
# ---------------------------------------------------------------------------- #

dat.3.demographics.cleaned.627 <- dat

save(dat.3.demographics.cleaned.627, file = here("Data_Sensitivity_Analysis","4_1_Demographic_Characteristics_627.RData"))


# ---------------------------------------------------------------------------- #
# Create demographics tables ----
# ---------------------------------------------------------------------------- #

dem_tbl <- dat$demographics

# Add condition

dem_tbl <- merge(dem_tbl, dat$study[, c("participant_id", "conditioning")],
                 by = "participant_id", all.x = TRUE)

# Add analysis sample indicators

dem_tbl <- merge(dem_tbl,
                 dat$participant[, c("participant_id", "exclude_analysis",
                                     "comp1scenario", "comp1postAffect")],
                 by = "participant_id", all.x = TRUE)


# Add cluster variable
dem_tbl <- merge(dem_tbl,
                 participant_cluster,
                 by = "participant_id", all.x = TRUE)

dem_tbl$cluster <- as.factor(dem_tbl$cluster)

# Define function to compute descriptives

compute_desc <- function(df) {
  # Compute sample size
  
  n <- data.frame(label = "n",
                  value = length(df$participant_id))
  
  # Compute mean and standard deviation for numeric variables
  
  num_res <- data.frame(label = "Age (years): M (SD)",
                        value = paste0(round(mean(df$age, na.rm = TRUE), 2), 
                                       " (", round(sd(df$age, na.rm = TRUE), 2), ")"))
  
  # Compute count and percentage for factor variables
  
  vars <- c("gender", "race_col", "ethnicity", "country_col", "education",
            "employment_stat", "income", "marital_stat")
  var_labels <- paste0(c("Gender", "Race", "Ethnicity", "Country", "Education",
                         "Employment Status", "Annual Income", "Marital Status"),
                       ": n (%)")
  
  fct_res <- data.frame()
  
  for (i in 1:length(vars)) {
    tbl <- table(df[, vars[i]])
    prop_tbl <- round(prop.table(tbl)*100, 1)
    
    tbl_res <- rbind(data.frame(label = var_labels[i],
                                value = NA),
                     data.frame(label = names(tbl),
                                value = paste0(as.numeric(tbl),
                                               " (", as.numeric(prop_tbl), ")")))
    fct_res <- rbind(fct_res, tbl_res)
  }
  
  # Combine results
  
  res <- rbind(n, num_res, fct_res)
  
  return(res)
}

# Define function to compute descriptives by cluster

compute_desc_by_cluster <- function(df) {
  clusters <- levels(droplevels(df$cluster))
  
  for (i in 1:length(clusters)) {
    df_cond <- df[df$cluster == clusters[i], ]
    
    cond_res <- compute_desc(df_cond)
    names(cond_res)[names(cond_res) == "value"] <- clusters[i]
    
    if (i == 1) {
      res_by_cond <- cond_res
    } else if (i > 1) {
      cond_res$label <- NULL
      
      res_by_cond <- cbind(res_by_cond, cond_res)
    }
  }
  
  return(res_by_cond)
}

# Compute descriptives

desrciptive_dem_all_697 <- compute_desc(dem_tbl)

desrciptive_dem_all_697_cluster <- compute_desc_by_cluster(dem_tbl)

descr_all_cluster <- cbind(desrciptive_dem_all_697,desrciptive_dem_all_697_cluster[-1])

indent <- "\MyIndent"
descr_all_cluster$label <- paste("\MyIndent ",descr_all_cluster$label)
colnames(descr_all_cluster) <- c("Characteristic","All","Engagement Cluster 1 : More Time Spent", "Engagement Cluster 2: Less Time Spent")

sink(here("Tables","4_1_desrciptive_dem_all_cluster.txt"))
stargazer(descr_all_cluster, type = "latex", title="Demographic characteristics of analyzed sample and engagement clusters", digits=2, summary = F,rownames = F)
sink()

# Save tables to CSV


write.csv(desrciptive_dem_all_697, 
          here("Tables","4_1_descriptive_dem_all_627.csv"), row.names = FALSE)
write.csv(desrciptive_dem_all_697_cluster, 
          here("Tables","4_1_descriptive_dem_all_627_cluster.csv"), row.names = FALSE)



