#--------------------------------------------------------------------------------#
# Create assessment table from individual tables
#--------------------------------------------------------------------------------#
createAssessmentTable <- function(dat.3, columnsInterest) {
  assessments.table <- data.frame()
  #list of assessments that go into task log except training sessions
  list.task.log.tables <-
    list(
      dass21_as = dat.3$dass21_as,
      credibility = dat.3$credibility,
      demographics = dat.3$demographics,
      mental_health_history = dat.3$mental_health_history,
      anxiety_identity = dat.3$anxiety_identity,
      oa = dat.3$oa,
      anxiety_triggers = dat.3$anxiety_triggers,
      rr = dat.3$rr,
      bbsiq = dat.3$bbsiq,
      comorbid = dat.3$comorbid,
      wellness = dat.3$wellness,
      mechanisms = dat.3$mechanisms,
      technology_use = dat.3$technology_use,
      affect = dat.3$affect,
      cc = dat.3$cc,
      session_review = dat.3$session_review,
      coach_prompt = dat.3$coach_prompt,
      return_intention = dat.3$return_intention,
      help_seeking = dat.3$help_seeking,
      evaluation = dat.3$evaluation,
      assessing_program = dat.3$assessing_program,
      covid19 = dat.3$covid19
    )
  #participant 1762 has two entries for ReturnIntention in the firstSession both in the task_log table and the return_intention table, remove earliest entry   for that particular participant in return_intention table
  #X value is 2934, we will be using time_on_page_mean which captures the mean time for those two entries
  list.task.log.tables$return_intention <- list.task.log.tables$return_intention %>% filter(X != 2934)
  
  columns <- columnsInterest
  #concatenate all tables into one using columns of interest
  for (i in 1:length(list.task.log.tables)) {
    df <- list.task.log.tables[[i]]
    column.present <- colnames(df) %in% columns
    df <- df[column.present]
    df$assessment <- names(list.task.log.tables[i])
    assessments.table <- bind_rows(assessments.table, df)
      }
  
  #add pre post tag label to affect entries
  assessments.table <-
    assessments.table %>% unite(
      c("tag", "assessment"),
      col = "assessment_name",
      na.rm = TRUE,
      sep = "")
      
    #remove entries for covid19 assessment since it is not part of R01 study and null values for participant_ids present in eligibility questionnaires
  assessments.table <-
      assessments.table %>% filter(!is.na(participant_id), assessment_name != "covid19")
  
  return(assessments.table)
}


#--------------------------------------------------------------------------------#
# Min-Max Normalization
#--------------------------------------------------------------------------------#

min_max_normalization <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#--------------------------------------------------------------------------------#
# Identify outliers using robust median absolute deviation approach
# Code from: https://github.com/hauselin/hausekeep/blob/master/R/outliersMAD.R
#--------------------------------------------------------------------------------#

#' @title Identify outliers using robust median absolute deviation approach
#' @name outliersMAD
#'
#' @description outliersMAD is used to identify outliers in vectors using Leys et al.'s (2003) median absolute deviation approach.
#'
#' @param x a vector of numbers
#' @param MADCutOff value to use as cutoff (Leys e tal. recommend 2.5 or 3.0 as default)
#' @param replaceOutliersWith if value is an outlier, what to replace it with? NA by default
#' @param showMADValues if TRUE, will show deviation score of each value
#' @param outlierIndices return index/position of outlier
#' @param bConstant a constant linked to the assumption of normality of the data, disregarding the abnormality induced by outliers
#' @param digits how many digits/decimals to round output to
#'
#' @return A vector with outliers identified (default converts outliers to NA)
#'
#' @details We can identify and remove outliers in our data by identifying data points that are too extreme—either too many standard deviations (SD) away from the mean or too many median absolute deviations (MAD) away from the median. The SD approach might not be ideal with extreme outliers, whereas the MAD approach is much more robust (for comparison of both approaches, see Leys et al., 2013, Journal of Experimental Social Psychology).
#'
#' @references \itemize{
#' \item Leys, C., Ley, C., Klein, O., Bernard, P., & Licata, L. (2013). Detecting outliers: Do not use standard deviation around the mean, use absolute deviation around the median. Journal of Experimental Social Psychology, 49(4), 764-766. doi:10.1016/j.jesp.2013.03.013 (\url{https://www.sciencedirect.com/science/article/pii/S0022103113000668})}
#' @seealso \code{\link{outliersZ}}
#'
#' @author Hause Lin
#'
#' @export
#'
#' @usage
#' outliersMAD(x, MADCutOff = 3.0, replaceOutliersWith = NA,
#' showMADValues = FALSE, outlierIndices = FALSE, bConstant = 1.4826, digits = 2)
#'
#' @examples
#' example <- c(1, 3, 3, 6, 8, 10, 10, 1000, -1000) # 1000 is an outlier
#' outliersMAD(example)
#' outliersMAD(example, MADCutOff = 3.0)
#' outliersMAD(example, MADCutOff = 2.5, replaceOutliersWith = -999)
#' outliersMAD(example, MADCutOff = 1.5, outlierIndices = TRUE)
#' outliersMAD(example, MADCutOff = 1.5, showMADValues = TRUE)
#' outliersMAD(example, MADCutOff = 1.5, showMADValues = TRUE, replaceOutliersWith = -88)
outliersMAD <- function(x, MADCutOff = 3.0, replaceOutliersWith = NA, showMADValues = FALSE, outlierIndices = FALSE, bConstant = 1.4826, digits = 2) {
  # bConstant: usually, b = 1.4826, a constant linked to the assumption of normality of the data, disregarding the abnormality induced by out- liers (Rousseeuw & Croux, 1993).
  
  # compute number of absolute MADs away for each value: formula: abs( ( x - median(x) ) )/ mad(x)
  MADAway <- (x - stats::median(x, na.rm = T)) / stats::mad(x, constant = bConstant, na.rm = T)
  absMADAway <- abs(MADAway)
  # subset data that has absMADAway greater than the MADCutOff and replace them with replace
  x[absMADAway > MADCutOff] <- replaceOutliersWith
  outliers <- length(x[absMADAway > MADCutOff])
  if (showMADValues) { # if values == TRUE, return number of mads for each value
    message("Showing MAD from median for each value.")
    message(paste0(outliers, " outliers detected."))
    return(round(MADAway, digits))
  } else if (outlierIndices) {
    message("Showing indices of outliers.")
    if (is.na(replaceOutliersWith)) {
      return(which(is.na(x)))
    } else {
      return(x[x == replaceOutliersWith])
    }
    
  } else {
    message(paste0(outliers, " outliers detected."))
    message(paste0("Outliers replaced with ", replaceOutliersWith))
    return(round(x, digits)) # otherwise, return original with outliers replaced
  }
}


# Title: Geometric Mean of Pairwise Ratios (GMPR) for Microbiome Sequencing data normalization
# Version: 0.1
# Authors: Jun Chen (chen.jun2@mayo.edu)
# Date: 2017/02/07
# Description: The function calculates the normalizing factors for microbiome sequencing data or, more generally, zeroinflated sequencing data. 
# The size factors can be used as offsets in count-based regression models or as divisors to produce normalized data


require(matrixStats)

GMPR <- function (comm, intersect.no = 10, ct.min = 1, trace = TRUE) {
  # Computes the GMPR size factor
  #
  # Args:
  #   comm: a matrix of counts, row - features (OTUs, genes, etc) , column - sample
  #   intersect.no: the minimum number of shared features between sample pair, where the ratio is calculated
  #   ct.min: the minimum number of counts required to calculate ratios
  
  #
  # Returns:
  #   a vector of the size factors with attribute 'NSS'. Samples with distinct sets of features will be output as NA.
  #         NSS:   number of samples with significant sharing (> intersect.no) including itself
  
  # mask counts < ct.min
  comm[comm < ct.min] <- 0
  
  if (is.null(colnames(comm))) {
    colnames(comm) <- paste0('S', 1:ncol(comm))
  }
  
  if (trace) cat('Begin GMPR size factor calculation ...\n')
  
  comm.no <- numeric(ncol(comm))
  gmpr <- sapply(1:ncol(comm),  function(i) {		
    if (i %% 50 == 0) {
      cat(i, '\n')
    }
    x <- comm[, i]
    # Compute the pairwise ratio
    pr <- x / comm
    # Handling of the NA, NaN, Inf
    pr[is.nan(pr) | !is.finite(pr) | pr == 0] <- NA
    # Counting the number of non-NA, NaN, Inf
    incl.no <- colSums(!is.na(pr))		
    # Calculate the median of PR
    pr.median <- colMedians(pr, na.rm=TRUE)
    # Record the number of samples used for calculating the GMPR
    comm.no[i] <<- sum(incl.no >= intersect.no)
    # Geometric mean of PR median
    if (comm.no[i] > 1) {
      return(exp(mean(log(pr.median[incl.no >= intersect.no]))))
    } else {
      return(NA)
    }
  }
  )
  
  if (sum(is.na(gmpr))) {
    warning(paste0('The following samples\n ', paste(colnames(comm)[is.na(gmpr)], collapse='\n'), 
                   '\ndo not share at least ', intersect.no, ' common taxa with the rest samples! ',
                   'For these samples, their size factors are set to be NA! \n', 
                   'You may consider removing these samples since they are potentially outliers or negative controls!\n',
                   'You may also consider decreasing the minimum number of intersecting taxa and rerun the procedure!\n'))
  }
  
  if (trace) cat('Completed!\n')
  if (trace) cat('Please watch for the samples with limited sharing with other samples based on NSS! They may be outliers! \n')
  names(gmpr) <- names(comm.no) <- colnames(comm)
  
  attr(gmpr, 'NSS') <- comm.no
  
  return(gmpr)
}



"%IN%" <- function(x, y) interaction(x) %in% interaction(y)

outlierCalculation <- function(df,col,T4) {
  col_name <- str_remove(col,"T4_min_mean_assessment_time_")
  col <-  rlang::sym(col)
  outlier_name <- rlang::sym(paste("outlierBoolean",col_name,sep = "_"))
  print(outlier_name)
  assessment_na <- rlang::sym(paste("na",col_name,sep="_"))
  imp_name <- rlang::sym(paste("T4_imp",col_name,sep="_"))
  score_name <-rlang::sym(paste("outlierScore",col_name,sep="_"))
  
  
  outlier_ps <- df %>% filter(abs(!!col-stats::median(!!col))/stats::mad(!!col)>3.0)
  df <- df %>% 
    mutate(!!outlier_name := if_else(select(df,participant_id) %IN% select(outlier_ps,participant_id),T,F),
           !!assessment_na :=ifelse(!!outlier_name,NA,!!col),
           !!imp_name := ifelse(is.na(!!assessment_na),median(!!assessment_na,na.rm = T),!!assessment_na),
           !!score_name := ifelse(!!outlier_name,1,0))
  
  T4 <- cbind(T4,select(df,!!col,!!imp_name,!!score_name))
  
  return(T4)
}

# ---------------------------------------------------------------------------- #
# Define check_relevant_files() ----
# ---------------------------------------------------------------------------- #

# from https://github.com/isaacahuvia/anxiety-identity-avoidance/blob/17170bd337c94ffcbfa2eb637ed040e453d03844/code/01a_define_functions.R#L49
# Define function to check that selected intermediate clean CSV data files contain
# those relevant to present manuscript (for full set of intermediate clean CSV data 
# files, see https://github.com/TeachmanLab/MT-Data-CalmThinkingStudy)

check_relevant_files <- function(filenames) {
  relevant_files <- c("affect.csv",
                      "angular_training.csv",
                      "anxiety_identity.csv",
                      "anxiety_triggers.csv",
                      "assessing_program.csv",
                      "bbsiq.csv",
                      "cc.csv",
                      "coach_prompt.csv",
                      "comorbid.csv",
                      "covid19.csv",
                      "credibility.csv",
                      "dass21_as.csv",
                      "demographics_race.csv",
                      "demographics.csv",
                      "evaluation.csv",
                      "help_seeking.csv",
                      "mechanisms.csv",
                      "mental_health_history.csv",
                      "oa.csv",
                      "participant.csv",
                      "return_intention.csv",
                      "rr.csv",
                      "session_review.csv",
                      "study.csv",
                      "task_log.csv",
                      "technology_use.csv",
                      "wellness.csv")
  
  if (all(relevant_files %in% filenames) == FALSE) {
    missing_files <- setdiff(relevant_files, filenames)
    
    warning(paste0(c("You are missing these files:", paste0(" ", missing_files))))
  }
  
  else {
    print("All relevant files included.")
  }
}

