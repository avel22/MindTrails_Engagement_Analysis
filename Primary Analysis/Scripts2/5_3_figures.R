### Adapting Jeremy Eberle's script
### https://github.com/jwe4ec/jp5ws/blob/main/Syntax/19_create_figures_linear_spline.R
### Eberle, J. W., Boukhechba, M., Sun, J., Zhang, D., Funk, D., Barnes, L., & Teachman, B. (2022, January 13). Shifting Episodic Prediction With Online Cognitive Bias Modification: A Randomized Controlled Trial. Retrieved from osf.io/jp5ws

### Modified by: Ángel Vela
### MS Thesis 
### University of Virginia
### May 2022
### The purpose of this script is to visualize predicted means and standard errors for the outcomes for each of the engagement clusters

#--------------------------------------------------------------------------------#
# loading the libraries ----
#--------------------------------------------------------------------------------#
#libraries 
pacman::p_load(tidyverse,purrr,here,mitml,ggplot2, gridExtra,stargazer,lme4,psycho,afex,nlme,optimx,emmeans,gtsummary)

#--------------------------------------------------------------------------------#
# loading the  data ----
#--------------------------------------------------------------------------------#
#session outcomes
load(here("Scripts2","Data_Primary_Analysis","pooledResults7LME.RData"))

# ---------------------------------------------------------------------------- #
# Define functions used throughout script ----
# ---------------------------------------------------------------------------- #

# Define function to compute model-predicted means and standard errors for
# model two_conditions_vs_neutral for various outcomes
compute_pred_means_4 <- function(modelList) {
  # Create data frame with codings of condition and time variables
  # engagement cluster 1 is reference group
  eng_cluster <- rep(c("1", "2"), each = 7)
  assessment <- rep(c("Baseline", "Session 3", 
                      "Session 5", "Follow-Up"), times = 2)
  
  pred_means <- data.frame(eng_cluster, assessment)
  
  pred_means$eng_cluster2 <- ifelse(pred_means$eng_cluster == "2", 1, 0)
  
  pred_means$t1[pred_means$assessment == "Baseline"] <- 0
  pred_means$t1[pred_means$assessment == "Session 3"] <- 3
  pred_means$t1[pred_means$assessment == "Session 5"] <- 5
  pred_means$t1[pred_means$assessment == "Follow-Up"] <- 5
  
  
  pred_means$t2[pred_means$assessment == "Baseline"] <- 0
  pred_means$t2[pred_means$assessment == "Session 3"] <- 0
  pred_means$t2[pred_means$assessment == "Session 5"] <- 0
  pred_means$t2[pred_means$assessment == "Follow-Up"] <- 1
  
  # Compute predicted means and standard errors using testConstraints
  
  for (i in 1:nrow(pred_means)) {
    eng_cluster2 <- pred_means$eng_cluster2[i]
    t1 <- pred_means$t1[i]
    t2 <- pred_means$t2[i]
    
    ctr <- paste0("(Intercept) + ",
                  "eng_cluster2 * ", eng_cluster2, " + ",
                  "t1 * ", t1, " + ",
                  "t2 * ", t2, " + ",
                  "`t1:eng_cluster2` * (", t1, "*", eng_cluster2, ") + ",
                  "`t2:eng_cluster2` * (", t2, "*", eng_cluster2, ") +",
                  "cred_on_gmc * ",0)
    
    mean <- testConstraints(modelList, constraints = ctr)
    
    pred_means$mean[i] <- mean$Qbar
    pred_means$se[i] <- sqrt(mean$T)
  }
  
  # Recode condition and assessment as factors for ggplot2
  
  pred_means$condition <- factor(pred_means$eng_cluster,
                                 levels = c("1",
                                            "2"
                                 ))
  pred_means$assessment <- factor(pred_means$assessment,
                                  levels = c("Baseline",
                                             "Session 1",
                                             "Session 2",
                                             "Session 3",
                                             "Session 4",
                                             "Session 5",
                                             "Follow-Up"))
  
  return(pred_means)
}


compute_pred_means <- function(modelList) {
  # Create data frame with codings of condition and time variables
  # engagement cluster 1 is reference group
  eng_cluster <- rep(c("1", "2"), each = 7)
  assessment <- rep(c("Baseline", "Session 1", "Session 2", "Session 3", 
                      "Session 4","Session 5", "Follow-Up"), times = 2)
  
  pred_means <- data.frame(eng_cluster, assessment)
  
  pred_means$eng_cluster2 <- ifelse(pred_means$eng_cluster == "2", 1, 0)
  
  pred_means$t1[pred_means$assessment == "Baseline"] <- 0
  pred_means$t1[pred_means$assessment == "Session 1"] <- 1
  pred_means$t1[pred_means$assessment == "Session 2"] <- 2
  pred_means$t1[pred_means$assessment == "Session 3"] <- 3
  pred_means$t1[pred_means$assessment == "Session 4"] <- 4
  pred_means$t1[pred_means$assessment == "Session 5"] <- 5
  pred_means$t1[pred_means$assessment == "Follow-Up"] <- 5
  
  
  pred_means$t2[pred_means$assessment == "Baseline"] <- 0
  pred_means$t2[pred_means$assessment == "Session 1"] <- 0
  pred_means$t2[pred_means$assessment == "Session 2"] <- 0
  pred_means$t2[pred_means$assessment == "Session 3"] <- 0
  pred_means$t2[pred_means$assessment == "Session 4"] <- 0
  pred_means$t2[pred_means$assessment == "Session 5"] <- 0
  pred_means$t2[pred_means$assessment == "Follow-Up"] <- 1
  
  # Compute predicted means and standard errors using testConstraints
  
  for (i in 1:nrow(pred_means)) {
    eng_cluster2 <- pred_means$eng_cluster2[i]
    t1 <- pred_means$t1[i]
    t2 <- pred_means$t2[i]
    
    ctr <- paste0("(Intercept) + ",
                  "eng_cluster2 * ", eng_cluster2, " + ",
                  "t1 * ", t1, " + ",
                  "t2 * ", t2, " + ",
                  "`t1:eng_cluster2` * (", t1, "*", eng_cluster2, ") + ",
                  "`t2:eng_cluster2` * (", t2, "*", eng_cluster2, ") +",
                  "cred_on_gmc * ",0)
    
    mean <- testConstraints(modelList, constraints = ctr)
    
    pred_means$mean[i] <- mean$Qbar
    pred_means$se[i] <- sqrt(mean$T)
  }
  
  # Recode condition and assessment as factors for ggplot2
  
  pred_means$condition <- factor(pred_means$eng_cluster,
                                 levels = c("1",
                                            "2"
                                            ))
  pred_means$assessment <- factor(pred_means$assessment,
                                  levels = c("Baseline",
                                             "Session 1",
                                             "Session 2",
                                             "Session 3",
                                             "Session 4",
                                             "Session 5",
                                             "Follow-Up"))
  
  return(pred_means)
}


# Define function to plot estimated means and standard errors. Colors obtained
# from Color Brewer 2.0 three-class Dark2 palette
# (https://colorbrewer2.org/#type=qualitative&scheme=Dark2&n=3). Checked for
# vision deficiency using HCL Wizard (http://hclwizard.org:3000/cvdemulator/).

create_plot <- function(pred_means, hidden_pts, title, y_title, scale_min, 
                        scale_max, legend_position) {
    ggplot(pred_means, 
         aes(x = assessment, y = mean, 
             group = eng_cluster_label, color = eng_cluster_label, linetype = eng_cluster_label)) +
    geom_line() +
    geom_point(data = pred_means[!(pred_means$assessment %in% hidden_pts), ]) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                  pred_means[!(pred_means$assessment %in% hidden_pts), ],
                  width = .3) +
    labs(title = title, 
         x = "Assessment",
         y = y_title) +
    scale_linetype_manual(name = "Engagement Group",
                          values = c("Less Time Spent" = "longdash",
                                     "More Time Spent" = "solid"))+
    scale_color_manual(name = "Engagement Group",
                       values = c("Less Time Spent" = "#1b9e77", 
                                  "More Time Spent" = "#7570b3"
                                  )) +
    scale_y_continuous(breaks = scale_min:scale_max, 
                       limits = c(scale_min, scale_max)) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 11),
          legend.key.width = unit(2, "cm")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    theme(legend.position = legend_position)
}




# ---------------------------------------------------------------------------- #
# Create figures for engagement clusters for outcomes ----
# ---------------------------------------------------------------------------- #

# Compute estimated means and standard errors

pred_means_OA <- 
  compute_pred_means(modelList_oa_lme_mod1)

pred_means_OA <- pred_means_OA %>% mutate(eng_cluster_label = ifelse(eng_cluster ==1, "Less Time Spent", "More Time Spent"))

pred_means_dass21 <- 
  compute_pred_means(modelList_dass21_lme_mod1)

pred_means_dass21 <- pred_means_dass21 %>% mutate(eng_cluster_label = ifelse(eng_cluster ==1, "Less Time Spent", "More Time Spent"))

pred_means_rr_neg <- 
  compute_pred_means(modelList_rr_neg_lme)

pred_means_rr_neg <- pred_means_rr_neg %>% mutate(eng_cluster_label = ifelse(eng_cluster ==1, "Less Time Spent", "More Time Spent"))


pred_means_rr_pos <-  
  compute_pred_means(modelList_rr_pos_lme_mod1)

pred_means_rr_pos <- pred_means_rr_pos %>% mutate(eng_cluster_label = ifelse(eng_cluster ==1, "Less Time Spent", "More Time Spent"))

pred_means_bbsiq <- 
  compute_pred_means(modelList_bbsiq_lme_mod1)
pred_means_bbsiq <- pred_means_bbsiq %>% mutate(eng_cluster_label = ifelse(eng_cluster ==1, "Less Time Spent", "More Time Spent"))

# Create plots

p_OA <- 
  create_plot(pred_means_OA, NA, "Anxiety Symptoms (OASIS)", 
              "Average Item Score", 0, 4, c(0.8, 0.8))

p_dass21 <-
  create_plot(pred_means_dass21, c("Session 1", "Session 2","Session 4"), "Anxiety Symptoms (DASS-21 AS)", 
              "Average Item Score", 0, 3, c(0.8, 0.8))

p_rr_neg <- 
  create_plot(pred_means_rr_neg, c("Session 1", "Session 2","Session 4"), "Negative Interpretation Bias (RR)", 
              "Average Item Score", 1, 4, c(0.8, 0.8))

p_rr_pos<-
  create_plot(pred_means_rr_pos, c("Session 1", "Session 2","Session 4"), "Positive Interpretation Bias (RR)", 
              "Average Item Score", 1, 4, c(0.8, 0.8))

p_bbsiq <- 
  create_plot(pred_means_bbsiq, c("Session 1", "Session 2","Session 4"), "Negative Interpretation Bias (BBSIQ)", 
              "Average Item Score", 0, 4, c(0.8, 0.8))

# extract a legend that is laid out horizontally
legend_b <- get_legend(
  p_OA + 
    guides(fill = guide_legend(ncol=2)) +
    theme(legend.position =c(0.5,0.5))
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .1))


p_OA 
p_dass21
p_rr_neg
p_rr_pos
p_bbsiq

est_means_outcomes <- cowplot::plot_grid(
  p_OA + theme(legend.position="none"),
  p_dass21 + theme(legend.position="none", axis.title.y=element_blank()),
  p_rr_pos + theme(legend.position="none",axis.title.y=element_blank()),
  p_rr_neg + theme(legend.position="none"),
  p_bbsiq + theme(legend.position="none",axis.title.y=element_blank()),
  legend_b, nrow = 2, ncol =3)

est_means_outcomes


png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","all_est_means_outcomes2.png"), units="in", width=10, height=8, res=300)
est_means_outcomes
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","oa_means_outcomes2.png"), units="in", width=10, height=8, res=300)
p_OA
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","dass21_means_outcomes2.png"), units="in", width=10, height=8, res=300)
p_dass21
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","rr_neg_means_outcomes2.png"), units="in", width=10, height=8, res=300)
p_rr_neg
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","rr_pos_means_outcomes2.png"), units="in", width=10, height=8, res=300)
p_rr_pos
dev.off()

png(here("Scripts2","Tables_Figures","Figures","JAMIA_EDITS","bbsiq_means_outcomes2.png"), units="in", width=10, height=8, res=300)
p_bbsiq
dev.off()

