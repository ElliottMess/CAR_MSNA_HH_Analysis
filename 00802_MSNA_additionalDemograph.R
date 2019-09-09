# setup
library(dplyr)
library(readr)
library(tidyr)
library(koboquest) # manage kobo questionnairs
library(parallel) # mclapply
library(kobostandards) # check inputs for inconsistencies
#devtools::install_github('mabafaba/kobostandards') 
library(xlsformfill) # generate fake data for kobo
#devtools::install_github('mabafaba/xlsformfill') 
library(hypegrammaR) # stats 4 complex samples
#devtools::install_github('ellieallien/hypegrammaR') 
library(composr) # horziontal operations
#devtools::install_github('mabafaba/composr') 
library(parallel)
library(knitr)
library(surveyweights)
library(stringr)
library(srvyr)
#source("functions/to_alphanumeric_lowercase.R") # function to standardise column headers (like check.names)
source("functions/analysisplan_factory.R")  # generate analysis plans
source("functions/remove_responses_from_sumstat.R")  # generate analysis plans
source("functions/format_hypothesis_test.R")
### source("SOME_NGA_SPECIFIC_FUNCTIONS")

# load questionnaire inputs
response_updated_cluster <- read.csv("./output/REACH_CAR_dataset_HH_MSNA_20190908.csv", stringsAsFactors = F)


template_analysisplan_file <- "./input/analysisplan_template_add.csv"

template_df <- read.csv(template_analysisplan_file, stringsAsFactors = F)

educ_ques <- c("sum_educ_3_presence_18_19.filles_13_18.0m",
               "sum_educ_3_presence_18_19.filles_13_18.0m_3m",
               "sum_educ_3_presence_18_19.filles_13_18.12m",
               "sum_educ_3_presence_18_19.filles_13_18.3m_6m",
               "sum_educ_3_presence_18_19.filles_13_18.6m_12m",
               "sum_educ_3_presence_18_19.filles_7_12.0m",
               "sum_educ_3_presence_18_19.filles_7_12.0m_3m",
               "sum_educ_3_presence_18_19.filles_7_12.12m",
               "sum_educ_3_presence_18_19.filles_7_12.3m_6m",
               "sum_educ_3_presence_18_19.filles_7_12.6m_12m",
               "sum_educ_3_presence_18_19.garcons_13_18.0m",
               "sum_educ_3_presence_18_19.garcons_13_18.0m_3m",
               "sum_educ_3_presence_18_19.garcons_13_18.12m",
               "sum_educ_3_presence_18_19.garcons_13_18.3m_6m","sum_educ_3_presence_18_19.garcons_13_18.6m_12m",
               "sum_educ_3_presence_18_19.garcons_7_12.0m",
               "sum_educ_3_presence_18_19.garcons_7_12.0m_3m",
               "sum_educ_3_presence_18_19.garcons_7_12.12m",
               "sum_educ_3_presence_18_19.garcons_7_12.3m_6m",
               "sum_educ_3_presence_18_19.garcons_7_12.6m_12m")


freq_ecole <- response_updated_cluster %>%
  summarise(
    freq_educ_2_inscrit_18_19.filles_7_12 = sum(sum_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm = T) / sum(sum_agegrp_7_12_filles * weights_sampling, na.rm = T),
    freq_educ_2_inscrit_18_19.filles_13_18 = sum(sum_educ_2_inscrit_18_19.filles_13_18*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18 * weights_sampling, na.rm = T),
    freq_educ_2_inscrit_18_19.garcons_7_12 = sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm = T) / sum(sum_agegrp_7_12_garcons*weights_sampling, na.rm = T),
    freq_educ_2_inscrit_18_19.garcons_13_18 = sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm=T) / sum(sum_agegrp_13_18_garcons*weights_sampling, na.rm=T),
    
    freq_educ_3_presence_18_19.filles_13_18.0m = sum(sum_educ_3_presence_18_19.filles_13_18.0m*weights_sampling,na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18* weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_13_18.0m_3m = sum(sum_educ_3_presence_18_19.filles_13_18.0m_3m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_13_18.12m = sum(sum_educ_3_presence_18_19.filles_13_18.3m_6m*weights_sampling, na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18* weights_sampling, na.rm=T),
    freq_educ_3_presence_18_19.filles_13_18.3m_6m = sum(sum_educ_3_presence_18_19.filles_13_18.6m_12m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_13_18.6m_12m = sum(sum_educ_3_presence_18_19.filles_13_18.12m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    
    freq_educ_3_presence_18_19.filles_7_12.0m = sum(sum_educ_3_presence_18_19.filles_7_12.0m*weights_sampling, na.rm = T) / sum(freq_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_7_12.0m_3m = sum(sum_educ_3_presence_18_19.filles_7_12.0m_3m*weights_sampling, na.rm = T) / sum(freq_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_7_12.12m = sum(sum_educ_3_presence_18_19.filles_7_12.3m_6m*weights_sampling, na.rm = T) / sum(freq_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.filles_7_12.3m_6m = sum(sum_educ_3_presence_18_19.filles_7_12.6m_12m*weights_sampling, na.rm =T) / sum(freq_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm=T),
    freq_educ_3_presence_18_19.filles_7_12.6m_12m = sum(sum_educ_3_presence_18_19.filles_7_12.12m*weights_sampling, na.rm = T) / sum(freq_educ_2_inscrit_18_19.filles_7_12*weights_sampling, na.rm = T),
    
    freq_educ_3_presence_18_19.garcons_13_18.0m = sum(sum_educ_3_presence_18_19.garcons_13_18.0m*weights_sampling, na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm=T),
    freq_educ_3_presence_18_19.garcons_13_18.0m_3m = sum(sum_educ_3_presence_18_19.garcons_13_18.0m_3m*weights_sampling, na.rm =T) /sum( sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.garcons_13_18.12m = sum(sum_educ_3_presence_18_19.garcons_13_18.3m_6m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.garcons_13_18.3m_6m = sum(sum_educ_3_presence_18_19.garcons_13_18.6m_12m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.garcons_13_18.6m_12m = sum(sum_educ_3_presence_18_19.garcons_13_18.12m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_13_18*weights_sampling, na.rm = T),
    
    freq_educ_3_presence_18_19.garcons_7_12.0m = sum(sum_educ_3_presence_18_19.garcons_7_12.0m*weights_sampling,na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm=T),
    freq_educ_3_presence_18_19.garcons_7_12.0m_3m = sum(sum_educ_3_presence_18_19.garcons_7_12.0m_3m*weights_sampling, na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.garcons_7_12.12m = sum(sum_educ_3_presence_18_19.garcons_7_12.3m_6m*weights_sampling, na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm=T),
    freq_educ_3_presence_18_19.garcons_7_12.3m_6m = sum(sum_educ_3_presence_18_19.garcons_7_12.6m_12m*weights_sampling, na.rm = T) / sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm = T),
    freq_educ_3_presence_18_19.garcons_7_12.6m_12m = sum(sum_educ_3_presence_18_19.garcons_7_12.12m*weights_sampling, na.rm=T) / sum(sum_educ_2_inscrit_18_19.garcons_7_12*weights_sampling, na.rm=T),
    freq_ig_7_gr_vulnerable.aucune = sum(sum_ig_7_gr_vulnerable.aucune*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm=T),
    freq_ig_7_gr_vulnerable.ena = sum(sum_ig_7_gr_vulnerable.ena*weights_sampling, na.rm = T)/ sum(ig_6_hh_membres_tot*weights_sampling, na.rm=T),
    freq_ig_7_gr_vulnerable.es = sum(sum_ig_7_gr_vulnerable.es*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.fa = sum(sum_ig_7_gr_vulnerable.fa*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.fe = sum(sum_ig_7_gr_vulnerable.fe*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.handi = sum(sum_ig_7_gr_vulnerable.handi*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.handi_ment = sum(sum_ig_7_gr_vulnerable.handi_ment*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.handi_phy = sum(sum_ig_7_gr_vulnerable.handi_phy*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T),
    freq_ig_7_gr_vulnerable.nsp = sum(sum_ig_7_gr_vulnerable.nsp*weights_sampling, na.rm = T) / sum(ig_6_hh_membres_tot*weights_sampling, na.rm = T)
  )
gather(key = "variable", value= "percent")

write.csv(freq_ecole, "./output/freq_scolaire_20190809.csv")
