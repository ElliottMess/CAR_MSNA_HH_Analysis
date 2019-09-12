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


#### READING LATEST DATAFRAME FROM TODAY!!
response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"), stringsAsFactors = TRUE)


template_analysisplan_file <- "./input/analysisplan_template.csv"

unwanted_cols <- unique(tolower(c("x","start", "end", "today", "q0_2_date", "consensus_note", "village",  "q0_1_enqueteur","village_autre", "ig_11_IDP_RtL_autre", 
                                  "ig_14_IDP_cond_retour_autre","ig_16_Ret_Rapat_abri_origine_non_raison_autre", "ig_15_IDP_RtR_Ret_Rapat_autre",
                                  "sante_1_accouch_autre","sante_1_accouch_maison_autre","sante_2_soin_recu_autre",
                                  "sante_3_soin_non_recu_autre","sante_4_0_4_malades_autre",  "sante_5_5plus_malades_autre",
                                  "educ_4_handi_acces_autre",   "protect_10_autre",           "educ_5_ecole_acces_autre",
                                  "nfi_2_1_type_abri_autre",    "nfi_propr_abri_autre",       "mssc_2_source_rev_autre",
                                  "mssc_4_dep_6M_autre","secal_6_agric_raisons_autre","wash_1_source_boisson_autre",
                                  "wash_2_source_autre_usage_autre", "wash_9_insuff_raisons_certains_groupes_autre",  "wash_9_insuff_raisons_autre",
                                  "wash_15_insuff_raisons_certains_groupes_autre", "wash_15_insuff_raisons_autre","wash_20_autres_autre",
                                  "sante_5_deces_relation_autre", "sante_5_deces_cause_autre",  "protect_2_femmes_risque_autre",
                                  "protect_2_hommes_risque_autre","protect_3_filles_risque_autre","protect_3_garcons_risque_autre",
                                  "protect_5_1_travail_force_autre", "protect_8_2_autre","protect_13_autre",
                                  "aap_1_1_source_confiance_autre", "aap_2_1_type_information_autre", "aap_3_canal_information_enpersonne",
                                  "aap_3_canal_information_autre", "aap_4_retour_fournisseurs_aide_autre", "educ_6_reponse_autre",
                                  "nfi_7_assistance_autre",     "secal_13_reponse_autre",     "wash_22_autres_autre",
                                  "sante_7_reponse_autre",      "note_comm_end", "sante_1_accouch_maison_raison_autre", "sum_sante_1_accouch_autre"
)))

#### ADMIN 0 ####
response_updated_cluster$admin_0 <- "RCA"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_0",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file)

analysisplan_admin_0 <- analysisplan_admin_0[!analysisplan_admin_0$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]

running_timezz <- data.frame(matrix(ncol = 4, nrow = 6) )

names(running_timezz) <- c("Level", "Start", "End", "Running_time")

### CRUNCH
start_time_admin0 <- Sys.time()

final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

end_time_admin0 <- Sys.time()

summary.stats_admin0 <- final_result_admin_0$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
# 
# cols_analysisplan <- final_result_admin_0$analysisplan %>% select(research.question, sub.research.question, dependent.var)
# 
# questions_nameslables <- select(questions, name, label)
# 
# start_time_admin0 <- Sys.time()
# summary.stats_admin0_nice <- summary.stats_admin0%>% 
#   left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
#   select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value)%>%
#   group_by( dependent.var,dependent.var.value)%>%
#   mutate(grouped_id = row_number())%>%
#   spread(key = repeat.var.value, value = numbers)%>%
#   select(-grouped_id)%>%
#   left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
#   map_to_file(paste0("./output/tables/","summary_stats_admin0_",format(Sys.time(), "%Y%m%d"),".csv"))


### ADMIN 0 GROUPE

analysisplan_admin_0_grp <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_0",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!analysisplan_admin_0_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!is.na(analysisplan_admin_0_grp$dependent.variable.type),]


start_time_admin0_grp <- Sys.time()

final_result_admin_0_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin0_grp <- Sys.time()

summary.stats_admin0_grp <- final_result_admin_0_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_grp_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
  
# 
# cols_analysisplan <- final_result_admin_0_grp$analysisplan %>% select(research.question, sub.research.question, dependent.var)
# 
# questions_nameslables <- select(questions, name, label)
# 
# summary.stats_admin0_grp_nice <- summary.stats_admin0_grp%>% 
#   left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
#   select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value, independent.var.value)%>%
#   group_by(dependent.var.value, independent.var.value)%>%
#   mutate(grouped_id = row_number())%>%
#   spread(key = repeat.var.value, value = numbers)%>%
#   select(-grouped_id)%>%
#   left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
#   map_to_file(paste0("./output/tables/","summary_stats_admin0_grp_",format(Sys.time(), "%Y%m%d"),".csv"))

#### ADMIN 0 sex HHD

analysisplan_admin_0_sexHHD <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_0",
                                                        independent.variable = "sexe_chef_menage",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_0_sexHHD <- analysisplan_admin_0_sexHHD[!analysisplan_admin_0_sexHHD$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_sexHHD <- analysisplan_admin_0_sexHHD[!is.na(analysisplan_admin_0_sexHHD$dependent.variable.type),]


start_time_admin0_sexHHD <- Sys.time()

final_result_admin_0_sexHHD <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin0_sexHHD <- Sys.time()

summary.stats_admin0_sexHHD <- final_result_admin_0_sexHHD$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin0_sexHHD_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 1 ####


analysisplan_admin_1 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_1",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)
analysisplan_admin_1 <- analysisplan_admin_1[!row.names(analysisplan_admin_1) %in% unwanted_cols,]
analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]

start_time_admin1 <- Sys.time()
final_result_admin_1 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_1, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)
end_time_admin1 <- Sys.time()

summary.stats_admin1 <- final_result_admin_1$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin1_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))
  
# 
# cols_analysisplan <- final_result_admin_1$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct
# 
# summary.stats_admin1_nice <- summary.stats_admin1%>%
#   left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
#   select(dependent.var, dependent.var.value, numbers, repeat.var.value)%>%
#   group_by( dependent.var, dependent.var.value ,repeat.var.value)%>%
#   mutate(grouped_id = row_number())%>%
#   spread(key = repeat.var.value, value = numbers)%>%
#   select(-grouped_id)%>%
#   map_to_file(paste0("./output/tables/","summary_stats_admin1_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 1 GROUPE ####

analysisplan_admin_1_grp <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_1",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)
analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!analysisplan_admin_1_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!is.na(analysisplan_admin_1_grp$dependent.variable.type),]


start_time_admin1_grp <- Sys.time()
final_result_admin_1_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_1_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

end_time_admin1_grp <- Sys.time()

summary.stats_admin1_grp <- final_result_admin_1_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin1_grp_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))

# 
# cols_analysisplan <- final_result_admin_1_grp$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()
# 
# summary.stats_admin1_grp_nice <- summary.stats_admin1_grp%>% 
#   left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
#   select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value, independent.var.value)%>%
#   mutate(admin_grp = paste(repeat.var.value, independent.var.value, sep = "_"))%>%
#   select(research.question, sub.research.question, dependent.var, numbers, admin_grp, dependent.var.value)%>%
#   group_by(admin_grp, dependent.var.value, dependent.var)%>%
#   mutate(grouped_id = row_number())%>%
#   spread(key = admin_grp, value = numbers)%>%
#   select(-grouped_id)%>%
#   left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
#   map_to_file(paste0("./output/tables/","summary_stats_admin1_grp_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 2 ####
analysisplan_admin_2 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_2",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_2 <- analysisplan_admin_2[!analysisplan_admin_2$dependent.variable %in% unwanted_cols,]
analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]

start_time_admin2 <- Sys.time()

final_result_admin_2 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_2, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)
end_time_admin2 <- Sys.time()

summary.stats_admin2 <- final_result_admin_2$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)%>%
  map_to_file(paste0("./output/tables/RAW/","summary_stats_admin2_RAW_",format(Sys.time(), "%Y%m%d"),".csv"))

# cols_analysisplan <- final_result_admin_2$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()
# 
# summary.stats_admin2_nice <- summary.stats_admin2%>% 
#   left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
#   select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value)%>%
#   group_by(dependent.var.value, dependent.var, repeat.var.value)%>%
#   mutate(grouped_id = row_number())%>%
#   spread(key = repeat.var.value, value = numbers)%>%
#   select(-grouped_id)%>%
#   left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
#   map_to_file(paste0("./output/tables/","summary_stats_admin2_",format(Sys.time(), "%Y%m%d"),".csv"))
# 
running_timezz <- data.frame(matrix(ncol = 4, nrow = 6) )

names(running_timezz) <- c("Level", "Start", "End", "Running_time")

running_timezz$Level <- c("admin0", "admin0_grp", "admin0_sexHHD", "admin1", "admin1_grp", "admin2")
running_timezz$Start <- c(start_time_admin0, start_time_admin0_grp, start_time_admin0_sexHHD, start_time_admin1, start_time_admin1_grp, start_time_admin2)
running_timezz$End <- c(end_time_admin0, end_time_admin0_grp, end_time_admin0_sexHHD, end_time_admin1, end_time_admin1_grp, end_time_admin2)
running_timezz <- running_timezz%>%
  mutate(Start = as.POSIXct(Start))%>%
  mutate(End = as.POSIXct(End))%>%
  mutate(Running_time = difftime(End, Start))

running_timezz                                 