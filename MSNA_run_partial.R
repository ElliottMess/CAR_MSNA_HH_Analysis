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

#### ADMIN 0 ####
response_updated_cluster$admin_0 <- "RCA"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_0",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file)

analysisplan_admin_0 <- analysisplan_admin_0[!analysisplan_admin_0$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]



### CRUNCH
final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)


summary.stats_admin0 <- final_result_admin_0$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_0$analysisplan %>% select(research.question, sub.research.question, dependent.var)

questions_nameslables <- select(questions, name, label)

summary.stats_admin0_nice <- summary.stats_admin0%>% 
  left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
  select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value)%>%
  group_by( dependent.var,dependent.var.value)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = repeat.var.value, value = numbers)%>%
  select(-grouped_id)%>%
  left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin0_",format(Sys.time(), "%Y%m%d"),".csv"))



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

final_result_admin_0_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin0_grp <- final_result_admin_0_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_0_grp$analysisplan %>% select(research.question, sub.research.question, dependent.var)

questions_nameslables <- select(questions, name, label)

summary.stats_admin0_grp_nice <- summary.stats_admin0_grp%>% 
  left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
  select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value, independent.var.value)%>%
  group_by(dependent.var.value, independent.var.value)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = repeat.var.value, value = numbers)%>%
  select(-grouped_id)%>%
  left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin0_grp_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 1 ####

analysisplan_admin_1 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_1",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)
analysisplan_admin_1 <- analysisplan_admin_1[!row.names(analysisplan_admin_1) %in% unwanted_cols,]
analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]

final_result_admin_1 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_1, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)
summary.stats_admin1 <- final_result_admin_1$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_1$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct

summary.stats_admin1_nice <- summary.stats_admin1%>%
  left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
  select(dependent.var, dependent.var.value, numbers, repeat.var.value)%>%
  group_by( dependent.var, dependent.var.value ,repeat.var.value)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = repeat.var.value, value = numbers)%>%
  select(-grouped_id)%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin1_",format(Sys.time(), "%Y%m%d"),".csv"))


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

final_result_admin_1_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_1_grp, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)


summary.stats_admin1_grp <- final_result_admin_1_grp$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_1_grp$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin1_grp_nice <- summary.stats_admin1_grp%>% 
  left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
  select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value, independent.var.value)%>%
  mutate(admin_grp = paste(repeat.var.value, independent.var.value, sep = "_"))%>%
  select(research.question, sub.research.question, dependent.var, numbers, admin_grp, dependent.var.value)%>%
  group_by(admin_grp, dependent.var.value, dependent.var)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = admin_grp, value = numbers)%>%
  select(-grouped_id)%>%
  left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin1_grp_",format(Sys.time(), "%Y%m%d"),".csv"))


#### ADMIN 2 ####
analysisplan_admin_2 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_2",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_2 <- analysisplan_admin_2[!analysisplan_admin_2$dependent.variable %in% unwanted_cols,]
analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]

final_result_admin_2 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_2, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

summary.stats_admin2 <- final_result_admin_2$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_2$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin2_nice <- summary.stats_admin2%>% 
  left_join(cols_analysisplan, by = c("dependent.var" = "dependent.var"))%>%
  select(research.question, sub.research.question, dependent.var, dependent.var.value,numbers, repeat.var.value)%>%
  group_by(dependent.var.value, dependent.var, repeat.var.value)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = repeat.var.value, value = numbers)%>%
  select(-grouped_id)%>%
  left_join(questions_nameslables, by = c("dependent.var" = "name"))%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin2_",format(Sys.time(), "%Y%m%d"),".csv"))
