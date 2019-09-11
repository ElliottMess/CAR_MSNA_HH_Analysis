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
response_updated_cluster <- read.csv(paste0("./output/REACH_CAR_dataset_HH_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"), stringsAsFactors = FALSE)
response_updated_cluster$pin_secal_fcs <- as.character(response_updated_cluster$pin_secal_fcs)

#####

#### ADMIN 2

df_santeprotect_admin2 <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_2)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            sum_pin_protec_peur = sum(pin_protec_peur * weights_sampling, na.rm = T)
  )%>%
  mutate(freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
         freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade
  )%>%
  select(admin_2, freq_enfantsmalades30j, freq_pers_malade_nonsoignee)%>%
  ungroup()%>%
  gather(key = "pins", value = "numbers", -admin_2)%>%
  mutate(category = "1")%>%
  select(admin_2, pins, category,  numbers)



template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

template_analysisplan <- read.csv(template_analysisplan_file, stringsAsFactors = F)

cols_to_analyse <- template_analysisplan$dependent.variable

response_updated_cluster <- response_updated_cluster %>%
  mutate_at(cols_to_analyse, as.character)

analysisplan_admin_2_pin <- make_analysis_plan_template(df= response_updated_cluster,
                                                        repeat.for.variable = "admin_2",
                                                        questionnaire = questionnaire,
                                                        hypothesis.type = "direct_reporting",
                                                        template_file = template_analysisplan_file)

final_result_admin_2_pin <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_2_pin, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin2_pin <- final_result_admin_2_pin$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_2_pin$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin2_pin_nice <- summary.stats_admin2_pin%>% 
  select(dependent.var, dependent.var.value, repeat.var.value, numbers)%>%
  rename(admin_2 = repeat.var.value, pins = dependent.var, category = dependent.var.value, numbers = numbers)%>%
  bind_rows(df_santeprotect_admin2)%>%
  select(admin_2, pins, category, numbers)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin2_pin_",format(Sys.time(), "%Y%m%d"),".csv"))


#####
#### ADMIN 1

df_santeprotect_admin1 <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_1)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            sum_pin_protec_peur = sum(as.numeric(pin_protec_peur) * weights_sampling, na.rm = T)
  )%>%
  mutate(freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
         freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade
  )%>%
  select(admin_1, freq_enfantsmalades30j, freq_pers_malade_nonsoignee)%>%
  ungroup()%>%
  gather(key = "pins", value = "numbers", -admin_1)%>%
  mutate(category = "1", )%>%
  select(admin_1, pins, category,  numbers)

template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

template_analysisplan <- read.csv(template_analysisplan_file, stringsAsFactors = F)

cols_to_analyse <- template_analysisplan$dependent.variable

response_updated_cluster <- response_updated_cluster %>%
  mutate_at(cols_to_analyse, as.character)

analysisplan_admin_1_pin <- make_analysis_plan_template(df= response_updated_cluster,
                                                        repeat.for.variable = "admin_1",
                                                        questionnaire = questionnaire,
                                                        hypothesis.type = "direct_reporting",
                                                        template_file = template_analysisplan_file)

final_result_admin_1_pin <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_1_pin, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin1_pin <- final_result_admin_1_pin$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_1_pin$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin1_pin_nice <- summary.stats_admin1_pin%>% 
  select(dependent.var, dependent.var.value, repeat.var.value, numbers)%>%
  rename(admin_1 = repeat.var.value, pins = dependent.var, category = dependent.var.value, numbers = numbers)%>%
  bind_rows(df_santeprotect_admin1)%>%
  select(admin_1, pins, category, numbers)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin1_pin_",format(Sys.time(), "%Y%m%d"),".csv"))


#####

###ADMIN 1 GROUP


df_santeprotect_admin1_grp <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_1, ig_8_statut_groupe)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            pin_sante_lieuaccouchement = sum(as.numeric(pin_sante_lieuaccouchement)*weights_sampling, na.rm = T),
            sum_pin_protec_peur = sum(as.numeric(pin_protec_peur) * weights_sampling, na.rm = T),
            sum_protect_11_1 = sum(protect_11_1_aumoinsun))%>%
  mutate(freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
         freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade
  )%>%
  select(admin_1, ig_8_statut_groupe, freq_enfantsmalades30j, freq_pers_malade_nonsoignee)%>%
  ungroup()%>%
  gather(key = "pins", value = "numbers", -admin_1, -ig_8_statut_groupe)%>%
  mutate(category = "1")%>%
  select(admin_1, ig_8_statut_groupe, pins, category,  numbers)


template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

analysisplan_admin_1_grp_pin <- make_analysis_plan_template(df= response_updated_cluster,
                                                            repeat.for.variable = "admin_1",
                                                            independent.variable = "ig_8_statut_groupe",
                                                            questionnaire = questionnaire,
                                                            hypothesis.type = "direct_reporting",
                                                            template_file = template_analysisplan_file)

final_result_admin_1_grp_pin <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                                analysisplan = analysisplan_admin_1_grp_pin, 
                                                                weighting = weighting_combined, 
                                                                cluster_variable_name = "clusters",
                                                                questionnaire = questionnaire)

summary.stats_admin1_grp_pin <- final_result_admin_1_grp_pin$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_1_grp_pin$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin1_grp_pin_nice <- summary.stats_admin1_grp_pin%>%
  select(repeat.var.value, independent.var.value, dependent.var, dependent.var.value, numbers)%>%
  rename(admin_1 = repeat.var.value, ig_8_statut_groupe = independent.var.value, pins = dependent.var, category = dependent.var.value, numbers = numbers)%>%
  bind_rows(df_santeprotect_admin1_grp)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin1_grp_pin_",format(Sys.time(), "%Y%m%d"),".csv"))


summary.stats_admin1_grp_totaux_pin_nice <- summary.stats_admin1_pin_nice%>%
  mutate(
    ig_8_statut_groupe = "totaux"
  )%>%
  bind_rows(summary.stats_admin1_grp_pin_nice)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin1_grp_totaux_pin_",format(Sys.time(), "%Y%m%d"),".csv"))

#####
# Admin 0

response_updated_cluster$admin_0 <- "admin_0"

df_santeprotect_admin0 <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_0)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            sum_pin_protec_peur = sum(pin_protec_peur * weights_sampling, na.rm = T)
  )%>%
  mutate(
    freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
    freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade
  )%>%
  select(admin_0, freq_enfantsmalades30j, freq_pers_malade_nonsoignee)%>%
  ungroup()%>%
  gather(key = "pins", value = "numbers", -admin_0)%>%
  mutate(category = "1")%>%
  select(admin_0, pins, category,  numbers)



template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

template_analysisplan <- read.csv(template_analysisplan_file, stringsAsFactors = F)

cols_to_analyse <- template_analysisplan$dependent.variable

response_updated_cluster <- response_updated_cluster %>%
  mutate_at(cols_to_analyse, as.character)

analysisplan_admin_0_pin <- make_analysis_plan_template(df= response_updated_cluster,
                                                        repeat.for.variable = "admin_0",
                                                        questionnaire = questionnaire,
                                                        hypothesis.type = "direct_reporting",
                                                        template_file = template_analysisplan_file)

final_result_admin_0_pin <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                            analysisplan = analysisplan_admin_0_pin, 
                                                            weighting = weighting_combined, 
                                                            cluster_variable_name = "clusters",
                                                            questionnaire = questionnaire)

summary.stats_admin0_pin <- final_result_admin_0_pin$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_0_pin$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin0_pin_nice <- summary.stats_admin0_pin%>% 
  select(dependent.var, dependent.var.value, repeat.var.value, numbers)%>%
  rename(admin_0 = repeat.var.value, pins = dependent.var, category = dependent.var.value, numbers = numbers)%>%
  bind_rows(df_santeprotect_admin0)%>%
  select(admin_0, pins, category, numbers)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin0_pin_",format(Sys.time(), "%Y%m%d"),".csv"))

#####

###ADMIN 0 GROUP


df_santeprotect_admin0_grp <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_0, ig_8_statut_groupe)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            pin_sante_lieuaccouchement = sum(as.numeric(pin_sante_lieuaccouchement)*weights_sampling, na.rm = T),
            sum_pin_protec_peur = sum(as.numeric(pin_protec_peur) * weights_sampling, na.rm = T),
            sum_protect_11_1 = sum(protect_11_1_aumoinsun))%>%
  mutate(
    freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
    freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade
  )%>%
  select(admin_0, ig_8_statut_groupe, freq_enfantsmalades30j, freq_pers_malade_nonsoignee)%>%
  ungroup()%>%
  gather(key = "pins", value = "numbers", -admin_0, -ig_8_statut_groupe)%>%
  mutate(category = "1")%>%
  select(admin_0, ig_8_statut_groupe, pins, category,  numbers)


template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

analysisplan_admin_0_grp_pin <- make_analysis_plan_template(df= response_updated_cluster,
                                                            repeat.for.variable = "admin_0",
                                                            independent.variable = "ig_8_statut_groupe",
                                                            questionnaire = questionnaire,
                                                            hypothesis.type = "direct_reporting",
                                                            template_file = template_analysisplan_file)

final_result_admin_0_grp_pin <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                                analysisplan = analysisplan_admin_0_grp_pin, 
                                                                weighting = weighting_combined, 
                                                                cluster_variable_name = "clusters",
                                                                questionnaire = questionnaire)

summary.stats_admin0_grp_pin <- final_result_admin_0_grp_pin$results %>%
  lapply(function(x){x$summary.statistic}) %>% do.call(rbind, .)

cols_analysisplan <- final_result_admin_0_grp_pin$analysisplan %>% select(research.question, sub.research.question, dependent.var)%>%distinct()

summary.stats_admin0_grp_pin_nice <- summary.stats_admin0_grp_pin%>%
  select(repeat.var.value, independent.var.value, dependent.var, dependent.var.value, numbers)%>%
  rename(admin_0 = repeat.var.value, ig_8_statut_groupe = independent.var.value, pins = dependent.var, category = dependent.var.value, numbers = numbers)%>%
  bind_rows(df_santeprotect_admin0_grp)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin0_grp_pin_",format(Sys.time(), "%Y%m%d"),".csv"))


summary.stats_admin0_grp_totaux_pin_nice <- summary.stats_admin0_pin_nice%>%
  mutate(
    ig_8_statut_groupe = "totaux"
  )%>%
  bind_rows(summary.stats_admin0_grp_pin_nice)%>%
  map_to_file(paste0("./output/tables/PIN/","summary_stats_admin0_grp_totaux_pin_",format(Sys.time(), "%Y%m%d"),".csv"))
