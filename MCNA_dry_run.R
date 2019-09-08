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
questions <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_survey.csv",
                      stringsAsFactors=F, check.names = F, encoding = "UTF-8")

choices <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_choices.csv", 
                    stringsAsFactors=F, check.names = F, encoding = "UTF-8")

choices$name <- gsub('[^ -~]', '', choices$name)
questions$name <- gsub('[^ -~]', '', questions$name)
questions$name <- tolower(questions$name)


## CAREFUL : have some " " at the end of some options. Replace them with nothing :
choices$list_name %<>% gsub(" ", "", .)

# test with hh loop added (need to run "loop_cleaning.R" file)
response <- read.csv("output/MSNA_HH_Analysed_data.csv", stringsAsFactors = F, encoding = "UTF-8")
response$mssc_2_source_rev_1 <- gsub("[^ -~]", "", response$mssc_2_source_rev_1)
response$mssc_2_source_rev_2 <- gsub("[^ -~]", "", response$mssc_2_source_rev_2)
response$mssc_2_source_rev_3 <- gsub("[^ -~]", "", response$mssc_2_source_rev_3)
response$aap_1_source_confiance <- gsub("[^ -~]", "", response$aap_1_source_confiance)
response$aap_3_canal_information <- gsub("[^ -~]", "", response$aap_3_canal_information)

to_alphanumeric_lowercase <-
  function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
names(response)<-to_alphanumeric_lowercase(names(response))

questionnaire <- load_questionnaire(data = response,
                                    questions = questions,
                                    choices = choices)

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


isnot_inquestionnaire <- names(response)[!names(response) %in% tolower(questions$name)]
isnot_inquestionnaire <- isnot_inquestionnaire[!isnot_inquestionnaire %in% unwanted_cols]
isnot_inquestionnaire <-  isnot_inquestionnaire[!questionnaire$question_is_sm_choice(isnot_inquestionnaire)]

message(paste(isnot_inquestionnaire, collapse = " \n"), " \n\n Those are re not in the questionnaire. Add them if you want to consider them")

# regroup Retourn? & Rapatri? as one category: 
response$ig_8_statut_groupe <-  recode(response$ig_8_statut_groupe, 
                                       retourne = "retourne_rapatrie", 
                                       rapatrie = "retourne_rapatrie")

# generate the stratification samplingframe
sampling.frame <- load_samplingframe(file = "input/sampling_fr_strata_v3.csv")
sampling.frame$population = gsub(",", "", sampling.frame$population)
sampling.frame = sampling.frame[!is.na(sampling.frame$population),]

sampling.frame$population %<>% as.numeric

## From sampling frame -> no IDP en site sur Bangui... Remplacer par IDP FA
response$stratum_column <- paste(response$admin_2, response$ig_8_statut_groupe, sep = "_")

# More cleaning to harmonize
response$stratum_column %<>% gsub("-", "_", .)
sampling.frame$stratum %<>% gsub("-", "_", .)

# delete data from responses that are not in the sampling frame:
response_strat <- response[(response$stratum_column %in% sampling.frame$stratum),]
response_strat$clusters <- NA

#sampling.frame$strata
weighting_sf <- map_to_weighting(sampling.frame = sampling.frame, 
                                 data = response_strat, 
                                 sampling.frame.population.column ="population", 
                                 sampling.frame.stratum.column = "stratum",
                                 data.stratum.column = "stratum_column")


# add cluster ids
# generate samplingframe
sampling.frame.clust <- load_samplingframe(file = "./input/sampling_fr_cluster_v5.csv")
### FIRST FIX LOCALITIES
response$localite_final_labels_admin2 %<>% gsub("_", "", .)
response$localite_final_labels_admin2 %<>% gsub("\\s", "", .)
response$localite_final_labels_admin2 %<>% tolower
sampling.frame.clust$villageall_admin2 %<>% gsub("_", "", .)
sampling.frame.clust$villageall_admin2 %<>% gsub("\\s", "", .)
sampling.frame.clust$villageall_admin2 %<>% tolower

response$localite_final_labels_admin2 <-  gsub('[^ -~]', '', response$localite_final_labels_admin2)
response <-  within(response, localite_final_labels_admin2[localite_final_labels_admin2 == "boborobocaranga" & localites_visitees_labels == "Boboro"] <- "boborobocaranga1")
response <-  within(response, localite_final_labels_admin2[localite_final_labels_admin2 == "boborobocaranga" & localites_visitees_labels == "BOBORO"] <- "boborobocaranga2")


# PASTE WITH THEIR ADMIN 2s
response$clusters <- paste(response$localite_final_labels_admin2)
sampling.frame.clust$villageall_admin2 <- gsub("bena-doemosossonakombo", "bena-doemososso-nakombo",sampling.frame.clust$villageall_admin2)

#verifying 
out <- response[!(response$localite_final_labels_admin2 %in% sampling.frame.clust$villageall_admin2),]

response_updated_cluster <- response[(response$localite_final_labels_admin2 %in% sampling.frame.clust$villageall_admin2),]


#sampling.frame.clust <- sampling.frame.clust[sampling.frame.clust$villageall_admin2 %in% under_cluster]

#MAP WOO 
cluster_weighting <- map_to_weighting(sampling.frame = sampling.frame.clust, 
                                      data = response_updated_cluster, 
                                      sampling.frame.population.column ="totalpop_fixe", 
                                      sampling.frame.stratum.column = "villageall_admin2",
                                      data.stratum.column = "clusters")




weighting_combined <- combine_weighting_functions(cluster_weighting, weighting_sf)

response_updated_cluster <- response_updated_cluster[, !colnames(response_updated_cluster) %in% unwanted_cols]
#### ADMIN 0 ####
response_updated_cluster$admin_0 <- "RCA"

template_analysisplan_file <- "./input/analysisplan_template.csv"

analysisplan_admin_0 <- make_analysis_plan_template(df= response_updated_cluster,
                                                   repeat.for.variable = "admin_0",
                                                   questionnaire = questionnaire,
                                                   hypothesis.type = "direct_reporting",
                                                   template_file = template_analysisplan_file)

analysisplan_admin_0_grp <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_0",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

#### ADMIN 1 ####

analysisplan_admin_1 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_1",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_1_grp <- make_analysis_plan_template(df= response_updated_cluster,
                                                        questionnaire = questionnaire,
                                                        repeat.for.variable = "admin_1",
                                                        independent.variable = "ig_8_statut_groupe",
                                                        hypothesis.type = "group_difference",
                                                        template_file = template_analysisplan_file
)

#### ADMIN 2 ####
analysisplan_admin_2 <- make_analysis_plan_template(df= response_updated_cluster,
                                                    repeat.for.variable = "admin_2",
                                                    questionnaire = questionnaire,
                                                    hypothesis.type = "direct_reporting",
                                                    template_file = template_analysisplan_file
)

analysisplan_admin_0 <- analysisplan_admin_0[!analysisplan_admin_0$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0 <- analysisplan_admin_0[!is.na(analysisplan_admin_0$dependent.variable.type),]

analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!analysisplan_admin_0_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_0_grp <- analysisplan_admin_0_grp[!is.na(analysisplan_admin_0_grp$dependent.variable.type),]

analysisplan_admin_1 <- analysisplan_admin_1[!row.names(analysisplan_admin_1) %in% unwanted_cols,]
analysisplan_admin_1 <- analysisplan_admin_1[!is.na(analysisplan_admin_1$dependent.variable.type),]

analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!analysisplan_admin_1_grp$dependent.variable %in% unwanted_cols,]
analysisplan_admin_1_grp <- analysisplan_admin_1_grp[!is.na(analysisplan_admin_1_grp$dependent.variable.type),]

analysisplan_admin_2 <- analysisplan_admin_2[!analysisplan_admin_2$dependent.variable %in% unwanted_cols,]
analysisplan_admin_2 <- analysisplan_admin_2[!is.na(analysisplan_admin_2$dependent.variable.type),]



# #### Loading missing functions ####
# this_folder <- getwd()
# setwd("C:/Users/Elliott Messeiller/Downloads/hypegrammaR-master(2)/hypegrammaR-master/R")
# files.sources = list.files()
# sapply(files.sources, source)
# setwd(this_folder)

#### Replacing infinite by NAs
response_updated_cluster <- do.call(data.frame,lapply(response_updated_cluster, function(x) replace(x, is.infinite(x),NA)))

### weights columns
response_updated_cluster$weights_sampling <- weighting_combined(response_updated_cluster)

# #### pcodes
# pcodes <- read.csv("./input/PCODES_Localites_OCHA.csv", stringsAsFactors = F, check.names = F)
# ##### cleaning pcodes
# pcodes$ADM1_NAME <- gsub("-", "_", pcodes$ADM1_NAME)
# pcodes$ADM1_NAME <- gsub("\'", "", pcodes$ADM1_NAME)
# pcodes$ADM1_NAME <- gsub(" ", "_", pcodes$ADM1_NAME)
# 
# pcodes$ADM2_NAME <- gsub("-", "_", pcodes$ADM2_NAME)
# pcodes$ADM2_NAME <-  gsub('[^ -~]', '', pcodes$ADM2_NAME)
# pcodes$ADM2_NAME <-  gsub('Mbr', 'Mbres', pcodes$ADM2_NAME)
# pcodes$ADM2_NAME <-  gsub('Mba', 'Mbaiki', pcodes$ADM2_NAME)
# 
# pcodes <- pcodes %>%
#   filter(ADM2_NAME %in% response_updated_cluster$admin_2)
# 
# pcodes$ADM3_NAME <- gsub("-", "_", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <-  gsub('[^ -~]', '', pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub(" ", "_", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("\'", "", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("\'", "", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("Badou_Ngoumb", "Baidou_Ngoumbourou", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("Griva_", "Grivai_Pamia", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("Haute_Ba", "Haute_Batouri", pcodes$ADM3_NAME)
# pcodes$ADM3_NAME <- gsub("Male", "Mala", pcodes$ADM3_NAME)
# 
# pcodes$combined_admin_levels <- paste(pcodes$ADM1_NAME, pcodes$ADM2_NAME, pcodes$ADM3_NAME, sep = "_")
# 
# pcodes <- pcodes%>%
#   select(combined_admin_levels, RowcaCode3)%>%
#   distinct()
# 
# response_updated_cluster$combined_admin_levels <- paste(response_updated_cluster$admin_1, response_updated_cluster$admin_2, response_updated_cluster$admin_3, sep ="_")
# 
# response_updated_cluster <- response_updated_cluster%>%
#   left_join(pcodes, by = c("combined_admin_levels"= "combined_admin_levels"))%>%
#               select(-combined_admin_levels)

write.csv(response_updated_cluster, paste0("REACH_CAR_dataset_HH_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"))


final_result_admin_0 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_0, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

final_result_admin_0_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_0_grp, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)


final_result_admin_1 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_1, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

final_result_admin_1_grp <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_1_grp, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)

final_result_admin_2 <- from_analysisplan_map_to_output(data = response_updated_cluster, 
                                                        analysisplan = analysisplan_admin_2, 
                                                        weighting = weighting_combined, 
                                                        cluster_variable_name = "clusters",
                                                        questionnaire = questionnaire)


# Print a massive table with everything (summary stats and p values)

# admin0

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

## admin0 groups

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




#### admin 1
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


##  admin 1 grp
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


# admin 2
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



###### Sante et protection ad-hoc pins

df_santeprotect_admin2 <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_2)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
    sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
    sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
    sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T)
  )%>%
  mutate(freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
         freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade,
         pin_pers_malade_soignee = if_else(freq_pers_malade_nonsoignee >=0 & freq_pers_malade_nonsoignee < .2, "1",
                                           if_else(freq_pers_malade_nonsoignee >= 0.2 & freq_pers_malade_nonsoignee < .4, "2",
                                                   if_else(freq_pers_malade_nonsoignee >= .4 & freq_pers_malade_nonsoignee < .6, "3",
                                                           if_else(freq_pers_malade_nonsoignee >= .6 & freq_pers_malade_nonsoignee <= .8, "4",
                                                                   if_else(freq_pers_malade_nonsoignee > .8  & freq_pers_malade_nonsoignee <=1, "5",
                                                                           NA_character_))))
           
         ),
         pin_enfantsmalades = if_else(freq_enfantsmalades30j >= 0 & freq_enfantsmalades30j <= .15,"1",
                                 if_else(freq_enfantsmalades30j > .15 & freq_enfantsmalades30j <= .25, "2",
                                         if_else(freq_enfantsmalades30j > .25 & freq_enfantsmalades30j <= .35, "3",
                                                 if_else(freq_enfantsmalades30j > .35 & freq_enfantsmalades30j <=.45, "4",
                                                         if_else(freq_enfantsmalades30j > .45 & freq_enfantsmalades30j <=1, "5", NA_character_)))))
    )%>%
  select(admin_2, pin_enfantsmalades, pin_pers_malade_soignee)


template_analysisplan_file <- "./input/analysisplan_template_pin.csv"

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
  mutate(admin_indic = paste(dependent.var, dependent.var.value, sep = "_"))%>%
  group_by(repeat.var.value)%>%
  select(-dependent.var, -dependent.var.value)%>%
  spread(key = admin_indic, value = numbers)%>%
  left_join(df_santeprotect_admin2, by = c("repeat.var.value" = "admin_2"))%>%
  mutate(pin_protec_2 = if_else(pin_protec_peur > 0 & pin_protec_peur <= .1, "1",
                                                                 if_else(pin_protec_peur > .1 & pin_protec_peur < .3, "3",
                                                                         if_else(pin_protec_peur >=.3 & pin_protec_peur <=1, "4",
                                                                                 NA_character_))),
         pin_sante_lieuaccouchement = if_else(pin_sante_lieuaccouchement >= .8 & pin_sante_lieuaccouchement <=1, "1",
                                              if_else(pin_sante_lieuaccouchement >= .6 & pin_sante_lieuaccouchement < .8, "2",
                                                      if_else(pin_sante_lieuaccouchement >= .4 & pin_sante_lieuaccouchement < .6, "3",
                                                              if_else(pin_sante_lieuaccouchement >= .2 & pin_sante_lieuaccouchement < .4, "4",
                                                                      if_else(pin_sante_lieuaccouchement >= 0 & pin_sante_lieuaccouchement < .2, "5",
                                                                              NA_character_)))))
  )%>%
  select(-pin_protec_peur , -pin_sante_lieuaccouchement, -pin_sante_lieuaccouchement_non)%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin2_pin_",format(Sys.time(), "%Y%m%d"),".csv"))

###### Sante et protection ad-hoc pins

maladies <- c("sum_sante_4_0_4_malades_autre_filles","sum_sante_4_0_4_malades_autre_garcons",
              "sum_sante_4_0_4_malades_diarrhee_filles","sum_sante_4_0_4_malades_diarrhee_garcons",
              "sum_sante_4_0_4_malades_toux_filles","sum_sante_4_0_4_malades_toux_garcons",
              "sum_sante_4_0_4_malades_fievre_filles","sum_sante_4_0_4_malades_fievre_garcons")


df_santeprotect_admin1_grp <- response_updated_cluster%>%
  mutate(nb_enfants_malades_30j = (sum_sante_2_malade_oui_0_5_filles + sum_sante_2_malade_oui_0_5_garcons),
         enfants_0_4_pond = sum_agegrp_0_4)%>%
  group_by(admin_1, ig_8_statut_groupe)%>%
  summarise(sum_enfantsmalades30j = sum(nb_enfants_malades_30j*weights_sampling, na.rm = T),
            sum_enfants_0_4_pond = sum(enfants_0_4_pond*weights_sampling, na.rm = T),
            sum_pers_malade = sum(sum_sante_2_malade_oui * weights_sampling, na.rm = T),
            sum_pers_malade_nonsoignee = sum((sum_sante_2_soin_recu_oui_autre + sum_sante_2_soin_recu_oui_maison + sum_sante_2_soin_recu_non) * weights_sampling, na.rm = T),
            pin_sante_lieuaccouchement = sum(pin_sante_lieuaccouchement*weights_sampling, na.rm = T)
  )%>%
  mutate(freq_enfantsmalades30j = sum_enfantsmalades30j / sum_enfants_0_4_pond,
         freq_pers_malade_nonsoignee = sum_pers_malade_nonsoignee / sum_pers_malade,
         pin_pers_malade_soignee = if_else(freq_pers_malade_nonsoignee >=0 & freq_pers_malade_nonsoignee < .2, "1",
                                           if_else(freq_pers_malade_nonsoignee >= 0.2 & freq_pers_malade_nonsoignee < .4, "2",
                                                   if_else(freq_pers_malade_nonsoignee >= .4 & freq_pers_malade_nonsoignee < .6, "3",
                                                           if_else(freq_pers_malade_nonsoignee >= .6 & freq_pers_malade_nonsoignee <= .8, "4",
                                                                   if_else(freq_pers_malade_nonsoignee > .8  & freq_pers_malade_nonsoignee <=1, "5",
                                                                           NA_character_))))
                                           
         ),
         pin_enfantsmalades = if_else(freq_enfantsmalades30j >= 0 & freq_enfantsmalades30j <= .15,"1",
                                      if_else(freq_enfantsmalades30j > .15 & freq_enfantsmalades30j <= .25, "2",
                                              if_else(freq_enfantsmalades30j > .25 & freq_enfantsmalades30j <= .35, "3",
                                                      if_else(freq_enfantsmalades30j > .35 & freq_enfantsmalades30j <=.45, "4",
                                                              if_else(freq_enfantsmalades30j > .45 & freq_enfantsmalades30j <=1, "5", NA_character_))))),
         pin_sante_lieuaccouchement = if_else(pin_sante_lieuaccouchement >= .8 & pin_sante_lieuaccouchement <=1, "1",
                                                                 if_else(pin_sante_lieuaccouchement >= .6 & pin_sante_lieuaccouchement < .8, "2",
                                                                         if_else(pin_sante_lieuaccouchement >= .4 & pin_sante_lieuaccouchement < .6, "3",
                                                                                 if_else(pin_sante_lieuaccouchement >= .2 & pin_sante_lieuaccouchement < .4, "4",
                                                                                         if_else(pin_sante_lieuaccouchement >= 0 & pin_sante_lieuaccouchement < .2, "5",
                                                                                                 NA_character_)))))
                
  )%>%
  select(admin_1, ig_8_statut_groupe, pin_enfantsmalades, pin_pers_malade_soignee)%>%
  ungroup()%>%
  select(admin_1, ig_8_statut_groupe, pin_enfantsmalades, pin_pers_malade_soignee)%>%
  gather(key = "pins", value = "numbers", -admin_1, -ig_8_statut_groupe)%>%
  mutate(admin_grp_score = paste(ig_8_statut_groupe, pins, sep = "_"))%>%
  select(admin_grp_score, admin_1, numbers)%>%
  group_by(admin_grp_score, admin_1)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = admin_grp_score, value = numbers)%>%
  select(-grouped_id)
  

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
  select(dependent.var, dependent.var.value,numbers, repeat.var.value, independent.var.value)%>%
  filter(dependent.var.value != "Aucune peur pour les adultes")%>%
  mutate(admin_grp_score = paste(dependent.var, independent.var.value, dependent.var.value, sep = "_"))%>%
  select(admin_grp_score, repeat.var.value, numbers)%>%
  group_by(admin_grp_score, repeat.var.value)%>%
  mutate(grouped_id = row_number())%>%
  spread(key = admin_grp_score, value = numbers)%>%
  select(-grouped_id)%>%
  left_join(df_santeprotect_admin1_grp, by = c("repeat.var.value" = "admin_1"))%>%
  map_to_file(paste0("./output/tables/","summary_stats_admin2_pin_",format(Sys.time(), "%Y%m%d"),".csv"))
