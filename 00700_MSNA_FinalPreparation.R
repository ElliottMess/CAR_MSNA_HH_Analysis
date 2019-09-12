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

response$localite_final_labels_admin2[response$localite_final_labels_admin2 == "boborobocaranga" & response$localites_visitees_labels == "Boboro"] <- "boborobocaranga1"
response$localite_final_labels_admin2[response$localite_final_labels_admin2 == "boborobocaranga" & response$localites_visitees_labels == "BOBORO"] <- "boborobocaranga2"


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

response_updated_cluster <- do.call(data.frame,lapply(response_updated_cluster, function(x) replace(x, is.infinite(x),NA)))

response_updated_cluster <- response_updated_cluster[, !colnames(response_updated_cluster) %in% unwanted_cols]

response_updated_cluster$pin_secal_fcs <- as.character(response_updated_cluster$pin_secal_fcs)



#### Replacing infinite by NAs

### weights columns
response_updated_cluster$weights_sampling <- weighting_combined(response_updated_cluster)

response_updated_cluster$pin_protec_acces_service <- as.character(response_updated_cluster$pin_protec_acces_service)

write.csv(response_updated_cluster, paste0("./output/REACH_CAR_dataset_HH_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"))


#### pcodes ####
final_dataset_tobeshared <- response_updated_cluster


pcodes <- read.csv("./input/PCODES_Localites_OCHA.csv", stringsAsFactors = F, check.names = F)
##### cleaning pcodes
pcodes$ADM1_NAME <- gsub("-", "_", pcodes$ADM1_NAME)
pcodes$ADM1_NAME <- gsub("\'", "", pcodes$ADM1_NAME)
pcodes$ADM1_NAME <- gsub(" ", "_", pcodes$ADM1_NAME)

pcodes$ADM2_NAME <- gsub("-", "_", pcodes$ADM2_NAME)
pcodes$ADM2_NAME <-  gsub('[^ -~]', '', pcodes$ADM2_NAME)
pcodes$ADM2_NAME <-  gsub('Mbr', 'Mbres', pcodes$ADM2_NAME)
pcodes$ADM2_NAME <-  gsub('Mba', 'Mbaiki', pcodes$ADM2_NAME)

pcodes <- pcodes %>%
  filter(ADM2_NAME %in% final_dataset_tobeshared$admin_2)


pcodes$combined_admin_levels <- paste(pcodes$ADM1_NAME, pcodes$ADM2_NAME, sep = "_")

pcodes <- pcodes%>%
  select(combined_admin_levels, RowcaCode2)%>%
  distinct()

final_dataset_tobeshared$combined_admin_levels <- paste(final_dataset_tobeshared$admin_1, final_dataset_tobeshared$admin_2, sep ="_")

final_dataset_tobeshared <- final_dataset_tobeshared%>%
  left_join(pcodes, by = c("combined_admin_levels"= "combined_admin_levels"))%>%
              select(-combined_admin_levels)

write.csv(final_dataset_tobeshared, paste0("./ExternalShares/REACH_CAR_dataset_HH_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"))
