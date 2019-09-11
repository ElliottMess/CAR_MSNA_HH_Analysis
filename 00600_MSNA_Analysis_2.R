  library(dplyr)
  library(magrittr)
  
  
  ### Loading files
  
  df_hh <- read.csv("output/MSNA_HH_Analysed_data.csv", stringsAsFactors = F)
  
  choices_form <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_choices.csv", stringsAsFactors = F)
  
  source('./functions/change_other_values.R')
  
  # ### Add variable for urban vs. rural ####
  # df_hh$urban_rural = ""
  # # import from Matt's file the var
  # popsize_village = read.csv("./input/Copy of caf_msna_sample_frame_villages_dtm_v3.csv", stringsAsFactors = F)
  # 
  # popsize_village$admin2Name2 = gsub("ï","i", popsize_village$admin2Name ,ignore.case = TRUE) 
  # popsize_village$admin2Name2 = gsub("é","e", popsize_village$admin2Name2,ignore.case = TRUE) 
  # popsize_village$admin2Name2 = gsub("è","e", popsize_village$admin2Name2, ignore.case = TRUE)
  # popsize_village$admin2Name2 = gsub(" ","-", popsize_village$admin2Name2 , ignore.case = TRUE)
  # popsize_village$admin2Name2 = gsub("?","e", popsize_village$admin2Name2 , ignore.case = TRUE)
  # popsize_village$VillageAll_test = gsub(" ","" , popsize_village$VillageAll ,ignore.case = TRUE)
  # popsize_village$VillageAll_test =  sapply(popsize_village$VillageAll_test, tolower) 
  # popsize_village$VillageAll_test = gsub("´","i" , popsize_village$VillageAll_test ,ignore.case = TRUE)
  # popsize_village$VillageAll_test = gsub("?z","e" , popsize_village$VillageAll_test ,ignore.case = TRUE)
  # 
  # popsize_village$VillageAll_admin2 = paste0(popsize_village$VillageAll_test, " _ ", popsize_village$admin2Name2, sep="")
  # 
  # for (i in 1:nrow(df_hh)){
  #   df_hh$urban_rural[i] = ifelse(is_in(df_hh$localite_final_labels_admin2[i], popsize_village$VillageAll_admin2), 
  #                                 subset(popsize_village, popsize_village$VillageAll_admin2 == df_hh$localite_final_labels_admin2[i])$UrbanRural[1], 
  #                                 df_hh$urban_rural[i]) 
  # }
  # 
  # table(df_hh$urban_rural)
  # 
  # not_inSF = as.data.frame(unique(subset(df_hh, !is_in(df_hh$localite_final_labels_admin2, popsize_village$VillageAll_admin2))$localite_final_labels_admin2))
  # write.csv(not_inSF, "output/urbainrural.csv")
  # 
    
  
  codes_others_aap <- read.csv("./input/questionnaire_MSNA_HH_autreToClean.csv", stringsAsFactors = F)%>%
    mutate_at(vars(ends_with("_autre")), removed_nonUTF)
  
  
  questions_toberecoded <- names(select(codes_others_aap, ends_with("_autre")))
  
  df_hh <- df_hh%>%
    mutate_at(questions_toberecoded, removed_nonUTF)
  
  list_to_add_aap_1_1_source_confiance_autre <- choices_form$name[choices_form$list_name == "source_confiance"]
  

  df_hh$aap_1_source_confiance <- change_select_one_value(question.name = "aap_1_source_confiance", 
                                                          other.q.name = "aap_1_1_source_confiance_autre", 
                                                          old.value.name  = "aap_1_1_source_confiance_autre", 
                                                          new.value.name = "aap_1_1_source_confiance_autre_recoding",
                                                          toadd.value = list_to_add_aap_1_1_source_confiance_autre,
                                                          data = df_hh,
                                                          codes.df = codes_others_aap)$aap_1_source_confiance
  
  
  list_to_aap_3_canal_information_autre <- choices_form$name[choices_form$list_name == "canal_information"]
  

  df_hh$aap_3_canal_information <- change_select_one_value(question.name = "aap_3_canal_information", 
                                                          other.q.name = "aap_3_canal_information_autre", 
                                                          old.value.name  = "aap_3_canal_information_autre", 
                                                          new.value.name = "aap_3_canal_information_autre_recoding",
                                                          toadd.value = list_to_aap_3_canal_information_autre,
                                                          data = df_hh,
                                                          codes.df = codes_others_aap)$aap_3_canal_information
  
  list_to_sante_5_deces_cause <- choices_form$name[choices_form$list_name == "deces_cause"]
  
  df_hh$sante_5_deces_cause <- change_select_one_value(question.name = "sante_5_deces_cause", 
                                                       other.q.name = "sante_5_deces_cause_autre", 
                                                       old.value.name  = "sante_5_deces_cause_autre", 
                                                       new.value.name = "sante_5_deces_cause_autre_recoded",
                                                       toadd.value = list_to_sante_5_deces_cause,
                                                       data = df_hh,
                                                       codes.df = codes_others_aap)$sante_5_deces_cause
  
  list_to_nfi_propr_abri <- choices_form$name[choices_form$list_name == "propr_abri"]
  
  df_hh$nfi_propr_abri <- change_select_one_value(question.name = "nfi_propr_abri", 
                                                       other.q.name = "nfi_propr_abri_autre", 
                                                       old.value.name  = "nfi_propr_abri_autre", 
                                                       new.value.name = "nfi_propr_abri_autre_recoded",
                                                       toadd.value = list_to_nfi_propr_abri,
                                                       data = df_hh,
                                                       codes.df = codes_others_aap)$nfi_propr_abri
  
  
  # Add a variable per inidcator - Per severity score if we have the scales ?  ####
  df_hh$sante_indicator_accouchement_assiste = ifelse(df_hh$sum_sante_1_accouch_lieu == "maison_assiste" | df_hh$sum_sante_1_accouch_lieu == "cs", 1, 0)
  df_hh$sante_indicator_accouchement_non_assiste = ifelse(df_hh$sum_sante_1_accouch_lieu == "maison_non_assiste" | df_hh$sum_sante_1_accouch_lieu == "autre", 1, 0)
  df_hh$sante_indicator_accouchement_nsp = ifelse(df_hh$sum_sante_1_accouch_lieu == "nsp" , 1, 0)
  
  df_hh$sante_5_deces_5moins = ifelse(df_hh$sante_5_deces_age < 5, 1, ifelse(df_hh$sante_5_deces_age >= 5, 0, NA))
  
  var_freq_scol <- c("sum_educ_3_presence_18_19.filles_7_12.12m" , "sum_educ_3_presence_18_19.filles_7_12.6m_12m" , "sum_educ_3_presence_18_19.filles_13_18.12m", "sum_educ_3_presence_18_19.filles_13_18.6m_12m",
                     "sum_educ_3_presence_18_19.garcons_7_12.12m" , "sum_educ_3_presence_18_19.garcons_7_12.6m_12m" , "sum_educ_3_presence_18_19.garcons_13_18.12m", "sum_educ_3_presence_18_19.garcons_13_18.6m_12m")
  
  df_hh <- df_hh%>%
    mutate(
      nb_enfants_scol_6mplus = rowSums(select(., var_freq_scol), na.rm = T),
      school_age_enfants = (sum_educ_2_inscrit_18_19_non + sum_educ_2_inscrit_18_19.filles_13_18 + sum_educ_2_inscrit_18_19.filles_7_12 + sum_educ_2_inscrit_18_19.garcons_13_18 + sum_educ_2_inscrit_18_19.garcons_7_12),
      freq_scolarise_6m = nb_enfants_scol_6mplus / school_age_enfants,
      pin_educ_freq = if_else(freq_scolarise_6m >= .8, "1",
                              if_else(freq_scolarise_6m < .8 & freq_scolarise_6m >= .6, "2",
                                      if_else(freq_scolarise_6m <.6 & freq_scolarise_6m >= .4, "3",
                                              if_else(freq_scolarise_6m < 0.4 & freq_scolarise_6m >= .2, "4",
                                                      if_else( freq_scolarise_6m < .2 & freq_scolarise_6m >=0, "5", NA_character_)
                                              )))
      )
    )

  
  #### SECAL
  
  
  df_hh <- df_hh %>%
    mutate(
      protect_11_1_aumoinsun = if_else(protect_11_1 >0 ,1, 0 )
    )
  
  df_hh <- df_hh%>%
    mutate(
      pin_score_nfi = if_else(score_nfi_tot >= 3.9, "5",
                              if_else(score_nfi_tot >= 3, "4",
                                      if_else(score_nfi_tot >= 2, "3",
                                              if_else(score_nfi_tot >= 1, "2",
                                                      if_else(score_nfi_tot <= 1 & score_nfi_tot >=0, "1",
                                                              NA_character_)
                                              ))))
    )
  
  
  df_hh <- df_hh %>%
    mutate(
      pin_mssc_marche = if_else(secal_9_acces_marche == "marche_inexist" |  secal_9_acces_marche == "marche_trop_cher" | 
                                  secal_9_acces_marche == "marche_exist_acces_dangereux" |  secal_9_acces_marche == "marche_exist_acces_inacess_transport" |
                                  secal_9_acces_marche == "marche_exist_acces_inacess_physique" | secal_9_acces_marche ==  "marche_exist_acces_inacess_secu", "5",
                                if_else(secal_9_acces_marche == "marche_non_appro_bien_alim", "4", 
                                        if_else(secal_9_acces_marche == "marche_non_appro_nfi", "3", 
                                                if_else(secal_9_acces_marche == "marche_accessible", "1", NA_character_)))),
      pin_protec_acces_service = if_else(protect_13.administration_leg == 1 | protect_13.comite_protection == 1 | protect_13.administration_loc == 1 | 
                                           protect_13.autre == 1 | protect_13.centre_ecoute == 1 | protect_13.relais_communautaire == 1 |
                                           protect_13.centre_ecoute == 1, "1",
                                         if_else(protect_13.aucun == 1 | protect_13.nsp == 1, "4", NA_character_)),
      protec_2_peur = if_else(protect_2_femmes == "oui" | protect_2_hommes == "oui", 1, 0),
      pin_protec_peur = if_else(protec_2_peur > 0 & protec_2_peur <= .1, "1",
                                if_else(protec_2_peur > .1 & protec_2_peur < .3, "3",
                                        if_else(protec_2_peur >=.3 & protec_2_peur <=1, "4",
                                                NA_character_)))
    )%>%
    mutate(
      pin_secal_hhs = if_else(hhs_score >= 5, "5", 
                              if_else(hhs_score >=4 , "4", 
                                      if_else(hhs_score >= 2 & hhs_score <=3, "3", 
                                              if_else(hhs_score ==1, "2",
                                                      if_else(hhs_score == 0, "1",
                                                              NA_character_))))),
      
      pin_secal_lcs = if_else(lcs_total == "urgence", "4", 
                              if_else(lcs_total == "crise", "3", 
                                      if_else(lcs_total == "stress", "2", 
                                              if_else(lcs_total == "minimal", "1", NA_character_)))),
      pin_secal_fcs = if_else(fcs_score2 > 35, "1",
                              if_else(fcs_score2 >21 & fcs_score2 <= 35, "4",
                                      if_else(fcs_score2 >= 0 & fcs_score2 <= 21, "5",
                                              NA_character_))),
      # aumoins_unenfant_diarrhee = if_else((sum_sante_4_0_4_malades_diarrhee_filles + sum_sante_4_0_4_malades_diarrhee_garcons)>0, 1, 0),
      nb_enfant_diarrhee = sum_sante_4_0_4_malades_diarrhee_filles + sum_sante_4_0_4_malades_diarrhee_garcons,
      pin_sante_diarrhee = if_else(nb_enfant_diarrhee == 0, "1",
                                   if_else(nb_enfant_diarrhee / sum_agegrp_0_4 <= 0.5, "3",
                                           if_else(nb_enfant_diarrhee / sum_agegrp_0_4 > 0.5, "5", NA_character_))),
      pin_wash_source = if_else(source_eau_combinee == "amelioree" & wash_4_temps == "sur_place", "1",
                                if_else(source_eau_combinee == "amelioree" & 
                                          (wash_4_temps == "0_5_min" | wash_4_temps == "5_15_min" | wash_4_temps == "16_30_min"), "2", 
                                        if_else(source_eau_combinee == "amelioree" & wash_4_temps == "31_plus_min", "3",  
                                                if_else(source_eau_combinee == "non_amelioree", "4", 
                                                        if_else(source_eau_combinee == "surface", "5", NA_character_))))),
      pin_wash_infra = if_else(wash_10_infra_sanit == "latrine_acceptable" & wash_11_sanit_partagee == "non", "1", 
                               if_else(wash_10_infra_sanit == "latrine_acceptable" & wash_11_sanit_partagee == "oui", "3", 
                                       if_else(wash_10_infra_sanit == "latrine_non_acceptable", "4", 
                                               if_else(wash_10_infra_sanit == "defec_air_libre" | 
                                                         wash_10_infra_sanit == "defec_air_libre_cours_d_eau" | 
                                                         wash_10_infra_sanit == "defec_air_libre_zone_precise", "5", NA_character_))))
    )
  
  
  ## PIN SECTORIEL
  df_hh <- df_hh%>%
    mutate(
      pin_sector_protect = if_else(protect_13 == "aucun" | protect_13 == "nsp", "4", "1"), 
      
      
    )
  
  ## PIN Inter new
  
  df_hh <- df_hh%>%
    mutate(
      pin_abri_surface = if_else((nfi_2_type_abri == "abri_urgence" & taille_abri_pp <= 3.5) | nfi_2_type_abri == "aucun", "5",
                                 if_else(nfi_2_type_abri == "abri_urgence" & taille_abri_pp > 3.5, "3",
                                         if_else(nfi_2_type_abri == "habitat_paille" & taille_abri_pp <= 3.5 , "2",
                                                 if_else(nfi_2_type_abri == "maison_dur" |  ( nfi_2_type_abri == "habitat_paille" & taille_abri_pp > 3.5), "1",
                                                         NA_character_)))),
      pin_sante_lieuaccouchement = if_else(sum_sante_1_accouch_lieu %in% c("cs"), "oui", 
                                           if_else(sum_sante_1_accouch_lieu != "cs", "non",
                                                   NA_character_))
    )
  df_hh$protect_11_1_detress <- replace_na(df_hh$protect_11_1, "0")
  
  df_hh <- df_hh %>%
    mutate(
      pin_protec_detresse = if_else(sum_agegrp_0_17 > 0 & is.na(protect_11_1) , "0",
                                    if_else(sum_agegrp_0_17 > 0 & protect_11_1 == 0, "0",
                                            if_else(sum_agegrp_0_17 > 0 & protect_11_1 > 0, "1",NA_character_))),
      protect_gbv = rowSums(select(., "protect_2_femmes_risque.mariage_force", "protect_2_femmes_risque.violence_sexuelles", "protect_3_filles_risque.mariage_force","protect_3_filles_risque.violence_sexuelles", "protect_3_garcons_risque.mariage_force", "protect_3_garcons_risque.violence_sexuelles"), na.rm = T),
      protect_gbv_oui = if_else(protect_gbv > 0, "1",
                                        "0")
      )
  
  write.csv(df_hh, "output/MSNA_HH_Analysed_data.csv")
  
  