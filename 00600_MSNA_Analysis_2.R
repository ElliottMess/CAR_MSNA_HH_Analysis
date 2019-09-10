library(dplyr)
library(magrittr)


### Loading files

df_hh <- read.csv("output/MSNA_HH_Analysed_data.csv", stringsAsFactors = F)


# Recode 'other' ####
pacs <- read.csv2("./input/other_recoded_PAC.csv", stringsAsFactors = F, sep = ",", check.names = F) %>% as.data.frame
# other_toberecoded = read.csv("./input/other_recoded_PAC.csv", stringsAsFactors = F)

names <- colnames(pacs)
names[6] <- "reponse_corr"
names -> colnames(pacs)

other_toberecoded = pacs

other_toberecoded = subset(other_toberecoded, other_toberecoded$reponse_corr != "")
liste_qst_other = unique(other_toberecoded$variable)
# liste_qst_other_borda = c("educ_5_ecole_acces_autre", 
#                           # "educ_6_reponse_autre", 
#                           "wash_22_autres_autre", "mssc_2_source_rev_autre", "wash_9_insuff_raisons_autre")
liste_qst_other_mc = c("aap_2_1_type_information_autre", "aap_4_retour_fournisseurs_aide_autre", 
                       # "ig_14_IDP_cond_retour_autre", 
                       "protect_13_autre", "wash_2_source_autre_usage_autre", "protect_2_femmes_risque_autre",
                       "protect_3_filles_risque_autre", "protect_3_garcons_risque_autre","protect_2_hommes_risque_autre", "wash_1_source_boisson_autre")
# liste_qst_other_other = c("mssc_4_dep_6M_autre", "wash_20_autres_autre")

other_toberecoded$value <- gsub(" /// instances: [0-9]*?$", "",other_toberecoded$value )

# Recode mc questions :
liste_qst_other_mc
question.name_mc = c("aap_2_type_information", "aap_4_retour_fournisseurs_aide", "protect_13", "wash_2_source_autre_usage", "protect_2_femmes_risque", 
                     "protect_2_filles_risque", "protect_2_garcons_risque", "protect_2_hommes_risque", "wash_1_source_boisson") 


other_toberecoded_mc = subset(other_toberecoded, is_in(other_toberecoded$variable, liste_qst_other_mc))
other_toberecoded_mc = subset(other_toberecoded_mc, other_toberecoded_mc$reponse_corr != "autre")

for (i in 1:nrow(other_toberecoded_mc)){
  other_toberecoded_mc$question.name[i] = paste0(question.name_mc[match(other_toberecoded_mc$variable[i], liste_qst_other_mc)], 
                                                 ".", other_toberecoded_mc$reponse_corr[i], sep="") 
  other_toberecoded_mc$question.name.autre[i] = paste0(question.name_mc[match(other_toberecoded_mc$variable[i], liste_qst_other_mc)], 
                                                       ".autre", sep="")
}

for(i in 1:nrow(df_hh)){
  for(j in 1:length(liste_qst_other_mc)){
    other_toberecoded_mc_j = subset(other_toberecoded_mc, other_toberecoded_mc$variable == liste_qst_other_mc[j])
    if(is_in(df_hh[[liste_qst_other_mc[j]]][i], other_toberecoded_mc_j$value)){
      other_toberecoded_mc_j_2 = subset(other_toberecoded_mc_j, other_toberecoded_mc_j$value == df_hh[[liste_qst_other_mc[j]]][i])
      if(length(df_hh[[other_toberecoded_mc_j_2$question.name]][i]) !=0){
        df_hh[[other_toberecoded_mc_j_2$question.name]][i] = 1
        df_hh[[other_toberecoded_mc_j_2$question.name.autre]][i] = 0
        df_hh[[question.name_mc[j]]][i] = paste0(df_hh[[question.name_mc[j]]][i], " ", other_toberecoded_mc_j_2$reponse_corr, sep="")
        df_hh[[question.name_mc[j]]][i] = sub("autre$", "", df_hh[[question.name_mc[j]]][i])
      }
    }
  }
}


# Add a variable per inidcator - Per severity score if we have the scales ?  ####
df_hh$sante_indicator_accouchement_assiste = ifelse(df_hh$sum_sante_1_accouch_lieu == "maison_assiste" | df_hh$sum_sante_1_accouch_lieu == "cs", 1, 0)
df_hh$sante_indicator_accouchement_non_assiste = ifelse(df_hh$sum_sante_1_accouch_lieu == "maison_non_assiste" | df_hh$sum_sante_1_accouch_lieu == "autre", 1, 0)
df_hh$sante_indicator_accouchement_nsp = ifelse(df_hh$sum_sante_1_accouch_lieu == "nsp" , 1, 0)

df_hh$sante_5_deces_5moins = ifelse(df_hh$sante_5_deces_age < 5, 1, ifelse(df_hh$sante_5_deces_age >= 5, 0, NA))

## Borda anaalysis -> Seth ? 

# Check if all indicators are in 
# 

var_freq_scol <- c("sum_educ_3_presence_18_19.filles_7_12.12m" , "sum_educ_3_presence_18_19.filles_7_12.6m_12m" , 
                   "sum_educ_3_presence_18_19.filles_13_18.12m", "sum_educ_3_presence_18_19.filles_13_18.6m_12m",
                   "sum_educ_3_presence_18_19.garcons_7_12.12m" , "sum_educ_3_presence_18_19.garcons_7_12.6m_12m" , 
                   "sum_educ_3_presence_18_19.garcons_13_18.12m", "sum_educ_3_presence_18_19.garcons_13_18.6m_12m")

df_hh <- df_hh%>%
  mutate(
    pin_abri_surface = if_else((nfi_2_type_abri == "abri_urgence" & taille_abri_pp <= 3.5) | nfi_2_type_abri == "aucun", "5",
                               if_else(nfi_2_type_abri == "abri_urgence" & taille_abri_pp > 3.5, "3",
                                       if_else(nfi_2_type_abri == "habitat_paille" & taille_abri_pp <= 3.5 , "2",
                                               if_else(nfi_2_type_abri == "maison_dur" |  ( nfi_2_type_abri == "habitat_paille" & taille_abri_pp > 3.5), "1",
                                                       NA_character_))))
  )%>%
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
    pin_sante_lieuaccouchement = if_else(sum_sante_1_accouch_lieu %in% c("cs"), "oui", 
                                         if_else(sum_sante_1_accouch_lieu != "cs", "non",
                                                 NA_character_))
  )

df_hh$protect_11_1_detress <- replace_na(df_hh$protect_11_1, "0")

df_hh <- df_hh %>%
  mutate(
    protect_11_1_aumoinsun = if_else(protect_11_1 >0 ,1, 0 ),
    pin_protec_detresse = if_else(sum_agegrp_0_17 > 0 & is.na(protect_11_1) , "0",
                                  if_else(sum_agegrp_0_17 > 0 & protect_11_1 == 0, "0",
                                          if_else(sum_agegrp_0_17 > 0 & protect_11_1 > 0, "1",NA_character_)))
  )

write.csv(df_hh, "output/MSNA_HH_Analysed_data.csv")