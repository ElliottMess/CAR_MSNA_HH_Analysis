data_final = read.csv("./output/REACH_CAR_MSNA_Final_dataset_0309_clean.csv", stringsAsFactors = F)

sampling_cluster = read.csv("./input/sampling_fr_cluster.csv", stringsAsFactors = F)
sampling_strata = read.csv("./input/sampling_fr_strata.csv", stringsAsFactors = F)

popsize_dtm = read.csv("./input/DTM_Rd7_avril_2019.csv", stringsAsFactors = F)
colnames(popsize_dtm) = popsize_dtm[1,]
popsize_dtm = popsize_dtm


### 1. Prepare Sampling Strata file : 
admin2_tbc = unique(subset(sampling_strata,  !is_in(sampling_strata$Admin2, data_final$admin2_labels))$Admin2)

sampling_strata$Admin2 <- gsub("é","e" , sampling_strata$Admin2 ,ignore.case = TRUE)
sampling_strata$Admin2 <- gsub("è","e" , sampling_strata$Admin2 ,ignore.case = TRUE)
sampling_strata$Admin2 <- gsub("ï","i" , sampling_strata$Admin2 ,ignore.case = TRUE)
sampling_strata$Admin2 <- gsub(" ","-" , sampling_strata$Admin2 ,ignore.case = TRUE)

admin2_tbc = unique(subset(sampling_strata,  !is_in(sampling_strata$Admin2, data_final$admin2_labels))$Admin2)

# all in H2R -> delete from sampling_strata file : 
sampling_strata = subset(sampling_strata, !is_in(sampling_strata$Admin2, admin2_tbc))


sampling_strata$stratum = paste(sampling_strata$Admin2, sampling_strata$Groupe, sep ="_")

data_final$ig_8_statut_Groupe = ifelse(data_final$ig_8_statut == "hote", data_final$ig_8_statut, 
                                       ifelse(data_final$ig_8_statut == "IDP_FA",  "deplaces_FA", 
                                              ifelse(data_final$ig_8_statut == "IDP_site","deplaces_site", "retournes")))


data_final$stratum = paste(data_final$admin2_labels, "_", data_final$ig_8_statut_Groupe, sep ="")


admin2_tbc_inSampling_notDS = unique(subset(sampling_strata,  !is_in(sampling_strata$stratum, data_final$stratum))$stratum)
admin2_tbc_inSampling_notDS = subset(sampling_strata, is_in(sampling_strata$stratum, admin2_tbc_inSampling_notDS))

admin2_tbc_inDS_notSampling = unique(subset(data_final,  !is_in(data_final$stratum, sampling_strata$stratum))$stratum)
admin2_tbc_inDS_notSampling = subset(data_final, is_in(data_final$stratum, admin2_tbc_inDS_notSampling))


## Add estimation for Kuango IDP in site, Bakama IDP in site & Amada-Gaza retourn?s
popsize_KuangoPDIFA = sum(as.numeric(subset(popsize_dtm, popsize_dtm$`_ADM2_NAME` == "Kouango")$hh_idp))
popsize_KuangoPDIFA = c("Kouango", popsize_KuangoPDIFA, "deplaces_site", "Kouango_deplaces_FA")
popsize_BakoumaPDISite = c("Bakouma", 200, "deplaces_site", "Bakouma_deplaces_site")
popsize_AmadaGazaPDISite = c("Amada-Gaza", 163, "retournes", "Amada-Gaza_retournes")

sampling_strata_v2 = rbind(sampling_strata, popsize_AmadaGazaPDISite, popsize_BakoumaPDISite, popsize_KuangoPDIFA)


## Delete others and considered as new returns
rm_FalseStratum = subset(data_final,  !is_in(data_final$stratum, sampling_strata_v2$stratum))
data_final = subset(data_final,  is_in(data_final$stratum, sampling_strata_v2$stratum))

DataCleaningLogBook = read.csv("./output/DataCleaningLogBook_0309.csv", stringsAsFactors = F)
DataCleaningLogBook = DataCleaningLogBook[,-1]
for (i in 1:nrow(rm_FalseStratum)) {
  DataCleaningLogBook = rbind(DataCleaningLogBook,
                              c(rm_FalseStratum$X_uuid[i], rm_FalseStratum$X_index[i],  
                                "ig_8_statut_Groupe", "Not in samplling frame", "", "Oui", 
                                rm_FalseStratum$stratum[i], "Qst supprim? de la DB"))
}


sampling_strata_v2 = subset(sampling_strata_v2,  is_in(sampling_strata$stratum, data_final$stratum))
write.csv(sampling_strata_v2, "./output/sampling_fr_strata_v2.csv")






### 2. Prepare Sampling cluster file :

# match admin 2 data : 
#table(unique(sampling_cluster$admin2))
#table(unique(data_final$admin2_labels))

sampling_cluster$admin2 <- gsub("é","e" , sampling_cluster$admin2 ,ignore.case = TRUE)
sampling_cluster$admin2 <- gsub("è","e" , sampling_cluster$admin2 ,ignore.case = TRUE)
sampling_cluster$admin2 <- gsub("ï","i" , sampling_cluster$admin2 ,ignore.case = TRUE)
sampling_cluster$admin2 <- gsub(" ","-" , sampling_cluster$admin2 ,ignore.case = TRUE)

admin2_tbc = unique(subset(sampling_cluster,  !is_in(sampling_cluster$admin2, data_final$admin2_labels))$admin2)
if (length(admin2_tbc)!=0){
  stop("Have to have same spelling for admin 2 in sampling_cluster and in data_final")
}



# match village data : 
#table(unique(sampling_cluster$VillageAll))
#table(unique(data_final$localite_final_labels))

sampling_cluster$VillageAll_test = gsub(" ","" , sampling_cluster$VillageAll ,ignore.case = TRUE)
sampling_cluster$VillageAll_test = sapply(sampling_cluster$VillageAll_test, tolower) 
sampling_cluster$VillageAll_test = gsub("ï","i" , sampling_cluster$VillageAll_test ,ignore.case = TRUE)
sampling_cluster$VillageAll_test = gsub("é","e" , sampling_cluster$VillageAll_test ,ignore.case = TRUE)
sampling_cluster$VillageAll_test = gsub("è","e" , sampling_cluster$VillageAll_test ,ignore.case = TRUE)
sampling_cluster$VillageAll_test = gsub("ê","e" , sampling_cluster$VillageAll_test ,ignore.case = TRUE)
sampling_cluster$VillageAll_admin2 = paste0(sampling_cluster$VillageAll_test, " _ ", sampling_cluster$admin2, sep="")

data_final$localite_final_labels = gsub(" ","" , data_final$localite_final_labels ,ignore.case = TRUE)
data_final$localite_final_labels_2 = sapply(data_final$localite_final_labels, tolower) 
data_final$localite_final_labels_2 = gsub("ï","i" , data_final$localite_final_labels_2 ,ignore.case = TRUE)
data_final$localite_final_labels_2 = gsub("é","e" , data_final$localite_final_labels_2 ,ignore.case = TRUE)
data_final$localite_final_labels_2 = gsub("è","e" , data_final$localite_final_labels_2 ,ignore.case = TRUE)
data_final$localite_final_labels_admin2 =  paste0(data_final$localite_final_labels_2, " _ ", data_final$admin2_labels, sep="")

for(i in 1:nrow(data_final)){
  data_final$localite_final_labels_admin2[i] = ifelse(data_final$localite_final_labels_admin2[i] == "unknown39(bondjoukombo) _ Abba",
                                                      "unknown39 _ Abba",
                                                      data_final$localite_final_labels_admin2[i])
  data_final$localite_final_labels_admin2[i] = ifelse(data_final$localite_final_labels_admin2[i] == "bakala-koupi _ Bakala",
                                                      "bakala _ Bakala",
                                                      data_final$localite_final_labels_admin2[i])
  data_final$localite_final_labels_admin2[i] = ifelse(data_final$localite_final_labels_admin2[i] == "balinguini _ Bakouma",
                                                      "baliguini _ Bakouma",
                                                      data_final$localite_final_labels_admin2[i])
  data_final$localite_final_labels_admin2[i] = ifelse(data_final$localite_final_labels_admin2[i] == "yamballa _ Bamingui",
                                                      "yambala _ Bamingui",
                                                      data_final$localite_final_labels_admin2[i])
}

village_tbc_inSampling_notDS  = unique(subset(sampling_cluster,  !is_in(sampling_cluster$VillageAll_admin2, data_final$localite_final_labels_admin2))$VillageAll_test)
village_tbc_inSampling_notDS
village_tbc_inDS_notSampling = unique(subset(data_final,  !is_in(data_final$localite_final_labels_admin2, sampling_cluster$VillageAll_admin2))$localite_final_labels_admin2)
village_tbc_inDS_notSampling

popsize_village = read.csv("./input/Copy of caf_msna_sample_frame_villages_dtm_v3.csv", stringsAsFactors = F)

popsize_village$GPS.point = paste(popsize_village$POINT_X, ",", popsize_village$POINT_Y, sep="")
popsize_village_total = popsize_village[, c("VillageAll", "GPS.point", "admin1Name", "admin1Pcod", "admin2Name", "admin2Pcod", "admin3Name", "admin3Pcod", "TotalPop")]
popsize_village_total$VillageAll_test = gsub(" ","" , popsize_village_total$VillageAll ,ignore.case = TRUE)
popsize_village_total$VillageAll_test =  sapply(popsize_village_total$VillageAll_test, tolower) 
popsize_village_total$VillageAll_test = gsub("´","i" , popsize_village_total$VillageAll_test ,ignore.case = TRUE)
#popsize_village_total$VillageAll_test = gsub("","e" , popsize_village_total$VillageAll_test ,ignore.case = TRUE)
popsize_village_total$VillageAll_test = gsub("ê","e" , popsize_village_total$VillageAll_test ,ignore.case = TRUE)

popsize_village_total$admin2Name2 = gsub("ï","i" , popsize_village_total$admin2Name ,ignore.case = TRUE)
popsize_village_total$admin2Name2 = gsub("é","e" , popsize_village_total$admin2Name2 ,ignore.case = TRUE)
popsize_village_total$admin2Name2 = gsub("è","e" , popsize_village_total$admin2Name2 ,ignore.case = TRUE)
popsize_village_total$admin2Name2 = gsub(" ","-" , popsize_village_total$admin2Name2 ,ignore.case = TRUE)

popsize_village_total$VillageAll_admin2 = paste0(popsize_village_total$VillageAll_test, " _ ", popsize_village_total$admin2Name2, sep="")

popsize_village_total = popsize_village_total[,c(1:4,11,6:10,12)]

colnames(popsize_village_total) = colnames(sampling_cluster)

# number of villages in dataset :
rep(unique(as.factor(data_final$localite_final_labels_admin2)))



sampling_cluster_new = subset(sampling_cluster,  is_in(sampling_cluster$VillageAll_admin2, data_final$localite_final_labels_admin2))
for (i in 1:length(village_tbc_inDS_notSampling)){
  if(is_in(village_tbc_inDS_notSampling[i], popsize_village_total$VillageAll_admin2)){
    sampling_cluster_new = rbind(sampling_cluster_new, subset(popsize_village_total, popsize_village_total$VillageAll_admin2 == village_tbc_inDS_notSampling[i]))
  }
}

village_tbc_inDS_notSampling = unique(subset(data_final,  !is_in(data_final$localite_final_labels_admin2, sampling_cluster_new$VillageAll_admin2))$localite_final_labels_admin2)
village_tbc_inDS_notSampling


## For missing villages, take the average size of villages in the admin 2 :
sampling_cluster_new_all = sampling_cluster_new
for (i in 1:length(village_tbc_inDS_notSampling)){
  sampling_cluster_new_all = rbind(sampling_cluster_new_all, c(subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$localite_final_labels[1],
                                                               "", 
                                                               subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$admin_1[1],
                                                               "",
                                                               subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$admin2_labels[1],
                                                               "",
                                                               subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$admin_3[1],
                                                               "", 
                                                               sum(subset(popsize_village_total, 
                                                                          popsize_village_total$admin2 == subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$admin2_labels[1])[,9]) / nrow(subset(popsize_village_total, 
                                                                                                                                                                                                                                              popsize_village_total$admin2 == subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$admin2_labels[1]))
                                                               , 
                                                               subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$localite_final_labels_2[1],
                                                               subset(data_final, data_final$localite_final_labels_admin2 == village_tbc_inDS_notSampling[i])$localite_final_labels_admin2[1]))
}


write.csv(sampling_cluster_new_all, "./output/sampling_fr_cluster_v5.csv")

write.csv(data_final, "./output/REACH_CAR_MSNA_Final_dataset_0309_clean_sampled.csv")