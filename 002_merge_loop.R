library(dplyr)
library(koboquest) # manage kobo questionnairs
library(parallel) # mclapply
library(kobostandards) # check inputs for inconsistencies
#devtools::install_github('mabafaba/kobostandards')
library(xlsformfill) # generate fake data for kobo
#devtools::install_github('mabafaba/xlsformfill')
library(hypegrammaR) # stats 4 complex samples
#devtools::install_github('ellieallien/hypegrammaR')
library(composr) # horziontal operations
library(parallel)
#devtools::install_github('sharonorengo/koboloops')
library(koboloops)
library(mergekobodata)
library(cleaninginspectoR)
#install.packages("vctrs")
library(vctrs)

### Importing final dataset (main dataset) :
#main <- read.csv("./output/REACH_CAR_MSNA_Final_dataset_2.csv", stringsAsFactors = F)
raw_data <- read.csv("input/questionnaire_MSNA_HH_2019-08-29.csv", stringsAsFactors = F)

choices_form <- read.csv("input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_choices.csv", stringsAsFactors = F)

removed_nonUTF <- function(x){
  x <- gsub('[^ -~]', '', x)
  x <- gsub(' ', "", x )
  x <- gsub("\'", "", x)
  x <- gsub("[(),.]", "", x)
  x <- tolower(x)
  return(x)
}

write.csv.withversion <- function(data, file.name){
  file.name <- paste(file.name, format(Sys.time(), "%Y%m%d"), sep="_")
    if(!file.exists(file.name)){return(file.name)}
    i=1
    repeat {
      f = paste(file.name,i,sep=".")
      if(!file.exists(f)){return(f)}
      i=i+1
    }
}



hh_ind <- read.csv("input/questionnaire_MSNA_HH_loop_2019-08-29.csv", stringsAsFactors = F, encoding = "UTF-8")%>%
  mutate_at(vars(ends_with("_autre")), removed_nonUTF)


###Cleaning HH data

DataCleaningLogBook_hh_ind <-data.frame( matrix(ncol = 8, nrow = 0))
names(DataCleaningLogBook_hh_ind) <- c("uuid", "index", "question.name", "Issue",	"feedback",	"changed",	"old.value",	"new.value")

log_cols <- names(DataCleaningLogBook_hh_ind)

hh_ind_cleaned_value <- read.csv("./input/questionnaire_MSNA_HH_loop_2019-08-29_autreToClean.csv", stringsAsFactors = F)%>%
  mutate_at(vars(ends_with("_autre")), removed_nonUTF)


change_select_one_value <- function(question.name, other.q.name, old.value.name, new.value.name, toadd.value, data, codes.df){
  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")

  old.value <- variable.coded[[old.value.name]]
  
  new.value <- variable.coded[[new.value.name]]
  
  other.q <- data[[other.q.name]] 
  question.v <- data[[question.name]]
  data[[other.q.name]] <- plyr::mapvalues(other.q, old.value, new.value)
  question.v[which(!(is.null(other.q)))] <- !(is.null(data[[other.q.name]]))
  question.v[data[[other.q.name]] %in% toadd.value] <- data[[other.q.name]][data[[other.q.name]] %in% toadd.value]
  data[[question.name]] <- question.v
  return(data)
}

change_select_one_value_log <- function(logbook, data, codes.df, other.q.name, new.value.name){
  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")
  
  variables.log <- data %>%
    inner_join(variable.coded)%>%
    select(X_submission__uuid, X_index, other.q.name, new.value.name)%>%
    mutate(uuid = X_submission__uuid, index = X_index, question.name = other.q.name, 
           feedback = "", Issue = "Autre a recoder", changed = "Oui", 
           old.value = UQ(sym(other.q.name)), new.value = UQ(sym(new.value.name)))%>%
    select(log_cols)
  
  rbind(logbook, variables.log)
  
  return(logbook)
}

#### Accouchement

list_to_add_accouch <- choices_form$name[choices_form$list_name == "accouch"]

hh_ind$sante_1_accouch <- change_select_one_value(question.name = "sante_1_accouch", 
                                                  other.q.name = "sante_1_accouch_autre", 
                                                  old.value.name  = "sante_1_accouch_autre", 
                                                  new.value.name = "sante_1_accouch_autre_recoding",
                                                  toadd.value = list_to_add_accouch,
                                                  data = hh_ind,
                                                  codes.df = hh_ind_cleaned_value)$sante_1_accouch

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_1_accouch_autre",
                                                          new.value.name = "sante_1_accouch_autre_recoding")

### Accouche maison

list_to_add_accouch_maison <- choices_form$name[choices_form$list_name == "accouch_maison"]

hh_ind$sante_1_accouch_maison <- change_select_one_value(question.name = "sante_1_accouch_maison", 
                                                  other.q.name = "sante_1_accouch_maison_autre", 
                                                  old.value.name  = "sante_1_accouch_maison_autre", 
                                                  new.value.name = "sante_1_accouch_maison_autre_recoding",
                                                  toadd.value = list_to_add_accouch_maison,
                                                  data = hh_ind,
                                                  codes.df = hh_ind_cleaned_value)$sante_1_accouch_maison

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_1_accouch_maison_autre",
                                                          new.value.name = "sante_1_accouch_maison_autre_recoding")
### Soin recu

list_to_add_soin_recu <- choices_form$name[choices_form$list_name == "soin_recu"]

hh_ind$sante_2_soin_recu <- change_select_one_value(question.name = "sante_2_soin_recu", 
                                                  other.q.name = "sante_2_soin_recu_autre", 
                                                  old.value.name  = "sante_2_soin_recu_autre", 
                                                  new.value.name = "sante_2_soin_recu_autre_recoding",
                                                  toadd.value = list_to_add_soin_recu,
                                                  data = hh_ind,
                                                  codes.df = hh_ind_cleaned_value)$sante_2_soin_recu

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_2_soin_recu_autre",
                                                          new.value.name = "sante_2_soin_recu_autre_recoding")

### Soin non-recu

list_to_add_soin_non_recu <- choices_form$name[choices_form$list_name == "soin_non_recu"]

hh_ind$sante_3_soin_non_recu <- change_select_one_value(question.name = "sante_3_soin_non_recu", 
                                                    other.q.name = "sante_3_soin_non_recu_autre", 
                                                    old.value.name  = "sante_3_soin_non_recu_autre", 
                                                    new.value.name = "sante_3_soin_non_recu_autre_recoding",
                                                    toadd.value = list_to_add_soin_non_recu,
                                                    data = hh_ind,
                                                    codes.df = hh_ind_cleaned_value)$sante_3_soin_non_recu

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_3_soin_non_recu_autre",
                                                          new.value.name = "sante_3_soin_non_recu_autre_recoding")

### Educ handi_acces

list_to_add_handi_acces <- choices_form$name[choices_form$list_name == "educ_enfant_handi"]

hh_ind$educ_4_handi_acces <- change_select_one_value(question.name = "educ_4_handi_acces", 
                                                    other.q.name = "educ_4_handi_acces_autre", 
                                                    old.value.name  = "educ_4_handi_acces_autre", 
                                                    new.value.name = "educ_4_handi_acces_autre_recoding",
                                                    toadd.value = list_to_add_handi_acces,
                                                    data = hh_ind,
                                                    codes.df = hh_ind_cleaned_value)$educ_4_handi_acces

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "educ_4_handi_acces_autre",
                                                          new.value.name = "educ_4_handi_acces_autre_recoding")



add_toselectmutlipe <- function(col_sm, col_sm_autre, other.choice.name, data){
   split_name <- gsub(".*?\\.","", names(data[col_sm]))
   data[[col_sm_autre]] <- trimws(data[[col_sm_autre]])
   in_col_sm_autre <- as.numeric(grepl(split_name, data[[col_sm_autre]], ignore.case=T))
   col_sm_value <- data[[col_sm]]
   value <- rowSums(cbind(in_col_sm_autre, col_sm_value))
   if(split_name == other.choice.name){
     value <- if_else(value >1,1,0)
     }else{
     value <- if_else(value >=1,1,0)
     }
   return(value)
}

### Malades <5 ans
change_select_multiple_value <- function(question.name, other.q.name, old.value.name, new.value.name, other.choice.name, toadd.value, data, codes.df){
  variable.coded <- codes.df%>%
    select(starts_with(other.q.name))%>%
    distinct()%>%
    filter(UQ(sym(other.q.name))!="")
  
  old.value <- variable.coded[[old.value.name]]
  new.value <- variable.coded[[new.value.name]]
  other.q <- data[[other.q.name]] 
  question.v <- data[[question.name]]
  data[[other.q.name]] <- plyr::mapvalues(other.q, old.value, new.value)
  list_sm_cols <- names(data%>%select(starts_with(paste0(question.name, "."))))
  cols_toadd <- paste0("sante_4_0_4_malades.", toadd.value)
  not_incols <- cols_toadd[!cols_toadd %in% list_sm_cols]
  data[,not_incols] <- NA
  list_sm_cols <- names(data%>%select(starts_with(paste0(question.name, "."))))
  
  data[,list_sm_cols] <- sapply(list_sm_cols, add_toselectmutlipe, col_sm_autre = other.q.name, other.choice.name = other.choice.name, data = data)

  return(data)
}


list_to_add_sante_4_0_4_malades <- choices_form$name[choices_form$list_name == "maladies_0_4"]

hh_ind <- change_select_multiple_value(question.name = "sante_4_0_4_malades", 
                                                  other.q.name = "sante_4_0_4_malades_autre", 
                                                  old.value.name  = "sante_4_0_4_malades_autre", 
                                                  new.value.name = "sante_4_0_4_malades_autre_recoding",
                                                  toadd.value = list_to_add_sante_4_0_4_malades,
                                                  data = hh_ind,
                                                  other.choice.name = "autre",
                                                  codes.df = hh_ind_cleaned_value)

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_4_0_4_malades_autre",
                                                          new.value.name = "sante_4_0_4_malades_autre_recoding")


###  sante_5_5plus_malades

list_to_add_sante_5_5plus_malades <- choices_form$name[choices_form$list_name == "maladies"]

hh_ind <- change_select_multiple_value(question.name = "sante_5_5plus_malades", 
                                                  other.q.name = "sante_5_5plus_malades_autre", 
                                                  old.value.name  = "sante_5_5plus_malades_autre", 
                                                  new.value.name = "sante_5_5plus_malades_autre_recoding",
                                                  toadd.value = maladies,
                                                  data = hh_ind,
                                                  other.choice.name = "autre",
                                  
                                                  codes.df = hh_ind_cleaned_value)

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "sante_5_5plus_malades_autre",
                                                          new.value.name = "sante_5_5plus_malades_autre_recoding")


###  protect_10 

list_to_add_protect_10  <- choices_form$name[choices_form$list_name == "type_activite_4_18"]

hh_ind <- change_select_multiple_value(question.name = "protect_10", 
                                  other.q.name = "protect_10_autre", 
                                  old.value.name  = "protect_10_autre", 
                                  new.value.name = "protect_10_autre_recoding",
                                  toadd.value = list_to_add_protect_10,
                                  other.choice.name = "autre",
                                  data = hh_ind,
                                  codes.df = hh_ind_cleaned_value)
    

DataCleaningLogBook_hh_ind <- change_select_one_value_log(DataCleaningLogBook_hh_ind, hh_ind, hh_ind_cleaned_value, 
                                                          other.q.name = "protect_10_autre",
                                                          new.value.name = "protect_10_autre_recoding")

write.csv(DataCleaningLogBook_hh_ind, paste0("./output/DataCleaningLogbook_hh_ind-",format(Sys.time(), "%Y%m%d"), ".csv"))

  ### Create 2 different final files including the loop:
#browseVignettes("koboloops")
## check loop that might be not correct :
# loops must have a member with age of respondent and start with the youngest child (for maternity info) (create var in raw_data file with 1 if wrong)
for (i in 1: nrow(raw_data)){
  raw_data$loop_Without_Resp[i] =  ifelse(!is.na(raw_data$ig_1_age)[i] & 
                                            is_in(raw_data$ig_1_age[i], subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$age_hh), 
                                          0,1)
  
  raw_data$loop_wihtout_RightOrderHHMembers[i] = ifelse(!is.na(raw_data$ig_1_age)[i] & 
                                                          any(subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$age_hh[1] <= subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$age_hh[-1]),
                                                        0,1)
  
  raw_data$loop_wihtout_RightOrderHHMembers_and_below5members[i] = ifelse(raw_data$loop_wihtout_RightOrderHHMembers[i] == 1 &
                                                                            any(subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$age_hh < 5),
                                                                          1,0)     
  
  raw_data$loop_wihtout_RightOrderHHMembers_and_NoInfoForMat[i] = ifelse(raw_data$loop_wihtout_RightOrderHHMembers[i] == 1 &
                                                                           any(subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$age_hh < 5) & 
                                                                           subset(hh_ind, hh_ind$X_parent_index == raw_data$X_index[i])$sante_1_accouch[1] == "",
                                                                         1,0) 
  
}



# Create variable to have '% of HH with at least one vulnerable person' : 
hh_ind$ig_7_gr_vulnerable.aucune = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.aucune %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.aucune %in% 1 |hh_ind$ig_7_gr_vulnerable_hommes_adultes.aucune %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.aucune %in% 1 ,1,0)
hh_ind$ig_7_gr_vulnerable.FE = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.FE %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.FE %in% 1,1,0)
hh_ind$ig_7_gr_vulnerable.FA = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.FA %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.FA %in% 1,1,0)
hh_ind$ig_7_gr_vulnerable.handi_phy = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.handi_phy %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.handi_phy %in% 1 |
                                               hh_ind$ig_7_gr_vulnerable_hommes_adultes.handi_phy %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.handi_phy %in% 1 ,1,0)
hh_ind$ig_7_gr_vulnerable.handi_ment = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.handi_ment %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.handi_ment %in% 1 |
                                                hh_ind$ig_7_gr_vulnerable_hommes_adultes.handi_ment %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.handi_ment %in% 1 ,1,0)
hh_ind$ig_7_gr_vulnerable.handi = ifelse(hh_ind$ig_7_gr_vulnerable.handi_phy == 1, 1, 
                                         ifelse(hh_ind$ig_7_gr_vulnerable.handi_ment  == 1, 1, 0))
hh_ind$ig_7_gr_vulnerable.nsp = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.nsp %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.nsp %in% 1 |
                                         hh_ind$ig_7_gr_vulnerable_hommes_adultes.nsp %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.nsp %in% 1 ,1,0)
hh_ind$ig_7_gr_vulnerable.ES = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_enfants.ES %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.ES %in% 1,1,0)
hh_ind$ig_7_gr_vulnerable.ENA = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_enfants.ENA %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.ENA %in% 1,1,0)
hh_ind$ig_7_gr_vulnerable.nsp = ifelse(hh_ind$ig_7_gr_vulnerable_femmes_adultes.nsp %in% 1 | hh_ind$ig_7_gr_vulnerable_femmes_enfants.nsp %in% 1 |
                                         hh_ind$ig_7_gr_vulnerable_hommes_adultes.nsp %in% 1 | hh_ind$ig_7_gr_vulnerable_hommes_enfants.nsp %in% 1 ,1,0)


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = raw_data, aggregate.function = sum, 
                                       variable.to.add = c(sum_ig_7_gr_vulnerable.aucune = "ig_7_gr_vulnerable.aucune",
                                                           sum_ig_7_gr_vulnerable.FE = "ig_7_gr_vulnerable.FE",
                                                           sum_ig_7_gr_vulnerable.FA = "ig_7_gr_vulnerable.FA",
                                                           sum_ig_7_gr_vulnerable.handi_phy = "ig_7_gr_vulnerable.handi_phy",
                                                           sum_ig_7_gr_vulnerable.handi_ment = "ig_7_gr_vulnerable.handi_ment",
                                                           sum_ig_7_gr_vulnerable.handi = "ig_7_gr_vulnerable.handi",
                                                           sum_ig_7_gr_vulnerable.ES = "ig_7_gr_vulnerable.ES",
                                                           sum_ig_7_gr_vulnerable.ENA = "ig_7_gr_vulnerable.ENA",
                                                           sum_ig_7_gr_vulnerable.nsp = "ig_7_gr_vulnerable.nsp"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")



# Create variable for access to maternity : 
#"% des m?nages n'ayant pas eu acc?s ? un service de maternit? lors du dernier accouchement (max 5 ans), par raison"
# % des m?nages n'ayant pas eu acc?s ? un service de maternit? lors du dernier accouchement (max 5 ans).
hh_ind$sante_1_cs = ifelse(hh_ind$sante_1_accouch =="cs",1,0)
hh_ind$sante_1_maison = ifelse(hh_ind$sante_1_accouch =="maison",1,0)
hh_ind$sante_1_maison_nonassiste = ifelse(hh_ind$sante_1_accouch =="maison" & hh_ind$sante_1_accouch_maison != "accouch_assiste",1,0)
hh_ind$sante_1_maison_assiste = ifelse(hh_ind$sante_1_accouch =="maison" & hh_ind$sante_1_accouch_maison == "accouch_assiste",1,0)
hh_ind$sante_1_autre = ifelse(hh_ind$sante_1_accouch =="autre",1,0)
hh_ind$sante_1_nsp = ifelse(hh_ind$sante_1_accouch =="nsp",1,0)


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_1_accouch_cs = "sante_1_cs",
                                                           sum_sante_1_accouch_maison = "sante_1_maison",
                                                           sum_sante_1_accouch_maison_assiste = "sante_1_maison_assiste",
                                                           sum_sante_1_accouch_maison_nonassiste = "sante_1_maison_nonassiste",
                                                           sum_sante_1_accouch_autre = "sante_1_autre",
                                                           sum_sante_1_accouch_nsp = "sante_1_nsp"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")



for (i in 1: nrow(main_withloop)){
  main_withloop$sante_1_accouch_autre[i] = subset(hh_ind, hh_ind$X_parent_index == main_withloop$X_index[i])$sante_1_accouch_autre[1]
  main_withloop$sante_1_accouch_maison_raison[i] = subset(hh_ind, hh_ind$X_parent_index == main_withloop$X_index[i])$sante_1_accouch_maison[1]
  main_withloop$sante_1_accouch_maison_raison_autre[i] = subset(hh_ind, hh_ind$X_parent_index == main_withloop$X_index[i])$sante_1_accouch_maison_autre[1]
}


# Create variable for at least one member having been ill: 
# '% des m?nages ayant eu au moins un membre malade au cours de 30 derniers jours, par type de soin re?u'
hh_ind$sante_2_malade_oui = ifelse(hh_ind$sante_2_malade =="oui",1,0)
hh_ind$sante_2_malade_non = ifelse(hh_ind$sante_2_malade =="non",1,0)
hh_ind$sante_2_malade_nsp = ifelse(hh_ind$sante_2_malade =="nsp",1,0)

hh_ind$sante_2_soin_recu_oui_autre = ifelse(hh_ind$sante_2_soin_recu =="autre",1,0)
hh_ind$sante_2_soin_recu_oui_autre_autre = hh_ind$sante_2_soin_recu_autre
hh_ind$sante_2_soin_recu_oui_cs = ifelse(hh_ind$sante_2_soin_recu =="cs",1,0)
hh_ind$sante_2_soin_recu_oui_maison = ifelse(hh_ind$sante_2_soin_recu =="maison",1,0)
hh_ind$sante_2_soin_recu_non = ifelse(hh_ind$sante_2_soin_recu =="non",1,0)




main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_2_malade_oui = "sante_2_malade_oui",
                                                           sum_sante_2_malade_non = "sante_2_malade_non",
                                                           sum_sante_2_malade_nsp = "sante_2_malade_nsp",
                                                           sum_sante_2_soin_recu_oui_autre = "sante_2_soin_recu_oui_autre",
                                                           sum_sante_2_soin_recu_oui_cs = "sante_2_soin_recu_oui_cs",
                                                           sum_sante_2_soin_recu_oui_maison = "sante_2_soin_recu_oui_maison",
                                                           sum_sante_2_soin_recu_non = "sante_2_soin_recu_non"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

# Create variable for reason not receiving healthcare : 
#"% des m?nages n'ayant pas acc?s au service de sant?, par raison"
hh_ind$sante_3_soin_non_recu_non_autre = ifelse(hh_ind$sante_3_soin_non_recu =="autre",1,0)
hh_ind$sante_3_soin_non_recu_infra_detruite_ca = ifelse(hh_ind$sante_3_soin_non_recu =="infra_detruite_ca",1,0)
hh_ind$sante_3_soin_non_recu_infra_detruite_nat = ifelse(hh_ind$sante_3_soin_non_recu =="infra_detruite_nat",1,0)
hh_ind$sante_3_soin_non_recu_infra_inexis = ifelse(hh_ind$sante_3_soin_non_recu =="infra_inexis",1,0)
hh_ind$sante_3_soin_non_recu_infra_trop_loin = ifelse(hh_ind$sante_3_soin_non_recu =="infra_trop_loin",1,0)
hh_ind$sante_3_soin_non_recu_insecurite = ifelse(hh_ind$sante_3_soin_non_recu =="insecurite",1,0)
hh_ind$sante_3_soin_non_recu_medic_indisp = ifelse(hh_ind$sante_3_soin_non_recu =="medic_indisp",1,0)
hh_ind$sante_3_soin_non_recu_nsp = ifelse(hh_ind$sante_3_soin_non_recu =="nsp",1,0)
hh_ind$sante_3_soin_non_recu_qualite_trop_faible = ifelse(hh_ind$sante_3_soin_non_recu =="qualite_trop_faible",1,0)
hh_ind$sante_3_soin_non_recu_staff_indisp = ifelse(hh_ind$sante_3_soin_non_recu =="staff_indisp",1,0)
hh_ind$sante_3_soin_non_recu_trop_cher = ifelse(hh_ind$sante_3_soin_non_recu =="trop_cher",1,0)

main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_3_soin_non_recu_non_autre = "sante_3_soin_non_recu_non_autre",
                                                           sum_sante_3_soin_non_recu_infra_detruite_ca = "sante_3_soin_non_recu_infra_detruite_ca",
                                                           sum_sante_3_soin_non_recu_infra_detruite_nat = "sante_3_soin_non_recu_infra_detruite_nat",
                                                           sum_sante_3_soin_non_recu_infra_inexis = "sante_3_soin_non_recu_infra_inexis",
                                                           sum_sante_3_soin_non_recu_infra_trop_loin = "sante_3_soin_non_recu_infra_trop_loin",
                                                           sum_sante_3_soin_non_recu_insecurite = "sante_3_soin_non_recu_insecurite",
                                                           sum_sante_3_soin_non_recu_medic_indisp = "sante_3_soin_non_recu_medic_indisp",
                                                           sum_sante_3_soin_non_recu_nsp = "sante_3_soin_non_recu_nsp",
                                                           sum_sante_3_soin_non_recu_qualite_trop_faible = "sante_3_soin_non_recu_qualite_trop_faible",
                                                           sum_sante_3_soin_non_recu_staff_indisp = "sante_3_soin_non_recu_staff_indisp",
                                                           sum_sante_3_soin_non_recu_trop_cher = "sante_3_soin_non_recu_trop_cher"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")


# Create var for type of illness
#"% de m?nages dont au moins un enfant de moins de 5 ans a pr?sent? une fi?vre, une diarrh?e et / ou une toux au cours de 30 derniers jours (par genre)"
hh_ind$sante_4_0_4_malades_oui_autre_filles = ifelse(hh_ind$sante_4_0_4_malades.autre %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_autre_garcons = ifelse(hh_ind$sante_4_0_4_malades.autre %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_4_0_4_malades_oui_diarrhee_filles = ifelse(hh_ind$sante_4_0_4_malades.diarrhee %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_diarrhee_garcons = ifelse(hh_ind$sante_4_0_4_malades.diarrhee %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_4_0_4_malades_oui_toux_filles = ifelse(hh_ind$sante_4_0_4_malades.toux %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_toux_garcons = ifelse(hh_ind$sante_4_0_4_malades.toux %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_4_0_4_malades_oui_fievre_filles = ifelse(hh_ind$sante_4_0_4_malades.fievre %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_fievre_garcons = ifelse(hh_ind$sante_4_0_4_malades.fievre %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_4_0_4_malades_oui_nsp_filles = ifelse(hh_ind$sante_4_0_4_malades.nsp %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_nsp_garcons = ifelse(hh_ind$sante_4_0_4_malades.nsp %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_4_0_4_malades_oui_palu_filles = ifelse(hh_ind$sante_4_0_4_malades.palu %in% 1 & hh_ind$sexe_hh %in% "femme",1,0)
hh_ind$sante_4_0_4_malades_oui_palu_garcons = ifelse(hh_ind$sante_4_0_4_malades.palu %in% 1 & hh_ind$sexe_hh %in% "homme",1,0)


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_4_0_4_malades_autre_filles = "sante_4_0_4_malades_oui_autre_filles",
                                                           sum_sante_4_0_4_malades_autre_garcons = "sante_4_0_4_malades_oui_autre_garcons",
                                                           sum_sante_4_0_4_malades_diarrhee_filles = "sante_4_0_4_malades_oui_diarrhee_filles",
                                                           sum_sante_4_0_4_malades_diarrhee_garcons = "sante_4_0_4_malades_oui_diarrhee_garcons",
                                                           sum_sante_4_0_4_malades_toux_filles = "sante_4_0_4_malades_oui_toux_filles",
                                                           sum_sante_4_0_4_malades_toux_garcons = "sante_4_0_4_malades_oui_toux_garcons",
                                                           sum_sante_4_0_4_malades_fievre_filles = "sante_4_0_4_malades_oui_fievre_filles", 
                                                           sum_sante_4_0_4_malades_fievre_garcons = "sante_4_0_4_malades_oui_fievre_garcons", 
                                                           sum_sante_4_0_4_malades_oui_nsp_filles = "sante_4_0_4_malades_oui_nsp_filles",
                                                           sum_sante_4_0_4_malades_oui_nsp_garcons = "sante_4_0_4_malades_oui_nsp_garcons"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")


#"% de m?nages dont les adultes (18 ans ou plus) ont souffert d'une maladie au cours des 30 derniers jours (par genre et 5-17 ans vs 18+) "
hh_ind$sante_5_5plus_malades.palu_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.palu %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.palu_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.palu %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.palu_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.palu %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.palu_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.palu %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.infec_resp_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.infec_resp %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.infec_resp_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.infec_resp %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.infec_resp_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.infec_resp %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.infec_resp_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.infec_resp %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.diarrhee_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.diarrhee %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.diarrhee_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.diarrhee %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.diarrhee_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.diarrhee %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.diarrhee_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.diarrhee %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.rougeole_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.rougeole %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.rougeole_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.rougeole %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.rougeole_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.rougeole %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.rougeole_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.rougeole %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.hepat_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.hepat %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.hepat_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.hepat %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.hepat_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.hepat %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.hepat_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.hepat %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.cholera_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.cholera %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.cholera_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.cholera %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.cholera_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.cholera %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.cholera_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.cholera %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.vih_sida_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.vih_sida %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.vih_sida_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.vih_sida %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.vih_sida_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.vih_sida %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.vih_sida_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.vih_sida %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.mening_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.mening %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.mening_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.mening %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.mening_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.mening %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.mening_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.mening %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.autre_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.autre %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.autre_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.autre %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.autre_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.autre %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.autre_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.autre %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.nsp_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.nsp %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.nsp_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.nsp %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.nsp_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.nsp %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.nsp_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.nsp %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

hh_ind$sante_5_5plus_malades.fievre_femmes_5_17 = ifelse(hh_ind$sante_5_5plus_malades.fievre %in% 1 & hh_ind$agegrp_0_17_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.fievre_femmes_18plus = ifelse(hh_ind$sante_5_5plus_malades.fievre %in% 1 & hh_ind$agegrp_18plus_femmes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.fievre_garcons_5_17 = ifelse(hh_ind$sante_5_5plus_malades.fievre %in% 1 & hh_ind$agegrp_0_17_hommes %in% 1 ,1,0)
hh_ind$sante_5_5plus_malades.fievre_garcons_18plus = ifelse(hh_ind$sante_5_5plus_malades.fievre %in% 1 & hh_ind$agegrp_18plus_hommes %in% 1 ,1,0)

main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_5_5plus_malades.palu_femmes_5_17 = "sante_5_5plus_malades.palu_femmes_5_17",
                                                           sum_sante_5_5plus_malades.palu_femmes_18plus = "sante_5_5plus_malades.palu_femmes_18plus",
                                                           sum_sante_5_5plus_malades.palu_garcons_5_17 = "sante_5_5plus_malades.palu_garcons_5_17",
                                                           sum_sante_5_5plus_malades.palu_garcons_18plus = "sante_5_5plus_malades.palu_garcons_18plus",
                                                           sum_sante_5_5plus_malades.infec_resp_femmes_5_17 = "sante_5_5plus_malades.infec_resp_femmes_5_17",
                                                           sum_sante_5_5plus_malades.infec_resp_femmes_18plus = "sante_5_5plus_malades.infec_resp_femmes_18plus",
                                                           sum_sante_5_5plus_malades.infec_resp_garcons_5_17 = "sante_5_5plus_malades.infec_resp_garcons_5_17",
                                                           sum_sante_5_5plus_malades.infec_resp_garcons_18plus = "sante_5_5plus_malades.infec_resp_garcons_18plus",
                                                           sum_sante_5_5plus_malades.diarrhee_femmes_5_17 = "sante_5_5plus_malades.diarrhee_femmes_5_17",
                                                           sum_sante_5_5plus_malades.diarrhee_femmes_18plus = "sante_5_5plus_malades.diarrhee_femmes_18plus",
                                                           sum_sante_5_5plus_malades.diarrhee_garcons_5_17 = "sante_5_5plus_malades.diarrhee_garcons_5_17",
                                                           sum_sante_5_5plus_malades.diarrhee_garcons_18plus = "sante_5_5plus_malades.diarrhee_garcons_18plus",
                                                           sum_sante_5_5plus_malades.rougeole_femmes_5_17 = "sante_5_5plus_malades.rougeole_femmes_5_17",
                                                           sum_sante_5_5plus_malades.rougeole_femmes_18plus = "sante_5_5plus_malades.rougeole_femmes_18plus",
                                                           sum_sante_5_5plus_malades.rougeole_garcons_5_17 = "sante_5_5plus_malades.rougeole_garcons_5_17",
                                                           sum_sante_5_5plus_malades.rougeole_garcons_18plus = "sante_5_5plus_malades.rougeole_garcons_18plus",
                                                           sum_sante_5_5plus_malades.hepat_femmes_5_17 = "sante_5_5plus_malades.hepat_femmes_5_17",
                                                           sum_sante_5_5plus_malades.hepat_femmes_18plus = "sante_5_5plus_malades.hepat_femmes_18plus",
                                                           sum_sante_5_5plus_malades.hepat_garcons_5_17 = "sante_5_5plus_malades.hepat_garcons_5_17",
                                                           sum_sante_5_5plus_malades.hepat_garcons_18plus = "sante_5_5plus_malades.hepat_garcons_18plus",
                                                           sum_sante_5_5plus_malades.cholera_femmes_5_17 = "sante_5_5plus_malades.cholera_femmes_5_17",
                                                           sum_sante_5_5plus_malades.cholera_femmes_18plus = "sante_5_5plus_malades.cholera_femmes_18plus",
                                                           sum_sante_5_5plus_malades.cholera_garcons_5_17 = "sante_5_5plus_malades.cholera_garcons_5_17",
                                                           sum_sante_5_5plus_malades.cholera_garcons_18plus = "sante_5_5plus_malades.cholera_garcons_18plus",
                                                           sum_sante_5_5plus_malades.vih_sida_femmes_5_17 = "sante_5_5plus_malades.vih_sida_femmes_5_17",
                                                           sum_sante_5_5plus_malades.vih_sida_femmes_18plus = "sante_5_5plus_malades.vih_sida_femmes_18plus",
                                                           sum_sante_5_5plus_malades.vih_sida_garcons_5_17 = "sante_5_5plus_malades.vih_sida_garcons_5_17",
                                                           sum_sante_5_5plus_malades.vih_sida_garcons_18plus = "sante_5_5plus_malades.vih_sida_garcons_18plus",
                                                           sum_sante_5_5plus_malades.mening_femmes_5_17 = "sante_5_5plus_malades.mening_femmes_5_17",
                                                           sum_sante_5_5plus_malades.mening_femmes_18plus = "sante_5_5plus_malades.mening_femmes_18plus",
                                                           sum_sante_5_5plus_malades.mening_garcons_5_17 = "sante_5_5plus_malades.mening_garcons_5_17",
                                                           sum_sante_5_5plus_malades.mening_garcons_18plus = "sante_5_5plus_malades.mening_garcons_18plus",
                                                           sum_sante_5_5plus_malades.autre_femmes_5_17 = "sante_5_5plus_malades.autre_femmes_5_17",
                                                           sum_sante_5_5plus_malades.autre_femmes_18plus = "sante_5_5plus_malades.autre_femmes_18plus",
                                                           sum_sante_5_5plus_malades.autre_garcons_5_17 = "sante_5_5plus_malades.autre_garcons_5_17",
                                                           sum_sante_5_5plus_malades.autre_garcons_18plus = "sante_5_5plus_malades.autre_garcons_18plus",
                                                           sum_sante_5_5plus_malades.nsp_femmes_5_17 = "sante_5_5plus_malades.nsp_femmes_5_17",
                                                           sum_sante_5_5plus_malades.nsp_femmes_18plus = "sante_5_5plus_malades.nsp_femmes_18plus",
                                                           sum_sante_5_5plus_malades.nsp_garcons_5_17 = "sante_5_5plus_malades.nsp_garcons_5_17",
                                                           sum_sante_5_5plus_malades.nsp_garcons_18plus = "sante_5_5plus_malades.nsp_garcons_18plus",
                                                           sum_sante_5_5plus_malades.fievre_femmes_5_17 = "sante_5_5plus_malades.fievre_femmes_5_17",
                                                           sum_sante_5_5plus_malades.fievre_femmes_18plus = "sante_5_5plus_malades.fievre_femmes_18plus",
                                                           sum_sante_5_5plus_malades.fievre_garcons_5_17 = "sante_5_5plus_malades.fievre_garcons_5_17",
                                                           sum_sante_5_5plus_malades.fievre_garcons_18plus = "sante_5_5plus_malades.fievre_garcons_18plus",
                                                           sum_agegrp_0_4 = "agegrp_0_4"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")


# maladies_ques <- c("sum_sante_4_0_4_malades_autre_filles","sum_sante_4_0_4_malades_autre_garcons",
#                    "sum_sante_4_0_4_malades_diarrhee_filles","sum_sante_4_0_4_malades_diarrhee_garcons",
#                    "sum_sante_4_0_4_malades_toux_filles","sum_sante_4_0_4_malades_toux_garcons",
#                    "sum_sante_4_0_4_malades_fievre_filles","sum_sante_4_0_4_malades_fievre_garcons",
#                    "sum_sante_4_0_4_malades_oui_nsp_filles","sum_sante_4_0_4_malades_oui_nsp_garcons",
#                    "sum_sante_5_5plus_malades.palu_femmes_5_17","sum_sante_5_5plus_malades.palu_femmes_18plus",
#                    "sum_sante_5_5plus_malades.palu_garcons_5_17","sum_sante_5_5plus_malades.palu_garcons_18plus",
#                    "sum_sante_5_5plus_malades.infec_resp_femmes_5_17","sum_sante_5_5plus_malades.infec_resp_femmes_18plus",
#                    "sum_sante_5_5plus_malades.infec_resp_garcons_5_17","sum_sante_5_5plus_malades.infec_resp_garcons_18plus",
#                    "sum_sante_5_5plus_malades.diarrhee_femmes_5_17","sum_sante_5_5plus_malades.diarrhee_femmes_18plus","sum_sante_5_5plus_malades.diarrhee_garcons_5_17",
#                    "sum_sante_5_5plus_malades.diarrhee_garcons_18plus","sum_sante_5_5plus_malades.rougeole_femmes_5_17",
#                    "sum_sante_5_5plus_malades.rougeole_femmes_18plus","sum_sante_5_5plus_malades.rougeole_garcons_5_17",
#                    "sum_sante_5_5plus_malades.rougeole_garcons_18plus","sum_sante_5_5plus_malades.hepat_femmes_5_17",
#                    "sum_sante_5_5plus_malades.hepat_femmes_18plus","sum_sante_5_5plus_malades.hepat_garcons_5_17",
#                    "sum_sante_5_5plus_malades.hepat_garcons_18plus","sum_sante_5_5plus_malades.cholera_femmes_5_17",
#                    "sum_sante_5_5plus_malades.cholera_femmes_18plus","sum_sante_5_5plus_malades.cholera_garcons_5_17",
#                    "sum_sante_5_5plus_malades.cholera_garcons_18plus","sum_sante_5_5plus_malades.vih_sida_femmes_5_17",
#                    "sum_sante_5_5plus_malades.vih_sida_femmes_18plus","sum_sante_5_5plus_malades.vih_sida_garcons_5_17",
#                    "sum_sante_5_5plus_malades.vih_sida_garcons_18plus","sum_sante_5_5plus_malades.mening_femmes_5_17",
#                    "sum_sante_5_5plus_malades.mening_femmes_18plus","sum_sante_5_5plus_malades.mening_garcons_5_17",
#                    "sum_sante_5_5plus_malades.mening_garcons_18plus","sum_sante_5_5plus_malades.autre_femmes_5_17",
#                    "sum_sante_5_5plus_malades.autre_femmes_18plus","sum_sante_5_5plus_malades.autre_garcons_5_17",
#                    "sum_sante_5_5plus_malades.autre_garcons_18plus","sum_sante_5_5plus_malades.nsp_femmes_5_17",
#                    "sum_sante_5_5plus_malades.nsp_femmes_18plus","sum_sante_5_5plus_malades.nsp_garcons_5_17",
#                    "sum_sante_5_5plus_malades.nsp_garcons_18plus","sum_sante_5_5plus_malades.fievre_femmes_5_17",
#                    "sum_sante_5_5plus_malades.fievre_femmes_18plus","sum_sante_5_5plus_malades.fievre_garcons_5_17",
#                    "sum_sante_5_5plus_malades.fievre_garcons_18plus"
# )
# 
# main_withloop <- main_withloop %>%
#   mutate_at(vars( maladies_ques),
#             ~case_when(
#               . >= 1 ~ "oui",
#               TRUE ~ "non"
#             ))


hh_ind$sante_2_malade_oui_0_5_garcons <- if_else(hh_ind$sante_2_malade_oui == 1 & hh_ind$agegrp_0_4 & hh_ind$sexe_hh %in% "homme",1,0)
hh_ind$sante_2_malade_oui_0_5_filles <- if_else(hh_ind$sante_2_malade_oui == 1 & hh_ind$agegrp_0_4 & hh_ind$sexe_hh %in% "femme",1,0)

main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_sante_2_malade_oui_0_5_filles = "sante_2_malade_oui_0_5_filles",
                                                           sum_sante_2_malade_oui_0_5_garcons = "sante_2_malade_oui_0_5_garcons"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")


# Create var for nut
#"% d'enfants de moins de 5 ans d?pist?s MAM / MAS"
nut_DB = select(hh_ind, c("age_hh", "age_hh_months", "agegrp_6m_4", "sexe_hh", "nut_1_enfant_present", "nut_2_muac", "nut_1_oedem", "X_parent_index"))
nut_DB = subset(nut_DB, nut_DB$agegrp_6m_4 == 1)

nut_DB_withmain <- add_parent_to_loop(loop = nut_DB, parent = main_withloop,
                                      variables.to.keep = c("admin_1", "admin_2", "admin_3", "X_index", "X_uuid"),
                                      uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

write.csv(nut_DB_withmain, "./output/nutrition_database_tobecleaned.csv")

hh_ind$nut_2_muac.MAS_fille_0_4 <- ifelse(hh_ind$nut_2_muac >0 & hh_ind$nut_2_muac <= 115 &  (hh_ind$age_hh >=0 & hh_ind$age_hh <5) & hh_ind$sexe_hh == "femme", 1, 0)
hh_ind$nut_2_muac.MAS_garcon_0_4 <- ifelse(hh_ind$nut_2_muac >0 & hh_ind$nut_2_muac <= 115 &  (hh_ind$age_hh >=0 & hh_ind$age_hh <5) & hh_ind$sexe_hh == "homme", 1, 0)
hh_ind$nut_2_muac.MAM_fille_0_4 <- ifelse(hh_ind$nut_2_muac >115 & hh_ind$nut_2_muac <= 125 &  (hh_ind$age_hh >=0 & hh_ind$age_hh <5) & hh_ind$sexe_hh == "femme", 1, 0)
hh_ind$nut_2_muac.MAM_garcon_0_4 <- ifelse(hh_ind$nut_2_muac >115 & hh_ind$nut_2_muac <= 125 &  (hh_ind$age_hh >=0 & hh_ind$age_hh <5) & hh_ind$sexe_hh == "homme", 1, 0)

main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_nut_2_muac.MASfille_0_4 = "nut_2_muac.MAS_fille_0_4",
                                                           sum_nut_2_muac.MASgarcon_0_4 = "nut_2_muac.MAS_garcon_0_4",
                                                           sum_nut_2_muac.MAMfille_0_4 = "nut_2_muac.MAM_fille_0_4",
                                                           sum_nut_2_muac.MAMgarcon_0_4 = "nut_2_muac.MAM_garcon_0_4"
                                       ),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")




# Create var for sant?
#"% des enfants de moins de 5 ans ayant souffert d'une maladie au cours des 30 derniers jours."
sante = select(hh_ind, c("age_hh", "age_hh_months", "sexe_hh", "sante_2_malade", "sante_4_0_4_malades.fievre", "sante_4_0_4_malades.diarrhee",
                         "sante_4_0_4_malades.toux", "sante_4_0_4_malades.autre", "sante_4_0_4_malades.nsp", "sante_4_0_4_malades_autre", 
                         "sante_2_soin_recu", "sante_4_0_4_malades", "X_parent_index"))
sante = subset(sante, sante$age_hh < 5)



sante_withmain <- add_parent_to_loop(loop = sante, parent = main_withloop,
                                     variables.to.keep = c("admin_1", "admin_2", "admin_3", "X_index", "X_uuid"),
                                     uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

write.csv(sante_withmain, "./output/sante_database_tobecleaned.csv")





# Create var for educ
#"Taux d'inscription scolaire, en comparaison avec l'ann?e pr?c?dente,  par tranche d'?ge et genre de l'enfant"
educ_DB = select(hh_ind, c("age_hh", "agegrp_4_18", "sexe_hh", "educ_2_inscrit_18_19", "educ_2_inscrit_18_19_level", "educ_2_inscrit_18_19_non", 
                           "educ_3_presence_18_19", "educ_3_presence_18_19_non","educ_4_handi_acces", "educ_4_handi_acces_autre", 
                           "X_parent_index"))

educ_DB = subset(educ_DB, educ_DB$agegrp_4_18 == 1)

educ_DB_withmain <- add_parent_to_loop(loop = educ_DB, parent = main_withloop,
                                       variables.to.keep = c("educ_5_ecole_acces_1",
                                                             "educ_5_ecole_acces_2",
                                                             "educ_5_ecole_acces_3", "educ_5_ecole_acces_autre",
                                                             "admin_1", "admin_2", "admin_3", "X_index", "X_uuid"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

write.csv(educ_DB_withmain, "./output/educ_database_tobecleaned.csv")

hh_ind$educ_2_inscrit_18_19.filles_7_12 <- ifelse(hh_ind$educ_2_inscrit_18_19 == "oui" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_2_inscrit_18_19.filles_13_18 <- ifelse(hh_ind$educ_2_inscrit_18_19 == "oui" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_2_inscrit_18_19.garcons_7_12 <- ifelse(hh_ind$educ_2_inscrit_18_19 == "oui" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_2_inscrit_18_19.garcons_13_18 <- ifelse(hh_ind$educ_2_inscrit_18_19 == "oui" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)

hh_ind$educ_3_presence_18_19.filles_7_12.12m <- ifelse(hh_ind$educ_3_presence_18_19 == "12M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_7_12.6m_12m <- ifelse(hh_ind$educ_3_presence_18_19 == "6M_12M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_7_12.3m_6m <- ifelse(hh_ind$educ_3_presence_18_19 == "3M_6M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_7_12.0m_3m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M_3M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_7_12.0m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "femme", 1,0)

hh_ind$educ_3_presence_18_19.garcons_7_12.12m <- ifelse(hh_ind$educ_3_presence_18_19 == "12M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_7_12.6m_12m <- ifelse(hh_ind$educ_3_presence_18_19 == "6M_12M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_7_12.3m_6m <- ifelse(hh_ind$educ_3_presence_18_19 == "3M_6M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_7_12.0m_3m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M_3M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_7_12.0m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M" & (hh_ind$age_hh >=7 & hh_ind$age_hh <=12) & hh_ind$sexe_hh == "homme", 1,0)

hh_ind$educ_3_presence_18_19.filles_13_18.12m <- ifelse(hh_ind$educ_3_presence_18_19 == "12M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_13_18.6m_12m <- ifelse(hh_ind$educ_3_presence_18_19 == "6M_12M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_13_18.3m_6m <- ifelse(hh_ind$educ_3_presence_18_19 == "3M_6M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_13_18.0m_3m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M_3M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_3_presence_18_19.filles_13_18.0m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "femme", 1,0)

hh_ind$educ_3_presence_18_19.garcons_13_18.12m <- ifelse(hh_ind$educ_3_presence_18_19 == "12M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_13_18.6m_12m <- ifelse(hh_ind$educ_3_presence_18_19 == "6M_12M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_13_18.3m_6m <- ifelse(hh_ind$educ_3_presence_18_19 == "3M_6M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_13_18.0m_3m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M_3M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_3_presence_18_19.garcons_13_18.0m <- ifelse(hh_ind$educ_3_presence_18_19 == "0M" & (hh_ind$age_hh >=13 & hh_ind$age_hh <=18) & hh_ind$sexe_hh == "homme", 1,0)

hh_ind$educ_4_handi_acces.descol_autre <- ifelse(hh_ind$educ_4_handi_acces == "descol_autre", 1,0)
hh_ind$educ_4_handi_acces.descol_acces <- ifelse(hh_ind$educ_4_handi_acces == "descol_acces", 1,0)
hh_ind$educ_4_handi_acces.descol_enseignement <- ifelse(hh_ind$educ_4_handi_acces == "descol_enseignement", 1,0)
hh_ind$educ_4_handi_acces.scol_non_opti <- ifelse(hh_ind$educ_4_handi_acces == "scol_non_opti", 1,0)
hh_ind$educ_4_handi_acces.scol_ok <- ifelse(hh_ind$educ_4_handi_acces == "scol_ok", 1,0)
hh_ind$educ_4_handi_acces.autre <- ifelse(hh_ind$educ_4_handi_acces == "autre", 1,0)

hh_ind$educ_4_handi_acces.descol_autre_filles <- ifelse(hh_ind$educ_4_handi_acces == "descol_autre" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_4_handi_acces.descol_acces_filles <- ifelse(hh_ind$educ_4_handi_acces == "descol_acces" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_4_handi_acces.descol_enseignement_filles <- ifelse(hh_ind$educ_4_handi_acces == "descol_enseignement" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_4_handi_acces.scol_non_opti_filles <- ifelse(hh_ind$educ_4_handi_acces == "scol_non_opti" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_4_handi_acces.scol_ok_filles <- ifelse(hh_ind$educ_4_handi_acces == "scol_ok" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$educ_4_handi_acces.autre_filles <- ifelse(hh_ind$educ_4_handi_acces == "autre"& hh_ind$sexe_hh == "femme", 1,0)

hh_ind$educ_4_handi_acces.descol_autre_garcons <- ifelse(hh_ind$educ_4_handi_acces == "descol_autre" & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_4_handi_acces.descol_acces_garcons <- ifelse(hh_ind$educ_4_handi_acces == "descol_acces"& hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_4_handi_acces.descol_enseignement_garcons <- ifelse(hh_ind$educ_4_handi_acces == "descol_enseignement" & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_4_handi_acces.scol_non_opti_garcons <- ifelse(hh_ind$educ_4_handi_acces == "scol_non_opti" & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_4_handi_acces.scol_ok_garcons <- ifelse(hh_ind$educ_4_handi_acces == "scol_ok" & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$educ_4_handi_acces.autre_garcons <- ifelse(hh_ind$educ_4_handi_acces == "autre" & hh_ind$sexe_hh == "homme", 1,0)

hh_ind$handi_4_18 = ifelse(hh_ind$agegrp_4_18 == 1 & (hh_ind$ig_7_gr_vulnerable_hommes_enfants.handi_ment == 1 |
                                                        hh_ind$ig_7_gr_vulnerable_femmes_adultes.handi_ment == 1 |
                                                        hh_ind$ig_7_gr_vulnerable_hommes_enfants.handi_phy == 1 |
                                                        hh_ind$ig_7_gr_vulnerable_femmes_enfants.handi_phy == 1), 1, 0)


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(
                                         sum_educ_2_inscrit_18_19.filles_7_12 = "educ_2_inscrit_18_19.filles_7_12",
                                         sum_educ_2_inscrit_18_19.filles_13_18 = "educ_2_inscrit_18_19.filles_13_18",
                                         sum_educ_2_inscrit_18_19.garcons_7_12 = "educ_2_inscrit_18_19.garcons_7_12",
                                         sum_educ_2_inscrit_18_19.garcons_13_18 = "educ_2_inscrit_18_19.garcons_13_18",
                                         sum_educ_3_presence_18_19.filles_7_12.12m = "educ_3_presence_18_19.filles_7_12.12m",
                                         sum_educ_3_presence_18_19.filles_7_12.6m_12m = "educ_3_presence_18_19.filles_7_12.6m_12m",
                                         sum_educ_3_presence_18_19.filles_7_12.3m_6m = "educ_3_presence_18_19.filles_7_12.3m_6m",
                                         sum_educ_3_presence_18_19.filles_7_12.0m_3m = "educ_3_presence_18_19.filles_7_12.0m_3m",
                                         sum_educ_3_presence_18_19.filles_7_12.0m = "educ_3_presence_18_19.filles_7_12.0m",
                                         
                                         sum_educ_3_presence_18_19.filles_13_18.12m = "educ_3_presence_18_19.filles_13_18.12m",
                                         sum_educ_3_presence_18_19.filles_13_18.6m_12m = "educ_3_presence_18_19.filles_13_18.6m_12m",
                                         sum_educ_3_presence_18_19.filles_13_18.3m_6m = "educ_3_presence_18_19.filles_13_18.3m_6m",
                                         sum_educ_3_presence_18_19.filles_13_18.0m_3m = "educ_3_presence_18_19.filles_13_18.0m_3m",
                                         sum_educ_3_presence_18_19.filles_13_18.0m = "educ_3_presence_18_19.filles_13_18.0m",
                                         
                                         sum_educ_3_presence_18_19.garcons_7_12.12m = "educ_3_presence_18_19.garcons_7_12.12m",
                                         sum_educ_3_presence_18_19.garcons_7_12.6m_12m = "educ_3_presence_18_19.garcons_7_12.6m_12m",
                                         sum_educ_3_presence_18_19.garcons_7_12.3m_6m = "educ_3_presence_18_19.garcons_7_12.3m_6m",
                                         sum_educ_3_presence_18_19.garcons_7_12.0m_3m = "educ_3_presence_18_19.garcons_7_12.0m_3m",
                                         sum_educ_3_presence_18_19.garcons_7_12.0m = "educ_3_presence_18_19.garcons_7_12.0m",
                                         
                                         sum_educ_3_presence_18_19.garcons_13_18.12m = "educ_3_presence_18_19.garcons_13_18.12m",
                                         sum_educ_3_presence_18_19.garcons_13_18.6m_12m = "educ_3_presence_18_19.garcons_13_18.6m_12m",
                                         sum_educ_3_presence_18_19.garcons_13_18.3m_6m = "educ_3_presence_18_19.garcons_13_18.3m_6m",
                                         sum_educ_3_presence_18_19.garcons_13_18.0m_3m = "educ_3_presence_18_19.garcons_13_18.0m_3m",
                                         sum_educ_3_presence_18_19.garcons_13_18.0m = "educ_3_presence_18_19.garcons_13_18.0m",
                                         
                                         sum_educ_4_handi_acces.descol_autre <- "educ_4_handi_acces.descol_autre",
                                         sum_educ_4_handi_acces.descol_acces <- "educ_4_handi_acces.descol_acces",
                                         sum_educ_4_handi_acces.descol_enseignement <- "educ_4_handi_acces.descol_enseignement",
                                         sum_educ_4_handi_acces.scol_non_opti <- "educ_4_handi_acces.scol_non_opti",
                                         sum_educ_4_handi_acces.scol_ok <- "educ_4_handi_acces.scol_ok",
                                         sum_educ_4_handi_acces.autre <- "educ_4_handi_acces.autre",
                                         
                                         sum_educ_4_handi_acces.descol_autre_filles <- "educ_4_handi_acces.descol_autre_filles",
                                         sum_educ_4_handi_acces.descol_acces_filles <- "educ_4_handi_acces.descol_acces_filles",
                                         sum_educ_4_handi_acces.descol_enseignement_filles <- "educ_4_handi_acces.descol_enseignement_filles",
                                         sum_educ_4_handi_acces.scol_non_opti_filles <- "educ_4_handi_acces.scol_non_opti_filles",
                                         sum_educ_4_handi_acces.scol_ok_filles <- "educ_4_handi_acces.scol_ok_filles",
                                         sum_educ_4_handi_acces.autre_filles <- "educ_4_handi_acces.autre_filles",
                                         
                                         sum_educ_4_handi_acces.descol_autre_garcons <- "educ_4_handi_acces.descol_autre_garcons",
                                         sum_educ_4_handi_acces.descol_acces_garcons <- "educ_4_handi_acces.descol_acces_garcons",
                                         sum_educ_4_handi_acces.descol_enseignement_garcons <- "educ_4_handi_acces.descol_enseignement_garcons",
                                         sum_educ_4_handi_acces.scol_non_opti_garcons <- "educ_4_handi_acces.scol_non_opti_garcons",
                                         sum_educ_4_handi_acces.scol_ok_garcons <- "educ_4_handi_acces.scol_ok_garcons",
                                         sum_educ_4_handi_acces.autre_garcons <- "educ_4_handi_acces.autre_garcons",
                                         
                                         sum_handi_4_18 = "handi_4_18"
                                       ),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

# Create var for child protect: 
#"% de m?nages avec des enfants de moins de 18 ans qui sont impliqu?s dans des types de travail"


hh_ind$protect_9_garcon <- ifelse(hh_ind$protect_9 == "oui" & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$protect_9_fille <- ifelse(hh_ind$protect_9 == "oui" & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$protect_9 <- ifelse(hh_ind$protect_9 == "oui", 1,0)



hh_ind[,c("protect_10.agric", "protect_10.peche", "protect_10.elevage","protect_10.carriere",
          "protect_10.petit_commerce", "protect_10.restauration","protect_10.artisanat", "protect_10.travaux_domestiques", 
          "protect_10.construction", "protect_10.transport", "protect_10.recrutes","protect_10.prostitution",
          "protect_10.autre","protect_10.nsp")] <-
  apply(hh_ind[,c("protect_10.agric", "protect_10.peche", "protect_10.elevage","protect_10.carriere",
                  "protect_10.petit_commerce", "protect_10.restauration","protect_10.artisanat", "protect_10.travaux_domestiques", 
                  "protect_10.construction", "protect_10.transport", "protect_10.recrutes","protect_10.prostitution",
                  "protect_10.autre","protect_10.nsp")], 2, function(x){replace(x, is.na(x), 0)})


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(sum_protect_10.agric = "protect_10.agric",
                                                           sum_protect_10.peche = "protect_10.peche",
                                                           sum_protect_10.elevage = "protect_10.elevage",
                                                           sum_protect_10.carriere = "protect_10.carriere",
                                                           sum_protect_10.petit_commerce = "protect_10.petit_commerce",
                                                           sum_protect_10.restauration = "protect_10.restauration",
                                                           sum_protect_10.artisanat = "protect_10.artisanat",
                                                           sum_protect_10.travaux_domestiques = "protect_10.travaux_domestiques",
                                                           sum_protect_10.construction = "protect_10.construction",
                                                           sum_protect_10.transport = "protect_10.transport",
                                                           sum_protect_10.recrutes = "protect_10.recrutes",
                                                           sum_protect_10.prostitution = "protect_10.prostitution",
                                                           sum_protect_10.autre = "protect_10.autre",
                                                           sum_protect_10.nsp = "protect_10.nsp",
                                                           sum_protect_9_garcon = "protect_9_garcon",
                                                           sum_protect_9_fille = "protect_9_fille",
                                                           sum_protect_9 = "protect_9"),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

hh_ind$agegrp_7_12_filles <- if_else(hh_ind$age_hh>=7 & hh_ind$age_hh<=12 & hh_ind$sexe_hh == "femme",1,0)
hh_ind$agegrp_7_12_garcons <- if_else(hh_ind$age_hh>=7 & hh_ind$age_hh<=12 & hh_ind$sexe_hh == "homme",1,0)
hh_ind$agegrp_13_18_filles <- if_else(hh_ind$age_hh>=13 & hh_ind$age_hh<=18 & hh_ind$sexe_hh == "femme",1,0)
hh_ind$agegrp_13_18_garcons <- if_else(hh_ind$age_hh>=13 & hh_ind$age_hh<=18 & hh_ind$sexe_hh == "homme",1,0)
hh_ind$agegrp_6m_4_filles <- if_else(hh_ind$agegrp_6m_4 == 1 & hh_ind$sexe_hh == "femme",1,0)
hh_ind$agegrp_6m_4_garcons <- if_else(hh_ind$agegrp_6m_4 == 1 & hh_ind$sexe_hh == "homme",1,0)

main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(
                                         sum_agegrp_7_12_filles = "agegrp_7_12_filles",
                                         sum_agegrp_7_12_garcons = "agegrp_7_12_garcons",
                                         sum_agegrp_13_18_filles = "agegrp_13_18_filles",
                                         sum_agegrp_13_18_garcons = "agegrp_13_18_garcons",
                                         sum_agegrp_6m_4_filles = "agegrp_6m_4_filles",
                                         sum_agegrp_6m_4_garcons = "agegrp_6m_4_garcons",
                                         sum_agegrp_6m_4 = "agegrp_6m_4"
                                       ),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

hh_ind$agegrp_0_17_femmes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=17 & hh_ind$sexe_hh == "femme",1,0) 
hh_ind$agegrp_0_18_femmes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=18 & hh_ind$sexe_hh == "femme",1,0) 
hh_ind$agegrp_18_59_femmes <- if_else(hh_ind$age_hh >=18 & hh_ind$age_hh <=59 & hh_ind$sexe_hh == "femme",1,0) 
hh_ind$agegrp_19_59_femmes <- if_else(hh_ind$age_hh >=19 & hh_ind$age_hh <=59 & hh_ind$sexe_hh == "femme",1,0) 
hh_ind$agegrp_59plus_femmes <- if_else(hh_ind$age_hh >=60 & hh_ind$age_hh <=1000 & hh_ind$sexe_hh == "femme",1,0) 

hh_ind$agegrp_0_17_hommes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=17 & hh_ind$sexe_hh == "homme",1,0) 
hh_ind$agegrp_0_18_hommes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=18 & hh_ind$sexe_hh == "homme",1,0) 
hh_ind$agegrp_18_59_hommes <- if_else(hh_ind$age_hh >=18 & hh_ind$age_hh <=59 & hh_ind$sexe_hh == "homme",1,0) 
hh_ind$agegrp_19_59_hommes <- if_else(hh_ind$age_hh >=19 & hh_ind$age_hh <=59 & hh_ind$sexe_hh == "homme",1,0) 
hh_ind$agegrp_59plus_hommes <- if_else(hh_ind$age_hh >=60 & hh_ind$age_hh <=1000 & hh_ind$sexe_hh == "homme",1,0) 

hh_ind$agegrp_0_17_tot <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=17 ,1,0) 
hh_ind$agegrp_0_18_tot <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <=18 ,1,0) 
hh_ind$agegrp_18_59_tot <- if_else(hh_ind$age_hh >=18 & hh_ind$age_hh <=59 ,1,0) 
hh_ind$agegrp_19_59_tot <- if_else(hh_ind$age_hh >=19 & hh_ind$age_hh <=59 ,1,0) 
hh_ind$agegrp_59plus_tot <- if_else(hh_ind$age_hh >=60 & hh_ind$age_hh <=1000,1,0) 

hh_ind$agegrp_5_17_femmes <- if_else(hh_ind$age_hh >=5 & hh_ind$age_hh <= 17 & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$agegrp_5_17_hommes <- if_else(hh_ind$age_hh >=5 & hh_ind$age_hh <= 17 & hh_ind$sexe_hh == "homme", 1,0)
hh_ind$agegrp_5_17 <- if_else(hh_ind$age_hh >=5 & hh_ind$age_hh <= 17, 1,0)

hh_ind$agegrp_0_4_femmes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <= 4 & hh_ind$sexe_hh == "femme", 1,0)
hh_ind$agegrp_0_4_hommes <- if_else(hh_ind$age_hh >=0 & hh_ind$age_hh <= 4 & hh_ind$sexe_hh == "homme", 1,0)


main_withloop <- affect_loop_to_parent(loop = hh_ind, parent = main_withloop, aggregate.function = sum , 
                                       variable.to.add = c(
                                         sum_agegrp_0_18_femmes = "agegrp_0_18_femmes",
                                         sum_agegrp_18_59_femmes = "agegrp_18_59_femmes",
                                         sum_agegrp_19_59_femmes = "agegrp_19_59_femmes",
                                         sum_agegrp_59plus_femmes = "agegrp_59plus_femmes",
                                         
                                         sum_agegrp_0_18_hommes = "agegrp_0_18_hommes",
                                         sum_agegrp_18_59_hommes = "agegrp_18_59_hommes",
                                         sum_agegrp_19_59_hommes = "agegrp_19_59_hommes",
                                         sum_agegrp_59plus_hommes = "agegrp_59plus_hommes",
                                         
                                         sum_agegrp_0_17_tot = "agegrp_0_17_tot",
                                         sum_agegrp_0_18_tot = "agegrp_0_18_tot",
                                         sum_agegrp_18_59_tot = "agegrp_18_59_tot",
                                         sum_agegrp_19_59_tot = "agegrp_19_59_tot",
                                         sum_agegrp_59plus_tot = "agegrp_59plus_tot",
                                         
                                         sum_agegrp_5_17_femmes = "agegrp_5_17_femmes",
                                         sum_agegrp_5_17_hommes = "agegrp_5_17_hommes",
                                         sum_agegrp_5_17 = "agegrp_5_17",
                                         
                                         sum_agegrp_0_4_femmes = "agegrp_0_4_femmes",
                                         sum_agegrp_0_4_hommes = "agegrp_0_4_hommes"
                                       ),
                                       uuid.name.loop = "X_parent_index", uuid.name.parent = "X_index")

write.csv(hh_ind, paste0("./output/REACH_CAR_dataset_Indiv_MSNA_", format(Sys.time(), "%Y%m%d"),".csv"))

write.csv(main_withloop, "./output/main_DS_withloop_tobecleaned.csv")
