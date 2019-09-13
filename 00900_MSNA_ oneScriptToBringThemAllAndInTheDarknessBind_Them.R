library(dplyr)

##### LOADING Agg Tables ####
table_raw_admin0 <- read.csv(paste0("./output/tables/RAW/summary_stats_admin0_RAW_20190912.csv"), stringsAsFactors = F)
table_raw_admin0_grp <- read.csv(paste0("./output/tables/RAW/summary_stats_admin0_grp_RAW_20190912.csv"), stringsAsFactors = F)
table_raw_admin0_sexHHD <- read.csv(paste0("./output/tables/RAW/summary_stats_admin0_sexHHD_RAW_20190912.csv"), stringsAsFactors = F)

table_raw_admin1 <- read.csv(paste0("./output/tables/RAW/summary_stats_admin1_RAW_20190912.csv"), stringsAsFactors = F)
table_raw_admin1_grp <- read.csv(paste0("./output/tables/RAW/summary_stats_admin1_grp_RAW_20190912.csv"), stringsAsFactors = F)

table_raw_admin2 <- read.csv(paste0("./output/tables/RAW/summary_stats_admin2_RAW_20190912.csv"), stringsAsFactors = F)

##### LOADING Frequency Tables ####

freq_admin0 <- read.csv(paste0("./output/freq/freq_admin0_.csv"), stringsAsFactors = F)
freq_admin0_grp <- read.csv(paste0("./output/freq/freq_admin0grp_.csv"), stringsAsFactors = F)
freq_admin0_sexHHD <- read.csv(paste0("./output/freq/freq_admin0_sexHHD_.csv"), stringsAsFactors = F)

freq_admin1 <- read.csv(paste0("./output/freq/freq_admin1_.csv"), stringsAsFactors = F)
freq_admin1_grp <- read.csv(paste0("./output/freq/freq_admin1grp_.csv"), stringsAsFactors = F)

freq_admin2 <- read.csv(paste0("./output/freq/freq_admin2_.csv"), stringsAsFactors = F)

##### LOADING Borda #####
borda_admin0 <- read.csv(paste0("./output/borda/borda_admin0.csv"), stringsAsFactors = F)
borda_admin0_grp <- read.csv(paste0("./output/borda/borda_admin0_grp.csv"), stringsAsFactors = F)
borda_admin0_sexHHD <- read.csv(paste0("./output/borda/borda_admin0_sexHHD.csv"), stringsAsFactors = F)

borda_admin1 <- read.csv(paste0("./output/borda/borda_admin1.csv"), stringsAsFactors = F)
borda_admin1_grp <- read.csv(paste0("./output/borda/borda_admin1_grp.csv"), stringsAsFactors = F)

borda_admin2 <- read.csv(paste0("./output/borda/borda_admin2.csv"), stringsAsFactors = F)

##### BIND admin0 agg tables ####

full_table_admin0 <- table_raw_admin0%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  rbind(table_raw_admin0_grp, table_raw_admin0_sexHHD)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

##### reshaping bordas admin0 #####

### admin0
borda_admin0 <- borda_admin0%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin0_1 <- borda_admin0%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="total", independent.var.value = "total", 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_1, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_2 <- borda_admin0%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="total", independent.var.value = "total", 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_3 <- borda_admin0%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="total", independent.var.value = "total", 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0 <- borda_admin0_1%>%
  bind_rows(borda_admin0_2, borda_admin0_3)


### admin0 grp
borda_admin0_grp_all <- borda_admin0_grp%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin0_grp_1 <- borda_admin0_grp_all%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_1, "%"))/100
         )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_grp_2 <- borda_admin0_grp_all%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_grp_3 <- borda_admin0_grp_all%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_grp <- borda_admin0_grp_1%>%
  bind_rows(borda_admin0_grp_2, borda_admin0_grp_3)

### admin0 sexHHD
borda_admin0_sexHHD <- borda_admin0_sexHHD%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin0_sexHHD_1 <- borda_admin0_sexHHD%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="sexe_chef_menage", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_1, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_sexHHD_2 <- borda_admin0_sexHHD%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="sexe_chef_menage", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_sexHHD_3 <- borda_admin0_sexHHD%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="sexe_chef_menage", independent.var.value = disaggregation, 
         repeat.var = repeat_var, repeat.var.value = "RCA", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin0_sexHHD <- borda_admin0_sexHHD_1%>%
  bind_rows(borda_admin0_sexHHD_2, borda_admin0_sexHHD_3)

##### ADMIN 0 Bind FREQ #####

freq_admin0 <- freq_admin0%>%
  mutate(dependent.var = variable, numbers = percent, independent.var = "total", independent.var.value = "total",
         repeat.var.value = "RCA", repeat.var = "admin_0", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

freq_admin0_grp <- freq_admin0_grp %>%
  mutate(dependent.var = variable, numbers = percent,
         independent.var ="ig_8_statut_groupe", independent.var.value = ig_8_statut_groupe,
         repeat.var.value = "RCA", repeat.var = "admin_0", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

freq_admin0_sexHHD <- freq_admin0_sexHHD %>%
  mutate(dependent.var = variable, numbers = percent,
         independent.var ="sexe_chef_menage", independent.var.value = sexe_chef_menage,
         repeat.var.value = "RCA", repeat.var = "admin_0", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

##### ADMIN 0 BIN ALL THE DATA FRAMES!!!! ####

final_admin0_alldataframes <- full_table_admin0 %>%
  bind_rows(borda_admin0, borda_admin0_grp, borda_admin0_sexHHD,
            freq_admin0, freq_admin0_grp, freq_admin0_sexHHD)

write.csv(final_admin0_alldataframes, paste0("./output/onetabletorullthemall/admin_0_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),".csv"))


##### BIND admin1 agg tables ####

full_table_admin1 <- table_raw_admin1%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  rbind(table_raw_admin1_grp)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

##### reshaping bordas admin1 #####

### admin1
borda_admin1 <- borda_admin1%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin1_1 <- borda_admin1%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_1, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1_2 <- borda_admin1%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1_3 <- borda_admin1%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1 <- borda_admin1_1%>%
  bind_rows(borda_admin1_2, borda_admin1_3)


### admin1 grp
borda_admin1_grp_all <- borda_admin1_grp%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin1_grp_1 <- borda_admin1_grp_all%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_1, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1_grp_2 <- borda_admin1_grp_all%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1_grp_3 <- borda_admin1_grp_all%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="ig_8_statut_groupe", independent.var.value = disaggregation, 
         repeat.var.value = repeat_var, repeat.var = "admin_1", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin1_grp <- borda_admin1_grp_1%>%
  bind_rows(borda_admin1_grp_2, borda_admin1_grp_3)


##### ADMIN 1 Bind FREQ #####

freq_admin1 <- freq_admin1%>%
  mutate(dependent.var = variable, numbers = percent, independent.var = "total", independent.var.value = "total",
         repeat.var.value = admin_1, repeat.var = "admin_1", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

freq_admin1_grp <- freq_admin1_grp %>%
  mutate(dependent.var = variable, numbers = percent,
         independent.var ="ig_8_statut_groupe", independent.var.value = ig_8_statut_groupe,
         repeat.var.value = admin_1, repeat.var = "admin_1", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)


##### ADMIN 1 BIN ALL THE DATA FRAMES!!!! ####

final_admin1_alldataframes <- full_table_admin1 %>%
  bind_rows(borda_admin1, borda_admin1_grp,
            freq_admin1, freq_admin1_grp)

write.csv(final_admin1_alldataframes, paste0("./output/onetabletorullthemall/admin_1_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),".csv"))

##### BIND admin2 agg tables ####

full_table_admin2 <- table_raw_admin2%>%
  mutate(independent.var = "total", independent.var.value = "total")%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

##### reshaping bordas admin2 #####

### admin2
borda_admin2 <- borda_admin2%>%
  separate(vars, into = c("question_1", "question_2", "question_3"), sep = " ")

borda_admin2_1 <- borda_admin2%>%
  select(question_1, rank_1, percent_1, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_1, dependent.var.value = rank_1, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_2", numbers = as.numeric(str_remove(percent_1, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin2_2 <- borda_admin2%>%
  select(question_2, rank_2, percent_2, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_2, dependent.var.value = rank_2, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_2", numbers = as.numeric(str_remove(percent_2, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin2_3 <- borda_admin2%>%
  select(question_3, rank_3, percent_3, repeat_var, disaggregation)%>%
  mutate(dependent.var =question_3, dependent.var.value = rank_3, independent.var="total", independent.var.value = "total", 
         repeat.var.value = repeat_var, repeat.var = "admin_2", numbers = as.numeric(str_remove(percent_3, "%"))/100
  )%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)

borda_admin2 <- borda_admin2_1%>%
  bind_rows(borda_admin2_2, borda_admin2_3)


##### ADMIN 2 Bind FREQ #####

freq_admin2 <- freq_admin2%>%
  mutate(dependent.var = variable, numbers = percent, independent.var = "total", independent.var.value = "total",
         repeat.var.value = admin_2, repeat.var = "admin_2", dependent.var.value = NA)%>%
  select(dependent.var, dependent.var.value, independent.var, independent.var.value, repeat.var, repeat.var.value, numbers)


##### ADMIN 2 BIN ALL THE DATA FRAMES!!!! ####

final_admin2_alldataframes <- full_table_admin2 %>%
  bind_rows(borda_admin2,freq_admin2)

write.csv(final_admin2_alldataframes, paste0("./output/onetabletorullthemall/admin_2_all_vars_aggregation", format(Sys.time(), "%Y%m%d"),".csv"))
