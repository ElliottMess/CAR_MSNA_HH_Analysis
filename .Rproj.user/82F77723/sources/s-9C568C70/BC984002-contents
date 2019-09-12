## Import data
educ_data = read.csv("./output/educ_database_tobecleaned.csv", stringsAsFactors = F)
final_data = read.csv("./output/REACH_CAR_MSNA_Final_dataset_0309.csv", stringsAsFactors = F)

educ_data_final = subset(educ_data, is_in(educ_data$X_parent_index, final_data$X_index))

write.csv(educ_data_final, "./output/educ_database_final.csv")




nut_data = read.csv("./output/nutrition_database_tobecleaned.csv", stringsAsFactors = F)

nut_data_final = subset(nut_data, is_in(nut_data$X_parent_index, final_data$X_index))

write.csv(nut_data_final, "./output/nutrition_database_final.csv")




sante_data = read.csv("./output/sante_database_tobecleaned.csv", stringsAsFactors = F)

sante_data_final = subset(sante_data, is_in(sante_data$X_parent_index, final_data$X_index))

write.csv(sante_data_final, "./output/sante_database_final.csv")