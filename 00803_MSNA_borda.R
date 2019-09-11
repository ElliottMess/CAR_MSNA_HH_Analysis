source("functions/borda_count.R")

df_hh <- read_csv("output/MSNA_HH_Analysed_data.csv")



borda_script_template <- read_csv("input/borda_analysis.csv")

borda_script_admin0 <- borda_script_template%>%
  mutate(
    disaggregate = NA,
    repeat_var = "admin_0"
  )

borda_applier(borda_script_admin0, df_hh) %>% 
  write_csv("output/borda/borda_admin0.csv",
            na = "")


borda_script_admin1 <- borda_script_template%>%
  mutate(
    disaggregate = NA,
    repeat_var = "admin_1"
         )

borda_applier(borda_script_admin1, df_hh) %>% 
  write_csv("output/borda/borda_admin1.csv",
            na = "")

borda_script_admin1_grp <- borda_script_template%>%
  mutate(
    disaggregate = "ig_8_statut_groupe",
    repeat_var = "admin_1"
  )

borda_applier(borda_script_admin1_grp, df_hh) %>% 
  write_csv("output/borda/borda_admin1_grp.csv",
            na = "")


borda_script_admin2 <- borda_script_template%>%
  mutate(
    disaggregate = NA,
    repeat_var = "admin_2"
  )

borda_applier(borda_script_admin2, df_hh) %>% 
  write_csv("output/borda/borda_admin2.csv",
            na = "")

