source("functions/borda_count.R")

df_hh <- read_csv("output/MSNA_HH_Analysed_data.csv")

borda_script <- read_csv("input/borda_analysis.csv")

borda_applier(borda_script, df_hh) %>% 
  write_csv("output/borda.csv",
            na = "")
