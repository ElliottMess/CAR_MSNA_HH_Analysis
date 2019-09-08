
# Mettre sa session R sur le chemin ou se trouve ce document

# Charger les outils dont on aura besoin
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
library(knitr)



# Charger le questionnaire et les choix
questions <- read.csv("./input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_survey.csv",
                      stringsAsFactors=F, check.names = F)

choices <- read.csv("./input/questionnaire_kobo_hh_combine_v4_FINAL_PourAnalyse_choices.csv",
                    stringsAsFactors=F, check.names = F)

