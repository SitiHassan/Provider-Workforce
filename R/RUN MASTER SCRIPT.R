library(readxl)
library(tidyverse)
library(fs)
library(purrr)
library(odbc)
library(DBI)
library(janitor)
library(tidyr)
library(stringr)

rm(list = ls())

directory_path <- "./Reports/02_Routine/BSOL_1236_Provider Workforce Dashboard/"

setwd(directory_path)

latest_month <- 202407

run_master_script <- function(){
  
  
  start_time = Sys.time()
  
  print("Script 1: Starting to extract raw data..")
  source(paste0(directory_path, "R scripts/01_BSOL_1236_Extract_Raw_Files.R"))
  print("Finished extracting raw data!")

  print("Script 2: Starting to create main output...")
  source(paste0(directory_path, "R scripts/02_BSOL_1236_Main_Output.R"))
  print("Finished creating the main output & global temp tables!")
  
  print("Script 3: Starting to calculate the vacancies...")
  source(paste0(directory_path, "R scripts/03_BSOL_1236_Vacancy_Calcs.R"))
  print("Finished calculating the vacancies!")
  
  print("Script 4: Starting to modify the KPI/Turnover/Sickness tables...")
  source(paste0(directory_path, "R scripts/04_BSOL_1236_Modify_Table.R"))
  print("Finished modifying the tables!")
  
  print("Script 5: Starting to create dataset for main overview page...")
  source(paste0(directory_path, "R scripts/05_BSOL_1236_Main_Overview.R"))
  print("Finished creating the dataset!")
  
  end_time = Sys.time()
  
  time_taken = end_time - start_time
  
  print(paste0("Total time taken to run all scripts: ", time_taken))
  
}

run_master_script()