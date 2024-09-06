
#1. Function to check column types -----------------------------------------------

check_column_types <- function(main_df, df_to_check){
  common_columns <- intersect(names(main_df), names(df_to_check))
  
  if(length(common_columns) == 0){
    cat("No common columns.\n")
    return(FALSE)
  } 
  
  main_types <- sapply(main_df[common_columns], class)
  check_types <- sapply(df_to_check[common_columns], class)
  if(!all(main_types == check_types)){
    cat("Mismatched column types.\n")
    return(FALSE)
  }
  
  return(TRUE)
}

#2. Function to join the main df and new df together -----------------------------

join_tables <- function(main_df, df_to_join){
  
  # Check column types and return if there's a mismatch
  if(!check_column_types(main_df, df_to_join)){
    cat("Column type mismatch, cannot join data frames.\n")
    return(NULL)
  }
  
  # Ensure both data frames have the same columns
  all_columns <- union(names(main_df), names(df_to_join))
  main_df[setdiff(all_columns, names(main_df))] <- NA
  df_to_join[setdiff(all_columns, names(df_to_join))] <- NA
  
  # Join the data frames
  result_df <- rbind(main_df, df_to_join) %>% 
    mutate(Load_Date = Sys.Date())
  
  cat("Rows bound, main_df length:", nrow(main_df), ", result_df length:", nrow(result_df), "\n")
  return(result_df)
}


#3. Update KPI table -----------------------------------------------------------

KPI_data <- joined_tables[[5]] %>% 
  filter(!`Workforce KPI` %in% c("All Staff Turnover  - 12 month rolling rate %",
                                 "Sickness Absence Rate  - 12 month rolling %"))

# KPI_data <- read_excel(file.path(directory_path, "Data/Staff_Turnover_Sickness_Rates.xlsx"),
#                        sheet = 'KPI 2')%>% 
#   mutate(Yearmonth = as.integer(Yearmonth))

Sickness_data <- read_excel(file.path(directory_path, "Data/Staff_Turnover_Sickness_Rates.xlsx"), 
                            sheet = "Staff Sickness Rate") 

Turnover_data <- read_excel(file.path(directory_path, "Data/Staff_Turnover_Sickness_Rates.xlsx"),
                            sheet = "Staff Turnover Rate") 
                  
Sickness_Turnover <- Sickness_data %>% 
  mutate(`Workforce KPI` = "Sickness Absence Rate  - 12 month rolling %",
         Maincode = "KPI0140",
         `Reference Sheet` = "Staff Sickness Rate") %>% 
  bind_rows(Turnover_data %>% 
            mutate(`Workforce KPI` = "All Staff Turnover  - 12 month rolling rate %",
                    Maincode = "KPI0100",
                   `Reference Sheet` = "Staff Turnover Rate")) %>% 
  pivot_longer(names_to = "Provider", cols = c(2:6),
               values_to = "Value") %>% 
  mutate(Yearmonth = str_sub(as.character(Date), start = 1, end = 7)) %>% 
  mutate(Yearmonth = as.integer(str_remove(Yearmonth, "-"))) %>%  
  mutate(Type = case_when(
    `Workforce KPI` == "All Staff Turnover  - 12 month rolling rate %" ~ "Turnover Rate",
     `Workforce KPI` == "Sickness Absence Rate  - 12 month rolling %" ~ "Sickness Rate")) %>%
  select(Maincode, Yearmonth, Value, Provider, `Workforce KPI`, `Reference Sheet`)

# Join the KPI data with the sickness + turnover data
updated_KPI <- rbind(KPI_data, Sickness_Turnover) %>% 
  filter(Yearmonth <= latest_month)
  
#4. Update the SQL KPI table -------------------------------------------------------

dbExecute(sql_connection, paste("DROP TABLE IF EXISTS", "##BSOL_1236_KPI"))
dbWriteTable(conn = sql_connection, name = "##BSOL_1236_KPI", 
             value = updated_KPI,
             overwrite = TRUE)


# ------------------------------------------------------------------------------
#5. Tables for sickness & turnover tabs with conditional formatting

sickness_turnover_plan <- read_excel(file.path(directory_path, "Reference/FY24_25_Planned_Activity.xlsx"),
                                     sheet = "Planned Sickness & Turnover") %>%
  mutate(Yearmonth = as.integer(Yearmonth)) 

Sickness_Turnover2 <- Sickness_Turnover %>% 
  select(Provider, Yearmonth, `Reference Sheet`, Value) %>% 
  rename(Actual = Value) %>% 
  left_join(sickness_turnover_plan %>% select(-Date), 
            by = c("Provider",  "Yearmonth", "Reference Sheet" = "Type"),
            relationship = "many-to-many") %>% 
  mutate(Variance = Actual - Plan,
         `% Difference` = Variance / Plan) %>% 
  pivot_longer(cols = c(Actual, Plan, Variance, `% Difference`),
               names_to = "Metric",
               values_to = "Value") %>% 
  filter(Yearmonth <= latest_month)


dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_Sickness_Turnover"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_Sickness_Turnover", 
             value = Sickness_Turnover2,
             overwrite = TRUE)


