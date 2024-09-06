
# 1. Read raw data -------------------------------------------------------------
sql_connection <- dbConnect(
  odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "Working",
  Trusted_Connection = "True"
)

# Read data from the previous financial years e.g., 21/22, 22/23, 23/24
historic_data <- dbGetQuery(
  sql_connection,
  "SELECT [Maincode]
      ,   [Yearmonth]
      ,   [Value]
      ,   [Provider] 
   FROM [Working].[dbo].[BSOL_1236_Provider_Workforce]"
)

# Read data from the current financial year e.g., 24/25
data_2425 <- read.csv(file.path(directory_path, "/Data/24-25/Combined Data/Combined_data_FY24_25.csv"))

# Combine both data
data <- rbind(historic_data, data_2425 %>% select(-Load_Date))

# 2. Read reference data -------------------------------------------------------

# File path for the latest occupation codes reference
FY24_25_reference_path <- file.path(directory_path, "/Reference/FY24_25_Occupation_Reference.xlsx")

# 3. Function to join the raw data with the reference file ---------------------

join_data <- function(data, reference_path){
  
  # Get a list of data frames, each corresponds to the reference from each sheet
  reference_list <- map(excel_sheets(reference_path), 
                        ~read_excel(reference_path, sheet = .x))
  
  # Join the main codes with the occupations' descriptions
  table <- map(reference_list, function(reference_data){
    
    output <- data %>% 
      
      inner_join(reference_data, by = "Maincode", relationship = "many-to-many") %>% 
      
      mutate(Value = ifelse(!is.na(Value), as.numeric(Value), NA_real_)) %>%
      
      tidyr::replace_na(list(Value = 0))
    
    return(output)
  })
  
  return(table)
  
}

# 4. Call function -------------------------------------------------------------

joined_tables <- join_data(data, FY24_25_reference_path)


# Combine agency and bank staffs tables
joined_tables[[13]] <- rbind(joined_tables[[3]], joined_tables[[4]])

for(i in 1:length(joined_tables)){
  print(unique(joined_tables[[i]]$`Reference Sheet`))
}

  
# Default all values in vacancy table as positives -----------------------------

joined_tables[[6]] <- joined_tables[[6]] %>% 
  mutate(Value = abs(Value))

# #8. Write global temporary tables ----------------------------------------------
print("Writing global temp tables...")


temp_table_names <- c("##BSOL_1236_Overall", "##BSOL_1236_Staff_Group", "##BSOL_1236_Bank_Staff", "##BSOL_1236_Agency_Staff",
                      "##BSOL_1236_KPI", "##BSOL_1236_Vacancy", "##BSOL_1236_IR", "##BSOL_1236_AHP_IR", "##BSOL_1236_Maternity",
                      "##BSOL_1236_PNA", "##BSOL_1236_PMA", "##BSOL_1236_HCSW", "##BSOL_1236_Bank_Agency")



for(i in seq_along(temp_table_names)){
  dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ", temp_table_names[i]))

  dbWriteTable(conn = sql_connection, name = temp_table_names[i],
               value = joined_tables[[i]],
               overwrite = TRUE)
}

print("Finished writing global temp tables!")

