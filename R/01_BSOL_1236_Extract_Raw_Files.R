
################################################################################
## This script is designed to extract raw data from the  original submissions
## The main function is read_and_process_data(); example of usage is below:

#E.g.,  read_and_process_data(
#   directory = "path/for/24-25",
#   yearmonth = c("202404", "202405", "202406", "202407", "202408", 
#  "202409", "202410", "202411", "202412", "202501","202502", "202503"),
#   combined_data_directory = "path/for/combined/24-25"
# )
## The output will be saved here: path/24-25/Combined Data/
## The output's file name is Combined_data_FY24_25.csv
################################################################################


# Start timer  
start_time <- Sys.time()

result <- system.time({
  
  read_and_process_data <- function(directory, yearmonth, combined_data_directory) {
    
    excel_files <- list.files(path = directory, full.names = TRUE)
  
    
    pull_excel_data <- function(data, provider){
      data <- data %>% 
        select(6:17, 19) %>% ## columns for periods and Maincode
        filter(!is.na(Maincode) & !Maincode %in% c("Subcode", "Maincode")) %>% 
        rename_with(.fn = ~yearmonth, .cols = 1:12) %>% 
        pivot_longer(
          cols = all_of(1:12),
          names_to = "Yearmonth",
          values_to = "Value"
        ) %>%
        mutate(Provider = provider) 
      
      return (data)
    }
    
    ## This part is to read and process all the raw excel files in the folder
    
    df_combined = tibble()
    Data_list <- list()
    
    # For every excel file in the folder
    for (file_path in excel_files){
      
      # Get the file name
      file_name <- basename(file_path)
      
      # Further get the provider name by manipulating the string of the filename
      provider <- str_split(file_name, "24-25")[[1]][1] %>% trimws()
      
      # Get the excel file's sheet names
      sheet_names <- excel_sheets(file_path)
      
      # Create an empty list to store the data from each sheet inside each excel file
      data_list <- list()
      
      # Loop through each excel sheet
      for (i in 3:9){ 
        
        # Read the data from each sheet name
        data <- read_excel(file_path, sheet = sheet_names[i], skip = 1)
        
        # Process the data by calling the function pull_excel_data
        output <- pull_excel_data(data, provider)
        
        # Add the data to the empty list created previously
        # The index of this list relates to the data for the corresponding excel sheet
        data_list[[i]] <- output
      }
      
      # Combine all data for each provider
      df_combined <- bind_rows(data_list) # Combine data from each sheet within one workbook
      Data_list[[length(Data_list)+1]] <- df_combined  # Save data from all workbooks (from all 5 providers) into a list
    }
    
    # Combine all data from all providers into one data frame
    df_all <- bind_rows(Data_list)
    df_all <- df_all %>% 
                mutate(Load_Date = Sys.Date()) %>% 
                filter(Yearmonth <= latest_month) # Filter the data to be up to the latest date
    
    # Save the data to a csv file
    write_csv(df_all, file.path(combined_data_directory, "Combined_data_FY24_25.csv"))
    
    # Move files to old data folder after processing
    file_move(dir_ls(directory), file.path(paste0(directory_path, "Data/24-25/Old Data"), 
                                           basename(dir_ls(directory))))
    
  }
  
  # Call the global function to process each set of Excel files
  read_and_process_data(
    directory = paste0(directory_path, "Data/24-25/New Data"),
    yearmonth = c("202404", "202405", "202406", "202407", "202408", "202409", "202410", "202411", "202412", "202501","202502", "202503"),
    combined_data_directory = paste0(directory_path, "Data/24-25/Combined Data")
  )
  
})


# End the timer
end_time <- Sys.time()

print(paste("Total time taken to extract the raw data: ", end_time - start_time, " seconds"))




