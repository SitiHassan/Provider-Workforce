
#1. Calculate vacancy rates ----------------------------------------------------

WTE_overall <- joined_tables[[1]]


Vacancy <- joined_tables[[6]]


##1.1 Main Vacancies -----------------------------------------------------------

Main_Vacancy <- Vacancy %>% 
  filter(`Staff Group` == "All Roles") %>%  # Get In Month Overall Staff Vacancies WTE
  select(c(2:4)) %>% 
  dplyr::rename(Vacancy = Value) %>% 
  inner_join(WTE_overall %>%
                filter(`Summary Staff WTE Detail` == "Total WTE Substantive Staff") %>%
                select(c(2:4)) %>%  
                dplyr::rename(Substantive = Value),  
             by = c("Provider", "Yearmonth")) %>% 
  select(Provider, Yearmonth, Vacancy, Substantive) %>% 
  mutate(Total = Vacancy + Substantive,
         Rate = Vacancy / Total,
         Type = "Main Vacancy")

##1.2. Nursing & Midwifery Vacancies --------------------------------------------

WTE_staff_group  <- joined_tables[[2]]

Nursing_Vacancy <- WTE_staff_group %>% 
  filter(`Staff Group` == "Nursing, midwifery and HV" &
          `Staff Type` == "Total") %>% 
  select(c(2:4)) %>% 
  dplyr::rename(Substantive = Value) %>% 
  inner_join((Vacancy %>%
                filter(`Staff Group` == "Nursing, midwifery and HV" & # Get In Month Overall Staff Vacancies WTE for this Staff Group
                         `Staff Type` == "Total") %>% 
                select(c(2:4)) %>% 
                dplyr::rename(Vacancy = Value)),
             by =  c("Provider", "Yearmonth")) %>% 
  select(Provider, Yearmonth, Vacancy, Substantive) %>% 
  mutate(Total = Vacancy + Substantive,
         Rate = Vacancy / Total,
         Type = "Nursing, Midwifery and HV Vacancy")


##1.3. BSOL Main Vacancies ------------------------------------------------------

BSOL_Total_WTE <- WTE_overall %>%
  filter(`Summary Staff WTE Detail` == "Total WTE Substantive Staff") %>%
  select(c(2:4)) %>%  
  dplyr::rename(Substantive = Value) %>%  
  group_by(Yearmonth) %>%  # Grouped by Provider to get the total WTE for system
  summarise(Substantive = sum(Substantive)) %>% 
  mutate(Provider = "BSOL Total ICS")


BSOL_Total_Vacancy <- Vacancy %>% 
  filter(`Staff Group` == "All Roles") %>% 
  select(c(2:4)) %>% 
  dplyr::rename(Vacancy = Value) %>% 
  group_by(Yearmonth) %>%  # Grouped by Provider to get the total vacancy for system
  summarise(Vacancy = sum(Vacancy)) %>% 
  mutate(Provider = "BSOL Total ICS")

## Inner join both to get the BSOL Main Vacancies

BSOL_Vacancy <- BSOL_Total_WTE %>% 
                            inner_join(BSOL_Total_Vacancy, by = c("Provider", "Yearmonth")) %>% 
                            select(Provider, Yearmonth, Vacancy, Substantive) %>% 
                            mutate(Total = Vacancy + Substantive,
                            Rate = Vacancy / Total,
                            Type = "Main Vacancy")

##1.4. BSOL Nursing & Midwifery Vacancies ---------------------------------------

BSOL_Total_Nursing_WTE <- WTE_staff_group %>% 
  filter(`Staff Group` == "Nursing, midwifery and HV" & `Staff Type` == "Total") %>% 
  select(c(2:4)) %>% 
  dplyr::rename(Substantive = Value) %>% 
  group_by(Yearmonth) %>%  # Grouped by Provider to get the total WTE for system
  summarise(Substantive = sum(Substantive)) %>% 
  mutate(Provider = "BSOL Total ICS")

BSOL_Total_Nursing_Vacancy <- Vacancy %>%
  filter(`Staff Group` == "Nursing, midwifery and HV" & `Staff Type` == "Total") %>% 
  select(c(2:4)) %>% 
  dplyr::rename(Vacancy = Value) %>% 
  group_by(Yearmonth) %>%   # Grouped by Provider to get the total vacancy for system
  summarise(Vacancy = sum(Vacancy)) %>% 
  mutate(Provider = "BSOL Total ICS")

## Inner join both to get the BSOL Nursing & Midwifery Vacancies

BSOL_Nursing_Vacancy<- BSOL_Total_Nursing_WTE %>% 
                                    inner_join(BSOL_Total_Nursing_Vacancy, by = c("Provider", "Yearmonth")) %>% 
                                    select(Provider, Yearmonth, Vacancy, Substantive) %>% 
                                    mutate(Total = Vacancy + Substantive,
                                    Rate = Vacancy / Total,
                                    Type = "Nursing, Midwifery and HV Vacancy")
  
#5. Combine All Vacancies  -----------------------------------------------------

joined_vacancies <- bind_rows(Main_Vacancy, Nursing_Vacancy, BSOL_Vacancy, BSOL_Nursing_Vacancy) %>% 
                    mutate_all(~ifelse(is.na(.), NA_real_, .))


#6. Write SQL table ------------------------------------------------------------

dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_Vacancy_Rate_Calc"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_Vacancy_Rate_Calc",
             value = joined_vacancies,
             overwrite = TRUE)

#7. Staffing Post Vs Plan & Vacancy Rates --------------------------------------

## Read the planned data for all financial years -------------------------------

planned_data <- read_xlsx(file.path(directory_path, "/Reference/FY24_25_Planned_Activity.xlsx"),
                          sheet = "Planned Staff") %>% 
  mutate(Yearmonth = as.integer(Yearmonth))

## Function to get planned activity for all providers & different types --------

get_plan <- function(planned_data, type = NULL, BSOL = FALSE ){
  
  if(BSOL == TRUE && is.null(type)){
    
    result <- planned_data %>% 
      group_by(Yearmonth) %>% 
      summarise(Plan = sum(Plan)) %>% 
      mutate(Provider = "BSOL Total ICS") %>% 
      select(Yearmonth, Provider, Plan)
  }
  
  else if (!is.null(type) && BSOL == FALSE){
    
    result <- planned_data %>% 
      filter(Type == type) %>% 
      select(Yearmonth, Provider, Plan)
  }
  
  else{
    
    stop("Invalid type supplied")
  }
  
  return (result)
}


## Function to get the final staffing plans + vacancy rates for ALL ------------

get_overall_plan <- function (vacancy_data, planned_data){
  
  result <- vacancy_data %>%
            select(Yearmonth, Provider, Substantive, Rate) %>% 
            dplyr::rename(VacancyRate = Rate) %>% 
            full_join (planned_data, by =  c("Provider", "Yearmonth")) %>% 
            mutate(Difference_WTE = Substantive - Plan,
            Percentage_Difference = Difference_WTE / Plan) %>%
            select(Yearmonth, Provider, Substantive, Plan, Difference_WTE, Percentage_Difference, VacancyRate)
  
  return (result)
}


## Main Staffing Post Vs Plan & Vacancy Rates ----------------------------------

main_plan <- get_plan(planned_data, type = "Substantive Plan", BSOL = FALSE)
main_BSOL_plan <- get_plan(main_plan, BSOL = TRUE)

main_overall_plan <- get_overall_plan(Main_Vacancy, main_plan)
BSOL_overall_plan <- get_overall_plan(BSOL_Vacancy, main_BSOL_plan)


ALL_Vacancy_plan <- bind_rows(main_overall_plan, BSOL_overall_plan) %>% 
                    mutate(Type = "Staffing Post Vs Plan Substantive")

## Nursing, Midwifery Staffing Post Vs Plan & Vacancy Rates --------------------

nursing_plan <- get_plan(planned_data, 
                        type = "Nursing, Midwifery Plan",
                        BSOL = FALSE)

nursing_BSOL_plan <- get_plan(nursing_plan, BSOL = TRUE)

nursing_overall_plan <- get_overall_plan(Nursing_Vacancy, nursing_plan)
nursing_BSOL_overall_plan <- get_overall_plan(BSOL_Nursing_Vacancy, nursing_BSOL_plan)

All_nursing_vacancy_plan <- bind_rows(nursing_overall_plan, nursing_BSOL_overall_plan) %>% 
                            mutate(Type = "Staffing Post Vs Plan Nursing & Midwifery")


## Agency Actual Vs Plan----------------------- ----------------------------

get_WTE <- function (WTE_table, staff_type, BSOL = FALSE){
  
    result <- WTE_table %>% 
      filter(`Summary Staff WTE Detail` == staff_type) %>%
      select(c(2:4)) %>% 
      dplyr::rename(Substantive = Value)
    
  if(BSOL == TRUE){
    
    result <- result %>% 
      group_by(Yearmonth) %>% 
      summarise(Substantive = sum(Substantive)) %>% 
      mutate(Provider = "BSOL Total ICS")

  }
  
  return (result)
}


get_actual_plan <- function(actual_WTE, planned_data){
  
    result <- actual_WTE %>% 
              full_join (planned_data, by =  c("Provider", "Yearmonth")) %>% 
              mutate(Difference_WTE = Substantive - Plan,
              Percentage_Difference = Difference_WTE / Plan) %>% 
              select(c(3, 1:2, 4:6))
    
  return (result)
  
}

agency_actual_WTE <- get_WTE(WTE_overall, "Agency Staff (including, agency and contract)",
                                       BSOL = FALSE)
BSOL_agency_actual_WTE <- get_WTE(WTE_overall, "Agency Staff (including, agency and contract)",
                                            BSOL = TRUE)

agency_plan <- get_plan(planned_data, type = "Agency Plan", BSOL = FALSE)
BSOL_agency_plan <- get_plan(agency_plan, BSOL = TRUE)

agency_overall_plan <- get_actual_plan(agency_actual_WTE, agency_plan)
BSOL_agency_overall_plan <- get_actual_plan(BSOL_agency_actual_WTE, BSOL_agency_plan)

All_agency_staffing_plan <- bind_rows(agency_overall_plan, BSOL_agency_overall_plan ) %>% 
  mutate(Type = "Agency Actual Vs Plan")

## Get Bank Actual Vs Plan -----------------------------------------------------


bank_actual_WTE <- get_WTE(WTE_overall, "Bank Staff",
                                     BSOL = FALSE)
BSOL_bank_actual_WTE <- get_WTE(WTE_overall, "Bank Staff",
                                          BSOL = TRUE) 

bank_plan <- get_plan(planned_data, type = "Bank Plan", BSOL = FALSE)
BSOL_bank_plan <- get_plan(bank_plan, BSOL = TRUE) 

bank_overall_plan <- get_actual_plan(bank_actual_WTE, bank_plan)
bank_BSOL_overall_plan <- get_actual_plan(BSOL_bank_actual_WTE, BSOL_bank_plan)

All_bank_staffing_plan <- bind_rows(bank_overall_plan, bank_BSOL_overall_plan ) %>% 
  mutate(Type = "Bank Actual Vs Plan")


## Write SQL tables ------------------------------------------------------------


combined <- bind_rows(ALL_Vacancy_plan, All_nursing_vacancy_plan,
                      All_agency_staffing_plan, All_bank_staffing_plan) %>%
  mutate_all(~ifelse(is.na(.), NA, .)) %>% 
  filter(Yearmonth <= latest_month)



dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_Staffing_Actual_Plan"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_Staffing_Actual_Plan", 
             value = combined,
             overwrite = TRUE)

## HCSW Vacancy rate -----------------------------------------------------------


# Data for HCSW vacancy rate
HCSW_Vacancy_Rate <- joined_tables[[12]] %>% 
  filter(`Measure Detail` %in% c("Health Care Support Worker reported Vacancy rate",
                           "Maternity Support Worker reported Vacancy rate")) %>% 
  filter(Yearmonth <= latest_month)

# Create data table for HCSW vacancy rate
dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_HCSW_Vacancy_Rate"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_HCSW_Vacancy_Rate", 
             value = HCSW_Vacancy_Rate,
             overwrite = TRUE)


# Update HCSW table to exclude the HCSW/MSW vacancy rates
joined_tables[[12]] <- joined_tables[[12]] %>% 
  filter(!`Measure Detail` %in% c("Health Care Support Worker reported Vacancy rate",
                                 "Maternity Support Worker reported Vacancy rate"))

dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_HCSW"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_HCSW", 
             value = joined_tables[[12]],
             overwrite = TRUE)



  
  
  
  