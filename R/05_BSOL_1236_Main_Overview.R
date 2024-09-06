
metric <- c( "Substantive Staff In Post", "Bank Usage", 
            "Agency Usage", "Vacancies", "Vacancy Rate",
            "Sickness Rate", "Turnover Rate", "Total Staff In Post")

staffGroup <- c("All Staff",
                "Ambulance Service Staff",
                "Any Other Staff",
                "Clinical Support",
                "Medical and dental",
                "NHS infrastructure support",
                "Nursing, midwifery and HV",
                "Scientific, therapeutic and technical")

system <- c("ICS", "UHB", "BWCH", "ROH", "BCHC", "BSMHFT")

period <- seq(from = as.Date("2021-04-01"), 
              to = as.Date("2025-03-01"),
              by = "month") %>% 
  str_sub(start = 1, end = 7) %>% 
  str_replace_all("-", "") %>% as.integer()
  

combinations <- expand.grid(Metric = metric,
                            `Staff Group` = staffGroup,
                            System = system,
                            Period = period,
                            stringsAsFactors = FALSE)


# Metric + All Staff -----------------------------------------------------------

metric1 <- combinations %>% 
  filter(Metric == "Substantive Staff In Post" &
         `Staff Group` == "All Staff") %>% 
  # Update substantive staff in post + all staff
  left_join(joined_tables[[1]] %>%
              select(c(Yearmonth, Provider, Value, `Summary Staff WTE Detail`)) %>% 
              filter(`Summary Staff WTE Detail` == "Total WTE Substantive Staff"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>% 
  select(-c(`Summary Staff WTE Detail`))

metric2 <- combinations %>% 
  filter(Metric == "Bank Usage" &
           `Staff Group` == "All Staff") %>% 
  # Update bank usage + all staff
  left_join(joined_tables[[13]] %>% 
               select(c(Yearmonth, Provider, Value, `Bank/Agency`, `Staff Type`)) %>% 
               filter(`Bank/Agency` == "Bank"  &
                        `Staff Type` == "Total") %>% 
               group_by(Yearmonth, Provider) %>% 
               summarise(Value = sum(Value),.groups = 'drop'),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth"))

metric3 <-  combinations %>% 
  filter(Metric == "Agency Usage" &
           `Staff Group` == "All Staff") %>% 
  # Update agency usage + all staff
  left_join(joined_tables[[13]] %>% 
              select(c(Yearmonth, Provider, Value, `Bank/Agency`, `Staff Type`)) %>% 
              filter(`Bank/Agency` == "Agency"  &
                       `Staff Type` == "Total") %>% 
              group_by(Yearmonth, Provider) %>% 
              summarise(Value = sum(Value),.groups = 'drop'),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth"))

metric4 <- combinations %>% 
  filter(Metric == "Vacancies" &
           `Staff Group` == "All Staff") %>% 
  # Update vacancies + all staff
  left_join(joined_tables[[6]] %>% 
              select(c(Yearmonth, Provider, Value, `Staff Group`, `Staff Type`)) %>% 
              filter(`Staff Group` == "All Roles"  &
                       `Staff Type` == "Total"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>% 
  select(-c(`Staff Group.y`, `Staff Type`)) %>% 
  rename(`Staff Group` = `Staff Group.x`)

metric5 <- combinations %>% 
  filter(Metric == "Vacancy Rate" &
           `Staff Group` == "All Staff") %>% 
  # Update vacancy rate + all staff
  left_join(joined_vacancies %>% 
              select(c(Yearmonth, Provider, Rate, Type)) %>% 
              filter(Type == "Main Vacancy"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>% 
  rename(Value = Rate) %>% 
  select(-c(Type))

metric6 <- combinations %>% 
  filter(Metric == "Sickness Rate" &
           `Staff Group` == "All Staff") %>% 
  # Update sickness rate + all staff
  left_join(Sickness_Turnover2 %>% 
              select(c(Yearmonth, Provider, Metric, `Reference Sheet`, Value)) %>% 
              filter(`Reference Sheet` == "Staff Sickness Rate" &
                       Metric == "Actual"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>%
  select(-c(`Reference Sheet`, Metric.y)) %>% 
  rename(Metric = Metric.x)

metric7 <- combinations %>% 
  filter(Metric == "Turnover Rate" &
           `Staff Group` == "All Staff") %>% 
  # Update turnover rate + all staff
  left_join(Sickness_Turnover2 %>% 
              select(c(Yearmonth, Provider, Metric, `Reference Sheet`, Value)) %>% 
              filter(`Reference Sheet` == "Staff Turnover Rate" &
                       Metric == "Actual"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>%
  select(-c(`Reference Sheet`, Metric.y)) %>% 
  rename(Metric = Metric.x)

metric8 <- combinations %>% 
  filter(Metric == "Total Staff In Post" &
           `Staff Group` == "All Staff") %>% 
  # Update substantive staff in post + all staff
  left_join(joined_tables[[1]] %>%
              select(c(Yearmonth, Provider, Value, `Summary Staff WTE Detail`)) %>% 
              filter(`Summary Staff WTE Detail` == "Total WTE all Staff"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth")) %>% 
  select(-c(`Summary Staff WTE Detail`))
# Metric + Other Staff Groups --------------------------------------------------

metric9 <- combinations %>% 
  filter(Metric == "Substantive Staff In Post" &
           !`Staff Group` == "All Staff") %>% 
  # Update substantive staff in post + other staff
  left_join(joined_tables[[2]] %>%
              select(c(Yearmonth, Provider, Value, `Staff Group`, `Staff Type`)) %>% 
              filter(`Staff Type` == "Total" & !`Staff Group` %in% c("Obs & Gynae")) %>% 
              group_by(Provider, Yearmonth, `Staff Group`) %>% 
              summarise(Value = sum(Value), .groups = 'drop'),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth",
                   "Staff Group"
                   ),
            relationship = "many-to-many")


metric10 <- combinations %>% 
  filter(Metric == "Bank Usage" &
           !`Staff Group` == "All Staff") %>% 
  # Update bank usage + other staff
  left_join(joined_tables[[13]] %>% 
              select(c(Yearmonth, Provider, Value, `Bank/Agency`, `Staff Group`, `Staff Type`)) %>% 
              filter(`Bank/Agency` == "Bank"  &
                       `Staff Type` == "Total"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth",
                   "Staff Group"),
            relationship = "many-to-many") %>% 
  select(-c(`Bank/Agency`, `Staff Type`))

metric11 <- combinations %>% 
  filter(Metric == "Agency Usage" &
           !`Staff Group` == "All Staff") %>% 
  # Update agency usage + other staff
  left_join(joined_tables[[13]] %>% 
              select(c(Yearmonth, Provider, Value, `Bank/Agency`, `Staff Group`, `Staff Type`)) %>% 
              filter(`Bank/Agency` == "Agency"  &
                       `Staff Type` == "Total"),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth",
                   "Staff Group"),
            relationship = "many-to-many") %>% 
  select(-c(`Bank/Agency`, `Staff Type`))

metric12 <- combinations %>% 
  filter(Metric == "Vacancies" &
           !`Staff Group` == "All Staff") %>% 
  # Update vacancies + other staff
  left_join(joined_tables[[6]] %>% 
              select(c(Yearmonth, Provider, Value, `Staff Group`, `Staff Type`)) %>% 
              filter(`Staff Type` == "Total" &
                       !`Staff Group` %in% c("All Roles", "Advanced Care", "CC/ICU Nursing")),
            by = c("System" = "Provider", 
                   "Period" = "Yearmonth",
                   "Staff Group"),
            relationship = "many-to-many") %>% 
  select(-c(`Staff Type`))

# Combine all metrics
main_overview <- bind_rows(metric1, metric2, metric3, metric4, metric5, metric6,
            metric7, metric8, metric9, metric10, metric11, metric12)

# Calculate values for ICS -----------------------------------------------------

sums_ICS <- main_overview %>%
  filter(System != "ICS") %>%
  group_by(Period, `Staff Group`, Metric) %>%
  summarise(Sum_Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(Sum_Value = ifelse( Metric %in% c("Vacancy Rate", "Sickness Rate", "Turnover Rate"),
                         NA, Sum_Value))

# Update the all metrics with ICS
updated_main_overview <- main_overview %>% 
  left_join(
    sums_ICS,
    by = c("Period", "Staff Group", "Metric")
  ) %>% 
  mutate(Value = ifelse(System == "ICS", Sum_Value, Value)) %>% 
  select(-c(Sum_Value)) %>% 
  mutate(Date = as.character(Period),
         Date = paste0(substr(Date, 1, 4), "-",substr(Date, 5, 6), "-01"),
         Date = as.Date(Date)) %>% 
  filter(Period <= latest_month)


# Write SQL table --------------------------------------------------------------

sql_connection <-
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "EAT_Reporting_BSOL",
    Trusted_Connection = "True"
  )

dbExecute(sql_connection, paste0("DROP TABLE IF EXISTS ##BSOL_1236_Main_Overview"))

dbWriteTable(conn = sql_connection, name = "##BSOL_1236_Main_Overview", 
             value = updated_main_overview,
             overwrite = TRUE)


