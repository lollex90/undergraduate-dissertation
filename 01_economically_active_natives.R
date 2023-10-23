# Step 1: Calculate the number of economically active natives -------------
source("00_function_definitions.R")

path_1 <- "~/UCL/Year 3/Dissertation/Data/unemployed_total_"
unemp_total_2023 <- read_yearly_data(paste0(path_1, "2023.xlsx"), 5)
unemp_total_2022 <- read_yearly_data(paste0(path_1, "2022.xlsx"), 5)
unemp_total_2021 <- read_yearly_data(paste0(path_1, "2021.xlsx"), 5)

path_2 <- "~/UCL/Year 3/Dissertation/Data/unemployment_rate_"
unemp_rate_2023 <- read_yearly_data(paste0(path_2, "2023.xlsx"), 4) %>% 
  mutate(year = "2023")
unemp_rate_2022 <- read_yearly_data(paste0(path_2, "2022.xlsx"), 4) %>% 
  mutate(year = "2022")
unemp_rate_2021 <- read_yearly_data(paste0(path_2, "2021.xlsx"), 4)  %>% 
  mutate(year = "2021")

active_population_estimate_2023 <- get_active_population_estimate(unemp_rate_2023, unemp_total_2023)
active_population_estimate_2022 <- get_active_population_estimate(unemp_rate_2022, unemp_total_2022)
active_population_estimate_2021 <- get_active_population_estimate(unemp_rate_2021, unemp_total_2021)

path_3 <- "~/UCL/Year 3/Dissertation/Intermediate Data/active_population_estimate_"
write.xlsx(active_population_estimate_2023, paste0(path_3, "2023.xlsx"))
write.xlsx(active_population_estimate_2022, paste0(path_3, "2022.xlsx"))
write.xlsx(active_population_estimate_2021, paste0(path_3, "2021.xlsx"))