source("00_function_definitions.R")

# Step 4: Get county characteristics --------------------------------------
path_4 <- "~/UCL/Year 3/Dissertation/Data/"
average_house_price <- read_yearly_data(paste0(path_4, "average_house_price_2021.xlsx"), 4)
average_wage <- read_yearly_data(paste0(path_4, "average_wage_2021.xlsx"), 3)
population <- read_yearly_data(paste0(path_4, "population_2021.xlsx"), 4)
city <- read_yearly_data(paste0(path_4, "population_2021.xlsx"), 4) %>% 
  mutate(city = grepl("m\\.", Nazwa) %>% as.numeric()) %>% 
  select(Kod, Nazwa, city)
production_age <- read_yearly_data(paste0(path_4, "production_age_2021.xlsx"), 4)
names(production_age)[3:5] <-c("total_production", "men_production", "women_production") 

names(average_house_price)[3] <- "house_price"
names(average_wage)[3] <- "wage"
names(population)[3] <- "population"

characteristics <- average_wage %>% 
  left_join(average_house_price, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
  left_join(population, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
  left_join(city, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
  left_join(production_age, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) 