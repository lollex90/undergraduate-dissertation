source("00_function_definitions.R")

# Step 5: Compute the refugee rate in January and October -----------------
data_ref_jan <- read_excel("~/UCL/Year 3/Dissertation/Intermediate Data/refugees_total.xlsx") %>% 
  mutate(TERYT = paste0(TERYT, "000")) %>% 
  filter(month == 10) %>% 
  left_join(characteristics, by = c("TERYT" = "Kod")) %>% 
  mutate(total_refugees_rate_jan = total_refugees/total_production) %>% 
  select(TERYT, Nazwa, total_refugees_rate_jan)

names(data_ref_jan)[1] <- "Kod"

data_ref_oct <- read_excel("~/UCL/Year 3/Dissertation/Intermediate Data/refugees_total.xlsx") %>% 
  mutate(TERYT = paste0(TERYT, "000")) %>% 
  filter(month == 7) %>% 
  left_join(characteristics, by = c("TERYT" = "Kod")) %>% 
  mutate(total_refugees_rate_oct = total_refugees/total_production) %>% 
  select(TERYT, Nazwa, total_refugees_rate_oct)

names(data_ref_oct)[1] <- "Kod"

characteristics <- characteristics %>% 
  left_join(data_ref_jan, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
  left_join(data_ref_oct, by = c("Kod" = "Kod", "Nazwa" = "Nazwa"))