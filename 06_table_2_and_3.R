source("00_function_definitions.R")

# Step 8: Get results for Table 2 -----------------------------------------

# load data
total_unemp <- get_data("unemployed_total", 5)[[1]]
total_unemp_fixed <- get_data("unemployed_total", 5)[[2]]

total_unemp_fixed_jan <- total_unemp_fixed %>% 
  filter(year == "2022" | year == "2023")

total_unemp_fixed_oct <- total_unemp_fixed %>% 
  filter(year == "2021" | year == "2022")

# fe
model_fe_jan <- plm(jan_rate ~ total_refugees_rate_jan + year, index = "Kod", 
                    model = "within", data = total_unemp_fixed_jan)
coeftest(model_fe_jan, vcov = vcovHC(model_fe_jan, type = 'HC0'))

# fd
model_fd_jan <- lm(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                     total_production + population + city, data = total_unemp)
coeftest(model_fd_jan, vcov = vcovHC(model_fd_jan, type = 'HC0'))

# iv
model_inst_jan <- ivreg(jan_change ~ total_refugees_rate_jan +  house_price + 
                          wage + total_production + population + city|
                          oswiadczenia_rate + house_price + wage + total_production + 
                          population + city, data = total_unemp)
coeftest(model_inst_jan, vcov = vcovHC(model_inst_jan, type = 'HC0'))

# Step 9: Get results for Table 3 -----------------------------------------

# fe
model_fe_oct <- plm(oct_rate ~ total_refugees_rate_oct + year, index = "Kod", 
                    model = "within", data = total_unemp_fixed_oct)
coeftest(model_fe_oct, vcov = vcovHC(model_fe_oct, type = 'HC0'))

# fd
model_fd_oct <- lm(oct_change ~ total_refugees_rate_oct +  house_price + wage + 
                     total_production + population + city, data = total_unemp)
coeftest(model_fd_oct, vcov = vcovHC(model_fd_oct, type = 'HC0'))

# iv
model_inst_oct <- ivreg(oct_change ~ total_refugees_rate_oct +  house_price + 
                          wage + total_production + population + city|
                          oswiadczenia_rate + house_price + wage + total_production + 
                          population + city, data = total_unemp)
coeftest(model_inst_oct, vcov = vcovHC(model_inst_oct, type = 'HC0'))