source("00_function_definitions.R")

# Step 12: F-test and error term correlation  -----------------------------
first_stage <- lm(total_refugees_rate_jan ~ house_price + wage + total_production
                  + population + city + oswiadczenia_rate, data = total_unemp)
first_Stage_restricted <- lm(total_refugees_rate_jan ~ house_price + wage + 
                               total_production + population + city, data = total_unemp)

waldtest(first_stage, first_Stage_restricted)

# Step 13: Results for Table 4 --------------------------------------------
low_unemp <- get_data("unemployed_low", 5)[[1]]
high_unemp <- get_data("unemployed_high", 5)[[1]]
policealne_unemp <- get_data("unemployed_policealne", 5)[[1]]
medium_unemp <- get_data("unemployed_medium", 5)[[1]]
zawodowe_unemp <- get_data("unemployed_zawodowe", 5)[[1]]
medium_unemp <- policealne_unemp %>% 
  mutate(jan_change = policealne_unemp$jan_change + zawodowe_unemp$jan_change + 
           medium_unemp$jan_change)

low_unemp_fixed <- get_data("unemployed_low", 5)[[2]]
high_unemp_fixed <- get_data("unemployed_high", 5)[[2]]
policealne_unemp_fixed <- get_data("unemployed_policealne", 5)[[2]]
medium_unemp_fixed <- get_data("unemployed_medium", 5)[[2]]
zawodowe_unemp_fixed <- get_data("unemployed_zawodowe", 5)[[2]]
medium_unemp_fixed <- policealne_unemp_fixed %>% 
  mutate(jan_rate = policealne_unemp_fixed$jan_rate + zawodowe_unemp_fixed$jan_rate + 
           medium_unemp_fixed$jan_rate)

high_unemp_fixed_jan <- high_unemp_fixed %>% 
  filter(year == "2022" | year == "2023")

low_unemp_fixed_jan <- low_unemp_fixed %>% 
  filter(year == "2022" | year == "2023")

medium_unemp_fixed_jan <- medium_unemp_fixed %>% 
  filter(year == "2022" | year == "2023")

# fe
model_fe_high <- plm(jan_rate ~ total_refugees_rate_jan + year, index = "Kod", 
                     model = "within", data = high_unemp_fixed_jan)
model_fe_low <- plm(jan_rate ~ total_refugees_rate_jan + year, index = "Kod",
                    model = "within", data = low_unemp_fixed_jan)
model_fe_medium <- plm(jan_rate ~ total_refugees_rate_jan + year, index = "Kod", 
                       model = "within", data = medium_unemp_fixed_jan)

coeftest(model_fe_high, vcov = vcovHC(model_fe_high, type = 'HC0'))
coeftest(model_fe_low, vcov = vcovHC(model_fe_low, type = 'HC0'))
coeftest(model_fe_medium, vcov = vcovHC(model_fe_medium, type = 'HC0'))


# fd
model_fd_high <- lm(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                      total_production + population + city, data = high_unemp)
model_fd_low <- lm(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                     total_production + population + city, data = low_unemp)
model_fd_medium <- lm(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                        total_production + population + city, data = medium_unemp)

coeftest(model_fd_high, vcov = vcovHC(model_fd_high, type = 'HC0'))
coeftest(model_fd_low, vcov = vcovHC(model_fd_low, type = 'HC0'))
coeftest(model_fd_medium, vcov = vcovHC(model_fd_medium, type = 'HC0'))


# iv
model_inst_high <- ivreg(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                           total_production + population + city| oswiadczenia_rate + 
                           house_price + wage + total_production + population + city,
                         data = high_unemp)
model_inst_low <- ivreg(jan_change ~ total_refugees_rate_jan +  house_price + wage + 
                          total_production + population + city| oswiadczenia_rate + 
                          house_price + wage + total_production + population + city,
                        data = low_unemp)
model_inst_medium <- ivreg(jan_change ~ total_refugees_rate_jan +  house_price + wage +
                             total_production + population + city| oswiadczenia_rate + 
                             house_price + wage + total_production + population + city, 
                           data = medium_unemp)

coeftest(model_inst_high, vcov = vcovHC(model_inst_high, type = 'HC0'))
coeftest(model_inst_low, vcov = vcovHC(model_inst_low, type = 'HC0'))
coeftest(model_inst_medium, vcov = vcovHC(model_inst_medium, type = 'HC0'))