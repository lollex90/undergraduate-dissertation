source("00_function_definitions.R")

# Step 10: Produce Table 6 ------------------------------------------------

# load data
total_offer <- get_data("job_offer", 5)[[1]]
total_offer_fixed <- get_data("job_offer", 5)[[2]]

total_offer_fixed_jan <- total_offer_fixed %>% 
  filter(year == "2022" | year == "2023")

total_offer_fixed_oct <- total_offer_fixed %>% 
  filter(year == "2021" | year == "2022")

# fe
model_fe_jan <- plm(jan_rate_raw ~ total_refugees_rate_jan + year, index = "Kod", 
                    model = "within", data = total_offer_fixed_jan)
coeftest(model_fe_jan, vcov = vcovHC(model_fe_jan, type = 'HC0'))

# fd
model_fd_jan <- lm(jan_change_raw ~ total_refugees_rate_jan +  house_price + wage + 
                     total_production + population + city, data = total_offer)
coeftest(model_fd_jan, vcov = vcovHC(model_fd_jan, type = 'HC0'))

# iv
model_inst_jan <- ivreg(jan_change_raw ~ total_refugees_rate_jan +  house_price + 
                          wage + total_production + population + city|
                          oswiadczenia_rate + house_price + wage + total_production + 
                          population + city, data = total_offer)
coeftest(model_inst_jan, vcov = vcovHC(model_inst_jan, type = 'HC0'))

# Step 11: Produce Table 1 ------------------------------------------------
mean(total_unemp$jan_2022)
sd(total_unemp$jan_2022)

mean(total_unemp$jan_2023)
sd(total_unemp$jan_2023)

mean(total_offer$jan_2022_raw)
sd(total_offer$jan_2022_raw)

mean(total_offer$jan_2023_raw)
sd(total_offer$jan_2023_raw)

mean(total_unemp$total_refugees_rate_jan)
sd(total_unemp$total_refugees_rate_jan)

mean(total_unemp$oswiadczenia_rate)
sd(total_unemp$oswiadczenia_rate)

mean(total_unemp$wage)
sd(total_unemp$wage)

mean(total_unemp$house_price)
sd(total_unemp$house_price)

mean(total_unemp$population)
sd(total_unemp$population)

mean(total_unemp$total_production)
sd(total_unemp$total_production)