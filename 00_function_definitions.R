# Step 0: function definitions --------------------------------------------
library(dplyr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(stringr)
library(tidyr)
library(randomizr)
library(lme4)
library(AER)
library(reshape2)
library(plm)
library(tibble)
library(rgdal)
library(rgeos)
library(tmap)

# this function reads yearly unemployment data given the file path
read_yearly_data <- function(file_path, k){
  
  data_raw <- read_excel(file_path, 2)
  data_return <- data_raw[k:nrow(data_raw), ]
  data_return[, 3:ncol(data_return)] <- sapply(data_return[, 3:ncol(data_return)], as.numeric)
  
  return(data_return)
}

# this function computes the differences in the unemployment for a given dataset
create_change_data <- function(data_2022){
  
  data_2022_change <- data_2022 %>% 
    mutate(styczeń_change = luty - styczeń,
           luty_change = marzec - luty, 
           marzec_change = kwiecień - marzec,
           kwiecień_change = maj - kwiecień,
           maj_change = czerwiec - maj,
           czerwiec_change = lipiec - czerwiec,
           lipiec_change = sierpień - lipiec,
           sierpień_change = wrzesień - sierpień,
           wrzesień_change = październik - wrzesień,
           październik_change = listopad - październik,
           listopad_change = grudzień - listopad) %>% 
    select(Kod, Nazwa, styczeń_change, luty_change, marzec_change, kwiecień_change, 
           maj_change, czerwiec_change, lipiec_change, sierpień_change, wrzesień_change, 
           październik_change, listopad_change)
  
  return(data_2022_change)
}

# this function changes the shape of data from wide to long 
unemp_change_to_long <- function(unemp, unemp_name){
  
  unemp_long <- unemp %>% 
    select(Kod, Nazwa, kwiecień_change, maj_change, czerwiec_change, lipiec_change, 
           sierpień_change, wrzesień_change, październik_change, listopad_change) %>% 
    melt(id.vars = c("Kod", "Nazwa"))
  
  names(unemp_long)[3:4] <- c("miesiac", unemp_name) 
  return(unemp_long)
}

# this function changes the shape of data from wide to long 
unemp_to_long <- function(unemp, unemp_name){
  
  unemp_long <- unemp %>% 
    select(Kod, Nazwa, kwiecień, maj, czerwiec, lipiec, sierpień, wrzesień,
           październik, listopad) %>% 
    melt(id.vars = c("Kod", "Nazwa"))
  
  names(unemp_long)[3:4] <- c("miesiac", unemp_name) 
  return(unemp_long)
}

# this function adds some additional variables to data
add_variables <- function(my_data){
  my_data <- my_data %>% 
    mutate(
      total_refugees_rate = total_refugees/total_production,
      total_refugees_change_rate = total_refugees_change/total_production,
      men_refugees_rate = men_refugees/total_production,
      men_refugees_change_rate = men_refugees_change/total_production,
      women_refugees_rate = women_refugees/total_production,
      women_refugees_change_rate = women_refugees_change/total_production,
      
      unemp_total_rate = (unemp_total/total_production)*1000,
      unemp_high_rate = (unemp_total_high/total_production)*1000,
      unemp_policealne_rate = (unemp_total_policealne/total_production)*1000,
      unemp_medium_rate = (unemp_total_medium/total_production)*1000,
      unemp_zawodowe_rate = (unemp_total_zawodowe/total_production)*1000,
      unemp_low_rate = (unemp_total_low/total_production)*1000,
      
      unemp_total_men_rate = (unemp_total_men/total_production)*1000,
      unemp_high_men_rate = (unemp_total_high_men/total_production)*1000,
      unemp_policealne_men_rate = (unemp_total_policealne_men/total_production)*1000,
      unemp_medium_men_rate = (unemp_total_medium_men/total_production)*1000,
      unemp_zawodowe_men_rate = (unemp_total_zawodowe_men/total_production)*1000,
      unemp_low_men_rate = (unemp_total_low_men/total_production)*1000,
      
      unemp_total_women_rate = (unemp_total_women/total_production)*1000,
      unemp_high_women_rate = (unemp_total_high_women/total_production)*1000,
      unemp_policealne_women_rate = (unemp_total_policealne_women/total_production)*1000,
      unemp_medium_women_rate = (unemp_total_medium_women/total_production)*1000,
      unemp_zawodowe_women_rate = (unemp_total_zawodowe_women/total_production)*1000,
      unemp_low_women_rate = (unemp_total_low_women/total_production)*1000
    )
  
  return(my_data)
} 

# this function adds some additional variables to data
add_variables_change <- function(my_data_change){
  my_data_change <- my_data_change %>% 
    mutate(
      total_refugees_rate = total_refugees/total_production,
      total_refugees_change_rate = total_refugees_change/total_production,
      men_refugees_rate = men_refugees/total_production,
      men_refugees_change_rate = men_refugees_change/total_production,
      women_refugees_rate = women_refugees/total_production,
      women_refugees_change_rate = women_refugees_change/total_production,
      
      unemp_total_change_rate = (unemp_total_change/total_production)*1000,
      unemp_high_change_rate = (unemp_high_change/total_production)*1000,
      unemp_policealne_change_rate = (unemp_policealne_change/total_production)*1000,
      unemp_medium_change_rate = (unemp_medium_change/total_production)*1000,
      unemp_zawodowe_change_rate = (unemp_zawodowe_change/total_production)*1000,
      unemp_low_change_rate = (unemp_low_change/total_production)*1000,
      
      unemp_total_men_change_rate = (unemp_total_men_change/total_production)*1000,
      unemp_high_men_change_rate = (unemp_high_men_change/total_production)*1000,
      unemp_policealne_men_change_rate = (unemp_policealne_men_change/total_production)*1000,
      unemp_medium_men_change_rate = (unemp_medium_men_change/total_production)*1000,
      unemp_zawodowe_men_change_rate = (unemp_zawodowe_men_change/total_production)*1000,
      unemp_low_men_change_rate = (unemp_low_men_change/total_production)*1000,
      
      unemp_total_women_change_rate = (unemp_total_women_change/total_production)*1000,
      unemp_high_women_change_rate = (unemp_high_women_change/total_production)*1000,
      unemp_policealne_women_change_rate = (unemp_policealne_women_change/total_production)*1000,
      unemp_medium_women_change_rate = (unemp_medium_women_change/total_production)*1000,
      unemp_zawodowe_women_change_rate = (unemp_zawodowe_women_change/total_production)*1000,
      unemp_low_women_change_rate = (unemp_low_women_change/total_production)*1000
    )
  
  return(my_data_change)
} 

# this is used to compute the number of economically active natives
get_active_population_estimate <- function(unemp_rate, unemp_total){
  
  active_population_estimate <- unemp_rate %>% 
    left_join(unemp_total, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    mutate(active_jan = (styczeń.y/styczeń.x)*100,
           active_feb = (luty.y/luty.x)*100,
           active_mar = (marzec.y/marzec.x)*100,
           active_apr = (kwiecień.y/kwiecień.x)*100,
           active_may = (maj.y/maj.x)*100,
           active_jun = (czerwiec.y/czerwiec.x)*100,
           active_jul = (lipiec.y/lipiec.x)*100,
           active_aug = (sierpień.y/sierpień.x)*100,
           active_sep = (wrzesień.y/wrzesień.x)*100,
           active_oct = (październik.y/październik.x)*100,
           active_nov = (listopad.y/listopad.x)*100,
           active_dec = (grudzień.y/grudzień.x)*100) %>% 
    select(Kod, Nazwa, active_jan, active_feb, active_mar, active_apr, active_may, active_jun,
           active_jul, active_aug, active_sep, active_oct, active_nov, active_dec)
  
  return(active_population_estimate)
}

# function to create an event plot
get_event_plot <- function(variable_type, title, ylab, ylocation){
  
  file_path <- paste0("~/UCL/Year 3/Dissertation/Data/", variable_type)
  
  unemp_rate_2015 <-  read_yearly_data(paste0(file_path, "_2015.xlsx"), 4) 
  unemp_rate_2016 <-  read_yearly_data(paste0(file_path, "_2016.xlsx"), 4)
  unemp_rate_2017 <-  read_yearly_data(paste0(file_path, "_2017.xlsx"), 4) 
  unemp_rate_2018 <-  read_yearly_data(paste0(file_path, "_2018.xlsx"), 4)
  unemp_rate_2019 <-  read_yearly_data(paste0(file_path, "_2019.xlsx"), 4) 
  unemp_rate_2020 <-  read_yearly_data(paste0(file_path, "_2020.xlsx"), 4)
  unemp_rate_2021 <-  read_yearly_data(paste0(file_path, "_2021.xlsx"), 4)
  unemp_rate_2022 <-  read_yearly_data(paste0(file_path, "_2022.xlsx"), 4)
  unemp_rate_2023 <-  read_yearly_data(paste0(file_path, "_2023.xlsx"), 4)
  
  months <- names(unemp_rate_2015)[3:14]
  
  names(unemp_rate_2015)[3:14] <- 1:12
  names(unemp_rate_2016)[3:14] <- 13:24
  names(unemp_rate_2017)[3:14] <- 25:36
  names(unemp_rate_2018)[3:14] <- 37:48
  names(unemp_rate_2019)[3:14] <- 49:60
  names(unemp_rate_2020)[3:14] <- 61:72
  names(unemp_rate_2021)[3:14] <- 73:84
  names(unemp_rate_2022)[3:14] <- 85:96
  names(unemp_rate_2023)[3:14] <- 97:108
  
  treatment_assignment <- data_ref_jan %>% 
    mutate(treat = if_else(total_refugees_rate_jan >= 
                             median(data_ref_jan$total_refugees_rate_jan), 1, 0)) %>% 
    select(Kod, treat)
  
  data_unemp <- unemp_rate_2015 %>% 
    left_join(unemp_rate_2016, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2017, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2018, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2019, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2020, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2021, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2022, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(unemp_rate_2023, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(treatment_assignment, by = c("Kod" = "Kod"))
  
  treat <- data_unemp %>% 
    filter(treat == 1) %>% 
    select(-Kod, -Nazwa, -treat)
  
  control <- data_unemp %>% 
    filter(treat == 0) %>% 
    select(-Kod, -Nazwa, -treat)
  
  treat_means <- colMeans(treat) 
  control_means <- colMeans(control)
  
  mean_difference <- as.data.frame(treat_means - control_means) %>% 
    mutate(month = rep(months, 9), 
           year = c(rep("2015", 12), rep("2016", 12), rep("2017", 12), rep("2018", 12), 
                    rep("2019", 12), rep("2020", 12), rep("2021", 12), rep("2022", 12), 
                    rep("2023", 12)))
  
  names(mean_difference)[1] <- "mean_diff"
  
  adjustment_model <- lm(mean_diff ~ month + year, data = mean_difference)
  coefficients <- adjustment_model$coefficients[2:12]
  
  coefficients <- c(coefficients, "monthczerwiec" = 0) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    mutate(rowname = gsub("*month", "", rowname))
  
  names(coefficients)[2] <- "adjustment"
  
  mean_difference <- mean_difference %>% 
    left_join(coefficients, by = c("month" = "rowname")) %>%
    mutate(mean_diff_adj = mean_diff - adjustment)
  
  plot(mean_difference$mean_diff_adj, xlab = "Miesiąc", ylab = ylab, 
       xaxt = "n", main = title)
  abline(v=86.5 , col="black", lty=2)
  text(x=86.5, y= ylocation, '  Początek wojny', col = "black")
  axis(1, at=c(1, 13, 25, 37, 49, 61, 73, 85, 97), 
       labels=c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))
}

# function to read raw unemployment data and perpare for regression 
get_data <- function(unemp_type, k){
  
  path_2021 <- paste0("~/UCL/Year 3/Dissertation/Data/", unemp_type, "_2021.xlsx")
  path_2022 <- paste0("~/UCL/Year 3/Dissertation/Data/", unemp_type, "_2022.xlsx")
  path_2023 <- paste0("~/UCL/Year 3/Dissertation/Data/", unemp_type, "_2023.xlsx")
  
  
  unemp_numbers_2021 <- read_yearly_data(path_2021, k) %>% 
    left_join(active_population_estimate_2021, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    mutate(jan_2021 = (styczeń/active_jan)*100, 
           jan_2021_raw = styczeń,
           feb_2021 = (styczeń/active_feb)*100, 
           mar_2021 = (styczeń/active_mar)*100, 
           apr_2021 = (styczeń/active_apr)*100, 
           may_2021 = (styczeń/active_may)*100, 
           jun_2021 = (styczeń/active_jun)*100, 
           jul_2021 = (styczeń/active_jul)*100, 
           aug_2021 = (styczeń/active_aug)*100, 
           sep_2021 = (styczeń/active_sep)*100, 
           oct_2021 = (styczeń/active_oct)*100, 
           nov_2021 = (styczeń/active_nov)*100, 
           dec_2021 = (styczeń/active_dec)*100) %>% 
    select(Kod, Nazwa, jan_2021, feb_2021, mar_2021, apr_2021, may_2021, jun_2021, jul_2021, 
           aug_2021, sep_2021, oct_2021, nov_2021, dec_2021, jan_2021_raw)
  
  unemp_numbers_2022 <- read_yearly_data(path_2022, k) %>%
    left_join(active_population_estimate_2022, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>%
    mutate(jan_2022 = (styczeń/active_jan)*100,
           jan_2022_raw = styczeń,
           feb_2022 = (styczeń/active_feb)*100,
           mar_2022 = (styczeń/active_mar)*100,
           apr_2022 = (styczeń/active_apr)*100,
           may_2022 = (styczeń/active_may)*100,
           jun_2022 = (styczeń/active_jun)*100,
           jul_2022 = (styczeń/active_jul)*100,
           aug_2022 = (styczeń/active_aug)*100,
           sep_2022 = (styczeń/active_sep)*100,
           oct_2022 = (styczeń/active_oct)*100,
           nov_2022 = (styczeń/active_nov)*100,
           dec_2022 = (styczeń/active_dec)*100) %>%
    select(Kod, Nazwa, jan_2022, feb_2022, mar_2022, apr_2022, may_2022, jun_2022, jul_2022,
           aug_2022, sep_2022, oct_2022, nov_2022, dec_2022, jan_2022_raw)
  
  unemp_numbers_2023 <- read_yearly_data(path_2023, k) %>%
    left_join(active_population_estimate_2023, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>%
    mutate(jan_2023 = (styczeń/active_jan)*100,
           jan_2023_raw = styczeń,
           feb_2023 = (styczeń/active_feb)*100,
           mar_2023 = (styczeń/active_mar)*100,
           apr_2023 = (styczeń/active_apr)*100,
           may_2023 = (styczeń/active_may)*100,
           jun_2023 = (styczeń/active_jun)*100,
           jul_2023 = (styczeń/active_jul)*100,
           aug_2023 = (styczeń/active_aug)*100,
           sep_2023 = (styczeń/active_sep)*100,
           oct_2023 = (styczeń/active_oct)*100,
           nov_2023 = (styczeń/active_nov)*100,
           dec_2023 = (styczeń/active_dec)*100) %>%
    select(Kod, Nazwa, jan_2023, feb_2023, mar_2023, apr_2023, may_2023, jun_2023, jul_2023,
           aug_2023, sep_2023, oct_2023, nov_2023, dec_2023, jan_2023_raw)
  
  final_data <- unemp_numbers_2021 %>%
    left_join(unemp_numbers_2022, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>%
    left_join(unemp_numbers_2023, by = c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>%
    left_join(characteristics, c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>%
    left_join(instrument, c("Kod" = "Kod")) %>%
    mutate(jan_change = jan_2023 - jan_2022,
           jan_change_prev = jan_2022 - jan_2021,
           jan_change_raw = jan_2023_raw - jan_2022_raw,
           jan_change_prev_raw = jan_2022_raw - jan_2021_raw,
           oct_change = oct_2022 - oct_2021,
           dec_change = dec_2022 - dec_2021,
           nov_change = nov_2022 - nov_2021, 
           oswiadczenia_rate = (oswiadczenia/total_production)*100,
           total_refugees_rate_jan = total_refugees_rate_jan*100, 
           total_refugees_rate_oct = total_refugees_rate_oct*100) %>% 
    select(Kod, Nazwa, oct_change, jan_change, jan_change_prev, jan_change_raw, 
           jan_change_prev_raw, jan_2023, jan_2022, jan_2023_raw, jan_2022_raw, jan_2021, 
           dec_change, nov_change, total_refugees_rate_jan, total_refugees_rate_oct, 
           oswiadczenia_rate, house_price, wage, total_production, population, city)
  
  unemp_numbers_2023_2 <- unemp_numbers_2023 %>% 
    mutate(jan_rate = jan_2023, 
           oct_rate = oct_2023,
           jan_rate_raw = jan_2023_raw,
           year = "2023") %>% 
    select(Kod, Nazwa, jan_rate, oct_rate, jan_rate_raw, year)
  
  unemp_numbers_2022_2 <- unemp_numbers_2022 %>% 
    mutate(jan_rate = jan_2022, 
           oct_rate = oct_2022,
           jan_rate_raw = jan_2022_raw,
           year = "2022") %>% 
    select(Kod, Nazwa, jan_rate, oct_rate, jan_rate_raw, year)
  
  unemp_numbers_2021_2 <- unemp_numbers_2021 %>% 
    mutate(jan_rate = jan_2021, 
           oct_rate = oct_2021,
           jan_rate_raw = jan_2021_raw,
           year = "2021") %>% 
    select(Kod, Nazwa, jan_rate, oct_rate, jan_rate_raw, year)
  
  final_data_2 <- bind_rows(unemp_numbers_2021_2, unemp_numbers_2022_2, unemp_numbers_2023_2) %>% 
    left_join(characteristics, c("Kod" = "Kod", "Nazwa" = "Nazwa")) %>% 
    left_join(instrument, c("Kod" = "Kod")) %>% 
    mutate(total_refugees_rate_jan = if_else(year == "2023", total_refugees_rate_jan*100, 0), 
           total_refugees_rate_oct = if_else(year == "2022", total_refugees_rate_oct*100, 0),
           oswiadczenia_rate = (oswiadczenia/total_production)*100)
  
  return(list(final_data, final_data_2))
}