source("00_function_definitions.R")

# Step 2: Load and prepare the instrument  --------------------------------
instrument <- read_excel("~/UCL/Year 3/Dissertation/Data/instrument_2021.xlsx", 6, 
                         col_types = c("text", "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", "numeric"))
instrument <- instrument[4:(nrow(instrument) - 1), c(1, 2, 8)]
names(instrument) <- c("Kod", "Nazwa_2", "oswiadczenia")
instrument <- instrument %>% 
  mutate(Kod = paste0(Kod, "000"))

# Step 3: Clean the refugee data ------------------------------------------
date_list <- c("20220414", "20220502", "20220602", "20220704", "20220802", 
               "20220902", "20221003", "20221102", "20221206", "20230103")
data_list <- list()

# read the refugee excel files
for(i in 1:length(date_list)){
  file_path <- paste0("~/UCL/Year 3/Dissertation/Data/refugees_", date_list[i], ".xlsx")
  data_list[[i]] <- read_excel(file_path)
} 

# get the number of total working-age refugees + working-age women + working-age men
data_list_working <- list()

for(i in 1:length(date_list)){
  data_current <- data_list[[i]]
  data_powiaty <- data_current[3:382, 1:2]
  data_current_working_age <- data_current[3:382, c(22:35, 52:64)]
  data_current_working_age[data_current_working_age == "<2"] <- "1"
  data_current_working_age <- data_current_working_age %>% 
    mutate_all(~as.numeric(as.character(.)))
  data_current_working_age <- data_current_working_age %>% 
    mutate(men_refugees = rowSums(data_current_working_age[, 1:14]), 
           women_refugees = rowSums(data_current_working_age[, 15:27]), 
           total_refugees = rowSums(data_current_working_age[, 1:27])) %>% 
    select(men_refugees, women_refugees, total_refugees)
  data_current_working_age <- bind_cols(data_powiaty, data_current_working_age)
  data_list_working[[i]] <- data_current_working_age
}

# Refugees long version, from April until max
ref_data_long <- data_list_working[[1]]
ref_data_long <- ref_data_long %>% 
  mutate(month = 1)

for (i in 2:length(date_list)){
  
  data_current <- data_list_working[[i]]
  data_current <- data_current %>% 
    mutate(month = i)
  
  ref_data_long <- bind_rows(ref_data_long, data_current)
}

# save as intermediate output
write.xlsx(ref_data_long, "~/UCL/Year 3/Dissertation/Intermediate Data/refugees_total.xlsx")
