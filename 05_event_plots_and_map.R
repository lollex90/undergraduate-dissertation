source("00_function_definitions.R")

# Step 6: Get event plots -------------------------------------------------
par(mar=c(5.1, 6.1, 4.1, 2.1))

get_event_plot("unemployment_rate", 
               "Dynamika różnicy w stopie bezrobocia rejestrownego \n między powiatami z wysokim i niskim odsetkiem uchodźców", 
               "Różnica w poziomie bezrobocia rejestrowanego \n między powiatami z wysokim i niskim odsetkiem uchodźców", 
               -2.6)
get_event_plot("job_offer", 
               "Dynamika różnicy w liczbie ofert pracy między \n powiatami z wysokim i niskim odsetkiem uchodźców", 
               "Różnica w liczbie ofert pracy między \n powiatami z wysokim i niskim odsetkiem uchodźców", 250)

# Step 7: Produce Map 1 ---------------------------------------------------
Output.Areas <- readOGR("~/UCL/Year 3/Dissertation/Powiaty", "Powiaty")
data_ref_map <- data_ref_jan %>% 
  mutate(Kod = gsub('.{3}$', '', Kod), 
         total_refugees_rate_jan = total_refugees_rate_jan*1000)
merged_data <- merge(Output.Areas, data_ref_map, by.x="JPT_KOD_JE", by.y="Kod")

map1 <- tm_shape(merged_data) +
  tm_borders(alpha = 0.4)+
  tm_fill("total_refugees_rate_jan", palette = "Greys", 
          title = "The number of \nrefugees per 1000 \ncitizens") +
  tm_layout(frame = FALSE, legend.text.size = 0.8, legend.title.size = 0.9)