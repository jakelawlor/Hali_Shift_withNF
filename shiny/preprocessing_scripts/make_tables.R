# make reactable tables of trends


trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()
trends %>% glimpse()


trends_can <- trends %>%
  filter(region == "Canada") %>%
  mutate(pattern = case_when(
    indicator == "Abundance" ~ ifelse(sign(estimate) == 1, "Increasing","Decreasing"),
    indicator == "Area Occupied" ~ ifelse(sign(estimate) == 1,"Expanding",'Conracting'),
    indicator == "Leading_Edge_East" ~ ifelse(sign(estimate) == 1,"Eastward Shift",'Westward Shift'),
    indicator == "Leading_Edge_North" ~ ifelse(sign(estimate) == 1,"Northward Shift",'Southward Shift'),
    indicator == "COG_East" ~ ifelse(sign(estimate) == 1,"Eastward Shift",'Westward Shift'),
    indicator == "COG_North" ~ ifelse(sign(estimate) == 1,"Northward Shift",'Southward Shift'),
    indicator == "Distance to Border" ~ ifelse(sign(estimate) == 1,"Moving away from border",'Moving towards border'),
    indicator == "Abundance Weighted Depth" ~ ifelse(sign(estimate) == 1,"Shifting towards deeper water",'Shifting towards shallower water'),
    
  )) %>%
  
  mutate(indicator2 = case_when(
    indicator == "Abundance" ~ "Trend in Abundance",
    indicator == "Abundance Weighted Depth" ~ "Average Depth",
    indicator == "Area Occupied" ~ "Area Occupied",
    indicator == "COG_East" ~ "Centre of Gravity Longitude",
    indicator == "COG_North" ~ "Centre of Gravity Latitude", 
    indicator == "Distance to Border" ~ "Distance to Border",
    indicator == 'Leading_Edge_East' ~ "Leading Edge Longitude",
    indicator == 'Leading_Edge_North' ~ "Leading Edge Latitude", 
    indicator == 'Trailing_Edge_East' ~ "Trailing Edge Longitude",
    indicator == 'Trailing_Edge_North' ~ "Trailing Edge Latitude", 
    
    
    
  ))  %>%
  mutate(indicator2 = factor(indicator2, 
                             levels = c("Trend in Abundance",
                                        "Average Depth",
                                        "Area Occupied",
                                        "Centre of Gravity Longitude",
                                        "Centre of Gravity Latitude",
                                        "Distance to Border",
                                        "Leading Edge Longitude",
                                        "Leading Edge Latitude",
                                        "Trailing Edge Longitude",
                                        "Trailing Edge Latitude"))) %>%
  select(-indicator) %>%
  rename( indicator = indicator2)
trends_can %>% glimpse()
trends_can <- trends_can %>%
  mutate(arrow = case_when(
    p_value <= 0.05 & indicator == "Trend in Abundance" ~ ifelse(sign(estimate) == 1, "↑","↓"),
    p_value <= 0.05 & indicator == "Area Occupied" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Leading Edge Longitude" ~ ifelse(sign(estimate) == 1,"→","←"),
    p_value <= 0.05 & indicator == "Leading Edge Latitude" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Centre of Gravity Longitude" ~ ifelse(sign(estimate) == 1,"→","←"),
    p_value <= 0.05 & indicator == "Centre of Gravity Latitude" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Distance to Border" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Average Depth" ~ ifelse(sign(estimate) == 1,"↓","↑"),
    TRUE ~ ""
    
  )) 



library(reactable)

trends_can %>% glimpse()

reactable_can <- trends_can %>%
  select(indicator,arrow, pattern, estimate, conf_low, conf_high, p_value, p_significant) %>%
  reactable(
    columns = list(
    indicator = colDef(name = "Indicator",
                       minWidth = 150),
    arrow = colDef(name = "",
                   maxWidth = 30),
    pattern = colDef(name = "Pattern",
                     minWidth = 200),
    estimate = colDef(name = "Trend",
                      format = colFormat(digits = 2)),
    conf_low = colDef(name = "Low Interval",
                      format = colFormat(digits = 2)),
    
    conf_high = colDef(name = "High Interval",
                      format = colFormat(digits = 2)),
    p_value = colDef(name = "p value",
                     cell = function(value) {
                       if (value < 0.01) {
                         "<.01"
                       } else if (value < 0.05) {
                         "<.05"
                       } else {
                         # Format other values as needed (e.g., round to 2 decimal places)
                         formatC(value, digits = 2, format = "f")
                       }
                     }),
    p_significant = colDef(name = "Significance")
  
    )
  )

reactable_can

# USA ---------------------------------------------------------------------

trends_usa <- trends %>%
  filter(region == "USA") %>%
  mutate(pattern = case_when(
    indicator == "Abundance" ~ ifelse(sign(estimate) == 1, "Increasing","Decreasing"),
    indicator == "Area Occupied" ~ ifelse(sign(estimate) == 1,"Expanding",'Conracting'),
    indicator == "Trailing_Edge_East" ~ ifelse(sign(estimate) == 1,"Eastward Shift",'Westward Shift'),
    indicator == "Trailing_Edge_North" ~ ifelse(sign(estimate) == 1,"Northward Shift",'Southward Shift'),
    indicator == "COG_East" ~ ifelse(sign(estimate) == 1,"Eastward Shift",'Westward Shift'),
    indicator == "COG_North" ~ ifelse(sign(estimate) == 1,"Northward Shift",'Southward Shift'),
    indicator == "Distance to Border" ~ ifelse(sign(estimate) == 1,"Moving away from border",'Moving towards border'),
    indicator == "Abundance Weighted Depth" ~ ifelse(sign(estimate) == 1,"Shifting towards deeper water",'Shifting towards shallower water'),
    
  )) %>%
  
  mutate(indicator2 = case_when(
    indicator == "Abundance" ~ "Trend in Abundance",
    indicator == "Abundance Weighted Depth" ~ "Average Depth",
    indicator == "Area Occupied" ~ "Area Occupied",
    indicator == "COG_East" ~ "Centre of Gravity Longitude",
    indicator == "COG_North" ~ "Centre of Gravity Latitude", 
    indicator == "Distance to Border" ~ "Distance to Border",
    indicator == 'Trailing_Edge_East' ~ "Trailing Edge Longitude",
    indicator == 'Trailing_Edge_North' ~ "Trailing Edge Latitude", 
    indicator == 'Trailing_Edge_East' ~ "Trailing Edge Longitude",
    indicator == 'Trailing_Edge_North' ~ "Trailing Edge Latitude", 
    
    
    
  ))  %>%
  mutate(indicator2 = factor(indicator2, 
                             levels = c("Trend in Abundance",
                                        "Average Depth",
                                        "Area Occupied",
                                        "Centre of Gravity Longitude",
                                        "Centre of Gravity Latitude",
                                        "Distance to Border",
                                        "Leading Edge Longitude",
                                        "Leading Edge Latitude",
                                        "Trailing Edge Longitude",
                                        "Trailing Edge Latitude"))) %>%
  select(-indicator) %>%
  rename( indicator = indicator2)

trends_usa2 <- trends_usa %>%
  mutate(symbol = case_when(
    pattern == "increasing"       ~ "↑",
    pattern == "decreasing"       ~ "↓",
    pattern == "eastward shift"   ~ "→",
    pattern == "westward shift"   ~ "←",
    TRUE                          ~ ""
  ))


trends_usa <- trends_usa %>%
  mutate(arrow = case_when(
    p_value <= 0.05 & indicator == "Trend in Abundance" ~ ifelse(sign(estimate) == 1, "↑","↓"),
    p_value <= 0.05 & indicator == "Area Occupied" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Trailing Edge Longitude" ~ ifelse(sign(estimate) == 1,"→","←"),
    p_value <= 0.05 & indicator == "Trailing Edge Latitude" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Centre of Gravity Longitude" ~ ifelse(sign(estimate) == 1,"→","←"),
    p_value <= 0.05 & indicator == "Centre of Gravity Latitude" ~ ifelse(sign(estimate) == 1,"↑","↓"),
    p_value <= 0.05 & indicator == "Distance to Border" ~ ifelse(sign(estimate) == 1,"↓","↑"),
    p_value <= 0.05 & indicator == "Average Depth" ~ ifelse(sign(estimate) == 1,"↓","↑"),
    TRUE ~ ""
    
  )) 


reactable_usa <- trends_usa %>%
  select(indicator, arrow, pattern, estimate, conf_low, conf_high, p_value, p_significant) %>%
  reactable(
    columns = list(
      indicator = colDef(name = "Indicator",
                         minWidth = 150),
      arrow = colDef(name = "",
                     maxWidth = 30),
      pattern = colDef(name = "Pattern",
                       minWidth = 200),
      estimate = colDef(name = "Trend",
                        format = colFormat(digits = 2)),
      conf_low = colDef(name = "Low Interval",
                        format = colFormat(digits = 2)),
      
      conf_high = colDef(name = "High Interval",
                         format = colFormat(digits = 2)),
      p_value = colDef(name = "p value",
                       cell = function(value) {
                         if (value < 0.01) {
                           "<.01"
                         } else if (value < 0.05) {
                           "<.05"
                         } else {
                           # Format other values as needed (e.g., round to 2 decimal places)
                           formatC(value, digits = 2, format = "f")
                         }
                       }),
      p_significant = colDef(name = "Significance")
      
    )
  )

reactable_usa


# save both ---------------------------------------------------------------
saveRDS(reactable_can,
        "shiny/www/premade_plots/can_table.rds")
saveRDS(reactable_usa,
        "shiny/www/premade_plots/usa_table.rds")





