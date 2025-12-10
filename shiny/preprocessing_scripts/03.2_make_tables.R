# make reactable tables of trends


# libraries ---------------------------------------------------------------
library(dplyr)
library(reactable)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")


# Upload data -------------------------------------------------------------
trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()

trends <- trends %>%
  mutate(region = recode(region,
                         "Canada" = "Region1",
                         "USA" = "Region2"))

# Region 1 ---------------------------------------------------------------------

## add annotations ============================
trends_Region1 <- trends %>%
  filter(region == "Region1") %>%
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

## add arrows ============================
trends_Region1 <- trends_Region1 %>%
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


### make table ============================
reactable_Region1 <- trends_Region1 %>%
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

reactable_Region1

# Region2 ---------------------------------------------------------------------

## add annotations ============================
trends_Region2 <- trends %>%
  filter(region == "Region2") %>%
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



## add arrows ============================
trends_Region2 <- trends_Region2 %>%
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

## make table ============================

reactable_Region2 <- trends_Region2 %>%
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

reactable_Region2


# save both ---------------------------------------------------------------
saveRDS(reactable_Region1,
        "shiny/www/premade_plots/Region1_table.rds")
saveRDS(reactable_Region2,
        "shiny/www/premade_plots/Region2_table.rds")
rm(list = ls())




