# make plot of slopes


# libraries ---------------------------------------------------------------
library(dplyr)
library(plotly)


# global plot opts -------------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")



# upload data -------------------------------------------------------------
trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()

trends <- trends %>%
  mutate(region = recode(region,
                         "Canada" = "Region1",
                         "USA" = "Region2"))

unique(trends$indicator)
trends <- trends %>%
  
  mutate(indicator2 = case_when(
    indicator == "Abundance" ~ "Trend in Abundance",
    indicator == "Abundance Weighted Depth" ~ "Average Depth",
    indicator == "Area Occupied" ~ "Area Occupied",
    indicator == "COG_East" ~ "Centre of Gravity East",
    indicator == "COG_North" ~ "Centre of Gravity North", 
    indicator == "Distance to Border" ~ "Distance to Border",
    indicator == 'Leading_Edge_East' ~ "Leading Edge East",
    indicator == 'Leading_Edge_North' ~ "Leading Edge North", 
    indicator == 'Trailing_Edge_East' ~ "Trailing Edge East",
    indicator == 'Trailing_Edge_North' ~ "Trailing Edge North", 
    

    
  ))  %>%
  mutate(indicator2 = factor(indicator2, 
                             levels = c("Trend in Abundance",
                                        "Average Depth",
                                        "Area Occupied",
                                        "Centre of Gravity East",
                                        "Centre of Gravity North",
                                        "Distance to Border",
                                        "Leading Edge East",
                                        "Leading Edge North",
                                        "Trailing Edge East",
                                        "Trailing Edge North"))) %>%
  select(-indicator) %>%
  rename( indicator = indicator2)

# make offset cols
trends <- trends %>%
  mutate(upper_offset  = conf_high - estimate,
         lower_offset  = estimate - conf_low)



# make plot ---------------------------------------------------------------
p_slopes <- plot_ly(data = trends,
        colors = c("Region1" = Region1_col,"Region2" = Region2_col)) %>%
  add_markers(
    x = ~estimate,
    y = ~indicator,
    color = ~region,
    marker = list(size = 12),
    #error_x = ~list(array = std_error),
    error_x = ~list(symmetric = FALSE,
                    arrayminus = lower_offset, 
                    array = upper_offset,
                    width = 4),
    text = ~paste0("<b>",region,"</b><br>",
                   
                   "Stardardized Slope: ", round(estimate,2), " ± ",round(std_error),"<br>",
                   p_significant," effect"),
    hoverinfo = "text"
  ) %>%
  config(displayModeBar = F) %>%
  layout(legend = list(orientation = "h",
                       x = .05,
                       y = 1.05),
         xaxis = list(title = "Effect"),
         yaxis = list(title = ""),
         margin = list(r=0,t=0,l=-20,b=0)) 

p_slopes

saveRDS(p_slopes,
        "shiny/www/premade_plots/slope_plot.rds")
rm(list = ls())
