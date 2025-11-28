# make plot of slopes



trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()
trends %>% glimpse()

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

trends <- trends %>%
  mutate(upper_offset  = conf_high - estimate,
         lower_offset  = estimate - conf_low)

p_slopes <- plot_ly(data = trends,
        colors = c("Canada" = "#D60E0A","USA" = "#0471A6")) %>%
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
                       x = .5,
                       y = 1.05),
         xaxis = list(title = "Effect"),
         yaxis = list(title = ""),
         margin = list(t=0,b=0,l=0,r=0))

p_slopes

saveRDS(p_slopes,
        "shiny/www/premade_plots/slope_plot.rds")

# Create sample data
data <- data.frame(
  x = 1:5,
  y = c(10, 12, 8, 15, 11),
  y_error = c(1, 1.5, 0.8, 2, 1.2), # Symmetric error for y
  x_error = c(0.2, 0.3, 0.1, 0.4, 0.25) # Symmetric error for x
)

# Create a Plotly scatter plot with error bars
fig <- plot_ly(data, x = ~x, y = ~y, type = 'scatter', mode = 'markers') %>%
  add_trace(
    error_y = list(array = ~y_error, color = 'red'), # Add symmetric y-error bars
    error_x = list(array = ~x_error, color = 'blue')  # Add symmetric x-error bars
  )

fig
