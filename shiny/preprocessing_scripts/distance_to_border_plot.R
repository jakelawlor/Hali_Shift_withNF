# premake distance to border plot

dtob<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/POC_DtoB.csv")) %>%
  janitor::clean_names()
dtob %>% glimpse()

### Timeseries: Distance to Shared Border 

dtob %>%
ggplot(aes(x = year, y = dist_mean, 
           colour = region, group = region)) +
  geom_line() +
  geom_ribbon(aes(ymin = dist_q5,
                  ymax = dist_q95,
                  fill = region),
              alpha = .2) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.8)+
  theme_bw() +
  labs(x = "Year", y = "Distance (km)",
       colour = "Region", fill = "Region")

# add variable for furthest distance to border (q95 for canada, q5 for US)
dtob <- dtob %>%
  mutate(furthest = ifelse(region == "Canada",dist_q95, dist_q5))

p_d_to_b <- plot_ly(data = dtob,
        colors = c("Canada" = "#D60E0A","USA" = "#0471A6")) %>%
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = dtob %>% filter(region == "Canada"),
    x = ~year,
    ymin = ~dist_q5,
    ymax = ~dist_q95,
    color = ~region,
    #split = ~region,
    legendgroup = ~region,
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(color = 'rgba(0,0,0,0)')#,
    # fillcolor = ~ifelse(Region == "US", "rgba(7,164,181,0.2)", "rgba(239,85,59,0.2)")
  ) %>%
  add_ribbons(
    data = dtob %>% filter(region == "USA"),
    x = ~year,
    ymin = ~dist_q5,
    ymax = ~dist_q95,
    color = ~region,
    #split = ~region,
    legendgroup = ~region,
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(color = 'rgba(0,0,0,0)')#,
    # fillcolor = ~ifelse(Region == "US", "rgba(7,164,181,0.2)", "rgba(239,85,59,0.2)")
  ) %>%
  # Lines for the mean estimates
  add_lines(
    data = dtob,
    x = ~year,
    y = ~dist_mean,
    color = ~region,
    #split = ~Region,
    legendgroup = ~region,
    showlegend = TRUE,
    text = ~paste0("<b>", region, "</b>:<br> ",
                   "Mean distance: ", round(dist_mean),"km<br>",
                   "Furthest: ", round(furthest),"km"),
    hoverinfo = "x+text",
    line = list(width = 2)
  )   %>%
  layout(
    margin = list(l = 50, r = 0, t = 40, b = 0),
    title = "Distance to Border",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Distance to Nearest Border Point (m)"),
    hovermode = "x unified",
    legend = list(title = list(text = "Region"))
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(legend = list(orientation = "v", x = 0.04, y = 0.97)) %>%
  add_annotations(
    x = 1995,         # x-coordinate for the annotation
    y = 50,       # y-coordinate for the annotation
    text = "USA/Canada Border", # The text content
    showarrow = FALSE, # Hide the arrow
    xref = "x",    # Reference x-coordinate to the plot's x-axis
    yref = "y",
    font = list(color="black", size=14, style = "bold")# Reference y-coordinate to the plot's y-axis
  ) %>%
  layout(
    shapes = list(
      list(
        type = "line",
        x0 = min(1990), x1 = max(2023), # Span the entire x-range
        y0 = 0, y1 = 0,                     # Position at y=0
        line = list(color = "black", dash = "dash", width = 2) # Customize line style
      )
    )
  )


p_d_to_b

saveRDS(p_d_to_b,
        "shiny/www/premade_plots/distance_to_border_plot.rds")
