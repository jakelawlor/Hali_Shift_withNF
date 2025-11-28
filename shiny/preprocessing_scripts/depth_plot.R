awd <- readr::read_csv("Data/Data_SHinyApp_Proof_of_Concept/POC_AWD.csv") %>%
  janitor::clean_names()
awd %>% glimpse()

ggplot(awd, 
       aes(x = year, y = depth_median , colour = region, group = region)) +
  geom_line() +
  geom_ribbon(aes(ymin = depth_q5,
                  ymax = depth_q95,
                  fill = region), 
              alpha = 0.25, colour = NA) +
  theme_bw() +
  labs(x = "Year", y = "Depth (m)",
       colour = "Region", fill = "Region")


p_depth <- plot_ly(data = awd,
                   colors = c("Canada" = "#D60E0A","USA" = "#0471A6")) %>%
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = awd %>% filter(region == "Canada"),
    x = ~year,
    ymin = ~depth_q5,
    ymax = ~depth_q95,
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
    data = awd %>% filter(region == "Canada"),
    x = ~year,
    y = ~depth_median,
    color = ~region,
    #split = ~Region,
    legendgroup = ~region,
    showlegend = TRUE,
    text = ~paste0("<b>", region, "</b>: ",
                   scales::comma(depth_median)),
    hoverinfo = "x+text",
    line = list(width = 2)
  ) %>%
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = awd %>% filter(region == "USA"),
    x = ~year,
    ymin = ~depth_q5,
    ymax = ~depth_q95,
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
    data = awd %>% filter(region == "USA"),
    x = ~year,
    y = ~depth_median,
    color = ~region,
    #split = ~Region,
    legendgroup = ~region,
    showlegend = TRUE,
    text = ~paste0("<b>", region, "</b>: ",
                   scales::comma(depth_median)),
    hoverinfo = "x+text",
    line = list(width = 2)
  ) %>%
  layout(
    margin = list(l = 50, r = 0, t = 40, b = 0),
    title = "Abundance-Weighted Depth",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Depth (m)"),
    hovermode = "x unified",
    legend = list(title = list(text = "Region"))
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(legend = list(orientation = "v", x = 0.88, y = 0.05))


p_depth

saveRDS(p_depth,
        "shiny/www/premade_plots/depth_plot.rds")
