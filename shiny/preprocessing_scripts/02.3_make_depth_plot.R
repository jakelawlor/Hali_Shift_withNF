# # make interactive plot of average depth



# libraries ---------------------------------------------------------------
library(plotly)
library(dplyr)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")


# upload data -------------------------------------------------------------
awd <- readr::read_csv("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_AWD_manipulated.csv") %>%
  janitor::clean_names()


# start plot --------------------------------------------------------------
p_depth <- plot_ly(data = awd,
                   colors = c("Region1" = Region1_col,"Region2" = Region2_col)) %>%
  
  # Region 1 traces ---------
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = awd %>% filter(region == "Region1"),
    x = ~year,
    ymin = ~depth_q5,
    ymax = ~depth_q95,
    color = ~region,
    legendgroup = ~region,
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(color = 'rgba(0,0,0,0)')
  ) %>%
  
  # Lines for the mean estimates
  add_lines(
    data = awd %>% filter(region == "Region1"),
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
  
  
  # Region 2 traces ---------
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = awd %>% filter(region == "Region2"),
    x = ~year,
    ymin = ~depth_q5,
    ymax = ~depth_q95,
    color = ~region,
    #split = ~region,
    legendgroup = ~region,
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(color = 'rgba(0,0,0,0)')
  ) %>%
  
  # Lines for the mean estimates
  add_lines(
    data = awd %>% filter(region == "Region2"),
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
  
  # adjust layout -----
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


rm(list = ls())
