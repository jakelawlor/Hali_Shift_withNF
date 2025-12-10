# # make interactive plot of distance to border


# libraries ---------------------------------------------------------------
library(plotly)
library(dplyr)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")



# upload data -------------------------------------------------------------
dtob<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_DtoB_manipulated.csv")) %>%
  janitor::clean_names()


# add variable for furthest distance to border (q95 for Region1, q5 for US)
dtob <- dtob %>%
  mutate(furthest = ifelse(region == "Region1",dist_q95, dist_q5))



p_d_to_b <- plot_ly(data = dtob,
        colors = c("Region1" = Region1_col,"Region2" = Region2_col)) %>%
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    data = dtob %>% filter(region == "Region1"),
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
    data = dtob %>% filter(region == "Region2"),
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
 # layout(legend = list(orientation = "v", x = 0.85, y = 0.05)) %>%
  add_annotations(
    x = 1997,         # x-coordinate for the annotation
    y = -50,       # y-coordinate for the annotation
    text = "Region2/Region1 Border", # The text content
    showarrow = FALSE, # Hide the arrow
    xref = "x",    # Reference x-coordinate to the plot's x-axis
    yref = "y",
    font = list(color="black", size=14, style = "bold")
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
rm(list = ls())
