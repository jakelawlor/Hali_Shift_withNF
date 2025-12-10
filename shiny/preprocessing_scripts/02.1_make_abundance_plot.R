# make abundance plot


# libraries ---------------------------------------------------------------
library(dplyr)
library(plotly)
library(scales)



# get plot colors ---------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")


# upload data -------------------------------------------------------------
abundance <- readr::read_csv("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_Abundance_manipulated.csv")


# begin plot --------------------------------------------------------------
p_abundance <- plot_ly(data = abundance,
                       colors = c("Region1" = Region1_col,"Region2" = Region2_col)
                       ) %>%
  # Ribbons for SD range (hidden from legend)
  add_ribbons(
    x = ~Year,
    ymin = ~(Estimate - SD),
    ymax = ~(Estimate + SD),
    color = ~Region,
    #split = ~Region,
    legendgroup = ~Region,
    hoverinfo = "none",
    showlegend = FALSE,
    line = list(color = 'rgba(0,0,0,0)')#,
    # fillcolor = ~ifelse(Region == "US", "rgba(7,164,181,0.2)", "rgba(239,85,59,0.2)")
  ) %>%
  # Lines for the mean estimates
  add_lines(
    x = ~Year,
    y = ~Estimate,
    color = ~Region,
    #split = ~Region,
    legendgroup = ~Region,
    showlegend = TRUE,
    text = ~paste0("<b>", Region, "</b>: ",
                   scales::label_number( scale_cut = cut_long_scale(),accuracy = .1)(Estimate)," ± ",
                   scales::label_number( scale_cut = cut_long_scale(),accuracy = .1)(SD)),
    hoverinfo = "x+text",
    line = list(width = 2)
  ) %>%
  layout(
    #    title = "Total Abundance",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Abundance Estimate"),
    hovermode = "x unified",
    legend = list(title = list(text = "Region")),
    margin = list(l = 40, r = 0, t = 0, b = 0)
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(legend = list(orientation = "v", x = 0.04, y = 0.94))


p_abundance

saveRDS(p_abundance,
        "shiny/www/premade_plots/abundance_plot.rds")


rm(list = ls())