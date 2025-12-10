# make interactive plot of area used
#
# this plot will be three plots: 
# - total area, 
# - percent of area,
# - area efficiency
# which will be chosen by a dropdown box at the top of the plot


# libraries ---------------------------------------------------------------
library(plotly)
library(dplyr)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")


# upload data -------------------------------------------------------------
area <- readr::read_csv("Data/Data_SHinyApp_Proof_of_Concept/NewData_NoCountries/POC_AreaOccupied_manipulated.csv") %>%
  janitor::clean_names()

# filter to the columns that show area for 90% of abundance
area_90 <- area %>% filter(threshold == 90)
rm(area)


# begin plot --------------------------------------------------------------
fig <- plot_ly()


## ---- 1) Area used (%): traces 1–2 (start visible) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Region1"),
    x = ~year,
    y = ~percent_area_used,
    name = "Region1",
    text = ~paste0(round(percent_area_used), "%"),
    hoverinfo = "x+text",
    line = list(color = Region1_col),
    visible = TRUE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "Region2"),
    x = ~year,
    y = ~percent_area_used,
    name = "Region2",
    text = ~paste0(round(percent_area_used), "%"),
    hoverinfo = "x+text",
    line = list(color = Region2_col),
    visible = TRUE
  )


## ---- 2) Area used (km2): traces 3–4 (start hidden) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Region1"),
    x = ~year,
    y = ~area_threshold,
    name = "Region1",
    text = ~paste0(scales::comma(round(area_threshold)), " km<sup>2</sup>"),
    hoverinfo = "x+text",
    line = list(color = Region1_col),
    visible = FALSE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "Region2"),
    x = ~year,
    y = ~area_threshold,
    name = "Region2",
    text = ~paste0(scales::comma(round(area_threshold)), " km<sup>2</sup>"),
    hoverinfo = "x+text",
    line = list(color = Region2_col),
    visible = FALSE
  )

## ---- 3) area_efficiency: traces 5–6 (start hidden) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Region1"),
    x = ~year,
    y = ~area_efficiency,
    name = "Region1",
    text = ~paste0(scales::comma(round(area_efficiency))),
    hoverinfo = "x+text",
    line = list(color = Region1_col),
    visible = FALSE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "Region2"),
    x = ~year,
    y = ~area_efficiency,
    name = "Region2",
    text = ~paste0(scales::comma(round(area_efficiency))),
    hoverinfo = "x+text",
    line = list(color = Region2_col),
    visible = FALSE
  )

## ---- visibility vectors for the buttons ----
# set options for which traces to show when each tab is selected
vis_area_percent      <- c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE) # first two traces for vis_area_used
vis_area_km    <- c(FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE) # secont two traces for total abundance
vis_area_eff       <- c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE)

## ---- add dropdown tab to view ----
fig <- fig %>%
  layout(
    hovermode = "x unified",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Area Used (%)"),
    legend = list(title = list(text = "Region")),
    margin = list(l = 50, r = 0, t = 50, b = 0),
    title = list(text = "Area Used (%)"),
    # make dropdown box to update plot
    updatemenus = list(
      list(
        type = "dropdown",
        x = 1,
        y = 1.1,
        showactive = TRUE,
        buttons = list(
          list(
            label = "Area Used (%)",
            method = "update",
            args = list(
              list(visible = vis_area_percent),
              list(
                title = "Area Used (%)",
                yaxis = list(title = "Total Area Used (%)")
              )
            )
          ),
          list(
            label = "Area Used (km<sup>2</sup>)",
            method = "update",
            args = list(
              list(visible = vis_area_km),
              list(
                title = "Area Used (km<sup>2</sup>)",
                yaxis = list(title = "Area Used (km<sup>2</sup>)")
              )
            )
          ),
          list(
            label = "Area Efficiency",
            method = "update",
            args = list(
              list(visible = vis_area_eff),
              list(
                title = "Area Efficiency",
                yaxis = list(title = "Area Efficiency")
              )
            )
          )
        )
      )
    )
  ) %>%
  config(displayModeBar = FALSE)

fig

saveRDS(fig,
        "shiny/www/premade_plots/area_plot.rds")

rm(list = ls())
