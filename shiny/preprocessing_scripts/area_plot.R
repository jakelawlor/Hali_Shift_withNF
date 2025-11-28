library(plotly)


area <- readr::read_csv("Data/Data_SHinyApp_Proof_of_Concept/POC_AreaOccupied.csv") %>%
  janitor::clean_names()
area %>% glimpse()

area %>%
  filter(threshold == 90) %>%
  ggplot(aes(x = year, 
             y = percent_area_used,
             color = region,
             linetype = as.character(threshold))) +
  geom_line()

p_area_1 <- plot_ly(data = area %>% filter(threshold == 90),
                    colors = c("Canada" = "#D60E0A","USA" = "#0471A6")) %>%
  add_lines(x = ~year, 
            y = ~percent_area_used,
            color = ~region,
            text = ~paste0(round(percent_area_used),"%"),
            hoverinfo = 'x+text') %>%
  layout(hovermode = "x unified",
         title = "Area Used",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Total Area Used (%)"),
         legend = list(title = list(text = "Region"))
  ) %>%
  config(displayModeBar = F)
p_area_1

p_area_2 <- plot_ly(data = area %>% filter(threshold == 90),
                    colors = c("Canada" = "#D60E0A","USA" = "#0471A6")) %>%
  add_lines(x = ~year, 
            y = ~area_threshold,
            color = ~region,
            text = ~paste0(scales::comma(round(area_threshold))," km<sup>2</sup>"),
            hoverinfo = 'x+text') %>%
  layout(hovermode = "x unified",
         title = "Total Abundance",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Total Area Used (km2)"),
         legend = list(title = list(text = "Region"))
  ) %>%
  config(displayModeBar = F)
p_area_2

p_area_3 <- plot_ly(data = area %>% filter(threshold == 90)) %>%
  add_lines(x = ~year, 
            y = ~area_efficiency,
            color = ~region,
            text = ~paste0(scales::comma(round(area_efficiency))),
            hoverinfo = 'x+text') %>%
  layout(hovermode = "x unified",
         title = "Total Area",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Area Efficiency"),
         legend = list(title = list(text = "Region"))
  ) %>%
  config(displayModeBar = F)
p_area_3



area_90 <- area %>% filter(threshold == 90)
fig <- plot_ly()

## ---- 1) Area used (%): traces 1–2 (start visible) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Canada"),
    x = ~year,
    y = ~percent_area_used,
    name = "Canada",
    text = ~paste0(round(percent_area_used), "%"),
    hoverinfo = "x+text",
    line = list(color = "#D60E0A"),
    visible = TRUE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "USA"),
    x = ~year,
    y = ~percent_area_used,
    name = "USA",
    text = ~paste0(round(percent_area_used), "%"),
    hoverinfo = "x+text",
    line = list(color = "#0471A6"),
    visible = TRUE
  )

## ---- 2) area_threshold: traces 3–4 (start hidden) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Canada"),
    x = ~year,
    y = ~area_threshold,
    name = "Canada",
    text = ~paste0(scales::comma(round(area_threshold)), " km<sup>2</sup>"),
    hoverinfo = "x+text",
    line = list(color = "#D60E0A"),
    visible = FALSE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "USA"),
    x = ~year,
    y = ~area_threshold,
    name = "USA",
    text = ~paste0(scales::comma(round(area_threshold)), " km<sup>2</sup>"),
    hoverinfo = "x+text",
    line = list(color = "#0471A6"),
    visible = FALSE
  )

## ---- 3) area_efficiency: traces 5–6 (start hidden) ----
fig <- fig %>%
  add_lines(
    data = area_90 %>% filter(region == "Canada"),
    x = ~year,
    y = ~area_efficiency,
    name = "Canada",
    text = ~paste0(scales::comma(round(area_efficiency))),
    hoverinfo = "x+text",
    line = list(color = "#D60E0A"),
    visible = FALSE
  ) %>%
  add_lines(
    data = area_90 %>% filter(region == "USA"),
    x = ~year,
    y = ~area_efficiency,
    name = "USA",
    text = ~paste0(scales::comma(round(area_efficiency))),
    hoverinfo = "x+text",
    line = list(color = "#0471A6"),
    visible = FALSE
  )

## ---- visibility vectors for the buttons ----
vis_area_used      <- c(TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE)
vis_total_abund    <- c(FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE)
vis_area_eff       <- c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE)

fig <- fig %>%
  layout(
    hovermode = "x unified",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Area Used (%)"),
    legend = list(title = list(text = "Region")),
    margin = list(l = 50, r = 0, t = 50, b = 0),
    title = list(text = "Area Used (%)"),
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
              list(visible = vis_area_used),
              list(
                title = "Area Used (%)",
                yaxis = list(title = "Total Area Used (%)")
              )
            )
          ),
          list(
            label = "Area Used (km2)",
            method = "update",
            args = list(
              list(visible = vis_total_abund),
              list(
                title = "Area Used (km2)",
                yaxis = list(title = "Area Used (km2)")
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
