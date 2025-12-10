# make pie charts for overall trends


# libraries ---------------------------------------------------------------
library(plotly)
library(scales)
library(stringr)
library(dplyr)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")



# upload data -------------------------------------------------------------
trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()
trends %>% glimpse()
trends <- trends %>%
  mutate(region = recode(region, "Canada" = "Region1",
                         "USA"= "Region2"))

trends_Region1 <- trends %>%
  filter(region == "Region1") %>%
  select(indicator, 
         region,
         estimate, 
         std_error, 
         p_value, 
         p_significant) 

trends_Region2 <- trends %>%
  filter(region == "Region2") %>%
  select(indicator, 
         region,
         estimate, 
         std_error, 
         p_value, 
         p_significant) 

rm(trends)

# make a pie plot in plotly
metrics <- c("Area Occupied",
             "Average Depth",
             "Edge Shift (N/S)",
             "Edge Shift (E/W)",
             "Distance to Border",
             "Centre Shift (N/S)",
             "Centre Shift (E/W)",
             "Trend in Abundance")

trends_Region1.2 <- trends_Region1 %>%
  mutate(metric = case_when(
    indicator == "Area Occupied" ~ "Area Occupied",
    indicator == "Abundance Weighted Depth" ~ "Average Depth",
    indicator == "Leading_Edge_East" ~ "Edge Shift (E/W)",
    indicator == "Leading_Edge_North" ~ "Edge Shift (N/S)",
    indicator == "Distance to Border" ~ "Distance to Border",
    indicator == "COG_East" ~ "Centre Shift (E/W)",
    indicator == "COG_North" ~ "Centre Shift (N/S)",
    indicator == "Abundance" ~ "Trend in Abundance"
  )) %>%
  relocate(metric) 
rm(trends_Region1)

trends_Region2.2 <- trends_Region2 %>%
  mutate(metric = case_when(
    indicator == "Area Occupied" ~ "Area Occupied",
    indicator == "Abundance Weighted Depth" ~ "Average Depth",
    indicator == "Trailing_Edge_North" ~ "Edge Shift (N/S)",
    indicator == "Trailing_Edge_East" ~ "Edge Shift (E/W)",
    indicator == "Distance to Border" ~ "Distance to Border",
    indicator == "COG_East" ~ "Centre Shift (E/W)",
    indicator == "COG_North" ~ "Centre Shift (N/S)",
    indicator == "Abundance" ~ "Trend in Abundance"
  )) %>%
  relocate(metric) 
rm(trends_Region2)

trends_Region1.2 <- trends_Region1.2 %>%
  mutate(metric = factor(metric, levels = metrics)) %>%
  arrange(metric)

trends_Region2.2 <- trends_Region2.2 %>%
  mutate(metric = factor(metric, levels = metrics)) %>%
  arrange(metric)

# add text columns

# add simplified description of what is happening
trends_Region1.2 <- trends_Region1.2 %>% 
  mutate(
    description = case_when(
      metric == "Area Occupied" ~ paste(p_significant, ifelse(estimate <0,"decrease","increase"), "in area occupied since 2006."),
      metric == "Average Depth" ~ paste(p_significant, ifelse(estimate <0,"downward","upward"), "shift in average depth since 2006."),
      metric == "Edge Shift (N/S)" ~ paste(p_significant, ifelse(estimate <0,"northward","southward"), "shift in range edge postition since 2006."),
      metric == "Edge Shift (E/W)" ~ paste(p_significant, ifelse(estimate <0,"eastward","westward"), "shift in range edge postition since 2006."),
      metric == "Distance to Border" ~ paste(p_significant, "shift", ifelse(estimate <0,"towards","away from"), "the jusisdictional border since 2006."),
      metric == "Centre Shift (N/S)" ~ paste(p_significant, ifelse(estimate <0,"northward","southward"), "shift in centroid postition since 2006."),
      metric == "Centre Shift (E/W)" ~ paste(p_significant, ifelse(estimate <0,"eastward","westward"), "shift in centroid postition since 2006."),
      metric == "Trend in Abundance" ~ paste(p_significant, ifelse(estimate <0,"decrease","increase"), "in total abundance within the study region since 2006.")
      
    )
  )

trends_Region1.2 <- trends_Region1.2 %>%
  mutate(description = str_wrap(description, 25))

trends_Region2.2 <- trends_Region2.2 %>% 
  mutate(
    description = case_when(
      metric == "Area Occupied" ~ paste(p_significant, ifelse(estimate <0,"decrease","increase"), "in area occupied since 2006."),
      metric == "Average Depth" ~ paste(p_significant, ifelse(estimate <0,"downward","upward"), "shift in average depth since 2006."),
      metric == "Edge Shift (N/S)" ~ paste(p_significant, ifelse(estimate <0,"northward","southward"), "shift in range edge postition since 2006."),
      metric == "Edge Shift (E/W)" ~ paste(p_significant, ifelse(estimate <0,"eastward","westward"), "shift in range edge postition since 2006."),
      metric == "Distance to Border" ~ paste(p_significant, "shift", ifelse(estimate <0,"towards","away from"), "the jusisdictional border since 2006."),
      metric == "Centre Shift (N/S)" ~ paste(p_significant, ifelse(estimate <0,"northward","southward"), "shift in centroid postition since 2006."),
      metric == "Centre Shift (E/W)" ~ paste(p_significant, ifelse(estimate <0,"eastward","westward"), "shift in centroid postition since 2006."),
      metric == "Trend in Abundance" ~ paste(p_significant, ifelse(estimate <0,"decrease","increase"), "in total abundance within the study region since 2006.")
    )
  )

trends_Region2.2 <- trends_Region2.2 %>%
  mutate(description = str_wrap(description, 25))


trends_Region1.2 <- trends_Region1.2 %>%
  mutate(
    p_annotate = case_when(
      p_value > .05 ~ paste0(round(p_value,2)),
      p_value < .05 & p_value > .01 ~ "<0.05",
      p_value < .01 ~ "<0.01"
    ),
    trend_unit = case_when(
      indicator == "Area Occupied" ~ "km<sup>2</sup>/year",
      indicator == "Abundance Weighted Depth" ~ "m/year",
      indicator == "Leading_Edge_North" ~ "km/year",
      indicator == "Leading_Edge_East" ~ "km/year",
      indicator == "Distance to Border" ~ "km/year",
      indicator == "COG_North" ~ "km/year",
      indicator == "COG_East" ~ "km/year",
      indicator == "Abundance" ~ "count/year",
      
    )
  ) 
trends_Region2.2 <- trends_Region2.2 %>%
  mutate(
    p_annotate = case_when(
    p_value > .05 ~ paste0(round(p_value,2)),
    p_value < .05 & p_value > .01 ~ "<0.05",
    p_value < .01 ~ "<0.01"
  ),
  trend_unit = case_when(
    indicator == "Area Occupied" ~ "km<sup>2</sup>/year",
    indicator == "Abundance Weighted Depth" ~ "m/year",
    indicator == "Trailing_Edge_North" ~ "km/year",
    indicator == "Trailing_Edge_East" ~ "km/year",
    indicator == "Distance to Border" ~ "km/year",
    indicator == "COG_North" ~ "km/year",
    indicator == "COG_East" ~ "km/year",
    indicator == "Abundance" ~ "count/year",
    
  )) 

trends_Region1.3 <- trends_Region1.2 %>%
  mutate(text = paste0(
    "<b>",metric,"</b><br><br>",
    description,
    "<br><br>",
    "Estimate: ",round(estimate,2)," ± ",round(std_error,2)," ", trend_unit,"<br>",
    "p value:", p_annotate,"<br>",
    p_significant, " Trend"
    
  ))
rm(trends_Region1.2)

trends_Region2.3 <- trends_Region2.2 %>%
  mutate(text = paste0(
    "<b>",metric,"</b><br><br>",
    description,
    "<br><br>",
    "Estimate: ",round(estimate,2)," ± ",round(std_error,2)," ",trend_unit,"<br>",
    "p value:", p_annotate,"<br>",
    p_significant, " Trend"
    
  ))
rm(trends_Region2.2)

# Add custom logo paths
logo_files <- c(
  "Area Occupied" = base64enc::dataURI(file ="shiny/www/icons/logo_area_occupied.png", mime = "image/png"),
  "Average Depth" = base64enc::dataURI(file = "shiny/www/icons/logo_depth.png", mime = "image/png"),
  "Range Edge" = base64enc::dataURI(file = "shiny/www/icons/logo_range_edge.png", mime = "image/png"),
  "Distance to Border" = base64enc::dataURI(file ="shiny/www/icons/logo_distance_to_border.png", mime = "image/png"),
  "Centre of Gravity" = base64enc::dataURI(file ="shiny/www/icons/logo_center_of_gravity.png", mime = "image/png"),
  "Trend in Abundance" = base64enc::dataURI(file = "shiny/www/icons/logo_abundance.png", mime = "image/png")
)

sp_logo <- list(
  "fish" = base64enc::dataURI(file ="shiny/shiny_processed_data/generic_fish.png", mime = "image/png")
)

n <- length(metrics)
radius = .34
angles = seq(0, 2*pi, length.out = 7)


logo_pos <- data.frame(
  x = 0.5 + radius * cos(angles[-1]),
  y = 0.5 + radius * sin(angles[-1])
)

# split two of the angles for edge shift n/s & e/w and cog shift n/s & e/w
angles_split <- c(angles,mean(angles[3:4]), mean(angles[5:6])) %>% sort()


trend_pal <- col_numeric(
  c("tomato2", "white", "cornflowerblue"),
  domain = c(-1, 1)
)


# make plot ---------------------------------------------------------------
pie_images <- lapply(seq_len(n), function(i) {
  list(
    source = logo_files[i],
    xref = "paper", yref = "paper",
    x = logo_pos$x[i], y = logo_pos$y[i],
    sizex = 0.25, sizey = 0.25,
    xanchor = "center", yanchor = "middle",
    layer = "above"
  )
})

# your species logo in center
center_image <- list(
  source = sp_logo[["fish"]],
  xref = "paper", yref = "paper",
  x = 0.5, y = 0.46,
  sizex = 0.24, sizey = 0.24,
  xanchor = "center", yanchor = "middle",
  layer = "above"
)

# combine them
all_images <- c(pie_images, list(center_image))

trends_Region1.4 <- trends_Region1.3 %>%
  mutate(piesize = case_when(str_detect(metric,"Centre|Edge") ~ 1,
                           TRUE ~ 2)) %>% glimpse()
rm(trends_Region1.3)

p_Region1 <- plot_ly(
  data = trends_Region1.4,
  labels = ~metric,
  type = "pie",
  hole = .33,
  text = ~text,
  textinfo = "none",
  values = ~ piesize,
  sort = F,
  hoverlabel = ~paste0(metric),
  pull = ~.025,
  marker = list(colors = ~trend_pal(estimate), 
                line = list(
                  #color = ~ifelse(p_value < .05, "transparent","transparent"),
                  #width = ~ifelse(p_value < .05,2,.5),
                  dash = "dash"
                  )),
 # width = 400, height = 400,
  hoverinfo = "text",
  source = "pie"
) %>%
  layout(
    margin = list(r = 2, l = 2, t = 2, b = 2),
    images = all_images,
    showlegend = FALSE
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    showlegend = FALSE,
    title = list(text = "<b>Region 1</b>",
                 font = list(size = 22,
                             lineheight = .1,
                             color = Region1_col
                             ),
                 y= .56)
  ) 


p_Region1





trends_Region2.4 <- trends_Region2.3 %>%
  mutate(piesize = case_when(str_detect(metric,"Centre|Edge") ~ 1,
                             TRUE ~ 2)) %>% glimpse()

rm(trends_Region2.3)

p_Region2 <- plot_ly(
  data = trends_Region2.4,
  labels = ~metric,
  type = "pie",
  hole = .33,
  text = ~text,
  textinfo = "none",
  values = ~ piesize,
  sort = F,
  hoverlabel = ~paste0(metric),
  pull = ~.025,
  marker = list(colors = ~trend_pal(estimate), 
                line = list(
                 # color = ~ifelse(p_value < .05, "black","transparent"),
                  #width = ~ifelse(p_value < .05,2,.5),
                  dash = "dash"
                )),
 # width = 400, height = 400,
  hoverinfo = "text",
  source = "pie"
) %>%
  layout(
    margin = list(r = 2, l = 2, t = 2, b = 2),
    images = all_images,
    showlegend = FALSE
  ) %>%
  config(displayModeBar = FALSE) %>%
  layout(
    showlegend = FALSE,
    title = list(text = "<b>Region 2</b>",
                 font = list(size = 22,
                             lineheight = .1,
                             color = Region2_col
                 ),
                 y= .56)
  ) 

p_Region2

# make shared legend


custom_colors <- colorRamp(c("cornflowerblue", "white", "tomato2")) # From blue to white to red

p_legend <- plot_ly(
  # dummy 1x1 heatmap, completely transparent
  #z = matrix(0, nrow = 1, ncol = 1),
  z = matrix(seq(from = -1, to = 1, length.out=8), nrow = 8, ncol = 1),
  type = "heatmap",
  zmin = -1,
  zmax = 1,
  opacity = 0,          # <-- hide the actual tile
  hoverinfo = "none",
  showscale = TRUE,
  reversescale = T,
  colors = custom_colors,
 # colorscale = list(
 #   list(0,   "tomato2"),
 #   list(0.25, "#FFB09D"),
 #   list(0.5, "white"),
 #   list(0.75,"#B9C8F7"),
 #   list(1,   "cornflowerblue")
 # ),
  showlegend = FALSE,
  colorbar = list(
    title = "Standardized<br>trend",
    ticks = "outside",
    tickvals = c(-1, -0.5, 0, 0.5, 1),
    ticktext = c("-1; Negative", "-0.5", "0; No change", "0.5", "1; Positive"),
    thickness = 20,
    len = 1
  )
) %>%
  layout(
    xaxis = list(
      visible = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      visible = FALSE,
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    margin = list(l = 0, r = 110, t = 0, b = 0),
    width  = 120,   # tweak to taste
    height = 300
  ) %>%
  layout(xaxis = list(fixedrange = TRUE),
         yaxis = list(fixedrange = TRUE)) %>%
  config(displayModeBar = FALSE) 
  
p_legend

saveRDS(p_Region2 ,
        "shiny/www/premade_plots/pie_Region2.rds")
saveRDS(p_Region1,
        "shiny/www/premade_plots/pie_Region1.rds")
saveRDS(p_legend,
        "shiny/www/premade_plots/pie_legend.rds")

rm(list = ls())
