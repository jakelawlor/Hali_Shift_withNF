
library(plotly)
library(scales)
library(stringr)
library(dplyr)
# make a pie plot in plotly
metrics <- c("Area Occupied",
             "Average Depth",
             "Range Edge",
             "Distance to Border",
             "Centre of Gravity",
             "Trend in Abundance")

trends<-read.csv(here::here("Data/Data_SHinyApp_Proof_of_Concept/Scaled_Slopes_all_indicators.csv")) %>%
  janitor::clean_names()
trends %>% glimpse()

trends_can <- trends %>%
  filter(region == "Canada") %>%
  select(indicator, 
         region,
         estimate, 
         std_error, 
         p_value, 
         p_significant) 

unique(trends_can$p_significant)

trends_usa <- trends %>%
  filter(region == "USA") %>%
  select(indicator, 
         region,
         estimate, 
         std_error, 
         p_value, 
         p_significant) 
unique(trends_usa$p_significant)

rm(trends)

trends_can2 <- trends_can %>%
  mutate(group = case_when(str_detect(indicator, "COG") ~ "COG",
                           str_detect(indicator,"Edge") ~ "Edge",
                           TRUE ~ indicator)) %>%
  group_by(group) %>%
  summarize(region = unique(region),
            estimate = mean(estimate),
            std_error = mean(std_error),
            p_value = mean(p_value))  %>%
  mutate(
    p_significant = case_when(
      p_value < 0.001 ~ "Very strong",
      p_value < 0.01  ~ "Strong",
      p_value < 0.05  ~ "Moderate",
      TRUE ~ "Not significant"
    )
  ) %>%
  mutate(metric = case_when(
    group == "Area Occupied" ~ "Area Occupied",
    group == "Abundance Weighted Depth" ~ "Average Depth",
    group == 'Edge' ~ "Range Edge",
    group == "Distance to Border" ~ "Distance to Border",
    group == "COG" ~ "Centre of Gravity",
    group == "Abundance" ~ "Trend in Abundance"
  )) %>%
  relocate(metric) %>%
  select(-group)
rm(trends_can)

trends_usa2 <- trends_usa %>%
  mutate(group = case_when(str_detect(indicator, "COG") ~ "COG",
                           str_detect(indicator,"Edge") ~ "Edge",
                           TRUE ~ indicator)) %>%
  group_by(group) %>%
  summarize(region = unique(region),
            estimate = mean(estimate),
            std_error = mean(std_error),
            p_value = mean(p_value))  %>%
  mutate(
    p_significant = case_when(
      p_value < 0.001 ~ "Very strong",
      p_value < 0.01  ~ "Strong",
      p_value < 0.05  ~ "Moderate",
      TRUE ~ "Not significant"
    )
  ) %>%
  mutate(metric = case_when(
    group == "Area Occupied" ~ "Area Occupied",
    group == "Abundance Weighted Depth" ~ "Average Depth",
    group == 'Edge' ~ "Range Edge",
    group == "Distance to Border" ~ "Distance to Border",
    group == "COG" ~ "Centre of Gravity",
    group == "Abundance" ~ "Trend in Abundance"
  )) %>%
  relocate(metric) %>%
  select(-group)
rm(trends_usa)

trends_can2 <- trends_can2 %>%
  mutate(metric = factor(metric, levels = metrics)) %>%
  arrange(metric)

trends_usa2 <- trends_usa2 %>%
  mutate(metric = factor(metric, levels = metrics)) %>%
  arrange(metric)

# add text columns
trends_can3 <- trends_can2 %>%
  mutate(text = paste0(
    "<b>",metric,"</b><br>",
    "Estimate: ",round(estimate,2)," ±",round(std_error,2),"<br>",
    p_significant, " Trend"
    
  ))
rm(trends_can2)

trends_usa3 <- trends_usa2 %>%
  mutate(text = paste0(
    "<b>",metric,"</b><br>",
    "Estimate: ",round(estimate,2)," ±",round(std_error,2),"<br>",
    p_significant, " Trend"
    
  ))
rm(trends_usa2)

# Add your own image paths here (must be accessible)
logo_files <- c(
  "Area Occupied" = base64enc::dataURI(file ="shiny/www/icons/logo_area_occupied.png", mime = "image/png"),
  "Average Depth" = base64enc::dataURI(file = "shiny/www/icons/logo_depth.png", mime = "image/png"),
  "Range Edge" = base64enc::dataURI(file = "shiny/www/icons/logo_range_edge.png", mime = "image/png"),
  "Distance to Border" = base64enc::dataURI(file ="shiny/www/icons/logo_distance_to_border.png", mime = "image/png"),
  "Centre of Gravity" = base64enc::dataURI(file ="shiny/www/icons/logo_center_of_gravity.png", mime = "image/png"),
  "Trend in Abundance" = base64enc::dataURI(file = "shiny/www/icons/logo_abundance.png", mime = "image/png")
)

sp_logo <- list(
  "Halibut" = base64enc::dataURI(file ="shiny/www/icons/halibut.png", mime = "image/png")
)

n <- length(metrics)
radius = .33
angles = seq(0, 2*pi, length.out = 7)


logo_pos <- data.frame(
  x = 0.5 + radius * cos(angles[-1]),
  y = 0.5 + radius * sin(angles[-1])
)

#trends_can3$logo <- NA
#for(i in 1:nrow(trends_can3)){
#  trends_can3$logo[i] <- logo_files[[as.character(trends_can3$metric[i])]]
#}
#trends_can3$logo_pos_x <- logo_pos$x
#trends_can3$logo_pos_y <- logo_pos$y
#
#trends_usa3$logo <- NA
#for(i in 1:nrow(trends_usa3)){
#  trends_usa3$logo[i] <- logo_files[[as.character(trends_usa3$metric[i])]]
#}
#trends_usa3$logo_pos_x <- logo_pos$x
#trends_usa3$logo_pos_y <- logo_pos$y
#rm(logo_pos, logo_files, angles, i, n, radius)


colors_can <- col_numeric(c("darkmagenta","white","darkcyan"),
                      domain = c(-1, 1))(trends_can3$estimate)

colors_usa <- col_numeric(c("darkmagenta","white","darkcyan"),
                          domain = c(-1, 1))(trends_usa3$estimate)




# -------------------------------------------------------------------------
# OLD -------------------------------------------------------------------
# -------------------------------------------------------------------------



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
  source = sp_logo[["Halibut"]],
  xref = "paper", yref = "paper",
  x = 0.5, y = 0.42,
  sizex = 0.2, sizey = 0.2,
  xanchor = "center", yanchor = "middle",
  layer = "above"
)

# combine them
all_images <- c(pie_images, list(center_image))

p_can <- plot_ly(
  data = trends_can3,
  labels = ~metric,
  type = "pie",
  hole = .33,
  text = ~text,
  textinfo = "none",
  hoverlabel = ~paste0(metric),
  marker = list(colors = ~col_numeric(c("darkmagenta","white","darkcyan"),
                                      domain = c(-1, 1))(estimate), 
                line = list(color = "black", width = ~ifelse(p_value < .05,6,1))),
  width = 400, height = 400,
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
    title = list(text = "<b>Region 1</b><br>Canada",
                 font = list(size = 22,
                             lineheight = .1,
                             color = "#D60E0A"
                             ),
                 y= .56)
  )

p_can


p_usa <- plot_ly(
  data = trends_usa3,
  labels = ~metric,
  type = "pie",
  hole = .33,
  text = ~text,
  textinfo = "none",
  hoverlabel = ~paste0(metric),
  marker = list(colors = ~col_numeric(c("darkmagenta","white","darkcyan"),
                                      domain = c(-1, 1))(estimate), 
                line = list(color = "black", width = ~ifelse(p_value < .05,6,1))),
  width = 400, height = 400,
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
    title = list(text = "<b>Region 2</b><br>USA",
                 font = list(size = 22,
                             color = "#0471A6"
                 ),
                 y= .56)
  )
p_usa 

saveRDS(p_usa ,
        "shiny/www/premade_plots/pie_usa.rds")
saveRDS(p_can,
        "shiny/www/premade_plots/pie_can.rds")

# -------------------------------------------------------------------------
# new ---------------------------------------------------------------------
# -------------------------------------------------------------------------



# NEW: add a constant 'one' column to each df for equal pie slice sizes
trends_can3$one <- 1
trends_usa3$one <- 1

# -----------------------------------------------------------
# 2. Domains for left/right pies (NEW compared to your original)
# -----------------------------------------------------------
left_dom  <- c(0.00, 0.48)   # left half of the figure
right_dom <- c(0.52, 1)   # right half of the figure

dom_width <- diff(left_dom)  # = 0.5

# -----------------------------------------------------------
# 3. Helper to convert local (0–1) x to global paper x (NEW)
# -----------------------------------------------------------
local_to_paper_x <- function(x_local, dom) {
  dom[1] + x_local * diff(dom)
}

# -----------------------------------------------------------
# 4. Image lists for BOTH pies (DIFFERENT from your original)
#    - You previously had just one set for one pie centered on page.
#    - Now we create a left (CAN) and right (USA) version.
# -----------------------------------------------------------

# --- Canada logos (left pie) ---
pie_images_can <- lapply(seq_len(n), function(i) {
  list(
    source  = logo_files[i],
    xref    = "paper",                     # same style as your original
    yref    = "paper",
    x       = local_to_paper_x(logo_pos$x[i], left_dom),  # shifted left
    y       = logo_pos$y[i],
    sizex   = 0.25 * dom_width,           # scaled for half-width domain
    sizey   = 0.25,
    xanchor = "center",
    yanchor = "middle",
    layer   = "above"
  )
})

center_image_can <- list(
  source  = sp_logo[["Halibut"]],
  xref    = "paper",
  yref    = "paper",
  x       = mean(left_dom),               # center of left pie
  y       = 0.5,
  sizex   = 0.2 * dom_width,
  sizey   = 0.2,
  xanchor = "center",
  yanchor = "middle",
  layer   = "above"
)

# --- USA logos (right pie) ---
pie_images_usa <- lapply(seq_len(n), function(i) {
  list(
    source  = logo_files[i],
    xref    = "paper",
    yref    = "paper",
    x       = local_to_paper_x(logo_pos$x[i], right_dom), # shifted right
    y       = logo_pos$y[i],
    sizex   = 0.25 * dom_width,
    sizey   = 0.25,
    xanchor = "center",
    yanchor = "middle",
    layer   = "above"
  )
})

center_image_usa <- list(
  source  = sp_logo[["Halibut"]],
  xref    = "paper",
  yref    = "paper",
  x       = mean(right_dom),              # center of right pie
  y       = 0.5,
  sizex   = 0.2 * dom_width,
  sizey   = 0.2,
  xanchor = "center",
  yanchor = "middle",
  layer   = "above"
)

# All images for the combined figure
all_images <- c(
  pie_images_can, list(center_image_can),
  pie_images_usa, list(center_image_usa)
)

# -----------------------------------------------------------
# 5. ONE figure with TWO pies (instead of subplot(p_can, p_usa))
# -----------------------------------------------------------

p_combined <- plot_ly(width = 850, height = 400) %>%
  
  # ---- Left pie (Canada) ----
add_pie(
  data = trends_can3,
  labels = ~metric,
  values = ~one,              # <<< NEW: REQUIRED, all slices equal
  hole = 0.25,
  text = ~text,
  textinfo = "none",
  hoverlabel = ~paste0(metric),
  marker = list(
    colors = colors_can,      # <<< DIFFERENCE: vector, not ~colors_can
    line = list(color = "grey20", width = 2)
  ),
  hoverinfo = "text",
  sort = FALSE,
  showlegend = FALSE,
  domain = list(x = left_dom, y = c(0, 1))  # <<< where domain actually belongs
) %>%
  
  # ---- Right pie (USA) ----
add_pie(
  data = trends_usa3,
  labels = ~metric,
  values = ~one,              # <<< NEW: same trick for USA
  hole = 0.25,
  text = ~text,
  textinfo = "none",
  hoverlabel = ~paste0(metric),
  marker = list(
    colors = colors_usa,
    line = list(color = "grey20", width = 2)
  ),
  hoverinfo = "text",
  sort = FALSE,
  showlegend = FALSE,
  domain = list(x = right_dom, y = c(0, 1))
) %>%
  
  layout(
    images = all_images,        # <<< NEW: all images for both pies
    showlegend = FALSE,
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    margin = list(l = 0, r = 0, t = 0, b = 0)
  ) %>%
  config(displayModeBar = FALSE)

p_combined

saveRDS(p_combined,
        "shiny/www/premade_plots/pie_combined.rds")
