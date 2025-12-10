# animated map clean script

# NEW version - with greyscale colored lines

# packages ----------------------------------------------------------------
library(dplyr)
library(terra)
library(sf)
library(plotly)
library(stringr)
library(tidyterra)
library(purrr)


# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")


# upload data -------------------------------------------------------------
eez_df <- readRDS( file = "shiny/shiny_processed_data/map_objects/eez_df.rds")
border2 <- readRDS( file = "shiny/shiny_processed_data/map_objects/border.rds")
zone_coords2 <- readRDS(file = "shiny/shiny_processed_data/map_objects/zone_coords2.rds")
coords <- readRDS(file = "shiny/shiny_processed_data/map_objects/png_coords.rds")
years <- readRDS(file = "shiny/shiny_processed_data/map_objects/rasteryears.rds")
nafo_centroids <- readRDS( "shiny/shiny_processed_data/map_objects/nafo_centroids.rds")
df_points <- readRDS("shiny/shiny_processed_data/map_objects/df_points.rds")
df_cumulative_lines <- readRDS("shiny/shiny_processed_data/map_objects/df_cumulative_lines.rds")
df_static_lines <- readRDS("shiny/shiny_processed_data/map_objects/df_static_lines.rds")
#nafo_geojson_obj <- readRDS( "shiny/shiny_processed_data/map_objects/nafo_geojson.rds")

#coords2 <- coords
##coords2[[1]]["xmin"] <- coords[[1]]["xmin"] -.002
##coords2[[4]]["xmin"] <- coords[[4]]["xmin"] -.002
#coords2[[3]]["ymin"] <- coords[[3]]["ymin"] -.04
#coords2[[4]]["ymin"] <- coords[[4]]["ymin"] -.04
#coords2[[2]]["xmax"] <- coords[[2]]["xmax"]+.05
#coords2[[3]]["xmax"] <- coords[[3]]["xmax"]+.05

# upload raster pngs ------------------------------------------------------

png_base64_list <- vector(mode = "list",
                          length = length(years)) %>%
  purrr::set_names(years)

# convert all .pngs to URIs
for(i in years){
  png_base64_list[[as.character(i)]] <-   base64enc::dataURI(file = paste0("shiny/shiny_processed_data/rasterPngs/yr",i,".png"), mime = "image/png")
  print(i)
}


# make helper functions ---------------------------------------------------
# function to add animated points for the focal parameter -- one trace each
add_animated_edge_points <- function(p, data, param, select_group) {
  d <- data %>% dplyr::filter(param == param, 
                              group == select_group)
  
  p %>%
    add_trace(
      data = d,
      legendgroup = select_group,
      name = select_group,
      type = "scattermapbox",
      mode = "markers",
      lon  = ~lon,
      lat  = ~lat,
      color = ~group,
      frame = ~Year,
      marker = list(size = 15),
      text = ~paste0(
        "<b>", group, "</b> ", Year, "<br>",
        round(lat, 2), " °N<br>",
        round(lon, 2), " °W"
      ),
      hoverinfo = "text"
    )
}


# add static black lines
add_static_lines_black <- function(p, data, param, select_group){
  d <- data %>% dplyr::filter(param == param, 
                              group == select_group)
  
  p %>%
    add_trace(
      data = d, 
      legendgroup = select_group,
      showlegend =  F,
      type = "scattermapbox",
      mode = "lines",
      lon = ~lon,
      lat = ~lat,
      color = ~group,
      line = list(width = 3, opacity = 1,
                  color = "black"),
      hoverinfo = "none"
    ) 
  
}

# function to add static lines behind the path of moving points -- 
# these will show all segments of travel for the focal parameter
add_static_lines_yearly <- function(p, data, param, select_group) {
  d <- data %>% dplyr::filter(param == param, 
                              group == select_group) %>%
    select(-Year)
  
  p %>%
    add_trace(
      data = d,
      mode=  "lines",
      x = ~lon,
      y = ~lat, 
      legendgroup = select_group,
      split = ~segment_id,
      line = list(color = ~col,
                  width = 4),
      showlegend = F,
      hoverinfo = "none"
    ) 
}

# function to add animated lines behind the path of moving points
# from year 1 to year {frame}, in the color of the dataset
add_cumulative_lines <- function(p, data, param, select_group){
  d <- data %>% dplyr::filter(param == param, 
                              group == select_group)
  p %>% 
  add_trace(
    data  = d,
    legendgroup = select_group,
    showlegend =  F,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~lon,
    lat   = ~lat,
    frame = ~Year,         # the cumulative frame column
    color = ~group,
    hoverinfo = "none",
    opacity = .8,
    line = list(width = 7)
  ) 
  
} # end helper functions -----------------------------------------------------------




# build plot --------------------------------------------------------------


## 1. Base plot ------------------------------------------------------------
p_base <- plot_ly(type = "scattermapbox",
        mode = "markers",
        colors = c("Region1" = Region1_col,"Region2" = Region2_col,
                   "Trailing Edge" = Region2_col,
                   "Leading Edge" = Region1_col)) %>%
  
  layout(
    margin = list(l = 50, r = 0, t = 0, b = 0),
    mapbox = list(
      style = 'carto-positron',
      zoom =3,
      hovermode = 'closest' ,
      center = list(lon = -60, lat = 45),
      layers = list(
        list(
          sourcetype = "image",
          below = "traces",
          source = png_base64_list[[1]],
          coordinates = coords,
          opacity = .85,
          visible = TRUE  # you can toggle this per frame
        )#,
      #  list(
      #    sourcetype = "geojson",
      #    source     = nafo_geojson_obj,
      #    type       = "fill",
      #    color      = "rgba(120, 81, 169, 0.25)",
      #    above      = "traces",
      #    visible = TRUE
      #  )
      )
    )
  )  %>%
  config(displayModeBar = F) %>%
  # add borders
  add_trace(
    data = eez_df, 
    mode= "lines",
    type  = "scattermapbox",
    lon   = ~X,
    lat   = ~Y,
    #  color = ~country,
    hoverinfo = "text",
    text = ~paste0(country, " EEZ"),
    line = list(width = 3,
                color = "#132f4d"),
    showlegend = F
  ) %>%
  add_trace(
    data = border2,
    mode = "lines",
    type = "scattermapbox",
    lon = ~X,
    lat = ~Y,
    hoverinfo = "text",
    text = ~paste0("Jurisdictional Boundary"),
    line = list(color = "#132f4d", 
                width = 3),
    showlegend = F
  ) 
  

p_base

## 2. add points and lines ------------------------------------------------------------
p1 <- p_base %>% 
  # add leading edge points and lines
  add_static_lines_yearly(data = df_static_lines, param = "edge", select_group = "Leading Edge") %>%
  #add_static_lines_black(data = df_static_lines, param = "edge", select_group = "Leading Edge") %>%
  add_cumulative_lines(data = df_cumulative_lines, param = "edge", select_group = "Leading Edge") %>%
  add_animated_edge_points(data = df_points, param = "edge", select_group = "Leading Edge") %>%
  
  # add trailingnedge points and lines
  add_static_lines_yearly(data = df_static_lines, param = "edge", select_group = "Trailing Edge") %>%
  #add_static_lines_black(data = df_static_lines, param = "edge", select_group = "Trailing Edge") %>%
  add_cumulative_lines(data = df_cumulative_lines, param = "edge", select_group = "Trailing Edge") %>%
  add_animated_edge_points(data = df_points, param = "edge", select_group = "Trailing Edge") %>%
  
  # add region 1 cog points and lines
  add_static_lines_yearly(data = df_static_lines, param = "cog", select_group = "Region1") %>%
  #add_static_lines_black(data = df_static_lines, param = "cog", select_group = "Region1") %>%
  add_cumulative_lines(data = df_cumulative_lines, param = "cog", select_group = "Region1") %>%
  add_animated_edge_points(data = df_points, param = "cog", select_group  = "Region1") %>%
  
  # add region 2 cog points and lines
  add_static_lines_yearly(data = df_static_lines, param = "cog", select_group = "Region2") %>%
  #add_static_lines_black(data = df_static_lines, param = "cog", select_group = "Region2") %>%
  add_cumulative_lines(data = df_cumulative_lines, param = "cog", select_group = "Region2") %>%
  add_animated_edge_points(data = df_points, param = "cog", select_group = "Region2") %>%
  
  # add button and year prefix
  plotly::animation_button(x = 0.03, y = -0.03, frame = 350, label = "Play") %>%
  plotly::animation_slider(currentvalue = list(prefix = "Year: "))

p1



## 3. add raster pngs ------------------------------------------------------
# add png rasters behind frames  ==
# use plotly_build so p$x$frames is created
p2 <- plotly_build(p1)
p2
p2$x[["shinyEvents"]] <- NULL
p2$x[["highlight"]]  <- NULL
p2$x[["base_url"]]  <- NULL

# loop through frames and add custom pngs from list
for (i in seq_along(p2$x$frames)) {
  yr <- p2$x$frames[[i]]$name 
  
  # create custom layout per frame that includes the raster image for that year
  # which are actually pngs stored in the png_base64_list
  p2$x$frames[[i]]$layout <- list(
    mapbox = list(
      layers = list(
        list(
          sourcetype  = "image",
          below       = "traces",
          source      = png_base64_list[[ as.character(yr) ]],
          coordinates = coords,
          opacity     = 1,
          visible     = TRUE
        )
      )
    )
  )
}
p2


## 4. move legend inside ---------------------------------------------------
p3 <- p2 %>%
  layout(margin = list(l = 0, r = 0, t = 0, b = 0),
         legend = list(x = 0.03,
                       y = .65,
                       xanchor = "left",
                       yanchor = "bottom",
                       bordercolor = "black",
                       borderwidth = 1))




rm(p_base, p1, p2)


## 5. add NAFO traces ------------------------------------------------------
# add nafo traces - invisible at first
p4 <- p3

## add buttons to turn on/off
n_poly <- length(zone_coords2)
base_traces <- plotly_build(p4)$x$data
n_base <- length(base_traces) 

####  a) add NAFO shapes =====
for (i in seq_along(zone_coords2)) {
  df_i <- zone_coords2[[i]]
  
  p4 <- p4 %>% add_trace(
    type = "scattermapbox",
    mode = "lines",
    lon = df_i$lon,
    lat = df_i$lat,
    line = list(color = "white", width = 1),
    hoverinfo = "text",
    showlegend = F,   

    text = paste0(
      "<b>NAFO zone ", df_i$zone[1], "</b><br>"
    ),
    name = paste0("zone_", df_i$zone[i]),    
    selected = list(line = list(width = 6)),
    unselected = list(line = list(width = 2)),
    visible = FALSE  # hidden by default for toggle button
  )
  
}
p4
#### b) add nafo centroids ======
p4 <- p4 %>%
  add_trace(
    data = nafo_centroids,
    type = "scattermapbox",
    mode = "markers",
    lon  = ~x,
    lat  = ~y,
    marker = list(
      size = 25,                             
      color = "rgba(255, 255, 255,.01)",   
      line  = list(color = "white", width = 0)
    ),
    hoverinfo = "text",
    text = ~paste0("<b>NAFO zone ", ZONE, "</b>"),
    showlegend = FALSE,
    name = "nafo_centroids",
    visible = FALSE  # start with visibility off
  )

p4

# make vectors to represent when traces are turned on and off
# here, the base traces will always stay true, and 
# nafo polygons/centroids will turn on and off

# NOTE: since we added colored line segments for each year,
# there are tons of traces that represent each segment
# (34 years * 4 params), so there are 70 total traces that
# represent cog (33 year segments + cumulative lines + point, for 2 cogs)
# and 70 total traces that represent edges (same math for each edge).
# In the shiny, we will turn off all COG traces when edge plot is 
# loaded, and turn of all edge traces when COG plot is loaded,
# removing 70 traces from these hard-coded vectors. So here,
# we'll subtract 70 from the "on" and "off" visibility vectors. 
n_cog_traces <- length(which(vapply(p4$x$data, function(tr) {
  identical(tr$legendgroup, "Region1")|identical(tr$legendgroup,"Region2")
}, logical(1))))
n_edge_traces <- length(which(vapply(p4$x$data, function(tr) {
  identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
}, logical(1))))
# this is not really the ideal way to do this, but it works, and 
# it saves us from having to save and story this heavy plot twice

all_off <- c(rep(TRUE, n_base - n_edge_traces), 
             rep(FALSE, n_poly),
             rep(FALSE, 1))
all_on  <- c(rep(TRUE, n_base - n_edge_traces),
             rep(TRUE, n_poly),
             rep(TRUE, 2))


#### c) add on/off buttons =====
p4$x$layout$updatemenus <-  list(
  
  # --- GROUP 1: Play / Pause ---
  list(
    type = "buttons",
    showactive = TRUE,
    direction = "right",
    x = 0.03,      
    y = -0.03,    
    xanchor = "left",
    pad = list(t = 0, r = 10),
    buttons = list(
      list(
        label = "Play",
        method = "animate",
        args = list(
          NULL,
          list(
            fromcurrent = TRUE,
            mode = "immediate",  
            frame = list(duration = 400, redraw = TRUE)
          )
        )
      ),
      list(
        label = "Pause",
        method = "animate",
        args = list(
          list(NULL), 
          list(
            mode  = "immediate",
            frame     = list(duration = 0, redraw = FALSE),
            transition = list(duration = 0)
          )
        )
      )
    )
  ),
  
  # --- GROUP 2: Toggle Zones ---
  list(
    type = "buttons",
    showactive = TRUE,
    direction = "right",
    x = 0.9,     
    xanchor = "right",
    y = -0.03,
    pad = list(t = 0, r = 10),
    buttons = list(
      list(
        label = "Show NAFO Zones",
        method = "update",
        args = list(list(visible = all_on))
      ),
      list(
        label = "Hide Zones",
        method = "update",
        args = list(list(visible = all_off))
      )
    )
  )
)

p4



## add png legends ---------------------------------------------------------
# to save memory, we presaved png legends representing the color scales
# for the raster fill (sqrt abundance) and the year segments (year)
# add the png to the plot here:
colorbar_base64 <- base64enc::dataURI(file ="shiny/www/rasterlegend2.png", mime="image/png")

p5 <- p4 %>%
  layout(
    images = list(
      list(
        source = colorbar_base64,
        x = 0.03, y = 0.62,
        xanchor = "left",
        yanchor = "top",
        sizex = .15*2.2, sizey = .22*2.2,
        layer = "above"
      )
    )
  )

p5 <- p5 %>%
  layout(legend = list(
    itemclick = FALSE, 
    itemdoubleclick = FALSE, 
    groupclick = FALSE
  ))




# save final map ----------------------------------------------------------
saveRDS(p5, "shiny/www/premade_plots/mapplot_NEW.rds")




# test shiny --------------------------------------------------------------
# in the shiny app, the full map will be uploaded each time either 
# the COG or EDGE map are requested, then the traces for non-target variable 
# will be removed. Only after removal will the "show" and "hide" buttons 
# function properly, since they rely on those traces being gone.
# test here

## 1) when COG is selected =========
p_cog <- p5
# identify edge traces to remove in first frame
edge_traces <- which(vapply(p_cog$x$data, function(tr) {
  identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
}, logical(1)))
# identify edge traces in other frames
edge_traces_frames <- which(vapply(p_cog$x$frame[[2]]$data, function(tr) {
  identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
}, logical(1)))
# remove both from map:
# remove in first frame:
p_cog$x$data[edge_traces] <- NULL
# remove in remaining frames:
for(i in seq_along(p_cog$x$frames)) {
  p_cog$x$frames[[i]]$data[edge_traces_frames] <- NULL
}
p_cog

## 2) when EDGE is selected =========
p_edge <- p5
# identify edge traces to remove in first frame
cog_traces <- which(vapply(p_edge$x$data, function(tr) {
  identical(tr$legendgroup, "Region1")|identical(tr$legendgroup,"Region2")
}, logical(1)))
# identify edge traces in other frames
cog_traces_frames <- which(vapply(p_edge$x$frame[[2]]$data, function(tr) {
  identical(tr$legendgroup, "Region1")|identical(tr$legendgroup,"Region2")
}, logical(1)))
# remove both from map:
# remove in first frame:
p_edge$x$data[cog_traces] <- NULL
# remove in remaining frames:
for(i in seq_along(p_edge$x$frames)) {
  p_edge$x$frames[[i]]$data[cog_traces_frames] <- NULL
}
p_edge




rm(list = ls())


