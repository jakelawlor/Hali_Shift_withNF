# animated map clean script



# packages ----------------------------------------------------------------

library(dplyr)
library(terra)
library(sf)
library(plotly)
library(stringr)
library(tidyterra)
library(purrr)

source("shiny/preprocessing_scripts/00_plot_opts.R")


eez_df <- readRDS( file = "shiny/shiny_processed_data/map_objects/eez_df.rds")
border2 <- readRDS( file = "shiny/shiny_processed_data/map_objects/border.rds")
zone_coords2 <- readRDS(file = "shiny/shiny_processed_data/map_objects/zone_coords2.rds")
coords <- readRDS(file = "shiny/shiny_processed_data/map_objects/png_coords.rds")
years <- readRDS(file = "shiny/shiny_processed_data/map_objects/rasteryears.rds")



# upload raster pngs ------------------------------------------------------

png_base64_list <- vector(mode = "list",
                          length = length(years)) %>%
  purrr::set_names(years)

# convert all .pngs to URIs
for(i in years){
  png_base64_list[[as.character(i)]] <-   base64enc::dataURI(file = paste0("shiny/shiny_processed_data/rasterPngs/yr",i,".png"), mime = "image/png")
  print(i)
}


# upload cog and edges ----------------------------------------------------
df_cog <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/POC_COG_manipulated.csv")
df_cog_line <- df_cog %>%
  arrange(Region, Year) %>%
  group_by(Region) %>%
  # for each year, keep all rows up to that year
  group_modify(~{
    this <- .
    # all distinct years for that region
    yrs <- sort(unique(this$Year))
    # build cumulative rows for each frame year
    bind_rows(lapply(yrs, function(fy) {
      cum <- this[this$Year <= fy, ]
      cum$frame_year <- fy
      cum
    }))
  }) %>%
  ungroup() %>%
  select(-Year) %>%
  rename(Year = frame_year)


# upload edges ------------------------------------------------------------

df_edge <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/POC_RangeEdge.csv")
df_edge %>% glimpse()
# edge coordinates are in east and west (meters?)
# change to geographic coords
Est_0.05<-df_edge[ , c("Year","Estimate_km_E_quantile_0.05", "Estimate_km_N_quantile_0.05" )]
Est_0.95<-df_edge[ , c("Year", "Estimate_km_E_quantile_0.95",  "Estimate_km_N_quantile_0.95")] 

# Convert from km to m,  make sf object in local UTM (meters), and reproject
convert_km_to_wgs84 <- function(df, east_col, north_col) {
  # Convert from km to m
  df[[east_col]] <- df[[east_col]] * 1000
  df[[north_col]] <- df[[north_col]] * 1000
  # Make sf object in local CRS
  pts_local <- st_as_sf(df, coords = c(east_col, north_col), crs = "EPSG:32621")
  # Reproject to WGS84
  pts_wgs84 <- st_transform(pts_local, crs = 4326)
  pts_wgs84["lon"] <- st_coordinates(pts_wgs84)[,1]
  pts_wgs84["lat"] <- st_coordinates(pts_wgs84)[,2]
  return(pts_wgs84)
}

#Trailing edge Estimates
df_te <- convert_km_to_wgs84(
  Est_0.05,
  east_col = "Estimate_km_E_quantile_0.05",
  north_col = "Estimate_km_N_quantile_0.05"
)
#Leading edge Estimates
df_le <- convert_km_to_wgs84(
  Est_0.95,
  east_col = "Estimate_km_E_quantile_0.95",
  north_col = "Estimate_km_N_quantile_0.95"
) 
rm(Est_0.05, Est_0.95, convert_km_to_wgs84, df_edge)

df_edge <- df_te %>%
  mutate(edge = "Trailing Edge") %>%
  st_drop_geometry() %>%
  rbind(df_le %>% 
          mutate(edge = "Leading Edge") %>%
          st_drop_geometry())
df_edge_line <- df_edge %>%
  arrange(edge, Year) %>%
  group_by(edge) %>%
  # for each year, keep all rows up to that year
  group_modify(~{
    this <- .
    # all distinct years for that region
    yrs <- sort(unique(this$Year))
    # build cumulative rows for each frame year
    bind_rows(lapply(yrs, function(fy) {
      cum <- this[this$Year <= fy, ]
      cum$frame_year <- fy
      cum
    }))
  }) %>%
  ungroup() %>%
  select(-Year) %>%
  rename(Year = frame_year)
rm(df_te, df_le)




# begin plot --------------------------------------------------------------

# base plot
p <-   plot_ly(type = "scattermapbox",
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
        )
      )#,
      # # make slider
      # sliders = list(
      #   list(
      #     active = 0,
      #     steps = lapply(1:length(years), function(i) {
      #       list(
      #         method = "animate",
      #         args = list(list(years[i]), 
      #                     list(mode = "immediate", 
      #                          frame = list(duration = 300, redraw = TRUE),
      #                          transition = list(duration = 300))),
      #         label = years[i]
      #       )
      #     })
      #   )
      # )
      
    ))  %>%
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
  ) %>%

  # add centroid traces, CANADA ==============
  # add static centroid lines - black
  add_trace(
    data = df_cog %>% filter(Region == "Region1"), 
    legendgroup = "Region1",
    showlegend =  F,
    type = "scattermapbox",
    mode = "lines",
    lon = ~centroid_longitude,
    lat = ~centroid_latitude,
    color = ~Region,
    line = list(width = 2, opacity = 1,
                color = "black"),
    hoverinfo = "none"
  ) %>%
  # add moving centroid lines - colored
  add_trace(
    data  = df_cog_line %>% filter(Region == "Region1"),
    legendgroup = "Region1",
    showlegend =  F,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~centroid_longitude,
    lat   = ~centroid_latitude,
    frame = ~Year,         # the cumulative frame column
    color = ~Region,
    hoverinfo = "none"
  ) %>%
  # add animated centroid points 
  add_trace(
    data = df_cog %>% filter(Region == "Region1"),
    legendgroup = "Region1",
    name = "Centroid in Region1",
    type = "scattermapbox",
    mode = "markers",
    lon = ~centroid_longitude,
    lat = ~centroid_latitude,
    color = ~Region,
    frame = ~Year,
    marker = list(size = 15),
    text = ~paste0("<b>",Region,"</b> ",Year,"<br>",
                   round(centroid_latitude,2)," °N<br>",
                   round(centroid_longitude,2)," °W"),
    hoverinfo = "text"
  ) %>%
  
  # add centroid traces, USA ==============
# add static centroid lines - black
add_trace(
  data = df_cog %>% filter(Region == "Region2"), 
  legendgroup = "Region2",
  showlegend =  F,
  type = "scattermapbox",
  mode = "lines",
  lon = ~centroid_longitude,
  lat = ~centroid_latitude,
  color = ~Region,
  line = list(width = 2, opacity = 1,
              color = "black"),
  hoverinfo = "none"
) %>%
  # add moving centroid lines - colored
  add_trace(
    data  = df_cog_line %>% filter(Region == "Region2"),
    legendgroup = "Region2",
    showlegend =  F,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~centroid_longitude,
    lat   = ~centroid_latitude,
    frame = ~Year,         # the cumulative frame column
    color = ~Region,
    hoverinfo = "none"
  ) %>%
  # add animated centroid points 
  add_trace(
    data = df_cog %>% filter(Region == "Region2"),
    legendgroup = "Region2",
    name = "Centroid in Region2",
    type = "scattermapbox",
    mode = "markers",
    lon = ~centroid_longitude,
    lat = ~centroid_latitude,
    color = ~Region,
    frame = ~Year,
    marker = list(size = 15),
    text = ~paste0("<b>",Region,"</b> ",Year,"<br>",
                   round(centroid_latitude,2)," °N<br>",
                   round(centroid_longitude,2)," °W"),
    hoverinfo = "text"
  ) %>%
  # add edge traces, LEADING EDGE ======================
  # add static centroid lines - black
  add_trace(
    data = df_edge %>% filter(edge == "Leading Edge"), 
    legendgroup = "Leading Edge",
    showlegend = F,
    type = "scattermapbox",
    mode = "lines",
    lon = ~lon,
    lat = ~lat,
    color = ~edge,
    line = list(width = 2, opacity = 1,
                color = "black"),
    hoverinfo = "none"
  ) %>%
  # add animated centroid lines - colored
  add_trace(
    data = df_edge_line %>% filter(edge == "Leading Edge"), 
    legendgroup = "Leading Edge",
    showlegend = F,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~lon,
    lat   = ~lat,
    frame = ~Year,         # the cumulative frame column
    color = ~edge,
    hoverinfo = "none"
  ) %>%
  
  # add edge points - animated
  add_trace(
    data = df_edge %>% filter(edge == "Leading Edge"), 
    legendgroup = "Leading Edge",
    type = "scattermapbox",
    mode = "markers",
    lon = ~lon,
    lat = ~lat,
    color = ~edge,
    frame = ~Year,
    marker = list(size = 15),
    text = ~paste0("<b>",edge,"</b> ",Year,"<br>",
                   round(lat,2)," °N<br>",
                   round(lon,2)," °W"),
    hoverinfo = "text"
  ) %>%
  # add edge traces, TRAILING EDGE ======================
# add static centroid lines - black
add_trace(
  data = df_edge %>% filter(edge == "Trailing Edge"), 
  legendgroup = "Trailing Edge",
  showlegend = F,
  type = "scattermapbox",
  mode = "lines",
  lon = ~lon,
  lat = ~lat,
  color = ~edge,
  line = list(width = 2, opacity = 1,
              color = "black"),
  hoverinfo = "none"
) %>%
  # add animated centroid lines - colored
  add_trace(
    data = df_edge_line %>% filter(edge == "Trailing Edge"), 
    legendgroup = "Trailing Edge",
    showlegend = F,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~lon,
    lat   = ~lat,
    frame = ~Year,         # the cumulative frame column
    color = ~edge,
    hoverinfo = "none"
  ) %>%
  
  # add edge points - animated
  add_trace(
    data = df_edge %>% filter(edge == "Trailing Edge"), 
    legendgroup = "Trailing Edge",
    type = "scattermapbox",
    mode = "markers",
    lon = ~lon,
    lat = ~lat,
    color = ~edge,
    frame = ~Year,
    marker = list(size = 15),
    text = ~paste0("<b>",edge,"</b> ",Year,"<br>",
                   round(lat,2)," °N<br>",
                   round(lon,2)," °W"),
    hoverinfo = "text"
  ) 

  
p

# use plotly_build so p$x$frames is created
p2 <- plotly_build(p)
p2

# loop through frames and add custom pngs from list
for (i in seq_along(p2$x$frames)) {
  yr <- p2$x$frames[[i]]$name 
  
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


# add pause button
p2$x$layout$updatemenus <- list(
  list(
    type = "buttons",
    showactive = T,
    direction = "left",
    x = .2,
    y = -.05,
    pad = list(t = 0, r = 10),
    buttons = list(
      list(
        label = "Play",
        method = "animate",
        args = list(
          NULL,
          list(
            fromcurrent = TRUE,
            frame = list(duration = 300, redraw = FALSE),
            transition = list(duration = 0)
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
  )
) 


p3 <- p2 %>%
  layout(margin = list(l = 0, r = 0, t = 0, b = 0),
             legend = list(x = 0.03,
                       y = 0.98,
                       bordercolor = "black",
                       borderwidth = 1))


#rm(p, p2)
p3
  

# add nafo traces - invisible
p4 <- p3

# add buttons to turn on/off
n_poly <- length(zone_coords2)
base_traces <- plotly_build(p4)$x$data
n_base <- length(base_traces) 

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
    name = paste0("zone_", df_i$zone[i]),    # This adds a hover halo that looks like bolding
    selected = list(line = list(width = 6)),
    unselected = list(line = list(width = 2)),
    visible = FALSE  # hidden by default for toggle button
  )
}
p4

all_off <- c(rep(TRUE, n_base), rep(FALSE, n_poly))
all_on  <- c(rep(TRUE, n_base), rep(TRUE,  n_poly))


p4$x$layout$updatemenus <-  list(
  
  # ---- GROUP 1: Play / Pause ----
  list(
    type = "buttons",
    showactive = TRUE,
    direction = "right",
    x = 0.03,      # adjust to taste
    y = -0.03,    # place below plot
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
            frame = list(duration = 300, redraw = TRUE),
            transition = list(duration = 0)
          )
        )
      ),
      list(
        label = "Pause",
        method = "animate",
        args = list(
          NULL,                           
          list(
            mode  = "immediate",
            frame     = list(duration = 0, redraw = FALSE),
            transition = list(duration = 0)
          )
        )
      )
    )
  ),
  
  # ---- GROUP 2: Toggle Zones ----
  list(
    type = "buttons",
    showactive = TRUE,
    direction = "right",
    x = 0.9,      # place this group on the right side
    xanchor = "right",
    y = -0.03,
    pad = list(t = 0, r = 10),
    buttons = list(
      list(
        label = "Show NAFO Zones",
        method = "update",
        #args = list(list(visible = all_on))
        args = list(list())  
      ),
      list(
        label = "Hide Zones",
        method = "update",
       # args = list(list(visible = all_off))
       args = list(list())  
      )
    )
  )
)

p4


p4 <- htmlwidgets::onRender(p4, "
function(el, x) {
  var gd = document.getElementById(el.id);

  //---------------------------------------------
  // 1. HOVER BOLDING FOR ZONE TRACES
  //---------------------------------------------
  var baseWidth = 1;
  var boldWidth = 5;

  gd.on('plotly_hover', function(e) {
    var tr = e.points[0].curveNumber;
    var n  = x.data.length;

    var widths = new Array(n).fill(baseWidth);
    var hoveredName = (x.data[tr].name || '');

    if (hoveredName.startsWith('zone_')) {
      widths[tr] = boldWidth;
    }

    Plotly.restyle(gd, {'line.width': widths});
  });

  gd.on('plotly_unhover', function(e) {
    var n = x.data.length;
    var widths = new Array(n).fill(baseWidth);
    Plotly.restyle(gd, {'line.width': widths});
  });

  //---------------------------------------------
  // 2. SHOW / HIDE ZONE TRACES BY NAME
  //---------------------------------------------
  function computeVisibility(showZones) {
    var vis = [];
    for (var i = 0; i < x.data.length; i++) {
      var nm = x.data[i].name || '';
      if (nm.startsWith('zone_')) {
        vis.push(showZones);
      } else {
        vis.push(true);  // never hide base traces
      }
    }
    return vis;
  }

  gd.on('plotly_buttonclicked', function(e) {
    var lbl = e.button.label;

    if (lbl === 'Show NAFO Zones') {
      Plotly.restyle(gd, {visible: computeVisibility(true)});
    }
    if (lbl === 'Hide Zones') {
      Plotly.restyle(gd, {visible: computeVisibility(false)});
    }
  });

}
")



# add fake colorscale for png images
colorbar_base64 <- base64enc::dataURI(file ="shiny/www/rasterlegend.png", mime="image/png")

p5 <- p4 %>%
  layout(
    images = list(
      list(
        source = colorbar_base64,
        x = 0.03, y = 0.6,
        xanchor = "left",
        yanchor = "top",
        sizex = .1*2.2, sizey = .22*2.2,
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

p5

saveRDS(p5, "shiny/www/premade_plots/mapplot.rds")

test <- readRDS( "shiny/www/premade_plots/mapplot.rds")

rm(list = ls())

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

#
## practice making layers disappear ----------------------------------------
#
#test <- halibut_plots$p_map
#lapply(test$x$data, function(t) t$legendgroup)
#lapply(test$x$frames[[]]$data, function(t) t$legendgroup)
#
#lapply(p3$x$data, function(t) t$legendgroup)
#trace_groups <- list(
#  "Canada" = c(4,5,6),
#  "USA" = c(7,8,9),
#  "Leading Edge" = c(10,11,12),
#  "Trailing Edge" = c(13,14,15)
#)
#
#plotlyProxy("p3", session) %>%
#  plotlyProxyInvoke("restyle",
#                    list(visible = TRUE),
#                    trace_groups$Canada
#  )
#
#plotlyProxy("p3")
#