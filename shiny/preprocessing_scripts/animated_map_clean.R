# animated map clean script



# packages ----------------------------------------------------------------

library(dplyr)
library(terra)
library(sf)
library(plotly)
library(stringr)
library(tidyterra)
library(purrr)


# upload data -------------------------------------------------------------
# get all abundance raster files
rast_files <- list.files("Data/Data_ShinyApp_Proof_of_Concept/EstAbundanceRasters/",
                    full.names=T)
# keep only sqrt abundance files
rast_files <-rast_files[stringr::str_detect(rast_files, 'Sqrt')]

# upload all files in a terra raster
rast <- rast(rast_files)
years <- stringr::str_extract(rast_files, "\\d+")
names(rast) <- paste0("Yr",years)
rm(rast_files)

# make pngs from each raster year

# get min and max value for color scale
layer_ranges <- minmax(rast) 
global_min <- min(layer_ranges[1,])    # Find the minimum across all layer minimums
global_max <- max(layer_ranges[2,])  
rm(layer_ranges)

# get matrix for dimensions
mat <- as.matrix(rast[[1]], wide = TRUE)
dim(mat)

for(i in 1:length(years)){
  # create static ggplot
  plot <- ggplot() +
    geom_spatraster(data = rast[[i]] %>% terra::project("EPSG:3857")) +
    scale_fill_gradientn(colors = viridisLite::viridis(100, begin = 0, end = 1,
                                          alpha = 1),
                         limits =  c(global_min, global_max),
                         na.value = "transparent")+
    theme_void() +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    coord_sf( expand = FALSE) +
    theme(plot.margin = margin(0,0,0,0))
  
  # save ggplot as png
  ggsave(plot,
         # NOTE that I had to fiddle with these dimensions to make them work
         # need to make sure the .png image doesn't have any extra white space
         # either along the top/bottom or right/left
         width = ncol(mat)*3.475*2, 
         height = nrow(mat)*5*2, 
         unit = "px",
         filename = paste0("shiny/shiny_processed_data/rasterPngs/yr",years[i],".png"))
  print(i)
  
}
rm(i, mat, plot)

# make png image of just the colorbar for the png images
all_png_bar <-   ggplot() +
  geom_spatraster(data = rast[[1]] %>% terra::project("EPSG:3857")) +
  scale_fill_gradientn(colors = viridisLite::viridis(100, begin = 0, end = 1,
                                                     alpha = 1),
                       limits =  c(global_min, global_max),
                       na.value = "transparent")+
  coord_sf( expand = FALSE) +
  theme(plot.margin = margin(0,0,0,0)) +
  labs(fill = "Sqrt\nAbundance") +
  guides(fill = guide_colorbar(theme = theme(legend.key.width = unit(.8,"lines"),
                                             legend.key.height = unit(8,"lines")))) +
  theme(legend.background = element_rect(fill = "white",color = "black",
                                         linewidth = .4),
        legend.text = element_text(size = 10))

all_png_bar
legend <- cowplot::get_legend(all_png_bar)

ggsave(cowplot::ggdraw(legend),
       width = 100*1.25, height = 220*1.25, units = "px", dpi = 125,
       filename = "shiny/shiny_processed_data/rasterlegend.png")

rm(global_max, global_min)

# make empty list of years
png_base64_list <- vector(mode = "list",
                          length = length(years)) %>%
  purrr::set_names(years)

# convert all .pngs to URIs
for(i in years){
  png_base64_list[[as.character(i)]] <-   base64enc::dataURI(file = paste0("shiny/shiny_processed_data/rasterPngs/yr",i,".png"), mime = "image/png")
  print(i)
}

# find extent for plotting .pngs on map
ext <- terra::ext(rast)
xmin <- ext[1]; xmax <- ext[2]
ymin <- ext[3]; ymax <- ext[4]

coords <- list(
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
)
rm(ext, ymin, ymax, xmin, xmax, i)



# upload cog and edges ----------------------------------------------------
df_cog <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/POC_COG.csv")
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

df_edge <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/POC_RangeEdge.csv")
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



# get other shapes --------------------------------------------------------
crs <- st_crs(rast)
study_area_bbox <- st_bbox(c(xmin = -80, ymin = 30, xmax = -44, ymax = 60))
study_area_polygon <- st_as_sfc(study_area_bbox)
st_crs(study_area_polygon) <- crs
rm(study_area_bbox)

#Read shapefiles match projections, and clip to bounding box
Hague <-  st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))
Hague <- st_transform (Hague, crs)

# upload and process canada eez
eez_can <- st_read("Data/eez_can/eez.shp")
eez_can <- rmapshaper::ms_simplify(eez_can) 
eez_can <- eez_can %>%
  select(geometry)
eez_can_ls <- eez_can %>%
  st_cast("MULTILINESTRING")
eez_can_ls <-  st_intersection(eez_can_ls, study_area_polygon)
eez_can_ls2 <- st_cast(eez_can_ls, "LINESTRING")
filtered_lines <- eez_can_ls2[as.numeric(st_length(eez_can_ls2)) >= 3000000, ]

points <- filtered_lines[1,] %>% st_cast("MULTIPOINT")
points2 <- points %>% st_cast("POINT")
ggplot() +
  geom_sf(data = points2 %>%
            mutate(n = 1:n()),
          aes(color = n)) + 
  scale_color_viridis_c()
points3 <- points2[5000:nrow(points2),]

line <- points3 %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_as_sf() %>%
  rmapshaper::ms_simplify(keep = .5)

ggplot() +
  geom_sf(data = line) + 
  scale_color_viridis_c()
rm(points, points2, points3, eez_can, eez_can_ls, eez_can_ls2, filtered_lines)

# do US eez
eez_us <- st_read("Data/eez_US/eez.shp")
eez_us <- rmapshaper::ms_simplify(eez_us) 
eez_us <- eez_us %>%
  select(geometry)
eez_us_ls <- eez_us %>%
  st_cast("MULTILINESTRING")
eez_us_ls <-  st_intersection(eez_us_ls, study_area_polygon)
eez_us_ls2 <- st_cast(eez_us_ls, "LINESTRING")
filtered_lines_us <- eez_us_ls2[as.numeric(st_length(eez_us_ls2)) >= 3000000, ]
ggplot() +
  geom_sf(data = filtered_lines_us)


points_us <- filtered_lines_us[1,] %>% st_cast("MULTIPOINT")
points2_us <- points_us %>% st_cast("POINT")
ggplot() +
  geom_sf(data = points2_us %>%
            mutate(n = 1:n()),
          aes(color = n)) + 
  scale_color_viridis_c()
points3_us <- points2_us[1:133,]
ggplot() +
  geom_sf(data = points3_us)

line_us <- points3_us %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_as_sf() 

ggplot() +
  geom_sf(data = line) + 
  geom_sf(data = line_us) 
rm(eez_us, eez_us_ls, eez_us_ls2, filtered_lines_us, points_us, points2_us, points3_us)

eez <- rbind(line %>%
               select(-rmapshaperid) %>%
               mutate(country = "Canada"),
             line_us %>% mutate(country = "US"))
rm(line, line_us)


eez_df <- eez %>%
  st_cast("LINESTRING") %>%
  st_coordinates() %>%
  as.data.frame() %>%
  group_by(L1) %>%
  tidyr::nest() %>%
  mutate(
    data = purrr::map(data, ~bind_rows(.x, tibble(X = NA_real_, Y = NA_real_)))
  ) %>%
  tidyr::unnest(data) %>%
  mutate(country = case_when(L1 == 1 ~ "Canada",
                            L1 == 2 ~ "USA"))


hague2 <- Hague %>%
  st_cast("LINESTRING") %>%
  st_segmentize(dfMaxLength = 1000) %>%
  st_coordinates() %>%
  as.data.frame()
rm(Hague)


# upload and process NAFO zones
nafo <-st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
nafo_zones <- nafo %>%
  st_drop_geometry() %>%
  filter(!is.na(ZONE)) %>%
  pull(ZONE)
saveRDS(nafo_zones,
        "shiny/www/nafo_zone_names.rds")

# remove islands from NAFO shapes
nafo <- nafo %>%
  filter(!is.na(ZONE))

# find number of points in each
n_coords <- map(.x = nafo %>% split(f = nafo$ZONE),
    .f = ~nrow(st_coordinates(.x))) %>% unlist()

nafo_list <- nafo %>% split(f = nafo$ZONE)
for(i in 1:length(nafo_list)){
  
  # calculate factor to keep to reach ~200 points in simplified shape
  factor <- round(200/n_coords[i],3)
  
  nafo_list[[i]] <- nafo_list[[i]] %>%
    rmapshaper::ms_simplify(keep = factor, keep_shapes = T)
  print(i)
}

nafo <- bind_rows(nafo_list, .id = "ZONE")
rm(nafo_list)


zone_coords <- map(1:nrow(nafo), function(i) {
  geom <- st_geometry(nafo)[[i]]
  geom <- st_cast(geom, "POLYGON")   # handle multipolygons
  map(geom, function(poly) {
    coords <- poly
    data.frame(
      lon = coords[,1],
      lat = coords[,2],
      zone = nafo$ZONE[i]
    )
  })
})
zone_coords2 <- map(
  .x = zone_coords,
  .f = ~purrr::map(.x, function(poly) poly %>% rbind(data.frame(lon=NA, lat=NA, zone = poly$zone))) %>%
    bind_rows()
)
rm(zone_coords, nafo)




# begin plot --------------------------------------------------------------

# base plot
p <-   plot_ly(type = "scattermapbox",
               colors = c("Canada" = "#D60E0A","USA" = "#0882bf",
                          "Trailing Edge" = "#10D183",
                          "Leading Edge" = "#F4D35E")) %>%

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
    data = hague2,
    mode = "lines",
    type = "scattermapbox",
    lon = ~X,
    lat = ~Y,
    hoverinfo = "text",
    text = ~paste0("Maritime Boundary"),
    line = list(color = "#132f4d", 
                width = 3),
    showlegend = F
  ) %>%

  # add centroid traces, CANADA ==============
  # add static centroid lines - black
  add_trace(
    data = df_cog %>% filter(Region == "Canada"), 
    legendgroup = "Canada",
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
    data  = df_cog_line %>% filter(Region == "Canada"),
    legendgroup = "Canada",
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
    data = df_cog %>% filter(Region == "Canada"),
    legendgroup = "Canada",
    name = "Centroid in Canada",
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
  data = df_cog %>% filter(Region == "USA"), 
  legendgroup = "USA",
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
    data  = df_cog_line %>% filter(Region == "USA"),
    legendgroup = "USA",
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
    data = df_cog %>% filter(Region == "USA"),
    legendgroup = "USA",
    name = "Centroid in USA",
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
    line = list(color = "orange", width = 2),
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
        label = "Show Zones",
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


p4 <- onRender(p4, "
function(el, x) {
  var gd = document.getElementById(el.id);

  //---------------------------------------------
  // 1. HOVER BOLDING FOR ZONE TRACES
  //---------------------------------------------
  var baseWidth = 2;
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

    if (lbl === 'Show Zones') {
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



# practice making layers disappear ----------------------------------------

test <- halibut_plots$p_map
lapply(test$x$data, function(t) t$legendgroup)
lapply(test$x$frames[[]]$data, function(t) t$legendgroup)

lapply(p3$x$data, function(t) t$legendgroup)
trace_groups <- list(
  "Canada" = c(4,5,6),
  "USA" = c(7,8,9),
  "Leading Edge" = c(10,11,12),
  "Trailing Edge" = c(13,14,15)
)

plotlyProxy("p3", session) %>%
  plotlyProxyInvoke("restyle",
                    list(visible = TRUE),
                    trace_groups$Canada
  )

plotlyProxy("p3")
