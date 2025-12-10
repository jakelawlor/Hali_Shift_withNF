# animated map prep
# make obects that will be used in animated_map_clean.R


# libraries ---------------------------------------------------------------
library(dplyr)
library(terra)
library(ggplot2)
library(sf)
library(purrr)
library(tidyterra)

# prep raw data -----------------------------------------------------------
## upload cog and edges --------
df_cog <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/POC_COG_manipulated.csv") %>%
  mutate(Region2 = ifelse(Region == "Region1","Region2",'Region1')) %>%
  mutate(Region = Region2) %>% 
  select(-Region2)

df_edge <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/POC_RangeEdge.csv")
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

# merge into one
df_edge <- df_te %>%
  mutate(edge = "Trailing Edge") %>%
  st_drop_geometry() %>%
  rbind(df_le %>% 
          mutate(edge = "Leading Edge") %>%
          st_drop_geometry())
rm(df_te, df_le)


## make cumulative lines ------------------------------------------------------

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


# make yearly line segments -----------------------------------------------
# make another dataframe that has data for line segments just between one 
# year and the next, and is colored in a greyscale palette based on the year.

# build segments: each row = segment from Year -> Year+1 within each edge
df_edge_segments <- df_edge %>%
  arrange(edge, Year) %>%
  group_by(edge) %>%
  mutate(
    lon_next  = lead(lon),
    lat_next  = lead(lat),
    Year_next = lead(Year)
  ) %>%
  # drop last year in each edge (no next point)
  filter(!is.na(lon_next), !is.na(lat_next)) %>%
  ungroup()

# set up greyscale palette by year (oldest = light, newest = dark)
year_vals <- sort(unique(df_edge_segments$Year))
pal_grey  <- colorRampPalette(c("white", "black"))
year_cols <- setNames(pal_grey(length(year_vals)), year_vals)

# make each segment a 2-point polyline, with its own id + colour
df_edge_segments_long <- df_edge_segments %>%
  group_by(edge) %>%
  mutate(
    segment_id = paste(edge, Year, sep = "_"),
    col        = year_cols[as.character(Year)],
    # 2-point polyline from (lon,lat) -> (lon_next,lat_next)
    lon = map2(lon, lon_next, ~c(.x, .y)),
    lat = map2(lat, lat_next, ~c(.x, .y))
  ) %>%
  tidyr::unnest(c(lon, lat))
rm(df_edge_segments)

# repeat for COG
df_cog_segments <- df_cog %>%
  arrange(Region, Year) %>%
  group_by(Region) %>%
  mutate(
    lon_next  = lead(centroid_longitude),
    lat_next  = lead(centroid_latitude),
    Year_next = lead(Year)
  ) %>%
  # drop last year in each edge (no next point)
  filter(!is.na(lon_next), !is.na(lat_next)) %>%
  ungroup()

# make each segment a 2-point polyline, with its own id + colour
df_cog_segments_long <- df_cog_segments %>%
  group_by(Region) %>%
  mutate(
    segment_id = paste(Region, Year, sep = "_"),
    col        = year_cols[as.character(Year)],
    # 2-point polyline from (lon,lat) -> (lon_next,lat_next)
    lon = map2(centroid_longitude, lon_next, ~c(.x, .y)),
    lat = map2(centroid_latitude, lat_next, ~c(.x, .y))
  ) %>%
  tidyr::unnest(c(lon, lat))
rm(df_cog_segments)


# fold separate datasets into one -----------------------------------------

df_points <- df_edge %>%
  mutate(param = "edge") %>%
  rename("group" = "edge") %>%
  rbind(df_cog %>%
          mutate(param = "cog") %>%
          rename("lon" = "centroid_longitude",
                 "lat" = "centroid_latitude",
                 "group" = "Region"))
rm(df_edge, df_cog)

# df for line segments which will be colored by year
df_static_lines <- df_edge_segments_long %>%
  select(Year, lon, lat, col, edge, segment_id) %>%
  rename("group" = "edge") %>%
  mutate(param = "edge") %>%
  rbind(df_cog_segments_long %>%
          select(Year, lon, lat, col, Region, segment_id) %>%
          rename("group" ="Region") %>%
          mutate(param = "cog"))
rm(df_edge_segments_long, df_cog_segments_long)

# df for line segments which will be cumulative
df_cumulative_lines <- df_edge_line %>% 
  mutate(param = "edge") %>%
  rename("group" = "edge") %>%
  rbind(df_cog_line %>%
          mutate(param = "cog") %>%
          rename("group" = "Region",
                 "lon" = "centroid_longitude",
                 "lat" = "centroid_latitude"))
rm(df_edge_line, df_cog_line)

# turn path line into spatial for plotting on pngs
lines_v <- terra::vect(
  df_static_lines,
  geom = c("lon", "lat"),
  crs  = "EPSG:4326"
)

# Reproject to the same CRS as your plotting raster (EPSG:3857)
lines_v_3857 <- terra::project(lines_v, "EPSG:3857")
rm(lines_v)
# Turn back into a data frame with x/y columns
lines_df_3857 <- terra::as.data.frame(lines_v_3857, geom = "XY") 
rm(lines_v_3857)



# make pngs of raster layers ----------------------------------------------
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
    scale_color_identity() +
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


## make static legend images -----------------------------------------------------
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

## extract legend 
legend <- cowplot::get_legend(all_png_bar)
# save
ggsave(cowplot::ggdraw(legend),
       width = 100*1.25, height = 220*1.25, units = "px", dpi = 125,
       filename = "shiny/www/rasterlegend.png")


## make double legend  -----------------------------------------------------
# redo legend, but one for the fill scale (abundance) and one for the color scale (years)
# make png image of just the colorbar for the png images
all_png_bar2 <-   ggplot() +
  geom_spatraster(data = rast[[1]] %>% terra::project("EPSG:3857")) +
  geom_path(
    data = lines_df_3857,   # or filter(lines_df_3857, Year <= years[i])
    aes(x = x, y = y, group = group, colour = as.numeric(Year)),
    linewidth = 1.25,
    alpha = .9,
    linejoin = "round",
    lineend = "round",
    linemitre = 100,
    show.legend = T
  ) +
  scale_colour_gradientn(
    colours = year_cols,
    limits  = range(year_vals),
    breaks  = pretty(year_vals, n = 10),   # or year_vals if you want all
    guide   = guide_colourbar(
      title = "Year"
    )
  ) +
  scale_fill_gradientn(colors = viridisLite::viridis(100, begin = 0, end = 1,
                                                     alpha = 1),
                       limits =  c(global_min, global_max),
                       na.value = "transparent")+
  coord_sf( expand = FALSE) +
  theme(plot.margin = margin(0,0,0,0)) +
  labs(fill = "Sqrt\nAbundance",
       color = "\nYear") +
  guides(fill = guide_colorbar(theme = theme(legend.key.width = unit(.6,"lines"),
                                             legend.key.height = unit(8,"lines")),
                               frame.colour = "black", ticks.colour = "black", 
                               frame.linewidth = .2, ticks.linewidth = .2,
                               alpha = 1),
         color =  guide_colorbar(theme = theme(legend.key.width = unit(.6,"lines"),
                                                     legend.key.height = unit(8,"lines")),
                                 frame.colour = "black", ticks.colour = "black", 
                                 frame.linewidth = .2, ticks.linewidth = .2,
                                 alpha = 1)) +
  theme(legend.background = element_rect(fill = "transparent",color = "transparent",
                                         linewidth = 0),
        legend.box.margin = margin(5,5,5,b=9),
        legend.margin = margin(0,0,0,0),
        legend.text = element_text(size = 10),
        legend.box = "horizontal",   # <--- this stacks inside a *single* box
        legend.box.background = element_rect(fill = ("white"), 
                                             color = "black",
                                             linewidth = .4)) 

## extract legend
legend2 <- cowplot::get_legend(all_png_bar2)
# save
ggsave(cowplot::ggdraw(legend2),
       width = 160*1.5, height = 220*1.5, units = "px", dpi = 150,
       filename = "shiny/www/rasterlegend2.png")
rm(global_max, global_min)
rm(lines_df_3857)
rm(legend, legend2)

# find extent for plotting .pngs on map
ext <- terra::ext(rast)
xmin <- ext[1]
xmax <- ext[2]
ymin <- ext[3] 
ymax <- ext[4]

coords <- list(
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
)
rm(ext, ymin, ymax, xmin, xmax)



# process other shapes --------------------------------------------------------
# make bounding box to which we'll cut other shapes
crs <- st_crs(rast)
study_area_bbox <- st_bbox(c(xmin = -80, ymin = 30, xmax = -44, ymax = 60))
study_area_polygon <- st_as_sfc(study_area_bbox)
st_crs(study_area_polygon) <- crs
rm(study_area_bbox)

# upload fake border between regions
border <- st_read("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/TheorheticalBorder_Shapefile/AdministrativeBorder.shp")
border <- st_transform(border, crs)


# upload and process canada eez
eez_can <- st_read("Data/eez_can/eez.shp")
eez_can <- rmapshaper::ms_simplify(eez_can)  # simplify vertices
eez_can <- eez_can %>% select(geometry) # extract geometry only
# cast to linestring
eez_can_ls <- eez_can %>% st_cast("MULTILINESTRING")
# crop to  study area
eez_can_ls <-  st_intersection(eez_can_ls, study_area_polygon)
# cast to linestring again
eez_can_ls2 <- st_cast(eez_can_ls, "LINESTRING")
# filter to only long lines
filtered_lines <- eez_can_ls2[as.numeric(st_length(eez_can_ls2)) >= 3000000, ]

# here, we only want the outline lines of the eez, not the coastal lines
# so we're going to disect it manually

# cast to points
points <- filtered_lines[1,] %>% st_cast("MULTIPOINT")
points2 <- points %>% st_cast("POINT")
# view
ggplot() +
  geom_sf(data = points2 %>%
            mutate(n = 1:n()),
          aes(color = n)) + 
  scale_color_viridis_c()
# keep only outer points -- approximate
points3 <- points2[5000:nrow(points2),]

# cast points to line
line <- points3 %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>%
  st_as_sf() %>%
  rmapshaper::ms_simplify(keep = .5)

# view
ggplot() +
  geom_sf(data = line) + 
  scale_color_viridis_c()
# ok, perfect - we got a simplified linestring that represents the outside of the eez
rm(points, points2, points3, eez_can, eez_can_ls, eez_can_ls2, filtered_lines)


eez <- line %>% select(-rmapshaperid) %>% mutate(country = "Canada")
rm(line)

# change to dataframe for plotly
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
  mutate(country = case_when(L1 == 1 ~ "Canada"#,
                            #L1 == 2 ~ "USA"
                            ))

# change border line to dataframe for plotly
border2 <- border %>%
  st_cast("LINESTRING") %>%
  # add segments every km
  st_segmentize(dfMaxLength = 1000) %>%
  st_coordinates() %>%
  as.data.frame()
rm(border)


# upload and process NAFO zones
nafo <-st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
# get zone names
nafo_zones <- nafo %>%
  st_drop_geometry() %>%
  filter(!is.na(ZONE)) %>%
  pull(ZONE)
# save zone names - to be used in shiny
saveRDS(nafo_zones, "shiny/www/nafo_zone_names.rds")

# remove borders of islands from NAFO shapes
nafo <- nafo %>% filter(!is.na(ZONE))

# find number of points in each
n_coords <- map(.x = nafo %>% split(f = nafo$ZONE),
    .f = ~nrow(st_coordinates(.x))) %>% unlist()
# some zones are very dense - ~20k+ vertices 
# we'll need to heavily simplify them to work in plotly plot

## simplify nafo zones -----------------------------------
# split into list
nafo_list <- nafo %>% split(f = nafo$ZONE) 
# loop through list and simplify all shapes to be ~200 vertices
for(i in 1:length(nafo_list)){
  
  # calculate factor to keep to reach ~200 points in simplified shape
  factor <- round(200/n_coords[i],3)
  
  nafo_list[[i]] <- nafo_list[[i]] %>%
    rmapshaper::ms_simplify(keep = factor, keep_shapes = T)
  print(i)
}

# bind together
nafo <- bind_rows(nafo_list, .id = "ZONE")
rm(nafo_list)

## convert to dataframes 
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

# add a line of NA coordinates after each distinct shape,
# or else plotly will plot lines connecting the last vertex of
# each shape to the first vertex of the next shape
zone_coords2 <- map(
  .x = zone_coords,
  .f = ~purrr::map(.x, function(poly) poly %>% 
                     rbind(data.frame(lon=NA, lat=NA, zone = unique(poly$zone)))) %>%
    bind_rows()
)
rm(zone_coords)

## get nafo centroids ------------------------- 
nafo_centroids <- st_centroid(nafo) %>%
  select(ZONE) %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2]) %>%
  st_drop_geometry()

rm(nafo)


# save processed objects --------------------------------------------------
# create directory
dir.create(path = "shiny/shiny_processed_data/map_objects/", showWarnings = F)
# save
saveRDS(eez_df, file = "shiny/shiny_processed_data/map_objects/eez_df.rds")
saveRDS(border2, file = "shiny/shiny_processed_data/map_objects/border.rds")
saveRDS(zone_coords2, file = "shiny/shiny_processed_data/map_objects/zone_coords2.rds")
saveRDS(coords, file = "shiny/shiny_processed_data/map_objects/png_coords.rds")
saveRDS(years, file = "shiny/shiny_processed_data/map_objects/rasteryears.rds")
saveRDS(nafo_centroids, file = "shiny/shiny_processed_data/map_objects/nafo_centroids.rds")
saveRDS(df_points, file = "shiny/shiny_processed_data/map_objects/df_points.rds")
saveRDS(df_cumulative_lines, file = "shiny/shiny_processed_data/map_objects/df_cumulative_lines.rds")
saveRDS(df_static_lines, file = "shiny/shiny_processed_data/map_objects/df_static_lines.rds")
#saveRDS(nafo_geojson_obj, file = "shiny/shiny_processed_data/map_objects/nafo_geojson.rds")


