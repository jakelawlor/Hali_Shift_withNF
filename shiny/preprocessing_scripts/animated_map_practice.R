

# plotly animation --------------------------------------------------------


library(terra)
# get all abundance raster files
files <- list.files("Data/Data_ShinyApp_Proof_of_Concept/EstAbundanceRasters/",
                    full.names=T)
files <-files[stringr::str_detect(files, 'Sqrt')]

# upload all files
rast <- rast(files)
years <- stringr::str_extract(files, "\\d+")
names(rast) <- paste0("Yr",years)

df_cog <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/POC_COG.csv")
#df_edge <- readr::read_csv("Data/Data_ShinyApp_Proof_of_Concept/POC_RangeEdge.csv")

plot(rast[[1:2]])

rast[[1]] %>% values()

plot_ly() %>%
  add_trace(data = rast[[1]],
            )

raster_matrix <- as.data.frame(rast[[1:3]],
                               xy=T)

# Convert to data.frame with coordinates (keeps NAs)
r_df <- as.data.frame(rast, xy = TRUE)

# Unique sorted coords; y should be decreasing so top row is first for Plotly
x_vals <- sort(unique(r_df$x))
y_vals <- sort(unique(r_df$y), decreasing = TRUE)

nx <- length(x_vals)
ny <- length(y_vals)

# Pre-compute integer indices (fast) for each row in r_df
# match returns the column/row numbers that we will use to fill matrices
col_idx <- match(r_df$x, x_vals)
row_idx <- match(r_df$y, y_vals)  # because y_vals is decreasing, this matches Plotly orientation


# helper to build matrix for a given layer name
build_matrix_for_layer <- function(df, col_index_vec, row_index_vec, layer_name, nrow = ny, ncol = nx) {
  vals <- df[[layer_name]]            # numeric vector including NAs, same length as df rows
  mat <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  # Fill matrix at the correct (row, col) positions
  mat[cbind(row_index_vec, col_index_vec)] <- vals
  mat
}


# Test single layer quickly (static)
test_mat <- build_matrix_for_layer(r_df, col_idx, row_idx, names(rast)[1])
plot_ly(z = test_mat, x = x_vals, y = y_vals, type = "heatmap")  # quick sanity check

# Build frames for animation: list of lists with data + frame name
frames <- lapply(names(rast), function(layer) {
  mat <- build_matrix_for_layer(r_df, col_idx, row_idx, layer)
  list(
    name = gsub("Yr", "", layer),
    data = list(
      list(
        z = mat,
        x = x_vals,
        y = y_vals,
        zmin = global_min,
        zmax = global_max,
        zauto=F, 
        type = "heatmap",
        colorscale = "Viridis",
        zauto = TRUE
      )
    )
  )
})

# Create initial data (first layer)
initial_mat <- build_matrix_for_layer(r_df, col_idx, row_idx, names(rast)[1])

max(rast)
 global_min = 0
 global_max = 47398

fig <- plot_ly(
  z = initial_mat,
  x = x_vals,
  y = y_vals,
  zmin = global_min,
  zmax = global_max,
  zauto=F, 
  type = "heatmap",
  colorscale = "Viridis",
  zauto = TRUE
) %>%
  layout(
    title = "Raster Data Plotly Heatmap",
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude"),
    updatemenus = list(
      list(
        type = "buttons",
        showactive = FALSE,
        buttons = list(
          list(
            label = "Play",
            method = "animate",
            args = list(NULL, list(frame = 500, transition = 0, fromcurrent = TRUE, mode = "immediate"))
          ),
          list(
            label = "Pause",
            method = "animate",
            args = list(NULL, list(mode = "immediate", frame = 0, transition = 0))
          )
        )
      )
    ),
    sliders = list(
      list(
        active = 0,
        currentvalue = list(prefix = "Year: "),
        steps = lapply(frames, function(f) {
          list(method = "animate", args = list(list(f$name)), label = f$name)
        })
      )
    )
  ) %>%
  animation_opts(frame = 500, transition = 0, redraw = FALSE) 

fig$x$frames <- frames
fig

# get unique sorted x/y coordinates
x_vals <- sort(unique(raster_matrix$x))
y_vals <- sort(unique(raster_matrix$y), decreasing = TRUE)  # Plotly expects top-to-bottom

length(x_vals) * length(y_vals)

# helper to reshape a single year column into a matrix
get_matrix <- function(df, col) {
  matrix(df[[col]], 
         nrow = length(y_vals), 
         ncol = length(x_vals), 
         byrow = F)
}

matrix(raster_matrix[["Yr1991"]])

get_matrix(raster_matrix, "Yr1990")

frames <- lapply(names(r)[1:3], function(layer) {
  list(
    data = list(
      list(
        z = get_matrix(raster_matrix, layer),
        x = x_vals,
        y = y_vals,
        type = "heatmap",
        colorscale = "Viridis"
      )
    ),
    name = gsub("Yr", "", layer)
  )
})

raster_matrix_piv <- raster_matrix %>%
  tidyr::pivot_longer(cols = contains("Yr"),
                      names_to = "year",
                      values_to = "value")  %>%
  mutate(year = as.numeric(stringr::str_remove(year, "Yr"))) %>%
  arrange(year)

raster_matrix_piv %>% head()

# Get coordinates for plotting (optional, but helpful for accurate spatial representation)


# Create the Plotly heatmap
fig <- plot_ly(
  data = raster_matrix_piv,
  x = ~x,
  y = ~y,
  z = ~value,
  frame = ~year, # This is the key for animation
  type = "heatmap",
  colorscale = "Viridis" # Or any other desired colorscale
) %>%
  layout(
    title = "Raster Data Plotly Heatmap",
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Latitude")
  )

fig


raster_matrix %>%
  ggplot( aes(x=x,y=y,
              fill = value)) +
  geom_tile() +
  facet_wrap(~year)




# try in leaflet ----------------------------------------------------------

# find global min and max value for color scale
layer_ranges <- minmax(rast) 
global_min <- min(layer_ranges[1,])    # Find the minimum across all layer minimums
global_max <- max(layer_ranges[2,])  
pal <- colorNumeric("viridis", domain = c(global_min, global_max),
                    na.color = "transparent")


dir.create("shiny/www/rasterPngs", showWarnings = FALSE)

for (i in 1:nlyr(rast)) {
  f <- file.path("shiny/www/rasterPngs", paste0("abundance_", names(rast)[i], ".png"))
  
  png(f, width = 600, height = 600)
  
  print(
    levelplot(
      rast[[i]],
      margin = FALSE,
      col.regions = viridis(100),
      at = seq(global_min, global_max, length.out = 101),
      na.color = "#00000000"   # <-- fully transparent NA
    )
  )
  dev.off()
}


ui <- fluidPage(
  titlePanel("Species Abundance Over Time"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = 1985, max = 2019, value = 1985, step = 1, animate = TRUE),
      checkboxGroupInput("points", "Show overlays:",
                         choices = c("Center of gravity", "Min edge", "Max edge"),
                         selected = "Center of gravity")
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)



map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(ext(rast)[c(1,2)]),
          lat = mean(ext(rast)[c(3,4)]),
          zoom = 5)

add_rast <- function(year = 1990){
  year_str <- paste0("Yr", year)
  img_path <- sprintf("shiny/www/rasterPngs/abundance_%s.png", year_str)
  
  map %>%
  clearImages() %>%
    addRasterImage(rast[[which(names(rast) == year_str)]],
                   colors = pal, opacity = 0.8) %>%
    leaflet::addLegend(colors = pal)
  
  library(leafem)
  

    img_path <- sprintf("www/rasters/abundance_Yr%d.png", input$year)
    
    map %>%
      clearGroup("raster") %>%
      addOverlayImages(
        img_path,
        bounds = terra::ext(rast),
        opacity = 0.8,
        group = "raster"
      )
  }
  
  
}

add_rast(2010)


server <- function(input, output, session) {
  years <- 1985:2019
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(terra::ext(rast)[c(1,2)]),
              lat = mean(terra::ext(rast)[c(3,4)]),
              zoom = 6)
  })
  
  
  
  
  observe({
    year_str <- paste0("Yr", input$year)
    img_path <- sprintf("www/rasters/abundance_%s.png", year_str)
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(rast[[which(names(rast) == year_str)]],
                     colors = pal, opacity = 0.8)
    
    # Optionally add points
    df_year <- df[df$year == input$year, ]
    proxy <- leafletProxy("map")
    proxy <- proxy %>% clearMarkers()
    
    if ("Center of gravity" %in% input$points) {
      proxy <- proxy %>% addCircleMarkers(
        data = df_year, lng = ~center_x, lat = ~center_y,
        popup = ~paste("Year:", year, "<br>Value:", round(center_val, 2)),
        color = "orange", radius = 6, fillOpacity = 0.8
      )
    }
    
    if ("Min edge" %in% input$points) {
      proxy <- proxy %>% addCircleMarkers(
        data = df_year, lng = ~min_x, lat = ~min_y,
        popup = ~paste("Min edge:", round(min_val, 2)),
        color = "blue", radius = 4
      )
    }
    
    if ("Max edge" %in% input$points) {
      proxy <- proxy %>% addCircleMarkers(
        data = df_year, lng = ~max_x, lat = ~max_y,
        popup = ~paste("Max edge:", round(max_val, 2)),
        color = "red", radius = 4
      )
    }
  })
}



library(shiny)
library(leaflet)
library(terra)

# assume 'rast' is your SpatRaster with layer names like "Yr1990" ...
extents <- terra::ext(rast)  # use for centering if needed
library(shiny)
ui <- fluidPage(
  sliderInput("year", "Year", min = 1990, max = 2024, value = 1990, step = 1, animate = TRUE),
  leafletOutput("map", height = 600)
)

server <- function(input, output, session) {
  pal <- colorNumeric("viridis", 
                      domain = range(terra::minmax(rast),
                                     na.rm = TRUE),
                      na.color = "transparent")
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = mean(extents[c(1,2)]), lat = mean(extents[c(3,4)]), zoom = 6)
  })
  
  observe({
    # get the layer index (or name) for the selected year
    layer_name <- paste0("Yr", input$year)
    lyr <- rast[[which(names(rast) == layer_name)]]
    
    leafletProxy("map") %>%
      clearImages() %>%
      addRasterImage(lyr, colors = pal, opacity = 0.8, project = TRUE, group = "raster") %>%
      clearControls() %>%
      addLegend(position = "bottomright", pal = pal, values = values(lyr), title = "Abundance")
  })
}

shinyApp(ui, server)



# Dependencies
library(terra)    # for SpatRaster handling
library(plotly)
library(dplyr)
library(stringr)

# ---------------------------
# Replace 'rast' and 'df' with your real objects
# Example placeholders (remove in production)
# rast <- rast("path/to/your/stack.tif")  # your SpatRaster with 35 layers named e.g. "Yr1990"
# df   <- data.frame(year = integer(), center_x = numeric(), center_y = numeric(),
#                    center_val = numeric(), min_x = numeric(), min_y = numeric(),
#                    min_val = numeric(), max_x = numeric(), max_y = numeric(), max_val = numeric())
# ---------------------------

# ---------- Helper: build x/y grid and indices (keeps NAs) ----------
r_df <- as.data.frame(rast, xy = TRUE)         # x,y + each layer column (keeps NAs)
x_vals <- sort(unique(r_df$x))
y_vals <- sort(unique(r_df$y), decreasing = TRUE)  # top-down for Plotly
nx <- length(x_vals); ny <- length(y_vals)

# integer indices for filling matrices
col_idx <- match(r_df$x, x_vals)
row_idx <- match(r_df$y, y_vals)

build_matrix_for_layer <- function(df, layer_name, row_idx, col_idx, nrow = ny, ncol = nx) {
  vals <- df[[layer_name]]
  mat <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  mat[cbind(row_idx, col_idx)] <- vals
  mat
}

# ---------- Compute global color scale ----------
# careful: terra::minmax returns a 2-column matrix, easiest to use global range:
global_min <- min(values(rast), na.rm = TRUE)
global_max <- max(values(rast), na.rm = TRUE)

# ---------- Prepare frame names and ordering ----------
layer_names <- names(rast)                    # e.g. "Yr1990", "Yr1991", ...
years <- as.numeric(str_remove(layer_names, "Yr"))  # extract numeric year
n_frames <- length(layer_names)

# ---------- Build initial matrices and traces (year 1) ----------
first_layer <- layer_names[1]
init_mat <- build_matrix_for_layer(r_df, first_layer, row_idx, col_idx)

# Get point data per year from df
# df must include columns: year, center_x, center_y, center_val, min_x, min_y, min_val, max_x, max_y, max_val
# Example: keep only relevant columns / ensure year type numeric
df <- df %>% mutate(year = as.numeric(year))

df_year1 <- df %>% filter(Year == years[1])

# Initial plotly traces: heatmap + 3 scatter traces (centers, mins, maxs)
p <- plot_ly(
  z = init_mat,
  x = x_vals,
  y = y_vals,
  type = "heatmap",
  colorscale = "Viridis",
  zmin = global_min,
  zmax = global_max,
  showscale = TRUE,
  name = paste0("abundance_", years[1])
)

# add point traces (these are normal scatter traces, will remain in legend)
p <- p %>% add_markers(
  data = df_year1,
  x = ~centroid_longitude, 
  y = ~centroid_latitude,
  marker = list(size = 8, color = "orange"),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  text = ~paste0("Year: ", year, "<br>Center val: ", round(centroid_latitude, 3)),
  name = "Center"
)




# ---------- Build frames (each frame must contain the full set of traces in the same order) ----------
# For each layer, build a frame with: heatmap, center scatter, min scatter, max scatter
frames <- vector("list", length = n_frames)
for (i in seq_len(n_frames)) {
  ly <- layer_names[i]
  yr <- years[i]
  mat <- build_matrix_for_layer(r_df, ly, row_idx, col_idx)
  
  # subset point data for this year
  dfi <- df %>% filter(year == yr)
  # create lists matching plotly trace structure
  heatmap_trace <- list(
    z = mat,
    x = x_vals,
    y = y_vals,
    type = "heatmap",
    colorscale = "Viridis",
    zmin = global_min,
    zmax = global_max,
    name = paste0("abundance_", yr)
  )
  center_trace <- list(
    x = dfi$centroid_longitude,
    y = dfi$centroid_latitude,
    type = "scatter",
    mode = "markers",
    marker = list(size = 8, color = "orange"),
    text = if (nrow(dfi) > 0) paste0("Year: ", dfi$Year, "<br>Center val: ", round(dfi$centroid_longitude,3)) else character(0),
    hoverinfo = "text",
    name = "Center"
  )
  
  frames[[i]] <- list(name = as.character(yr), data = list(heatmap_trace, center_trace))
}

# Attach frames
p$x$frames <- frames

# Build slider steps
slider_steps <- lapply(frames, function(f) {
  list(method = "animate",
       args = list(list(f$name), list(mode = "immediate", frame = list(duration = 400, redraw = TRUE), transition = list(duration = 0))),
       label = f$name)
})

# Layout: slider + play/pause buttons. Legend is enabled so user can toggle Center/Min/Max.
p <- p %>% layout(
  title = "Abundance time series with point overlays",
  xaxis = list(title = "Longitude"),
  yaxis = list(title = "Latitude"),
  showlegend = TRUE,
  sliders = list(list(active = 0, steps = slider_steps, currentvalue = list(prefix = "Year: "))),
  updatemenus = list(
    list(type = "buttons",
         showactive = FALSE,
         y = 0.05,
         x = 0.1,
         xanchor = "right",
         yanchor = "top",
         buttons = list(
           list(label = "Play",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 500, redraw = TRUE), fromcurrent = TRUE, transition = list(duration = 0)))),
           list(label = "Pause",
                method = "animate",
                args = list(NULL, list(frame = list(duration = 0, redraw = FALSE), mode = "immediate", transition = list(duration = 0))))
         ))
  )
)

# Final: show plot
p



library(plotly)
library(terra)

# Convert first raster to data.frame
rast_df <- as.data.frame(rast[[1]], xy = TRUE, na.rm = TRUE)
names(rast_df)[3] <- "abundance"

years <- names(rast)

# Initialize the plot
p <- plot_ly(
  data = rast_df,
  lon = ~x,
  lat = ~y,
  z = ~abundance,
  type = "densitymapbox",
  coloraxis = "coloraxis",
  text = ~round(abundance^2),
  hoverinfo = "text",
  name = years[1],
  markers = list(size = 10)
) %>%
  layout(
    mapbox = list(style = "carto-positron"),
    coloraxis = list(colorscale = "Viridis",
                     cmin = global_min,
                     cmax = global_max)
  )

p
# Add your points
p <- p %>% add_trace(
  data = df[df$Year == years[1],],
  inherit = F,
  lon = ~centroid_longitude,
  lat = ~centroid_latitude,
  text = ~paste("Year:", Year, "<br>Center:", round(centroid_latitude, 3)),
  type = "scattermapbox",
  mode = "markers",
  marker = list(size = 8, color = "orange"),
  name = "Center"
)







library(plotly)

fig <- plot_ly() 
fig <- fig %>% layout(
  mapbox = list(
    style = "carto-positron", 
    zoom = 2, 
    center = list(lon = -95.71, lat = 37.09),
    layers = list(
      list(
        sourcetype = "image",
        source = "shiny/www/rasterPngs/abundance_Yr1990.png", # Replace with your image URL
        type = "raster",
        coordinates = list(
          c(-100, 40), # Example coordinates for the image corners
          c(-90, 40),
          c(-90, 30),
          c(-100, 30)
        )
      )
    )
  )
)

fig


rast2 <- rast %>% as.data.frame(xy = T)

dim(matrix(rast[[1]]))

xyFromCell(rast[[1]],1:ncell(rast))



# Get XY coordinates for all cells
xy_coords <- xyFromCell(rast[[1]], 1:ncell(rast))

# Get values for all cells
raster_values <- values(rast[[1]])

# Combine into an XYZ matrix
xyz_matrix <- cbind(xy_coords, Z = raster_values)

# Display the first few rows of the resulting matrix
head(xyz_matrix)
plot(xyz_matrix)


coerce(rast[[1]], to = "matrix")
as.matrix(rast[[1]], wide = T)

volcano
image(volcano)


year <- 2000
plot_ly() %>%
  add_markers(type = "heatmap",
              data = as.data.frame(rast[[which(str_detect(names(rast),as.character(year)))]], xy=T),
              x = ~x,
              y = ~y,
              color = ~get(paste0("Yr",year))) %>%
  add_markers(data = df %>% filter(Year == year),
              x = ~centroid_longitude,
              y = ~centroid_latitude,
              color = ~Region)

library(leaflet)

pal <- colorNumeric("viridis", values(rast),
                    na.color = "transparent")

i <- 20
leaflet() %>%
  addTiles() %>%
  addRasterImage(rast[[i]],
                 colors = pal) %>%
  addLegend(pal = pal, values = values(rast, na.rm=T),
            title = "Surface temp",
            opacity = .9) %>%
  addCircleMarkers(data = df[df$Year == c(1990:2023)[i],],
             lng = ~centroid_longitude,
             lat = ~centroid_latitude,
             color = "orange", 
             opacity = 1)


addLegend(colors = pal(seq(global_min,global_max,
                             length.out = 10)), values = global_min:global_max,
            labels = global_min:global_max)

plot(rast[[1]])

pal(global_min:global_max)

plet(rast[[1]])



r <- rast("nc/oisst-sst.nc")

us_cities = read.csv("https://raw.githubusercontent.com/plotly/datasets/master/us-cities-top-1k.csv")

plot_ly(type = "scattermapbox") %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -60, lat = 45)))  %>%
  add_trace(
    data = df, 
    type = "scattermapbox",
    mode = "lines",
    lon = ~centroid_longitude,
    y = ~centroid_latitude,
    color = ~Region,
    line = list(width = 1, opacity = 1)
  ) %>%
  add_trace(
    data = df,
    type = "scattermapbox",
    mode = "markers",
    lon = ~centroid_longitude,
    lat = ~centroid_latitude,
    color = ~Region,
    frame = ~Year,
    marker = list(size = 15)
  ) %>%
  updatemenus = list(
    list(
      type = "buttons",
      y = 0.8,
      buttons = list(
        
        list(method = "animate",
             args = list("line.color", "red"),
             label = "Red")))
  ))
add_scattergeo(
  data = df,
  lat = ~centroid_latitude,
  lon = ~centroid_longitude,
  marker = list(color = "fuchsia"),
  type = 'scattermapbox',
)
  plotly::add_markers(
    data = df,
    x = ~centroid_longitude,
    y = ~centroid_latitude,
    marker = list(color = "fuchsia")
  #  frame = ~Year
  ) 

  
  plot_ly(data = df, 
          x = ~centroid_longitude,
          y = ~centroid_latitude,
          type = "scattermapbox",
          marker = list(color = "fuchsia",
                        symbol = "square"
                       )) 
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2.5,
        center = list(lon = -88, lat = 34)))
    
  

  ## WORKING V1, showing only centroid points (animated) and lines (static)
  plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2.5,
        center = list(lon = -60, lat = 45))) %>%
    # add static centroid lines
    add_trace(
      data = df_cog, 
      type = "scattermapbox",
      mode = "lines",
      lon = ~centroid_longitude,
      lat = ~centroid_latitude,
      color = ~Region,
      line = list(width = 1, opacity = 1)
    ) %>%
    # add animated centroid points
    add_trace(
      data = df_cog,
      type = "scattermapbox",
      mode = "markers",
      lon = ~centroid_longitude,
      lat = ~centroid_latitude,
      color = ~Region,
      frame = ~Year,
      marker = list(size = 15)
    ) 
  
  
  ## WORKING V2, plot "fake" raster as points:
  # make dataframe from raster
  test <- rast %>% as.data.frame(xy = T) %>%
    tidyr::pivot_longer(cols = contains("Yr"),
                        names_to = "Year",
                        values_to = "Value") %>%
    mutate(Year = as.numeric(str_remove(Year,"Yr")))
  
  # plot
  plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2.5,
        center = list(lon = -60, lat = 45))) %>%
    # add fake raster layer
    add_trace(data = test, 
              type = "scattermapbox",
              # mode = "markers",
              lon = ~x,
              lat = ~y,
              color = ~Value,
              frame = ~Year,
              hoverinfo = "none",
              symbols = "square",
              symbol = "square",
              marker = list(
                #symbol = "squares",
                size = 10,
                opacity = 0.8,
                sizemin = 0.1
              )) %>%
    # add static centroid lines
    add_trace(
      data = df_cog, 
      type = "scattermapbox",
      mode = "lines",
      lon = ~centroid_longitude,
      y = ~centroid_latitude,
      color = ~Region,
      line = list(width = 1, opacity = 1)
    ) %>%
    # add animated centroid points
    add_trace(
      data = df_cog,
      type = "scattermapbox",
      mode = "markers",
      lon = ~centroid_longitude,
      lat = ~centroid_latitude,
      color = ~Region,
      frame = ~Year,
      marker = list(size = 15)
    ) 
  
  
  # NOT WORKING V3:
  # raster image does not align with pink points, and it is also showing ABOVE the points instead of below.
  library(tidyterra)
  library(viridis)
  

  layer_ranges <- minmax(rast) 
  global_min <- min(layer_ranges[1,])    # Find the minimum across all layer minimums
  global_max <- max(layer_ranges[2,])  
  
  for(i in 1:length(years)){
    # create static ggplot
    plot <- ggplot() +
      geom_spatraster(data = rast[[i]] %>% terra::project("EPSG:3857")) +
      scale_fill_gradientn(colors = viridis(100, begin = 0, end = 1,
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
           width = ncol(mat)*3.475*2, 
           height = nrow(mat)*5*2, 
           unit = "px",
           filename = paste0("shiny/www/rasterPngs/yr",years[i],".png"))
    print(i)
    
  }
  
  # make empty list of years
  years <- sort(unique(df$Year))
  png_base64_list <- vector(mode = "list",
                            length = length(years)) %>%
    purrr::set_names(years)
  
  for(i in years){
    png_base64_list[[as.character(i)]] <-   base64enc::dataURI(file = paste0("shiny/www/rasterPngs/yr",i,".png"), mime = "image/png")
    print(i)
  }
  
  ext <- terra::ext(rast)
  xmin <- ext[1]; xmax <- ext[2]
  ymin <- ext[3]; ymax <- ext[4]
  
  coords <- list(
    c(xmin, ymax),
    c(xmax, ymax),
    c(xmax, ymin),
    c(xmin, ymin)
  )
  
  # plot
p <-   plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2.5,
        center = list(lon = -60, lat = 45),
        layers = list(
          list(
            sourcetype = "image",
            below = "traces",
            source = png_base64_list[[1]],
            coordinates = coords,
            opacity = 1,
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
        
      )) %>%
    # add static centroid lines - black
   add_trace(
     data = df, 
     type = "scattermapbox",
     mode = "lines",
     lon = ~centroid_longitude,
     y = ~centroid_latitude,
     color = ~Region,
     line = list(width = 1, opacity = 1,
                 color = "black"),
     hoverinfo = "none"
   ) %>%
  # add moving centroid lines - colored
  add_trace(
    data  = df_line,
    type  = "scattermapbox",
    mode  = "lines",             # or "lines+markers"
    lon   = ~centroid_longitude,
    lat   = ~centroid_latitude,
    frame = ~frame_year,         # the cumulative frame column
    color = ~Region,
    hoverinfo = "none"
    # group = ~Region#,             # keep lines connected within region
    #line  = list(width = 2)
  ) %>%
    # add animated centroid points - outlined
    add_trace(
      data = df,
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
  config(displayModeBar = F)
  
p
p2 <- plotly_build(p)
p2

for (i in seq_along(p2$x$frames)) {
  yr <- p2$x$frames[[i]]$name  # Year value corresponding to this frame
  
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



p2$x$layout$updatemenus <- list(
  list(
    type = "buttons",
    showactive = FALSE,
    direction = "left",
    x = 0.1,
    y = 1.15,
    pad = list(t = 0, r = 10),
    buttons = list(
      list(
        label = "Play",
        method = "animate",
        args = list(
          NULL,
          list(
            fromcurrent = TRUE,
            frame = list(duration = 300, redraw = TRUE),
            transition = list(duration = 0)
          )
        )
      ),
      list(
        label = "Pause",
        method = "animate",
        args = list(
          # CRITICAL: leave frame name list EMPTY
          list(NULL),  
          list(
            mode      = "immediate",
            frame     = list(duration = 0, redraw = FALSE),
            transition = list(duration = 0)
          )
        )
      )
    )
  )
)

p2 %>%
  hide_legend()

ggplot() +
  geom_spatraster(data = rast[["Yr2006"]]) +
  scale_fill_gradientn(colors = viridis(100),
                       limits = c(global_min, global_max),
                       na.value = "transparent")

  
  plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom =2.5,
        center = list(lon = -60, lat = 45),
        layers = list(
          list(
            sourcetype = "image",
            below = "traces",
            source = png64,
            coordinates = coords,
            opacity = 1,
            visible = TRUE  # you can toggle this per frame
          )
        )
        
        )) %>%
    add_trace(
      type = "scattermapbox",
      mode = "markers",
      lat = ~unlist(coords)[c(2,4,6,8)],
      lon = ~unlist(coords)[c(1,3,5,7)],
      marker = list(size = 15, 
                    color = "red")
    ) %>%
    add_trace(data = test[test$Year == test$Year[1],], 
              type = "scattermapbox",
              # mode = "markers",
              lon = ~x,
              lat = ~y,
              color = ~Value,
              frame = ~Year,
              hoverinfo = "none",
              marker = list(
                #symbol = "squares",
                color = "magenta",
                size = 10,
                opacity = 0.8,
                sizemin = 0.1
              )) 

  
  
  ggplot() +
    geom_spatraster(data = rast[[1]]) +
    scale_fill_viridis() +
    geom_point(data = test[test$Year == test$Year[1],],
               aes(x = x, y = y),
               size = .1, 
               color = "magenta") +
    coord_sf(expand = F)
    
  
  plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -60, lat = 45)
      )
    ) %>%
    add_trace(
      data = df,
      x = ~centroid_longitude,
      y = ~centroid_latitude,
      frame = ~Year,
      color = ~Region
    )
  
  plot_ly() %>%
    add_trace(
      data = df,
      x = ~centroid_longitude,
      y = ~centroid_latitude,
      frame = ~Year,
      color = ~Region
    )
  
  
  df_line <- df %>%
    arrange(Region, Year) %>%
    group_by(Region) %>%
    # For each Year, keep all rows up to that year
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
    ungroup()
  
  plot_ly(type = "scattermapbox") %>%
    layout(
      mapbox = list(
        style = 'open-street-map',
        zoom  = 2.5,
        center = list(lon = -60, lat = 45)
      )
    ) %>%
    add_trace(
      data  = df_line,
      type  = "scattermapbox",
      mode  = "lines",             # or "lines+markers"
      lon   = ~centroid_longitude,
      lat   = ~centroid_latitude,
      frame = ~frame_year,         # the cumulative frame column
      color = ~Region#,
     # group = ~Region#,             # keep lines connected within region
      #line  = list(width = 2)
    )
  
  
  

# practice adding show/hide nafo zones ------------------------------------

  
  p <- plot_ly(type = "scattermapbox") %>%
    layout(mapbox = list(
      style = 'carto-positron',
      zoom =2.5,
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
      ))) 
  
  
  # Add the zone boundaries
  for (i in seq_along(zone_coords2)) {
    df_i <- zone_coords2[[i]]
    
    p <- p %>% add_trace(
      type = "scattermapbox",
      mode = "lines",
      lon = df_i$lon,
      lat = df_i$lat,
      line = list(color = "orange", width = 2),
      hoverinfo = "text",
      showlegend = F,
      text = paste0(
        "<b>", df_i$zone[1], "</b><br>"
      ),
      name = df_i$name[1],
      # This adds a hover halo that looks like bolding
      selected = list(line = list(width = 6)),
      unselected = list(line = list(width = 2)),
      visible = T  # hidden by default for toggle button
    )
  }
  
  p
  
  n_poly <- length(zone_coords2)
  n_base <- 1  # number of your base map traces
  
  all_off <- c(rep(TRUE, n_base), rep(FALSE, n_poly))
  all_on  <- c(rep(TRUE, n_base), rep(TRUE,  n_poly))
  
  p <- p %>% layout(
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.5, y = 1,
        buttons = list(
          list(
            label = "Show Zones",
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
  )
  
  p <- p %>% add_trace(
    type = "scattermapbox",
    mode = "lines",
    lon = df_i$lon,
    lat = df_i$lat,
    line = list(
      color = "orange",
      width = 2
    ),
    hoverlabel = list(bgcolor = "white"),
    hoverinfo = "text",
    
    # This adds a hover halo that looks like bolding
    selected = list(line = list(width = 6)),
    unselected = list(line = list(width = 2))
  )
  
  
  p <- onRender(p, "
function(el, x) {
  var gd = document.getElementById(el.id);

  // base width
  var baseWidth = 2;
  var boldWidth = 5;

  gd.on('plotly_hover', function(e) {
    var n = x.data.length;
    var widths = new Array(n).fill(baseWidth);
    var tr = e.points[0].curveNumber;
    widths[tr] = boldWidth;
    Plotly.restyle(gd, { 'line.width': widths });
  });

  gd.on('plotly_unhover', function(e) {
    var n = x.data.length;
    var widths = new Array(n).fill(baseWidth);
    Plotly.restyle(gd, { 'line.width': widths });
  });
}
")
  
  p
  
  