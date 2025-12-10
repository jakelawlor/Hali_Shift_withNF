# make study area plot
# here we'll create a static plot of the study areas 


# libraries ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggrepel)
library(sf)



# global plot opts --------------------------------------------------------
source("shiny/preprocessing_scripts/00_plot_opts.R")

# upload data -------------------------------------------------------------
# get all abundance raster files
rast_files <- list.files("Data/Data_ShinyApp_Proof_of_Concept/EstAbundanceRasters/",
                         full.names=T)
# keep only sqrt abundance files
rast_files <-rast_files[stringr::str_detect(rast_files, 'Sqrt')]

# upload first
rast <- rast(rast_files[1])
rm(rast_files)


# make raster into polygon ------------------------------------------------
# change all values to 1
rast <- terra::clamp(rast, lower = 1, upper = 1, values = T )
poly <- as.polygons(rast)
plot(poly)
nrow(st_coordinates(st_as_sf(poly)))
rm(rast)


# get countries for context -----------------------------------------------
countries <- rnaturalearth::ne_countries(country = c("United States of America","Canada"), 
                                         returnclass = "sf") %>%
  rmapshaper::ms_simplify(keep = .5) %>%
  vect()

countries <- countries %>% 
  select(admin)


# get border --------------------------------------------------------------
# note that this is a fake border between these two hypothetical study regions
border <- sf::st_read("Data/Data_ShinyApp_Proof_of_Concept/NewData_NoCountries/TheorheticalBorder_Shapefile/AdministrativeBorder.shp") %>%
  vect()



# split study area --------------------------------------------------------
poly_split <- terra::split(poly, border) %>%
  mutate(region = c("Region2","Region1"))
poly_centroids <- centroids(poly_split, inside = T) 

# add buffer to crop countries
poly_buffer <- poly_split %>% terra::buffer(700000)
# view
ggplot() + 
  geom_spatvector(data = countries) +
  geom_spatvector(data = poly_buffer) +
  geom_spatvector(data = poly_split)

# crop countries to buffered border
countries2 <- terra::crop(countries, ext(poly_buffer))
plot(countries2)



# make map ----------------------------------------------------------------
map <- ggplot() +
  geom_spatvector(data = countries2) +
  geom_spatvector(data = poly_split,
                  aes(fill = region),
                  alpha = .8) +
  scale_fill_manual(values = c("Region1" = Region1_col,
                               "Region2" = Region2_col)) +
  geom_spatvector(data = border) +
  coord_sf(xlim = c(-80,-40),
           ylim = c(36,55),
           expand = F
           ) +
  geom_spatvector_text(data = poly_centroids, 
                       aes(label = region), 
                       size = 4, color = "black",
                       fontface = "bold") + # Add labels
  
  ggthemes::theme_map() +
  theme(legend.position = "none") +
  theme(panel.border = element_rect(color = "black",
                                        fill = "white")) +
  labs(title = "Study Area") +
  theme(plot.title = element_text(size = 18))

map <- map +
  theme(plot.margin = margin(0,0,0,0))


# save --------------------------------------------------------------------
# view with dimensions
map + ggview::canvas(5.5,4)
# save as .png
ggplot2::ggsave(
  filename = "shiny/www/premade_plots/study_area.png",
  plot     = map,
  width    = 5.5*.8,    # adjust to taste
  height   = 4*.8,
  dpi      = 150   # lower dpi = smaller file
)

  
