library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)

plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    info = "#E4E4E4"
  )
)


# Load and wrangle data ---------------------------------------------------

data_path <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C"

data <- read_csv(data_path) %>% 
  # select and rename relevant columns
  select(
    sighting_date = `Sighting Date`,
    common_name = `Common Name`,
    scientific_name = `Scientific Name`,
    sighting_count = `Sighting Count`,
    lat,
    lon,
    location_desc = loc1_desc,
    site_name
  )

# Info box values ---------------------------------------------------------

num_sightings <- sum(data$sighting_count)
unique_birds <- length(unique(data$common_name))
unique_locations <- length(unique(data$site_name))
avg_daily_sightings <- data %>% 
  group_by(sighting_date) %>% 
  summarise(sighting_count = sum(sighting_count)) %>% 
  pull(sighting_count) %>% 
  mean()

