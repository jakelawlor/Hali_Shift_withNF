# Global options for Transboundary Jusisdictional Framework Shiny



# libraries ---------------------------------------------------------------
# load the following packages
library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(DT)
library(fresh)
library(shinyWidgets)
library(scales)
library(reactable)


# source UI tabs ----------------------------------------------------------
# The content for each tab is stored in a separate file. 
# Source all .R files in the current directory that start with "ui_":
sapply(list.files(
  pattern = "^ui_.*\\.R$",
  path = ".",
  full.names = TRUE
),
source)





plot_colour <- "#8965CD"

theme <- create_theme(
  bs4dash_color(
    lime = "#52A1A5",
    olive = "#4A9094",
    purple = "#8965CD"
  ),
  bs4dash_status(
    primary = "#E1EDED",
    secondary = "#a7effa"
  )
)




# upload premade plots -----------------------------------------------------------

halibut_plots <- list()
halibut_plots$p_pie_can <- readRDS("www/premade_plots/pie_can.rds")
halibut_plots$p_pie_usa <- readRDS("www/premade_plots/pie_usa.rds")
halibut_plots$p_depth <- readRDS("www/premade_plots/depth_plot.rds")
halibut_plots$p_abundance <- readRDS("www/premade_plots/abundance_plot.rds")
halibut_plots$p_area <- readRDS("www/premade_plots/area_plot.rds")
halibut_plots$p_map <- readRDS("www/premade_plots/mapplot.rds")
halibut_plots$p_dist <- readRDS("www/premade_plots/distance_to_border_plot.rds")
halibut_plots$p_slopes <- readRDS("www/premade_plots/slope_plot.rds")
halibut_plots$can_table <- readRDS("www/premade_plots/can_table.rds")
halibut_plots$usa_table <- readRDS("www/premade_plots/usa_table.rds")
halibut_plots$p_depth 


halibut_plots$p_pie_can$x$layout$width  <- NULL
halibut_plots$p_pie_can$x$layout$height <- NULL
halibut_plots$p_pie_usa$x$layout$width  <- NULL
halibut_plots$p_pie_usa$x$layout$height <- NULL

offsets <- c(Canada = -0.1, USA = +0.1)

nafo_zone_names <- readRDS("www/nafo_zone_names.rds")




# fix carousel function ---------------------------------------------------

# Drop-in replacement for bs4Dash::carousel with correct right control
fixedCarousel <- function(..., id, indicators = TRUE, width = 12, .list = NULL) {
  # Collect items (usually bs4Dash::carouselItem(...) calls)
  items <- c(list(...), .list)
  n <- length(items)
  
  if (n == 0) stop("fixedCarousel() needs at least one carouselItem")
  
  # Make sure the first item is "active" if the user didn't set it
  # This assumes items are bs4Dash carouselItem divs with class "carousel-item"
  items[[1]]$attribs$class <- paste(
    unique(c(strsplit(items[[1]]$attribs$class %||% "", " ")[[1]], "active")),
    collapse = " "
  )
  
  # Indicators (little dots)
  ind_tags <- if (indicators) {
    tags$ol(
      class = "carousel-indicators",
      lapply(seq_len(n) - 1L, function(i) {
        tags$li(
          `data-target` = paste0("#", id),
          `data-slide-to` = i,
          class = if (i == 0L) "active" else NULL
        )
      })
    )
  } else {
    NULL
  }
  
  # Inner items
  inner <- tags$div(
    class = "carousel-inner",
    items
  )
  
  # Controls – **both** prev and next have proper href + data-target
  controls <- tags$div(
    # prev
    tags$a(
      class = "carousel-control-prev",
      href = paste0("#", id),
      `data-target` = paste0("#", id),
      role = "button",
      `data-slide` = "prev",
      tags$span(
        class = "carousel-control-prev-icon",
        `aria-hidden` = "true"
      ),
      tags$span(class = "sr-only", "Previous")
    ),
    # next
    tags$a(
      class = "carousel-control-next",
      href = paste0("#", id),
      `data-target` = paste0("#", id),
      role = "button",
      `data-slide` = "next",
      tags$span(
        class = "carousel-control-next-icon",
        `aria-hidden` = "true"
      ),
      tags$span(class = "sr-only", "Next")
    )
  )
  
  # Full carousel wrapper, inside a column like bs4Dash does
  column(
    width,
    tags$div(
      id = id,
      class = "carousel slide",
      `data-ride` = "carousel",
      ind_tags,
      inner,
      controls
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x


