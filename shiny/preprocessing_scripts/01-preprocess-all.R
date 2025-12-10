
# master script to remake all plots

# make plots of individual variables ---------------------------------------
# make all tabular data plots
source("shiny/preprocessing_scripts/02.1_make_abundance_plot.R")
source("shiny/preprocessing_scripts/02.2_make_area_plot.R")
source("shiny/preprocessing_scripts/02.3_make_depth_plot.R")
source("shiny/preprocessing_scripts/02.4_make_distance_to_border_plot.R")

# make map plots
source("shiny/preprocessing_scripts/02.5.1_prep_animated_map_data.R")
source("shiny/preprocessing_scripts/02.5.2_make_animated_map_by_year.R")
source("shiny/preprocessing_scripts/02.5.2_make_animated_map_clean.R")

# pie charts
source("shiny/preprocessing_scripts/03.1_make_circle_plot.R")
source("shiny/preprocessing_scripts/03.2_make_tables.R")
source("shiny/preprocessing_scripts/03.3_make_slope_plot.R")
source("shiny/preprocessing_scripts/03.4_make_study_area_map.R")



test <- readRDS("shiny/www/premade_plots/abundance_plot.rds")
testmap <- readRDS("shiny/www/premade_plots/mapplot.rds")
