#create and save an estimate density rasters for each year

# Load package
library(VAST)
library(stringr)
library(patchwork)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(gstat)
library(rasterize)
library(here)
library(splines)
library(tidyverse)
library(lubridate)
library(sf)
library(googledrive)
library(ggforce)
library(patchwork)
library(DHARMa)
library(forecast)
library(sp)
library(RColorBrewer)
library(tibble)
library(terra)

devtools::source_url("https://raw.github.com/aallyn/TargetsSDM/main/R/vast_functions.R")
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))
# Andrew's Functions, I cloned his directory here: C:/Users/FergusonK/Documents/Halibut/TargetsSDM_Clone/TargetsSDM/R/
source(here::here("R/VAST_functions/dfo_functions.R"))
source(here::here("R/VAST_functions/nmfs_functions.R"))
source(here::here("R/VAST_functions/combo_functions.R"))
source(here::here("R/VAST_functions/enhance_r_funcs.R"))
#source(here::here("R/VAST_functions/vast_functions.R"))
source(here::here("R/VAST_functions/covariate_functions.R"))
source(here::here("R/VAST_functions/project.fit_model_aja.R"))
source(here::here("R/VAST_functions/DHARMa utilities.R"))
source(here::here("R/VAST_functions/SDM_PredValidation_Functions.R"))#PresenceAbsence library is gone so this doesn't work anymore- 
source(here::here("R/VAST_functions/vast_function_edits.R"))
source(here::here("R/VAST_functions/vast_plotting_functions.R"))
source(here::here("R/VAST_functions/vast_functions.R"))#the file is different when you pull it straight from Andrew's directory...need to use this one in order for the plots to work
source(here::here("R/VAST_functions/kf_vast_function_edits.R"))

# Extracting and plotting predicted density at grid locs, smooth over a regular grid, bin the years based on timeframe
  fit<- readRDS( here::here("2025-04-23/Halibut_BC/SpST_mod_fit.rds")) 
  vast_fit<-fit#just in case
  # This is a list of valid variables for spatial plotting 
  valid_vars <- c("D_gct", "R1_gct", "R2_gct", "P1_gct", "P2_gct", 
                  "Omega1_gc", "Omega2_gc", "Epsilon1_gct", "Epsilon2_gct", 
                  "Index_gctl")
#load shapefile for masking interpolation
  region_shape <- vect(here::here("R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))
  region_shape <- project(region_shape, crs("EPSG:4326"))

#run whichever season you want to plot 
#select for spring 
  spring_times<- as.character(seq(0, 101, by = 3)) #because there are 3 seasons for each year in the model 
  spring_range <- range(as.numeric(spring_times))
  season_times<-spring_times

####KEEPING SUMMER AND FALL AS AN OPTION----
    #SELECT FOR SUMMER
    #summer_times<- as.character(seq(1, 101, by = 3)) #because there are 3 seasons for each year in the model 
    #summer_range <- range(as.numeric(summer_times))
    #season_times<-summer_times

    #SELECT FOR FALL
    #fall_times<- as.character(seq(1, 101, by = 3)) #because there are 3 seasons for each year in the model 
    #fall_range <- range(as.numeric(fall_times))
    #season_times<-fall_times
####END OF SUMMER/FALL OPTIONS----

#settings for pred_df_interp(), 
#get values blank raster for interpolation to reflect the max/min lat/lon, across an even grid 
  loc_data <- if (fit$spatial_list$fine_scale == TRUE) fit$extrapolation_list else fit$spatial_list
  loc_df <- if (fit$spatial_list$fine_scale == TRUE) loc_data$Data_Extrap[which(loc_data$Data_Extrap[, "Include"] > 0),
                                                                      c("Lon", "Lat")] else loc_data$latlon_s[1:loc_data$n_x, ]
      x1 <- min(loc_df$Lon)
      x2 <- max(loc_df$Lon)
      y1 <- min(loc_df$Lat)
      y2 <- max(loc_df$Lat)
      buffer <- 0.1 # degrees
      x1 <- x1 - buffer
      x2 <- x2 + buffer
      y1 <- y1 - buffer
      y2 <- y2 + buffer
      aspect_ratio <- (x2 - x1) / (y2 - y1)
      n2 <- 150  # number of y-grid cells (latitude)
      n1 <- round(n2 * aspect_ratio)
      xlim_use <- c(-74, -46)
      ylim_use <- c(36, 53)


#Call to the one i want  
  #Instead of the equally spaced prediction grid, 
  #VAST aggregates over a smaller number of unique area-weighted locations 
  #(usually integration points or centroids of spatial knots or prediction grid cells).
  #Index_gctl already includes area-weighted contribution it is ok that it has strayed from the regular grid
  #it is a spatially representative subset that makes the model run better
  #Index_gctl multiplies D_gct by the site and area weight from the model 
    #Index is the Estimated total abundance values at gctl:
    # g (Sites: extrapolation-grid centroid locations)
    # c (category, only Halibut in this case)
    # t (time step, we have year and month (spring, summer, fall) and are selecting for spring...see spring_times)
    # l (location, or stratum, we will be selecting for the full area)
      spatial_var <- "Index_gctl" 
      index_data <- vast_fit$Report[[spatial_var]]
      # Filter for full survey area where Stratum == "Stratum_1", and Time = Spring
      index_data_filtered <- index_data[, ,dimnames(index_data)[[3]] %in% season_times, "Stratum_1", drop = FALSE]
      #Remove 'Stratum' column, not needed anymore...leaving category because we may go mulit-species one day 
      pred_array <- array(index_data_filtered, dim = dim(index_data_filtered)[1:3], 
                    dimnames = dimnames(index_data_filtered)[1:3])
      str(pred_array)
#OPTION: Redistribute for plotting: tested a few distributions and sqrt() was the best for visualizing change
      #pred_array <-sqrt(pred_array)
      #keeping some alternatives on standby
      #pred_array <- log(pmax(pred_array, 1e-10))
      #pred_array <- log(pred_array+ 0.1) #plot pred_array instead of log
#now i have a spring time prediction array for each year and g-Site   

# Collect spatial location data (lon & Lat)
  spat_data <- if (vast_fit$spatial_list$fine_scale == TRUE) vast_fit$extrapolation_list else vast_fit$spatial_list
  locs <- if (vast_fit$spatial_list$fine_scale == TRUE) spat_data$Data_Extrap[which(spat_data$Data_Extrap[, "Include"] > 0), c("Lon", "Lat")] else spat_data$latlon_s[1:spat_data$n_x, ]
  row.names(locs) <- NULL

#turn pred_array into a list of yearly slices
years <- 1990:2023 #assign the actual years instead of time steps at this point
pred_list <- lapply(seq_along(years), function(i) {
  list(
    Year = years[i],
    Estimate = pred_array[, , i, drop = FALSE]
  )
})
str(pred_list)

out_dir <- here::here("Data/Data_SHinyApp_Proof_of_Concept/EstAbundanceRasters")#for output

#loop the list and interpolate  a raster for each slice
for (i in seq_along(pred_list)) {
  Year <- pred_list[[i]]$Year
  #take the estimates
  Y_data <- pred_list[[i]]$Estimate  
  #and join them with the coordinates  
  data_df <- data.frame(locs, z = as.vector(Y_data)) %>%
      distinct(Lon, Lat, z)
    pred_df <- na.omit(data.frame("x" = data_df$Lon, "y" = data_df$Lat, "layer" = data_df$z))#clear NAs
    #interpolate with akima(x, y, z)
    pred_df_interp <- interp(pred_df[, 1], pred_df[, 2], pred_df[, 3],
                             duplicate = "mean", extrap = TRUE,
                             xo = seq(-73.4, -46.4, length = 293),
                             yo = seq(38.5, 52.3, length = 150))    
    #turn the interpolation into a raster, project it and mask it to the study area 
    rast <- rast(pred_df_interp)
    crs(rast) <- "EPSG:4326"
    r_masked <- mask(crop(rast, region_shape), region_shape)
    plot(r_masked, main = paste("Year:", pred_list[[i]]$Year))
    # Save raster file (GeoTIFF)
    writeRaster(r_masked, filename = paste0(out_dir, "/", "Est_Abun_raw", Year, ".tif"), 
                overwrite = TRUE)
       }
#Test
r_squrt <- rast(here::here("Data/Data_SHinyApp_Proof_of_Concept/EstAbundanceRasters/Est_Abun_Sqrt2022.tif"))
r_raw <- rast(here::here("Data/Data_SHinyApp_Proof_of_Concept/EstAbundanceRasters/Est_Abun_Raw2022.tif"))

plot(r_squrt, main = "Test sqrt")
plot(r_raw, main = "Test raw")

