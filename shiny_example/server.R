function(input, output) {
  
  # Number of birds sighted per day
  output$plot_unique_birds_by_day <- renderPlotly({
    
    data %>% 
      group_by(sighting_date) %>% 
      summarise(birds_sighted = sum(sighting_count)) %>% 
      plot_ly(
        x = ~sighting_date,
        y = ~birds_sighted,
        type = "scatter",
        mode = "lines",
        line = list(color = plot_colour)
      ) %>% 
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Birds Sighted")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Number of unique bird types by site
  output$plot_unique_birds_site <- renderPlotly({
    
    data %>% 
      group_by(site_name) %>% 
      summarise(unique_birds = n_distinct(common_name)) %>% 
      arrange(unique_birds) %>% 
      mutate(site_name = if_else(site_name == "Dynon Road Tidal Canal Wildlife Sanctuary", "Dynon Road Tidal Canal<br>Wildlife Sanctuary", site_name)) %>% 
      mutate(site_name = factor(site_name, levels = .$site_name)) %>% 
      plot_ly(
        x = ~unique_birds,
        y = ~site_name,
        type = "bar",
        marker = list(color = plot_colour),
        orientation = "h"
      ) %>% 
      layout(
        xaxis = list(title = "Unique Birds"),
        yaxis = list(title = "") 
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Total Sightings per Day per Bird
  output$plot_bird_totals_per_day <- renderPlotly({
    
    data %>% 
      group_by(common_name) %>%
      summarise(sighting_count = sum(sighting_count)) %>% 
      arrange(desc(sighting_count)) %>% 
      mutate(common_name = factor(common_name, levels = rev(.$common_name))) %>% 
      slice(1:input$show_top_n) %>% 
      plot_ly(
        x = ~sighting_count,
        y = ~common_name,
        type = "bar",
        marker = list(color = plot_colour),
        orientation = "h"
      ) %>% 
      layout(
        xaxis = list(title = "Count"),
        yaxis = list(title = "")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Sightings by location
  output$plot_sightings_by_location <- renderLeaflet({
    
    sites <- data %>% 
      group_by(site_name, lat, lon) %>% 
      summarise(sighting_count = sum(sighting_count))
    
    leaflet(data = sites) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        ~lon,
        ~lat,
        radius = ~sighting_count/100,
        color = plot_colour,
        fillOpacity = 1,
        popup = ~paste0(site_name, "<br>Birds: ", sighting_count)
      )
  })
  
  output$table_sites <- renderDT({
    
    data %>% 
      select(site_name, location_desc, lat, lon) %>% 
      unique() %>% 
      rename(
        `Site Name` = site_name,
        `Location Description` = location_desc,
        Latitude = lat,
        Longitude = lon
      )
    
  })
  
  output$table_birds <- renderDT({
    
    data %>% 
      group_by(common_name, scientific_name) %>% 
      summarise(`Total Sightings` = sum(sighting_count)) %>% 
      rename(
        `Common Name` = common_name,
        `Scientific Name` = scientific_name
      )
    
  })
  
}