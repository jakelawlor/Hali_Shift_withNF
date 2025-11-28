
function(input, output, session) {
  
  # render plotly pie charts
  output$p_pie_can_out <- renderPlotly({
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    
    # get plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_pie_can  
  
    })
  
  output$p_pie_usa_out <- renderPlotly({
    # require the selected species plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_pie_usa  
  })
  
  observeEvent(input$pie_resized, {
    output$p_pie_usa_out <- renderPlotly({
      get(paste0(tolower(input$choose_species),"_plots"))$p_pie_usa     })
  })
  
  output$p_slopes <- renderPlotly({
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    
    get(paste0(tolower(input$choose_species),"_plots"))$p_slopes %>%
          layout(legend = list(orientation = "h",
                           x = .05,
                           y = 1.05),
             margin = list(r=0,t=0,l=-20,b=0))
  })
  
  
  output$selected_plot <- renderPlotly({
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    
    req(input$showplot_buttons != "") 
    
    if(input$showplot_buttons != "p_map_edge" &
       input$showplot_buttons != "p_map_cog"){
    selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[[input$showplot_buttons]]
    }
    
    if(input$showplot_buttons == "p_map_edge"){
      selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[["p_map"]]
    
      cg_idx_first <- which(vapply(selected_plot$x$data, function(tr) {
        identical(tr$legendgroup, "Canada")|identical(tr$legendgroup,"USA")
      }, logical(1)))
      cg_idx_frames <- which(vapply(selected_plot$x$frame[[2]]$data, function(tr) {
        identical(tr$legendgroup, "Canada")|identical(tr$legendgroup,"USA")
      }, logical(1)))
      
      # remove in first frame
      selected_plot$x$data[cg_idx_first] <- NULL
      
      # remove in all other frames
      for(i in seq_along(selected_plot$x$frames)) {
        selected_plot$x$frames[[i]]$data[cg_idx_frames] <- NULL
      }

    }
    
    if(input$showplot_buttons == "p_map_cog"){
      selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[["p_map"]]
      
      cg_idx_first <- which(vapply(selected_plot$x$data, function(tr) {
        identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
      }, logical(1)))
      cg_idx_frames <- which(vapply(selected_plot$x$frame[[2]]$data, function(tr) {
        identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
      }, logical(1)))
      
      # remove in first frame
      selected_plot$x$data[cg_idx_first] <- NULL
      
      # remove in all other frames
      for(i in seq_along(selected_plot$x$frames)) {
        selected_plot$x$frames[[i]]$data[cg_idx_frames] <- NULL
      }
      
    }
    
    return(selected_plot)
    
  })
  


  output$can_table <- renderReactable({
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    
    get(paste0(tolower(input$choose_species),"_plots"))$can_table  
    
  })
  
  output$usa_table <- renderReactable({
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    
    get(paste0(tolower(input$choose_species),"_plots"))$usa_table  
    
  })



  
  # Download button
  output$report_region_selector <- renderUI({
    req(input$report_breakdown)
    
    if (input$report_breakdown == "Country") {
      checkboxGroupInput(
        inputId = "region_choice",
        label   = "Select country:",
        choices = c("Canada", "USA"),
        #selected = "Canada"
      )
      
    } else if (input$report_breakdown == "NAFO zones") {
      div(
        class = "checkbox-grid",
        checkboxGroupInput(
          inputId = "region_choice",
          label   = "Select NAFO zone:",
          choices = nafo_zone_names
        )
      )
    }
  })
  
  
}
