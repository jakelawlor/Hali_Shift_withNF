# DSAF Shiny App SERVER 
function(input, output, session) {
  
  
  # make popup box on load =====================
  showModal(
    modalDialog(
      title = "Welcome",
      "Thank you for visiting the prototype DSAF application. This purpose of this application is to demonstrate how the DSAF framework could be used to assess fisheries across regions. Data from this application are hypothetical and to be used for demonstration purposes only.",
      easyClose = FALSE, # User must use a button to close
      footer = modalButton("OK")
    )
  )
  

  # get  plots for selected species --------------------------------------
  
  ## 1) pie charts =====================
  output$p_pie_Region1_out <- renderPlotly({
    
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_pie_Region1  
    
    })
  
  # pie chart 2
  output$p_pie_Region2_out <- renderPlotly({
    
    # require the selected species plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_pie_Region2  
    
  })
  
  # pie chart legend
  output$p_pie_legend <- renderPlotly({
    
    # require the selected species plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_pie_legend  
    
  })
  
  
  ## 2) slope plot =====================
  output$p_slopes <- renderPlotly({
    
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # upload plot
    get(paste0(tolower(input$choose_species),"_plots"))$p_slopes
  })
  
  
  
  ## 3) individual indicator plots ------------------------------------------
  # the user will input a choice on "showplot_buttons" to show one of 6 plots
  # and output it as output$selected_plot
  
  # individual indicator plots, as selected
  output$selected_plot <- renderPlotly({
    
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    req(input$showplot_buttons != "") 
    
    ### 3a) get plot by name if it's not a mapped variable: =========
    if(input$showplot_buttons != "p_map_edge" &
       input$showplot_buttons != "p_map_cog"){
    selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[[input$showplot_buttons]]
    }
    
    ## 3b) get range edge map plot: ===========================
    # since COG and Range Edge are both on one plot ("p_map"), we have to
    # edit the plot in the server to remove the traces that don't pertain
    # to the selected variable (COG or Edge)
    if(input$showplot_buttons == "p_map_edge"){
      
      # upload the shared map (with COG and Edges)
      selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[["p_map"]]
    
      # identify which traces pertain to the COGs in Region1 and Region2 (and not edges)
      cg_idx_first <- which(vapply(selected_plot$x$data, function(tr) {
        identical(tr$legendgroup, "Region1")|identical(tr$legendgroup,"Region2")
      }, logical(1)))
      cg_idx_frames <- which(vapply(selected_plot$x$frame[[2]]$data, function(tr) {
        identical(tr$legendgroup, "Region1")|identical(tr$legendgroup,"Region2")
      }, logical(1)))
      
      # remove COG traces from the first frame
      selected_plot$x$data[cg_idx_first] <- NULL
      
      # remove COG traces in all other frames
      for(i in seq_along(selected_plot$x$frames)) {
        selected_plot$x$frames[[i]]$data[cg_idx_frames] <- NULL
      }

    }
    
    ## 3c) get COG map plot: ===========================
    # since COG and Range Edge are both on one plot ("p_map"), we have to
    # edit the plot in the server to remove the traces that don't pertain
    # to the selected variable (COG or Edge)
    if(input$showplot_buttons == "p_map_cog"){
      
      # upload the shared map (with COG and Edges)
      selected_plot <- get(paste0(tolower(input$choose_species),"_plots"))[["p_map"]]
      
      # identify which traces pertain to the leading and trailing edges (and not COGs)
      cg_idx_first <- which(vapply(selected_plot$x$data, function(tr) {
        identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
      }, logical(1)))
      cg_idx_frames <- which(vapply(selected_plot$x$frame[[2]]$data, function(tr) {
        identical(tr$legendgroup, "Leading Edge")|identical(tr$legendgroup,"Trailing Edge")
      }, logical(1)))
      
      # remove Edge traces in first frame
      selected_plot$x$data[cg_idx_first] <- NULL
      
      # remove edge traces in all other frames
      for(i in seq_along(selected_plot$x$frames)) {
        selected_plot$x$frames[[i]]$data[cg_idx_frames] <- NULL
      }
      
    }
    
    # return plot:
    return(selected_plot)
    
  })
  


  ## 4) upload tables -----------------------------------------------------------
  ### 4a) Table - region 1 =======================
  output$Region1_table <- renderReactable({
    
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get premade table
    get(paste0(tolower(input$choose_species),"_plots"))$Region1_table  
    
  })
  
  ### 4b) Table - region 2 =========================
  output$Region2_table <- renderReactable({
    
    # require the selected species' plot to exist
    req(exists(paste0(tolower(input$choose_species),"_plots"))) 
    # get premade table
    get(paste0(tolower(input$choose_species),"_plots"))$Region2_table  
    
  })
  

  ## 5) get study area map ------------------------------------------------------
  output$study_area <- renderUI({
    
    # require "choose_breakdown" (jurisdictional boundaries or nafo zones) to exist
    req(input$choose_breakdown)
    req(!is.null(study_area_imgs[[input$choose_breakdown]]))
    
    # get image
    img_src <- study_area_imgs[[input$choose_breakdown]]
    
    tags$img(
      src   = img_src,
      style = "width: 100%; height: 250px; object-fit: contain;"
    )
  })



  # download button ---------------------------------------------------------
  
  ### a) render UI for checkboxes ======================
  # first, make a dynamic UI with renderUI that shows options to 
  # select from a checkbox of regions (region1/region2) "Jurisdictional Boundaries" is the selected breakdown,
  # and select from NAFO zone names if "NAFO zones" is the selected breakdown
  # and allows 
  output$report_region_selector <- renderUI({
    req(input$summary_breakdown)
    
    # create checkbox for regions
    if (input$summary_breakdown == "jurisdictional") {
      checkboxGroupInput(
        inputId = "region_choice",
        label   = "Select country:",
        choiceNames =  c("Region 1", "Region 2"),
        choiceValues = c("Region1","Region2")
      )
      
    } else if (input$summary_breakdown == "nafo") {
      # create checkbox for NAFO zones
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
  
  ### b) download handler ======================
  output$downloadsummary <- downloadHandler(
    filename = function() {
      paste0(paste0(input$summary_species,collapse="-"),"_",
             paste(input$region_choice,collapse="-"),".pdf")
    # The name the user will see when downloading
    },
    content = function(file) {
      file.copy("www/example_summary.pdf", file) # Copy the premade PDF to the download location
    },
    contentType = "application/pdf" # Specify the content type
  )
  
}
