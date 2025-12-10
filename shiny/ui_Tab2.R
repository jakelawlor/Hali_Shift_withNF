# Transboundary Shifts Shiny Tab 2

# DATA TAB

Tab2 <- function(dat){
  
  fluidPage(
    
    # title and subtitle =====
    app_text$tab2$title,
    app_text$tab2$subtitle,
    
    # ROW 1 ----------------------------------------
    fluidRow(
      
      ## box 1) selectors ===============
      box(
        title =  app_text$tab2$box1$title,
        status = "info",
        collapsible = T,
        solidHeader = TRUE,
        width = 4,
        selectInput("choose_species",
                    label = "Focal Species",
                    choices = c("Species X" = "sppX", "Species Y" = "sppY"),
                    selected = "sppX"),
        selectInput("choose_breakdown",
                    label = "Regional Division",
                    choices = c("Jurisdictional Boundaries" = "jurisdictional", "NAFO Zones" = "nafo"),
                    selected = "jurisdictional"),
        uiOutput("study_area")  # instead of plotOutput
      ),
      
      ## box 2) Pie plots ===============
      box(
        title =  app_text$tab2$box2$title,
        status = "info",
        collapsible = T,
        solidHeader = TRUE,
        width = 8,
        fluidRow(
          column(5,
                 div(class = "fixed-square",
                     plotlyOutput("p_pie_Region1_out", width = "100%", height = "100%")
                 )),
          column(5,
                 div(class = "fixed-square",
                     plotlyOutput("p_pie_Region2_out", width = "100%", height = "100%")
                 )
          ),
          column(2,
                 plotlyOutput("p_pie_legend", width = "100%", height = "100%"))
        ))
      
    ),
    
    
    # ROW 2 ----------------------------
    fluidRow(
      
      ## Box 3) Indicator Plots ==============
      box(width = 12,
          title =  app_text$tab2$box3$title,
          status = "info",
          solidHeader = T,
          
          fluidRow(
            column(3,
                   prettyRadioButtons(
                     "showplot_buttons",
                     label = "Select Variable",
                     choiceNames =  c("Abundance","Area Occupied", "Average Depth",
                                      "Range Edge", "Distance to Border","Centre of Gravity"),
                     choiceValues=  c("p_abundance","p_area","p_depth","p_map_edge",
                                      "p_dist","p_map_cog"),
                     selected =  "p_abundance" ,
                     status = "info",
                     shape = "curve",
                     outline = T,
                     fill = T,
                     thick = T,
                     animation = NULL,
                     icon = NULL,
                     plain = FALSE,
                     bigger = FALSE,
                     inline = FALSE,
                     width = NULL
                   )),
            column(9,
                   plotlyOutput(
                     "selected_plot",
                     height = "500px"
                   ))
          ))
    ),
    
    
    
    # ROW X -------------------------------------------------------------------
    ## Box X) projections ======================
    fluidRow(
      
      box(
        title =  app_text$tab2$boxX$title,
        app_text$tab2$boxX$body,
        width = 12,
        status = "info",
        solidHeader = T
        
      )
      
    ),
    
    
    
    # ROW 3 --------------------------------------
    fluidRow(
      ## Box 3) Trends tables =========================
      tabBox(
        title = "Trends Data",
        width = 12,
        type = "tabs",
        status = "info",
        solidHeader = TRUE,
        
        
        tabPanel(
          title = app_text$tab2$box4$tab1$title,
          app_text$tab2$box4$tab1$lead,
          br(),
          reactableOutput("Region1_table")
        ),
        tabPanel(
          title = app_text$tab2$box4$tab2$title,
          app_text$tab2$box4$tab2$lead,
          br(),
          reactableOutput("Region2_table")
        )
        
        
      ) 
    )
  )
  
  
}


