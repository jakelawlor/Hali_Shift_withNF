# Transboundary Shifts Shiny Tab 2

######### ABOUT ############

Tab2 <- function(dat){
  
  fluidPage(
    
    #  circle plots and slope plot ======================
    h2("Variable changes within regions"),
    fluidRow(
      box(
        title = "Selections", 
        status = "info",
        collapsible = T,
        solidHeader = TRUE,
        width = 4,
        selectInput("choose_species",
                    label = "Focal Species",
                    choices = c("Halibut", "Lobster"),
                    selected = "Halibut"),
        selectInput("choose_breakdown",
                    label = "Regional Division",
                    choices = c("International Boundaries", "NAFO Zones"),
                    selected = "International Boundaries")
      ),
      box(title = "Multivariate Trends", 
          status = "info",
          collapsible = T,
          solidHeader = TRUE,
        width = 8,
        fluidRow(
        column(6,
        div(class = "fixed-square",
            plotlyOutput("p_pie_can_out", width = "100%", height = "100%")
        )),
      #  title = "Trends in USA", 
      #  status = "info",
      #  collapsible = T,
      #  solidHeader = TRUE,
       # width = 4,
      column(6,
        div(class = "fixed-square",
            plotlyOutput("p_pie_usa_out", width = "100%", height = "100%")
        )
      )
      ))
      
    ),
    
    fluidRow(
      
      box(width = 12,
          title = "Show Individual Trends",
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
              selected =  character(0) ,
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
      
      ## Tab box ----
      tabBox(
        title = "Trends Data",
        width = 12,
        type = "tabs",
        status = "info",
        solidHeader = TRUE,
        
        tabPanel(
          "USA trends",
          "Indicator trends since 2006, during the period of highest warming.",
          br(),
          reactableOutput("usa_table")
        ),
        tabPanel(
          "Canada trends",
          "Indicator trends since 2006, during the period of highest warming.",
          br(),
          reactableOutput("can_table")
        )
        
      
    ) 
      
    )
  
  
}


