# Transboundary Shifts Shiny Tab 3
# SCIENTIFIC DETAILS

######### ABOUT ############

Tab3 <- function(dat){
  
  fluidPage(
    
    box(width = 12,
        title = NULL,
        status = "info",
        solidHeader = TRUE,
        collapsible = F,
        h3("Species Distribution Models"),
        HTML("<p>We fitted multiple Atlantic halibut species distribution models (SDMs) using the vector autoregressive spatio-temporal (VAST) modeling approach,
          predicting abundance caught per survey tow as a function of several environmental predictors. After model fitting and selection, our best-fit SDM 
          predicted abundances as a function of environmental covariates, spatial, and spatio-temporal random effects. <br><br>
          We use model outputs from our SDMs to derive all indicators for halibut range shifts, outlined below. </p>")
        ),
    
    fluidRow(
      # left box - indicator details
      box(
        title = NULL,
        status = "info",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 8,
        
         h3("Shift Indicator Calculations"),
         p("Each indicator was derived from Species Distribution Model (SDM) outputs for the focal species. 
           Indicator definitions, units, and interpretations differ, and can are standardized after calculation
           into comparable units of slope. See indicator details here."),
        br(),
        fixedCarousel(
          id = "mycarousel",
          width = 12,
          carouselItem(
            active = TRUE,
            fluidRow(column(1),
                     column(10,
                            h4("1. Abundance Trends"),
                            p("We used SDM outputs to calculate the predicted abundance in each cell of the study area. Abundance trends show the temporal change in predicted abundance within all cells in each study area. "),
                            br(),br()
                     ),
                     column(1)
            )
          ),
          carouselItem(
            fluidRow(column(1),
                     column(10,
                            h4("2. Area Occupied"),
                            p("Placeholder paragraph for calculation of area occupied"),
                            br(),br()
                     ),
                     column(1)
            )
          ),
          carouselItem(
            fluidRow(column(1),
                     column(10,
                            h4("3. Centre of Gravity"),
                            p("Placeholder paragraph for centre of gravity"),
                            br(),br()
                     ),
                     column(1)
            )
          ),
          carouselItem(
            fluidRow(column(1),
                     column(10,
                            h4("4. Range Edge"),
                            p("Placeholder paragraph for range edge"),
                            br(),br()
                     ),
                     column(1)
            )
          ),
          carouselItem(
            fluidRow(column(1),
                     column(10,
                            h4("5. Average Depth"),
                            p("Placeholder paragraph for average depth"),
                            br(),br()
                     ),
                     column(1)
            )
          ),
          carouselItem(
            fluidRow(column(1),
                     column(10,
                            h4("6. Distance to Border"),
                            p("Placeholder paragraph for distance to border"),
                            br(),br()
                     ),
                     column(1)
            )
          )
        )
      ),
      
      # right box - slopes plot
      box(
        status = "info",
        collapsible = F,
        title = NULL,
        solidHeader = TRUE,
        width = 4,
        h3("Average Slopes"), 
        p("Standardized slopes for shift indicators."),
        div(class = "fixed-square",
            plotlyOutput("p_slopes", width = "100%", height = "100%")
        )
      )
      
    )
    
    
  )
  
  
}
