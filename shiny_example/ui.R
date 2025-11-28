dashboardPage(
  title = "Bird Sightings",
  
  freshTheme = theme,
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # Header ----
  header = dashboardHeader(
    status = "lime",
    title = dashboardBrand(
      title = "Bird Sightings",
      color = "olive",
      image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    ),
    controlbarIcon = icon("circle-info"),
    fixed = TRUE,
    rightUi = dropdownMenu(
      badgeStatus = "info",
      type = "notifications",
      notificationItem(
        text = "Success",
        status = "success",
        icon = icon("circle-check")
      ),
      notificationItem(
        text = "Warning",
        status = "warning",
        icon = icon("circle-exclamation")
      ),
      notificationItem(
        text = "Error",
        status = "danger",
        icon = icon("circle-xmark")
      )
    )
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("bar-chart")
      )
    )
  ),
  
  # Control bar ----
  controlbar = dashboardControlbar(),
  
  # Footer ----
  footer = dashboardFooter(
    left = "Ashleigh Latter",
    right = "2024"
  ),
  
  # Body ----
  body = dashboardBody(
    tabItems(
      
      # Home tab ----
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Welcome!",
          status = "info",
          lead = "Visualising bird survey results from the City of Melbourne in February and March 2018",
          href = "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C",
          btnName = "Download",
          "Data available from the City of Melbourne Open Data Portal"
        ),
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Ashleigh Latter",
              subtitle = "Developer",
              image = "https://yt3.googleusercontent.com/kABO8qsiX0FKrvAsjdbU8q98mxSydtE4vpwu03omQ-WtRli9Lo1OTlDhjN05FNsUof2YhuHYvQ=s176-c-k-c0x00ffffff-no-rj",
              type = 1
            ),
            status = "purple",
            "Super impressive bio."
          ),
          
          box(
            title = "My favourite quote",
            width = 6,
            collapsible = FALSE,
            blockQuote("Just because you're trash, doesn't mean you can't do great things. It's called garbage can, not garbage cannot.", color = "purple")
          )
          
        )
        
      ),
      
      # Dashboard tab ----
      tabItem(
        tabName = "dashboard",
        
        ## Info boxes ----
        fluidRow(
          
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Total Bird Sightings",
              value = num_sightings,
              icon = icon("list"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = unique_birds,
              title = "Species Identified",
              icon = icon("dove"),
              color = "primary"
            )
          ),
          
          column(
            width = 4,
            infoBox(
              width = 12,
              value = unique_locations,
              title = "Total Sites",
              icon = icon("location-dot"),
              color = "primary"
            )
          )
          
        ),
        
        ## Sortable boxes ----
        fluidRow(
          sortable(
            width = 6,
            
            box(
              title = "Unique Birds Found at each Location", 
              width = 12, 
              status = "olive",
              collapsible = FALSE, 
              ribbon(
                text = "NEW",
                color = "olive"
              ),
              
              plotlyOutput("plot_unique_birds_site")
            ),
            
            box(
              title = "Birds Sighted Per Day",
              width = 12, 
              closable = TRUE, 
              status = "olive",
              
              plotlyOutput("plot_unique_birds_by_day")
            )
            
          ),
          
          sortable(
            width = 6,
            
            box(
              title = "Bird Sightings by Location",
              width = 12,  
              status = "olive",
              collapsible = FALSE,
              maximizable = TRUE,
              
              leafletOutput("plot_sightings_by_location")
              
            ),
            
            box(
              title = "Total Sightings For Each Bird",
              width = 12, 
              status = "olive",
              collapsible = FALSE, 
              label = boxLabel(
                text = "Label", 
                status = "primary", 
                tooltip = "I'm a label!"),
              
              sidebar = boxSidebar(
                id = "boxsidebarid",
                numericInput(
                  inputId = "show_top_n",
                  label = "Show Top N",
                  value = 6,
                  min = 1,
                  max = 50,
                  width = "97%"
                )
              ),
              
              plotlyOutput("plot_bird_totals_per_day")
            )
            
          )
        ),
        
        ## Tab box ----
        tabBox(
          title = "Data",
          width = 12,
          type = "tabs",
          status = "olive",
          solidHeader = TRUE,
          
          tabPanel(
            "Site Locations",
            DTOutput("table_sites")
          ),
          tabPanel(
            "Birds",
            DTOutput("table_birds")
          )
          
        )
      )
    )
  )
)