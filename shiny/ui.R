dashboardPage(
  title = "Transboundary Range Shifts",
  
  freshTheme = theme,
  
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  
  # random fixes
  tags$script(HTML("
  $(document).on('shiny:connected', function() {
    var selector = '#mycarousel';
    var nextCtrl = $(selector + ' .carousel-control-next');

    // Make 'next' symmetric with 'prev'
    nextCtrl.attr('data-target', selector);
    nextCtrl.attr('href', '#');  // optional, for consistency
  });
")),
  
  # Header ----
  header = dashboardHeader(
    #status = "teal",
    #title = "",
    title = dashboardBrand(
      title = "Assessing Shifts",
      # color = "teal",
      image = "https://upload.wikimedia.org/wikipedia/commons/2/2c/Maple_leaf_transparent.png"
     # image = "https://images.unsplash.com/photo-1539664030485-a936c7d29c6e?q=80&w=1160&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"
    ),
    controlbarIcon = icon("envelope"),
    fixed = TRUE
  ),
  
  # Sidebar ----
  sidebar = dashboardSidebar(
    collapsed = TRUE,
    
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "About",
        tabName = "about",
        icon = icon("circle-info")
      ),
      menuItem(
        "Spatial Narrative",
        tabName = "spatial",
        icon = icon("earth-americas")
      ),
      menuItem(
        "Science Details",
        tabName = "details",
        icon = icon("magnifying-glass")
      ),
      menuItem(
        "Assessment Process",
        tabName = "process",
        icon = icon("screwdriver-wrench")
      )
    )
  ),
  
  # Control bar (right) ----
  controlbar = dashboardControlbar(
    box(
      width =12,
      headerBorder = FALSE,
      solidHeader = TRUE,
      title = NULL, 
      collapsible = FALSE,
      h3("Contact"),
      h4("Project Leads"),
      strong("Nancy L Shackell"),
      tags$a(
        "Nancy.Shackell@dfo-mpo.gc.ca",
        target = "_blank",
        href = "mailto:Nancy.Shackell@dfo-mpo.gc.ca"
      ),br(),
      strong("Kiyomi Ferguson"),
      tags$a(
        "Kiyomi.Ferguson@dfo-mpo.gc.ca",
        target = "_blank",
        href = "mailto:Kiyomi.Ferguson@dfo-mpo.gc.ca"
      ),

      br(),
      br(),
      p("This work was conducted with the support of Department of Fisheries and Oceans, Canada, and the National Oceanic and Atmospheric Administration, USA.  ")
    )
  ),
  
  # Footer ----
  footer = dashboardFooter(
    "App development by Jake Lawlor\nc 2025",
  ),
  
  # Body ----
  body = dashboardBody(
    
    tags$head(
      includeCSS(path = "www/style.css")
    ),
    
    tags$script(HTML("
    var resizeObserver = new ResizeObserver(entries => {
      for (let entry of entries) {
        Shiny.setInputValue('pie_resized', Math.random());
      }
    });
    Shiny.addCustomMessageHandler('observePie', function(id) {
      resizeObserver.observe(document.getElementById(id));
    });
  ")),
    
    

    tabItems(
      
      # About tab ----
      tabItem(
        tabName = "about",
        Tab1(dat)
      ),
      
      # Spatial Data tab ----
      tabItem(
        tabName = "spatial",
        
        Tab2(dat),
      ),
      
      
      # Scientific Details tab ----
      tabItem(
        tabName = "details",
        
        Tab3(dat),
      ),

        
     
      # Assessment Process Data tab ----
      
      tabItem(
        tabName = "process",
        Tab4(dat)
              
      )
    )
  )
)



