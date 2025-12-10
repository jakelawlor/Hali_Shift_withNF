# Main UI Page for shiny app
#
# contains header, sidebar, control bar, footer, and body placements
# note that body tabs (tabs 1-4) are created in separate scripts for organization



dashboardPage(
  title = "Transboundary Range Shifts",
  
  freshTheme = theme,
  
  dark = NULL,
  help = NULL,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  
  # Header ----
  header = dashboardHeader(
    title = dashboardBrand(
      title = app_text$header$title,
      image = "https://upload.wikimedia.org/wikipedia/commons/2/2c/Maple_leaf_transparent.png"
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
        text = app_text$header$item1,
        tabName = "about",
        icon = icon("circle-info")
      ),
      menuItem(
        text = app_text$header$item2,
        tabName = "spatial",
        icon = icon("earth-americas")
      ),
      menuItem(
        text = app_text$header$item3,
        tabName = "details",
        icon = icon("magnifying-glass")
      ),
      menuItem(
        text = app_text$header$item4,
        tabName = "download",
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
      app_text$contact
    )
  ),
  
  # Footer ----
  footer = dashboardFooter(
    app_text$footer
  ),
  
  
  # Body ----
  body = dashboardBody(
    
    # upload css
    tags$head(
      includeCSS(path = "www/style.css")
    ),
    
    # css to try to make pie charts resize when the window resizes
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
    
   # show tabs------
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

      # Download Summary tab ----
      
      tabItem(
        tabName = "download",
        Tab4(dat)
              
      )
    )
  )
) # end dashboard page



