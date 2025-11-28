# Transboundary Shifts Shiny Tab 1

######### ABOUT ############

Tab1 <- function(dat){
  
  fluidPage(
    
    jumbotron(
      title = "Distribution Shifts Assessment Framework",
      status = "info",
      lead = "A strategy for managing the shift of resources across jurisdictional boundaries.",
      btn = NULL,
      btnName = NULL,
      tags$head(
        tags$style(HTML(
          ".mb-4, .my-4 {
          width: 100%;
          float: left
          }"
        )))
      ,
      
      tags$img(
        src = base64enc::dataURI(file = "www/icons/logo_full.png", mime = "image/png"),
        alt = "Logo for multivariate range shift indicator",
        class = "wrap-image",
        style = "margin-top: -100px"
      ),
      
      HTML(
        "<br>How are changes in oceanic environmental conditions affecting the geographic distributions of commercial fisheries?  Can climate change effects be detected  across a species’ range,and aggregated by management zones to capture the spatial redistribution of a stock?  
      <br><br>
      Here, we present a tool to identify, assess, and categorize the nature of spatial and temporal changes in the abundance of marine species, and identify regions where climate-induced changes carry implications for fisheries management. This can be applied both within and across jurisdictional borders (e.g., fisheries management zones or divisions).
      <br><br>
      The contents of this proof of concept  website are intended to support the development of a fuller Distribution shift Assessment Framework, demonstrating the potential of the tool  that is under development as of December 2025
      <br>
      </p>
  ")
    ),
    
    fluidRow(
      column(1),
      column(10,
             tags$img(src = base64enc::dataURI(file = "www/framework.png", mime = ""), width = "100%", alt = "Description of my image",
                      class = "image-wrap")),
      column(1)
    ),
    br(),
    
    
    
    
    
    # indicators explained ----------------------------------------------------
    
    box(width = 12,
        collapsible = F,
        title = NULL,
        solidHeader = T,
        status = "info",
        h2("Shift Indicators"),
        p("A shift in distribution is interpreted using several different metrics that can be estimated from Species Distribution Model (SDM) output. A local perspective might estimate a stable stock abundance but a wider perspective using a suite of indicators may reveal an expansion into a new area. A suite of indicators provides a fuller picture of shifting distributions across localities/ zones/ divisions/ areas of interest. This application provides a fulsome packaging and summarization of distribution shift indicators to inform fisheries management and policy."),
        
        fluidRow(
          column(1),
          column(10,
                 # indicators row 1
                 fluidRow(
                   class = "flex-row",
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     height = "100%",
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_abundance.png", mime = ""), height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>Trends in local, regional and total abundance over time, estimated from region-specific surveys.</p>")
                   ),
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_area_occupied.png", mime = ""), height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>Total area containing 90% of species abundance., Area occupied describes how spread out a population is:; taken with other indicators (e.g., abundance trend), increasing area occupied could indicate the spread of a species’ into the region (if abundance is increasing), or changes in density of populations (if area is stable but abundance is decreasing).</p>")
                   ),
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_center_of_gravity.png", mime = ""), height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>Average latitude and longitude of species' occurrences, weighted by abundance, sometimes referred to as the Centre of Distribution.Climate change expectations suggest that centres of gravity should move poleward due to warming.</p>")
                   )),
                 # row 2:
                 fluidRow(
                   class = "flex-row",
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_range_edge.png", mime = ""),  height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>Change in the position of the southernmost (trailing) range edge and northernmost (leading) range edge of modeled species' distribution. With climate change, the expectation is that both leading and trailing range edges will shift towards the poles.</p>")
                   ),
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_depth.png", mime = ""), height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>Abundance-weighted average of depth occurrences.While shifts in depth can reflect either the migration towards deeper, cooler waters, the bathymetry of new regions of occupation, or both, depth shifts can be interpreted with other biogeographic change factors to holistically explain changes to a species’ use of space.</p>")
                   ),
                   box(
                     width = 4,
                     status = "secondary",
                     background = "secondary",
                     headerBorder = F,
                     title = NULL,
                     solidHeader = T,
                     collapsible = F,
                     tags$img(src = base64enc::dataURI(file = "www/icons/logo_distance_to_border.png", mime = ""), height = "100px", alt = "Description of my image", class = "image-wrap"),
                     HTML("<p>The shortest path from the centre of gravity to the nearest point on the border(s) of interest. Positive trends indicate the weight of a species’ abundance is shifting away from the region of interest, whereas negative trends indicate the species is approaching.</p>")
                   )
                 )
          ),
          column(1)
        ),
        
        br(),br(),
        
        # paragraph
        h2("A Systematic Approach to Interpreting Shift Indicators"),
        HTML(
          "<p>The first indicator to consider is the abundance trend, it reflects the overall temporal <b>status</b> of the population. The spatial narrative can then be built by systematically incorporating interpretations of the other shift indicators.  
      Alone, a positive or negative change in area occupied (AO) reflects <b>expansion</b> or <b>contraction</b> of a species distribution respectively. Coupling AO and the abundance trend, and defining the relationship between them will also identify any changes in <b>density</b>, achieving a general indication of redistribution.
      Changes in centre of gravity, range edge, depth weighted abundance, and distance to border, can then be incorporated to indicate <b>shifts in range or aggregation</b> -directional/geographic trends- that accompany the redistribution.
      Together, these details can characterize regional changes in distribution where various types of management strategies may be necessary.</p>"
        )
        
    ),
    
    
#    h1("Shift Indicators"),
#    h2("Our framework assesses range shifts by six variables:"),
#    
#    fluidRow(
#      # Area occupied box
#      box(title = NULL, 
#          height = 300,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_area_occupied.png", mime = ""), height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<p><b>Definition:</b> Area used to contain 90% of species abundance, as total area, percent of total marine area, and area efficiency.</p><br>
#               <b>Interpretation:</b> Increasing area use indicates an inward shift of a species while decreasing area use suggests a species' exit from the jurisdiction.")
#      ), 
#      
#      # abundance box
#      box(title = NULL, 
#          height = 300,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_abundance.png", mime = ""), height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<p><b>Definition:</b> Trends in total abundance over time, estimated from region-specific surveys. </p><br>
#               <b>Interpretation:</b> Abundance increases within jursidictions indicate shifts of population inwards, while abundance decreases 
#               indicate declining population trends.")
#      ), 
#      
#      # Centre of Gravity box
#      box(title = NULL, 
#          height = 300,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_center_of_gravity.png", mime = ""), height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<p><b>Definition:</b> Average latitude and longitude of species' occurrences, weighted by abundance.</p><br>
#               <b>Interpretation:</b> Climate expectations suggest that centres of gravity should move north following climate change. 
#               Here, northward and eastward shifts indicate the species is moving into Canadian waters.")
#      ), 
#      
#      # Distance to Border
#      box(title = NULL, 
#          height = 330,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_distance_to_border.png", mime = ""), height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<p><b>Definition:</b> The shortest path from the centre of gravity to the nearest point on the international border</p><br>
#               <b>Interpretation:</b> A centre of gravity approaching a border (distance decreasing over time) indicates that a species' range is shrinking within the country,
#               while a distance that is increasing over time indicates that the species is moving further into the jurisdiction")
#      ),
#      
#      # Range edge box
#      box(title = NULL, 
#          height = 330,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_range_edge.png", mime = ""),  height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<b>Definition:</b> Change in the position of the minimum (5%) and maximum (95%) of modeled species' distributions.</p><br>
#               <b>Interpretation:</b> Range edge shifts contracting towards borders show decreases in species' area within jurisdictions, while expansions show spread into an EEZ.")
#      ), 
#      
#      # Avg depth box
#      box(title = NULL, 
#          height = 330,
#          width = 4,
#          status = "info",
#          collapsible = F,
#          solidHeader = TRUE,
#          tags$img(src = base64enc::dataURI(file = "www/icons/logo_depth.png", mime = ""), height = "100px", alt = "Description of my image",
#                   class = "image-wrap"),
#          HTML("<p><b>Definition:</b> Abundance-weighted average of depth occurrences.</p><br><br>
#               <b>Interpretation:</b> While shifts in depth can reflect either the migration towards deeper, cooler waters, the bathymetry of new regions of occupation, or both, depth shifts can be interpreted with other biogeographic change factors to holistically explain space use.")
#      )
#      
#      
#      
#      
#    )
  )
  
}


