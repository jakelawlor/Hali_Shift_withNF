# Script to store all text to go into shiny app

app_text <- list(
  

  # header text -------------------------------------------------------------
  header = list(
    title = "Assessing Shifts",
    item1 = "About",
    item2 = "Spatial Narrative",
    item3 = "Science Details",
    item4 = "Download Summary"
  ),
  
  # contact sidebar text -------------------------------------------------------------
  contact = list(
    h3("Contact"),
    h4("Project Leads"),
    strong("Kiyomi Ferguson"),
    tags$a( "Kiyomi.Ferguson@dfo-mpo.gc.ca",  target = "_blank", href = "mailto:Kiyomi.Ferguson@dfo-mpo.gc.ca"), 
    br(),br(),
    strong("Nancy L Shackell"),
    tags$a("Nancy.Shackell@dfo-mpo.gc.ca", target = "_blank", href = "mailto:Nancy.Shackell@dfo-mpo.gc.ca"),
    br(),
    br(),
    p("This work was conducted with the support of Department of Fisheries and Oceans, Canada, and the National Oceanic and Atmospheric Administration, USA.  ")
  ),
  
  # footer text -------------------------------------------------------------
  footer = list(
    "App development by Jake Lawlor\nc 2025"
  ),
  
  # tab1 text - About page -------------------------------------------------------------
  tab1 = list(
    jumbotron = list(
      title = "Distribution Shifts Assessment Framework",
      lead = "A strategy for managing the shift of resources across jurisdictional boundaries.",
      body =  HTML("<br><br>How are changes in oceanic environmental conditions affecting the geographic distributions of commercial fisheries? Can climate change effects be detected  across a species’ range and aggregated by management zones to capture the spatial redistribution of a stock? <br> <br>Here, we present a tool to identify, assess, and categorize the nature of spatial and temporal changes in the abundance  of marine species, and identify regions where climate-induced changes carry implications for fisheries management.  This can be applied both within and across jurisdictional borders (e.g., fisheries management zones or divisions).<br> <br><i>The contents of this proof of concept  website are intended to support the development of a fuller Distribution  Shift Assessment Framework, demonstrating the potential of the tool  that is under development as of December 2025.</i><br></p>")
    ),
    
    box1 = list(
      title1 =  h2("Shift Indicators"),
      paragraph1 = p("A shift in distribution is interpreted using several different metrics that can be estimated from Species Distribution Model (SDM) output. A local perspective might estimate a stable stock abundance but a wider perspective using a suite of indicators may reveal an expansion into a new area. A suite of indicators provides a fuller picture of shifting distributions across localities/ zones/ divisions/ areas of interest. This application provides a fulsome packaging and summarization of distribution shift indicators to inform fisheries management and policy."),
      card_abundance = HTML("<p>Trends in local, regional and total abundance over time, estimated from region-specific surveys. Climate change expectations suggest species will decrease in abundance in areas where temperatures warm outside their tolerance limits, and increase where previously cold waters are becoming more tolerable.</p>"),
      card_abundance = HTML("<p>Trends in local, regional and total abundance over time, estimated from region-specific surveys. Climate change expectations suggest species will decrease in abundance in areas where temperatures warm outside their tolerance limits, and increase where previously cold waters are becoming more tolerable.</p>"),
      card_area_occupied = HTML("<p>Total area containing 90% of species abundance. Area occupied describes how spread out a population is. Taken with other indicators (e.g., abundance trend), increasing area occupied could indicate the expansion of a population, with either an increase, decrease, or stable density- each with highly different implications and interpretations. As climate change alters ocean conditions and environmental suitability, species are expected to shift, expand, or contract the areas they occupy based on the spatial availability of suitable habitat and population dynamics.</p>"),
      card_cog = HTML("<p>Average latitude and longitude of species' occurrences, weighted by abundance, also referred to as the Centre of Distribution.Climate change expectations suggest that centres of gravity should move poleward due to warming.</p>"),
      card_edge = HTML("<p>Change in the position of the southernmost (trailing) range edge and northernmost (leading) range edge of modeled species' distribution. With climate change, the expectation is that both leading and trailing range edges will shift towards the poles.</p>"),
      card_depth = HTML("<p>Abundance-weighted average of depth occurrences.While shifts in depth can reflect either the migration towards deeper, cooler waters, the bathymetry of new regions of occupation, or both, depth shifts can be interpreted with other biogeographic change factors to holistically explain changes to a species’ use of space.</p>"),
      card_dist_to_border = HTML("<p>The shortest path from the centre of gravity to the nearest point on the management border(s) of interest. Increasingly positive or negative trends indicate the weight of a species’ abundance is shifting away from the shared border, whereas trends moving towards 0  indicate the species is approaching this border.</p>"),
      title2 = h2("A Systematic Approach to Interpreting Shift Indicators"),
      paragraph2 = HTML( "<br><p>The first indicator to consider is the abundance trend, it reflects the overall temporal <b>status</b> of the population. The spatial narrative can then be built by systematically incorporating interpretations of the other shift indicators.<br><br>Alone, a positive or negative change in area occupied (AO) reflects <b>expansion</b> or <b>contraction</b> of a species distribution respectively. Coupling AO and the abundance trend, and defining the relationship between them will also identify any changes in <b>density</b>, achieving a general indication of redistribution.<br><br>Changes in centre of gravity, range edge, depth weighted abundance, and distance to border, can then be incorporated to indicate and differentiate between <b>shifts in range</b> or <b>aggregation</b> -directional/geographic trends- that accompany the redistribution.<br><br>Together, these details can characterize regional changes in distribution where various types of management strategies may be necessary.</p>" )
    )
  ),
  
  # tab2 text - Data page -------------------------------------------------------------
  tab2 = list(
    title = h2("Variable changes within regions"),
    subtitle = h4("*In Development"),
    box1 = list(
      title = "Selections"
    ),
    box2 = list(
      title = "Regional Trends"
    ),
    box3 = list(
      title = "Show Individual Trends"
    ),
    box4 = list(
      tab1 = list(title = "Region 1 trends",
                  lead =  "Indicator trends since 2006, during the period of highest warming."),
      tab2 = list(title = "Region 2 trends",
                  lead =  "Indicator trends since 2006, during the period of highest warming.")
    ),
    # placeholder box for projections
    boxX = list(title = "Indicator Projections",
                body = HTML("Placeholder box for projecting indicator values into the future<br><br>"))
  ),
  
  # tab3 text - Science Details page -------------------------------------------------------------
  tab3 = list(
    title =   h2("Scientific Details (Placeholder)"),
    subtitle =  h4("*In Development"),
    box1 = list(
      title =  h3("Brief summary of Steps 1-3 in DSAF (fig1)"),
      body = HTML("<h4>Step 1</h4> <p>Spatio-temporal species distribution models (SDMs) are designed in consideration of known, species-specific, biological information and ranges.</p>
                   <h4>Step 2</h4>
                   <p>Using the SDM output, the six shift indicators are calculated across the time-series and within each management region, with the trends and significance becoming the shift indicator values. </p>
                   <h4>Step 3</h4>
                   <p>
                   <div class='substep-block'>
                     <h5>Step 3.1.</h5><p> A principal component analysis (PCA) is conducted to rank shift indicators based on significance.</p>
                     <h5>Step 3.2.</h5><p> A correlation matrix is used to identify shift indicator pairings that suggest shifts in spatial range and/or aggregations.</p>
                     <h5>Step 3.3.</h5><p> The results are passed through a decision tree to produce a summary of biogeographic changes.</p>
                   </div>
                   </p> ")
    ),
    box2 = list(
      title = h3("Average Slopes"), 
      body =  p("Standardized slopes for shift indicators.")
    )
  ),
  
  # tab4 text - Download Page -------------------------------------------------------------
  tab4 = list(
    title =  h2("Generate Species Summary"),
    subtitle = h4("*In Development"),
    box1 = list(
      title = "Generate Report"
    )
  )
  

)
