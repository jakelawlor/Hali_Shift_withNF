# Transboundary Shifts Shiny Tab 3

######### ABOUT ############

Tab4 <- function(dat){
  
 # fluidRow(
 #   column(6,
 #          h2("Multivariate Species Reports"),
 #          h4("*In Development"),
 #          p("We use these findings to identify relationships relevant to species' management. This process occurs in two phases:"),
 #          
 #          h4("Phase 1"),
 #          h5("Science Divisions: Steps 1-3 in DSAF (fig1)"),
 #          HTML("<p>For each species, we will identify correlations between range shift indicator pairs within regions, and identify key pairings that suggest multivariate species’ changes. Indicator correlation matrices and their flagged pairings will help to holistically describe biogeographic changes and trends for species within regions.
 #          <br><br>
 #            Then, we will generate summary reports describing species’ changes in and across fishing zones/divisions of interest, which can be downloaded by the user. The goal of our multivariate analysis is to operationalize distribution shift indicators to support evidence-based decision making. 
 #               </p>"
 #          ),
 #          br(),
 #          h4("Phase 2"),
 #          h5("Management Divisions: Steps 4 -5in DSAF (fig 1)"),
 #          p("Fisheries and Ecosystem management sectors can use output summary reports in reporting, decision-making and risk-based assessment processes."),
 #          
 #          box(status ="info",
 #              title = "Generate Report",
 #              solidHeader = T,
 #              width = 12,
 #              fluidRow(
 #                column(6,
 #                       selectInput("summary_species",
 #                                   label = "Focal Species",
 #                                   multiple = T,
 #                                   choices = c("Species X" = "sppx", "Species Y" = "sppy"),
 #                                   selected = "sppx")),
 #                column(6,
 #                       # 1) First-level selection:
 #                       selectInput(
 #                         inputId = "summary_breakdown",
 #                         label   = "Regional Division:",
 #                         choices = c("Jurisdictional Boundaries" = "jurisdictional", "NAFO zones" = "nafo"),
 #                         selected = "jurisdictional"
 #                       ),
 #                       # 2) Second-level selector, created dynamically
 #                       uiOutput("report_region_selector")
 #                       )
 #              ),
 #                     downloadButton("downloadsummary", "Generate Summary")
 #              
 #          )
 #          
 #          
 #   ),
 #   column(6,
 #          tags$img(src = base64enc::dataURI(file = "www/placeholder.png", mime = ""),
 #                   #height = "500px", 
 #                   width = "75%",
 #                   class = "image-wrap"),
 #          )
 # )
  
  
  fluidPage(
    app_text$tab4$title,
    app_text$tab4$subtitle,
    
    box(status ="info",
        title =  app_text$tab4$box1$title,
        solidHeader = T,
        width = 12,
        fluidRow(
          column(6,
                 selectInput("summary_species",
                             label = "Focal Species",
                             multiple = T,
                             choices = c("Species X" = "sppx", "Species Y" = "sppy"),
                             selected = "sppx")),
          column(6,
                 # 1) First-level selection:
                 selectInput(
                   inputId = "summary_breakdown",
                   label   = "Regional Division:",
                   choices = c("Jurisdictional Boundaries" = "jurisdictional", "NAFO zones" = "nafo"),
                   selected = "jurisdictional"
                 ),
                 # 2) Second-level selector, created dynamically
                 uiOutput("report_region_selector")
          )
        ),
        downloadButton("downloadsummary", "Generate Summary")
        
    )
  )
  
}
