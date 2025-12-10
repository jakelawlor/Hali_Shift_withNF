# Transboundary Shifts Shiny Tab 1

######### ABOUT ############

Tab1 <- function(dat){
  
  fluidPage(
    
    jumbotron(
      title = app_text$tab1$jumbotron$title,
      status = "info",
      lead = app_text$tab1$jumbotron$lead,
      btn = NULL,
      btnName = NULL,
      tags$head(
        tags$style(HTML(
          ".mb-4, .my-4 {
          width: 70%;
          float: left;
          display: block
          }"
        )))
      ,
      
      tags$img(
        src = base64enc::dataURI(file = "www/icons/logo_full.png", mime = "image/png"),
        class = "wrap-image",
        style = "margin-top: -50px"
      ),
      
      app_text$tab1$jumbotron$body
    ),
    
    fluidRow(
      column(1),
      column(10,
             tags$img(src = base64enc::dataURI(file = "www/framework.jpg", mime = ""), width = "100%", alt = "Description of my image",
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
        fluidRow(
          column(1),
          column(10,
                 app_text$tab1$box1$title1,
                 br(),
                 app_text$tab1$box1$paragraph1,
                 br(),
                 
                 fluidRow(
                   column(12,
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
                              app_text$tab1$box1$card_abundance
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
                              app_text$tab1$box1$card_area_occupied
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
                              app_text$tab1$box1$card_cog
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
                              app_text$tab1$box1$card_edge
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
                              app_text$tab1$box1$card_depth
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
                              app_text$tab1$box1$card_dist_to_border
                            )
                          )
                   )
                 ),
                 
                 br(),br(),
                 
                 # paragraph
                 app_text$tab1$box1$title2,
                 app_text$tab1$box1$paragraph2
          ),
          column(1))
    ) # end box
    
  )
  
}


