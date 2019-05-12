library(shiny)
library(shinymaterial)

# overall help here: https://ericrayanderson.github.io/shinymaterial/
# color universe here: http://materializecss.com/color.html
# icon universe here :http://materializecss.com/icons.html

source('global.R')


ui <- material_page(
  title = "Databrew app example",
  nav_bar_color = "blue",
  nav_bar_fixed = FALSE,
  
  
  # Place side-nav in the beginning of the UI
  material_side_nav(fixed = FALSE, # Place side-nav tabs within side-nav
                    material_side_nav_tabs(side_nav_tabs = c("Tracer survey" = "tracer_survey",
                                                             "About" = "about_section"),
                                           icons = c("assessment","favorite_border" ))
  ),
  
  material_row(
    material_column(
      width = 3,
      material_card(
        title = "",
        depth = 4,
        material_dropdown(
          input_id = "questions",
          label = "Choose survey question",
          choices = tracer_survey_questions,
          selected = tracer_survey_questions[1]
        ))),
    material_column(
      width = 4,
      material_card(title = '',
                    depth = 4,
                    textOutput('text_instructions'))),
    material_column(width = 10,
                    # Define side-nav tab content
                    material_side_nav_tab_content(
                      side_nav_tab_id = "tracer_survey"
                    ),
                    
                    
                    # Define side-nav tab content
                    material_side_nav_tab_content(
                      side_nav_tab_id = "about_section",
                      tags$h4("About tab"),
                      
                      # reference webiste 
                      tags$a(
                        target = "_blank",
                        class = "btn orange",
                        href = "databrew.cc",
                        "Visite our website!"
                      )
                      
                    )
                    
    )
    
    
    
    
    
  )    
  
  
)

server <- function(input, output) {
  
  output$text_output <- renderText({

    
    
  })
  
}
shinyApp(ui = ui, server = server)