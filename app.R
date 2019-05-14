library(shiny)
library(shinymaterial)
library(tidyverse)
library(writexl)

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
      width = 5,
      material_card(
        title = "",
        depth = 4,
        uiOutput('questions_ui'),
        br(),
          material_card(
            depth = 4,
            plotOutput('progress_plot')
          )
        )),
    material_column(
      width = 5,
      br(),br(),
      textOutput('text_instructions'),
      uiOutput('text_options'),
      fluidRow(
        column(6,
               material_button(input_id = 'submit_button', 
                               label = 'Submit')),
        column(6)
      ),
      uiOutput('excel_ui'),
     DT::dataTableOutput('current_table')
    ),
    
    material_column(width = 4,
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
                        href = "http://databrew.cc",
                        "Visite our website!"
                      )
                    )
    )
  )
    
)

server <- function(input, output) {
  
  showModal(
    modalDialog(title = 'Log in', 
                footer = modalButton('Submit'),
                fluidPage(
                  textInput('user',
                            'Username', 
                            value = 'Oleksiy'),
                  passwordInput('password',
                                'Password')
                ))
  )
  current_values <- reactiveValues(data = data.frame())
  
  # reactive value for storing the select index
  select_index <- reactiveVal(1)
  
  observeEvent(input$submit_button, {
    message('submit button was pressed')
    old_index <- select_index()
    new_index <- old_index + 1
    select_index(new_index)
    message('the new index is ', new_index)
    message('adding user data to reactive values')
    key <- input$questions
    if(!is.null(key)){
      sub_number <- which(key == tracer_survey_questions)
      if(sub_number %in% dropdown_choices){
        value <- input$dropdown
      } else {
        value <- input$direct_text
      }
      old_data <- current_values$data
      new_data <- data.frame(user = input$user, key = key, value = value, time_stamp = Sys.time())
      updated_data <- bind_rows(old_data, new_data)
      current_values$data <- updated_data
      message('updating current values')
    } 
  })
  
  output$questions_ui <- renderUI({
    si <- select_index()
    selectInput(
     "questions",
      label = "Choose survey question",
      choices = tracer_survey_questions,
      selected = tracer_survey_questions[si]
    )
  })
  
  output$text_instructions <- renderText({
    question <- input$questions
    # question <- tracer_survey_questions[1]
    
    sub_number <- which(question == tracer_survey_questions)
    
    return(tracer_survey_sub[sub_number])
    
    
    
  })
  
  output$text_options <- renderUI({
    # questions 5,6,7,10,11,12,13
    # 5 - individaul, organizational, enabling environment
    # 6 - training, technical assistance, knowledge production, knowledge sharing
    # 7 - training, technical assistance, knowledge production, knowledge sharing
    # 10 - government, civil society groups/ngos, academia, research non academia, m&e netweorks, private sector, donor agencies, other CLEAR centers
    # 11 - government, civil society groups/ngos, academia, research non academia, m&e netweorks, private sector, donor agencies, other CLEAR centers
    # 12 - evaluators, users of evaluation research/policy makers, Commissioners or Managers of Evaluations, other
    # 13 - executive level, managerial level, technical or professional
    
    question <- input$questions
    if(!is.null(question)){
      # question <- tracer_survey_questions[1]
      message('the question is ', question)
      sub_number <- which(question == tracer_survey_questions)
      message('the sub number is ', sub_number)
      if(sub_number %in% dropdown_choices) {
        
        if(sub_number == 5){
          dropdown_choices <- c('Individual', 'Organizational', 
                                'Enabling Environment')
          
        } 
        if(sub_number %in% c(6, 7)){
          dropdown_choices <- c('Training', 'Technical assistance', 
                                'Knowledge production', 'Knowledge sharing')
          
        } 
        if(sub_number %in% c(10, 11)){
          dropdown_choices <- c('Government', 'Civil society groups/NGOs', 'Academia',
                                'Research non-academia', 'M&E netweorks', 'Private sector', 
                                'Donor agencies', 'Other CLEAR centers')
          
        } 
        if(sub_number == 12){
          dropdown_choices <- c('Evaluators', 'Users of evaluation research/policy makers', 'Commissioners or managers of evaluations', 'other')
          
          
        }
        message('this is a dropdown input')
        
        selectInput('dropdown', 
                    label = '', 
                    choices = dropdown_choices, 
                    selected = dropdown_choices[1])
        
      } else {
        message('this is a text input')
        material_text_box(input_id = 'direct_text', 
                          label = '')
      }
    } else {
      return(NULL)
    }
  })
  
  output$current_table <- DT::renderDataTable({
    df <- current_values$data
    if(!is.null(df)){
      if(nrow(df) > 0){
        # Keep only the most recent entry
        df <- df %>%
          group_by(key) %>%
          filter(time_stamp == max(time_stamp))
        DT::datatable(df)
      }
    }
  })
  
  output$progress_plot <- renderPlot({
    df <- current_values$data
    if(!is.null(df)){
      if(nrow(df) > 0){
        # Keep only most recent values for each key
        df <- df %>%
          group_by(key) %>%
          filter(time_stamp == max(time_stamp))
      }
      done <- nrow(df)
      total <- length(tracer_survey_questions)
      remaining <- total - done
      percent_done <- round((done/total)*100, 2)
      percent_remaining <- 100 - percent_done
      pd <- data.frame(percent_done, percent_remaining)
      pd <- gather(pd, key, value, percent_done:percent_remaining) %>% mutate(group = 1)
      pd$key <- factor(pd$key,
                       levels = c('percent_remaining',
                                  'percent_done'),
                       labels = c('Remaining',
                                  'Finished'))
      ggplot(pd, aes(group, value,
                     fill = key)) + 
        geom_bar(stat = 'identity') +
        labs(x = '',
             y = '') +
        coord_flip() +
        scale_fill_manual(name = '',
                          values = rev(c('darkorange', 'blue'))) +
        theme_minimal() +
        theme(axis.text = element_text(size = 0),
              legend.position = 'none') +
        guides(fill = guide_legend(reverse=T)) +
        geom_text(data = pd %>%
                    dplyr::filter(key == 'Finished'),
                           aes(label = paste0(value, '% finished'),
                               y = ifelse(value < 40,
                                          value + 20,
                                          value - 20)),
                  color = 'white',
                  size = 6) +
        theme(panel.grid = element_blank())
    }
  },
  height = 100)
  
  output$excel_ui <- renderUI({
    ok <- FALSE
    df <- current_values$data
    if(!is.null(df)){
      if(nrow(df) > 0){
        ok <- TRUE
      }
    }
    
    if(ok){
      fluidPage(
        fluidRow(
          column(12, align = 'center',
                 material_button('write_excel',
                                 label = 'Export below data to Excel'))
        )
      )} else {
       fluidPage() 
      }
    })
}


shinyApp(ui = ui, server = server)