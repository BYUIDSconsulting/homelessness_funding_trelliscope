library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(readr)
library(shinycssloaders)
library(trelliscopejs)
library(shinythemes)
library(shinydashboard)
library(readr)
library(homelessR)
crime <- read.csv('/Users/Becca/Documents/Data Consulting/county_data.csv', sep = ',', header = TRUE)
# Define UI for application that draws a histogram
hud <- hud
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'bea_cols', 
                    label = 'BEA (Bureau of Economic Activity)',
                    choices = c("State", "Number.of.CoCs", "Overall.Homeless", "Sheltered.Total.Homeless", "Unsheltered.Homeless", "Overall.Homeless.Individuals","Sheltered.Total.Homeless.Individuals", "Unsheltered.Homeless.Individuals",                                                                   
                                "Overall.Homeless.People.in.Families", "Sheltered.Total.Homeless.People.in.Families", "Unsheltered.Homeless.People.in.Families", "Overall.Homeless.Family.Households",  "Sheltered.Total.Homeless.Family.Households", "Unsheltered.Homeless.Family.Households", "Overall.Chronically.Homeless.Individuals", "Sheltered.Total.Chronically.Homeless.Individuals", "Unsheltered.Chronically.Homeless.Individuals", "Overall.Homeless.Veterans", "Sheltered.Total.Homeless.Veterans", "Unsheltered.Homeless.Veterans", "year",                                                                   
                                "Sheltered.Total.Chronically.Homeless", "Unsheltered.Chronically.Homeless", "Overall.Chronically.Homeless.People.in.Families",                                    
                                "Sheltered.Total.Chronically.Homeless.People.in.Families","Unsheltered.Chronically.Homeless.People.in.Families","Overall.Homeless...Under.18", "Overall.Homeless...Age.18.to.24","Overall.Homeless...Over.24")),
        selectInput(inputId = 'census_cols',
                    label = 'Census',
                    choices = c('Column 1', 'Column 2')),
        selectInput(inputId = 'crime_cols',
                    label = 'Crime',
                    choices = c('state', 'violent_crime', 'murder_and_nonnegligent_manslaughter', 'rape', 'robbery', 'aggravated_assault', 'property_crime', 'burglary', 'larceny_theft', 'motor_vehicle_theft', 'arson', 'year')),
        selectInput(inputId = 'fund_cols',
                    label = 'Funding',
                    choices = c('Column 1', 'Column 2')),
        selectInput(inputId = 'hud_cols',
                    label = 'HUD (Dept. Housing and Urban Development)',
                    choices = c('Column 1', 'Column 2')),
        selectInput(inputId = 'trello',
                    label = 'Pre-made Trelliscopes',
                    choices = c('Violent Crime over the Years by State', 'Murder over the Years by State',
                                'Rape over the Years by State', 'Robbery over the Years by State', 
                                'Aggravated Assailt over the Years by State', 
                                'Property Crime over the Years by State', 'Burglary over the Years by State', 
                                'Theft over the Years by State', 'Motor Vehicle Theft over the Years by State',
                                'Arson over the Years by State')),
        actionButton(inputId = 'view_trells',
                     label = "View"), ## view_trells 
        h3('Create your own trelliscope'),
        textInput(inputId = 'x-axis',
                  label = 'X-Axis'),
        textInput(inputId = 'y-axis',
                  label = 'Y-Axis'),
        textInput(inputId = 'facet',
                  label = 'Facet By'),
        textInput(inputId = 'save_trell',
                  label = 'Name to Save'),
        actionButton(inputId = 'trells',
                     label = 'Create Trelliscope')
      ), # sidebarPanel
      
      mainPanel(
        plotOutput(outputId = 'graphs', 
                   width = "1024px",
                   height = "768px"),
        withSpinner(trelliscopeOutput(outputId = 'plot')),
        textOutput(outputId = 'description_funding_graph')
      ) # mainPanel
    ) # sidebarLayout
  ) # fluidPage

server <- function(input, output, session) {
  observeEvent(input$view_trells, {
    showModal(modalDialog(title = 'Link to Trelliscope:',
                          helpText(a('Click the link to view trelliscope', 
                                     href = paste0('file:///Users/Becca/Documents/Data%20Consulting/homelessR/create_url/www/index.html#display=', input$trello, '&nrow=2&ncol=2&arr=row&pg=1&labels=state&sort=state;asc&filter=&sidebar=-1&fv=',
                                                   target = 'blank'))),
                          easyClose = TRUE))
  })
}

#observeEvent(input$trells, {
## when the button is clicked, create the trelliscope in the shiny app
## observeEvent for trells
# output$plot <- renderTrelliscope({
##    input$trells
#  trell <- ggplot(data = cri, aes_string(x = isolate(input$x-axis), y = isolate(input$y-axis))) +
#    geom_line() +
#    geom_point() +
#    theme_bw() +
#    ylab(input$column) +
#    facet_trelliscope(~ input$facet, nrow = 2, ncol = 2, name = input$save_trell, 
#                      path = '/Users/Becca/Documents/Data Consulting/homelessR/create_url/www')
#       ggsave(trell)
# trell
#crime %>%
#  group_by(input$facet) %>% 
#  nest() %>%
#  mutate(
#    panel = map_plot(data, ~
#                       ggplot(data = crime, aes_string(x = input$x-axis, y = input$y-axis)) +
#                       geom_line() +
#                       gemo_point() +
#                       theme_bw() +
#                       ylab(input$column)
#    )) %>%
#  ungroup() %>%
#  trelliscope(name = "Crime and Homelessness", ncol = 2, nrow = 2, self_contained = TRUE)
#)
# }) ## end of the trelliscope

#}) ## observeEvent for trells


# Run the application 
shinyApp(ui = ui, server = server)
