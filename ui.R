
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Hugo_finances",
               
               
               
               
               navbarMenu('Global activity',
                   
                   tabPanel('Table', "Let's look at our data:", DT::dataTableOutput("tabl")),
               
               tabPanel('Plot',
                        
                        mainPanel(
                            
                            "Here is my global financial activity.", br(), br(),
                            
                            plotly::plotlyOutput('glob'))
                        
               )
               ),
               
               
               tabPanel('Plot',
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                uiOutput('categories'),
                                uiOutput('dates'),
                                uiOutput('montant'),
                                
                                selectInput(
                                    "timu", #for time unit
                                    "Choose your time aggregation unit",
                                    c('Day', 'Week', 'Month',
                                      #'Bimonth', 'Trimester', #Maybe later
                                      'Year', 'All time'),
                                    selected = 'Day',
                                    multiple = FALSE,
                                    selectize = TRUE,
                                    width = NULL,
                                    size = NULL
                                ),
                                
                                actionButton('go', label='WooGO !')),
                            
                            mainPanel(
                                
                                plotly::plotlyOutput('rldl'), br(), br()
                                
                            ))),
               
               
               tabPanel('About Woogo',  
                        
                        "This dashboard was constructed to display data about my financial activities (Hugo).",
                        
                        br(), br(),
                        
                        "For instance, one can print the amount of money spent on Restauration in August 2021.", br(), br(),
                        
                        img(src='stonks.jpeg')
               )
               
               
    )
)
