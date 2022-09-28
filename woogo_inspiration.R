
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage("Woogo",
               
               tabPanel('Plot',
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                uiOutput('orga_controls'),
                                uiOutput('action_controls'),
                                uiOutput('date_controls'),
                                
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
               
               tabPanel('Global',
                        
                        mainPanel(
                            
                            "Here is the global activity in Wooflash", br(), br(),
                            
                            plotly::plotlyOutput('glob'))
                        
               ),
               
               navbarMenu('More',
                          
                          tabPanel('Table', "Let's look at our data:", DT::dataTableOutput("sample")),
                          
                          
                          tabPanel('About Woogo',  
                                   "This dashboard was constructed to display data about some organizations using Wooflash",
                                   
                                   br(), br(),
                                   
                                   "For instance, one can print the number of actions emitted from a group of organizations.", br(), br(),
                                   
                                   img(src='wooflash.png')
                          ))
               
               
    )
)

shinyServer(function(input, output) {
    
    con <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = Sys.getenv('WOOCLAP_DBNAME'),
                          host = Sys.getenv('WOOCLAP_HOST'),
                          port = Sys.getenv('WOOCLAP_PORT'),
                          user = Sys.getenv('WOOCLAP_USER'),
                          password = Sys.getenv('WOOCLAP_PASSWORD')
    )
    
    q0 <- "SELECT * FROM unified_data.wf_organization_action" #Gathering info in a data.table
    data <- data.table::data.table(dbGetQuery(conn=con, q0))
    
    output$sample <- DT::renderDataTable({
        data[1:100]
    })
    
    output$glob <- plotly::renderPlotly({
        plot_ly(data = data[, .(sum = sum(count)), by = day],
                x = ~day,
                y = ~sum,
                type = 'bar', name = 'actionsday',
                marker = list(color = '#9A75F8')) %>%
            layout(title = 'Wooflash global activity',
                   plot_bgcolor = "#d4fad5", 
                   xaxis = list(title = 'Date'),
                   yaxis = list(title = 'Actions'))
    })
    
    output$orga_controls <- renderUI({
        orgas <- data[, unique(orga_name)]
        selectInput(
            "org",
            "Choose your fighter",
            c('All organizations', orgas), #We add an option.
            selected = 'All organizations',
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
        )
    })
    
    output$action_controls <- renderUI({
        actions <- data[, unique(action)]
        selectInput(
            "act",
            "Choose your action type",
            c('All actions', actions),
            selected = 'All actions',
            multiple = TRUE,
            selectize = TRUE,
            width = NULL,
            size = NULL
        )
    })
    
    output$date_controls <- renderUI({
        dateRangeInput('date',  #Picked from Wooshiny
                       label = 'Choose your date range (yyyy-mm-dd)',
                       start = data[, min(day)],
                       end = data[, max(day)],
                       min = data[, min(day)],
                       max = data[, max(day)]
        )
    })
    
    
    
    
    
    output$rldl <- plotly::renderPlotly({
        
        
        input$go  #Creating a dependency. When one clicks the button, rldl will rerun its code because the value of 
        #input$go would have changed. 
        #Now, we need to isolate other variables reacting to inputs.
        
        
        #Treating the 'all options' cases
        acti <- isolate(if( 'All actions' %in% req(input$act) ) { #beginning isolation
            data[, unique(action)]
        } else {
            input$act
        })
        
        orga <- isolate(if(req(input$org) == 'All organizations') {
            data[, unique(orga_name)]
        } else {
            input$org
        })
        
        isolate(if (input$timu == 'Day') { #Here, we could think of attributing a number of days corresponding to the time aggregation
            dt <- isolate(data[action %in% acti & orga_name %in% orga & day %between% input$date, #unit and then group by, but I think it would take longer
                               .(sum = sum(count)), by = day])
            
            xax <- dt[[1]]
            yax <- dt[[2]] #We have to differentiate axis depending on the time aggregation unit.
            
        } else if(input$timu=='Week') {
            dt <- isolate(data[action %in% acti  & orga_name %in% orga & day %between% input$date,
                               lapply(.SD, sum), by = .(year(day), week(day)), .SDcols = 'count'])
            
            week_year <- dt[, paste(week, year, sep = '-')]
            xax <- factor(week_year, levels = as.character(week_year))
            yax <- dt[[3]]
        } else if(input$timu=='Month') {
            
            dt <- isolate(data[action %in% acti  & orga_name %in% orga
                               & day %between% input$date, lapply(.SD,sum), 
                               .(year(day),month(day)), .SDcols='count'])
            
            xax <- factor(x = paste(dt[[2]],dt[[1]],sep='-'), levels=as.character(paste(dt[[2]],dt[[1]],sep='-'))) #Getting temporal order
            yax <- dt[[3]]
            
        } else if(input$timu=='Year') {
            
            dt <- isolate(data[action %in% acti  & orga_name %in% orga & day %between% input$date,
                               lapply(.SD,sum), year(day), .SDcols='count'])
            xax <- dt[[1]]
            yax <- dt[[2]]
            
        } else if(input$timu=="All time") { #could replace else if by else
            
            dt <- isolate(data.table::data.table(
                date="All time",
                count=data[action %in% acti  & orga_name %in% orga   #We need a little adaptation
                           & day %between% input$date, sum(count)])
            ) #to have 2 columns
            xax <- dt[[1]]
            yax <- dt[[2]]
            
        }) #ending else if and isolate
        
        #In fact, we could even imagine inputting a number of days and grouping for each possibility
        #That would give much freedom to the user. But #keep it simple.
        
        
        fig2 <- isolate(plotly::plot_ly(
            x = xax,
            y = yax,
            type='bar', name='real deal',
            marker=list(color='#9A75F8')) %>%
                
                layout(title = paste(input$org, 'activity', '(', paste(input$act, collapse=' - ', sep=' '), ')', sep=' '), #adapting the title to the content
                       plot_bgcolor = "#d4fad5",
                       xaxis = list(title = 'Date'), yaxis = list(title = 'Actions')))
        
        fig2
    })
    
    
    
    
})