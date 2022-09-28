#Installing useful packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load('data.table', 'DT', 'plotly', 'shiny', 'lubridate')

#Server

shinyServer(function(input, output) {
    
    #Gathering data in table
    dt <- fread(file='/Users/hugocaetano/Desktop/Mes_finances/mes_finances.csv') #classcol fonctionne si mal !
    
    #The lubridate package is pretty useful for date handling.
    dt[[2]] <- dt[,as.numeric(Montant)]
    dt[[3]] <- dt[, dmy(Date)]
    
    
    
    #Plot inputs
    
    #We must generate UI inputs in server because we need access to dt.
    output$categories <- renderUI({
        ctg <- dt[, unique(Categorie)]
        selectInput(
            "ctg",
            "Choose pertinent categories",
            c('All categories', ctg),
            selected = 'All categories',
            multiple = TRUE,
            selectize = TRUE,
            width = NULL,
            size = NULL
        )
    })
    
    
    
    output$dates <- renderUI({
        dateRangeInput('Dates',  
                       label = 'Choose your date range',
                       start = dt[, min(Date)],
                       end = dt[, max(Date)],
                       min = dt[, min(Date)],
                       max = dt[, max(Date)]
        )
    })
    
    
    output$montant <- renderUI({
        mtnmax <- dt[,max(Montant)]
        sliderInput("montant",
                    "Select a pertinent money interval",
                    min = 0,
                    max = mtnmax,
                    value = c(0,mtnmax))
        
    })
    
    #Plot outputs
    #Now that we have defined all useful inputs, let's output some nice plots !
    
    #Global histogram: print only with parameters
    output$rldl <- plotly::renderPlotly({
        
    input$go  #Creating a dependency. When one clicks the button, rldl will rerun its code because the value of 
    #input$go would have changed. 
    #Now, we need to isolate other variables reacting to inputs.
    
    #Treating the 'all options' cases
    acti <- isolate(if( 'All categories' %in% req(input$act) ) { #beginning isolation
        dt[, unique(categories)]
    } else {
        input$act
    })
    
    isolate(if (input$timu == 'Day') { #Here, we could think of attributing a number of days corresponding to the time aggregation
        dt <- isolate(dt[Categorie %in% categories & Date %between% input$date & Montant %between% input$montant, #unit and then group by, but I think it would take longer
                           .(sum = sum(Montant)), by = Date])
        
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
            
            layout(title = paste('My financial activity between', paste(input$dates, sep='and'), '(', paste(input$categories, collapse=' - ', sep=' '), ')', sep=' '), #adapting the title to the content
                   plot_bgcolor = "#d4fad5",
                   xaxis = list(title = 'Date'), yaxis = list(title = 'Spent money')))
    
    fig2
    
    })
    #Camembert
    
    
    #Histogram with all categories differentiated by colors
    
    
    #Table
    
    output$tabl <- DT::renderDataTable(
        dt
    )
    
    
    #Global financial activity:
    
    #Hist
    
    globex <- dt[DoG=="D", .(Expenses=sum(Montant)), by=Date] #expenses per day
    
    output$glob <- renderPlotly( {
        
        plot_ly(globex, type='bar', marker=list(color='green'),
                x=~Date, y=~Expenses
                )
    })
    
    #Camembert
    
})

#TODOLIST

#Afficher juste des summaries comme dans Wooshiny.