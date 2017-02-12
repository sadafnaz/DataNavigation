suppressMessages (library(shiny))
suppressMessages (library(ggplot2))
suppressMessages (library(scales))
suppressMessages (library(DT))
suppressMessages (library(quantreg))
suppressMessages (library(Hmisc))
suppressMessages (library(quantreg))
library(tidyr)
library(shiny)
library(DT)
library(RMySQL)
library(shinydashboard)
library(dplyr)
library(DBI)
library(ggplot2)
library(plotly)

rm(list=ls(all=TRUE))
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

stat_sum_df <- function(fun, geom="point", ...) {
  stat_summary(fun.data=fun,  geom=geom,  ...)
}
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun,  geom=geom,  ...)
}
options(shiny.maxRequestSize=100*1024^2)



#global option if using table option in many tabs, etc

conn <- dbConnect(drv = RMySQL::MySQL(),
                  dbname = "newschema",host = "localhost",username = "root",password = "1234")
 
rs <- dbSendQuery(conn = conn, 'select id,species,plant_population,project_descriptor,trial_year,country,descriptor_name,score_value from abc')
Data <-fetch(rs, n=-1)
rs1<-dbSendQuery(conn = conn, 'select * from abc')
abc <-fetch(rs1, n=-1)

ui<- bootstrapPage(
  # to align data in center
  headerPanel(list(tags$head(tags$style(type = "text/css",HTML("th { text-align: center; color: #000000;  }"))))),
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Data Distribution", 
                 br(), h5('Filter data set and visualize distribution of the score values of filtered data', 
                          '\n' ,br(),br(), downloadButton('downloadData', 'Download Filtered Data'))
        ), # tabpanel
        
        tabPanel("How To",
                 h5("1. Filter data table: Navigate data using column filters of each variable."),br(),
                 h5("2. Visual displays: Population distributions of filtered data are displayed. Click on the plot and get information in the box below."), br(),                          
                 h5("3. Copy/Save visual displays: Click on the plot and choose copy or save image."),br(),
                 h5("4. Print filtered data and visual displays: Click on the table, choose 'Open Frame' where a new window will open with print option."),br(), 
                 h5("5. Download filtered data: Open App in a browser to download filtered data by clicking on download button")

        )# tabpanel 
      ) # tabsetPanel #, width=2
      ), #sidebarPanel
    
    mainPanel( 
      
      fluidRow(
        column(10, plotOutput('plot', height = 350)), #verbatimTextOutput("event")),
        column (10, dataTableOutput('tbl')) 
        
        
        
      )

)#mainPanel
)#sidebarLayout
)#fluidPage

server <-function(input, output) {    
  
  #options(DT.options = list(pageLength = 5)) 
  
  output$tbl = renderDataTable({
    
    # a custom table container
    sketch = htmltools::withTags(table(
      
      class = 'display',
      thead(
        tr(
          #th(rowspan = 1, ' '),
          th(colspan = 6, 'Design Factors'),
          th(colspan = 2, 'Variate')
          , backgroundcolor=c('red', 'blue') , fill=FALSE
        )#, tableHeader(Data)
        #,tr(lapply(rep(c('Length', 'Width'), 2), th))
        
        ,tr(lapply(c('id','species','plant_population','project_descriptor','trial_year','country','descriptor_name','score_value'), th))
        
      )
    ))
    print(sketch)
    datatable(Data,
              
              #get rid of auto generated id col
              rownames = FALSE,
              # to adjust col width of table
              #autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = c(1, 3))),
              container = sketch,
              #configure the table to automatically fill it's containing element.otherwise scrolling option
              #fillContainer = getOption("DT.fillContainer",TRUE), autoHideNavigation = getOption("DT.autoHideNavigation", FALSE),
              #An id for the widget (a random string by default).
              elementId = TRUE, 
              #Table lines
              #class = 'cell-border stripe', 
              class= 'compact cell-border',
              callback= JS("return table;"),  
              
              options = list( # to adjust col width of table
                autoWidth = TRUE, columnDefs = list(list(width = '250px', targets =  "_all", className = 'dt-center')),
                
                initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#D3D3D3', 'color': '#fff', 'font-color': '#000000'});","}"),
                #I("function(settings, json){ new $.fn.dataTable.FixedHeader(this, {left:   true,right:  true} );}"),
                pageLength = 10,server = FALSE, searchHighlight = TRUE ,search = list(search = ''), align='middle'
                #, colnames(Data) = c('<span style="color:red">ID</span>', '<em>population</em>')
              ), 
              #Filter each column  
              #filter=list('top',clear = FALSE, plain = TRUE)
              filter='top'
    )%>%
      formatStyle('descriptor_name',  headercolor = 'red', backgroundColor = 'LightGray', fontWeight = 'bold')
    #formatStyle('score_value',  headercolor = 'red', backgroundColor = 'LightGray', fontWeight = 'bold')
    #formatStyle('Design Factors',  headercolor = 'red', backgroundColor = 'orange', fontWeight = 'bold')
  })
  
  
     output$plot = renderPlot({
    filtered_data <- input$tbl_rows_all
    #plot(density(Data[filtered_data, "score_value"]), main= "Probability Density Plot")
    qplot((Data[filtered_data, "score_value"]),geom="histogram")+ geom_histogram(aes(fill = ..count..))
    
    #geom_histogram(aes(fill = ..count..)),
    #stat_bin: binwidth defaulted to range/30
  })

  
  output$downloadData = downloadHandler(filename = function() { paste(input$downloadData, "FilteredData",Sys.Date(),".csv",sep="") },
                                        'FilteredData.csv', content = function(file) {
                                          #join_string <- "select city.* from city inner join tbl on city.ID = filtered_data.ID"
                                          s = input$tbl_rows_all
                                          #total <- merge.data.frame(city,Data[s, , drop = FALSE],by="ID")
                                          total <- merge.data.frame(abc,Data[s, , drop = FALSE],by = intersect(names(abc), names(Data[s, , drop = FALSE])))
                                          write.csv(total, file)
                                          #write.csv(Data[s, , drop = FALSE], file)
                                        })

}

shinyApp(ui, server)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
