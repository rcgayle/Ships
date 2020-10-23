library(shiny)
library(leaflet)
library(leaflet.extras)
library(Imap)
library(magrittr)
source('ships.r')
Ships<-readRDS('SHIPS.RData')
attach(Ships)
ship_type<-as.factor(ship_type)
ST<-levels(ship_type)


ui<-fluidPage(
   tags$style(type = "text/css",
             "h1 {color: blue; font-weight: bold; font-size: 18px;}",
             "body {background-color: gainsboro;}",
             "label {font-size: 18px; color: blue;
                   font-weight: bold;}"), 
   tags$head(tags$style(HTML(
            "#table {font-size: 18px;color: red; font-weight: bold;
                        background-color: white;}",
            "#text {font-size: 18px;color: red; font-weight: bold;
                        background-color: white;}"))),
                   
   navlistPanel(
   tabPanel(tags$h1('Select Vessel'),
       hr(),
       selectInput('typ', 'Vessel Type', choice=ST, selected=ST[1]),
       hr(),
       actionButton(inputId='go1', label='Save Type',
          style="color: blue; font-size: 18px; font-weight: bold; 
                   background-color: white;"),
        hr(),
        uiOutput("vesselName"),
        hr(),
        actionButton(inputId='go2', label='Save Name',
          style="color: blue; font-size: 18px; font-weight: bold; 
                   background-color: white;"),
        hr(),
        actionButton(inputId='go3', label='Reset',
          style="color: blue; font-size: 18px; font-weight: bold; 
                   background-color: white;"),
        hr()
            ),  
            
      
   tabPanel(tags$h1('Plot of Vessel Movement'),
       leafletOutput("map", height='550px', width='100%')
             ),          
   tabPanel(tags$h1('Position Data'),
        tags$h1('Position Data'),
        hr(),
        tableOutput('table'),
        hr(),
        verbatimTextOutput('text')
        )  
              )             
        
            )

        
server<-function(input, output, session){

v<-reactiveValues(S1=data.frame(), S2=data.frame(), S3=data.frame(),N=c(), I=0,
       DST=0)

observeEvent(input$go1,
                 {v$S1<-subset(Ships, ship_type==input$typ)
                 v$N<-levels(as.factor((v$S1)$SHIPNAME))}
                    )
                    
observeEvent(input$go2,
   if (length(v$S1)!=0)
     {v$S2<-subset(v$S1, (v$S1)$SHIPNAME==input$nm)
     v$I<-idobs(v$S2)
     v$S3<-v$S2[v$I:(v$I+1), c(1:2,20) ]
     v$DST<-round(gdist(v$S3[1,2], v$S3[1,1], v$S3[2,2], v$S3[2,1], units='m'))
                 }
   else{}
                    )
                    
observeEvent(input$go3,
             if (length(v$S2)!=0)
                 {v$S1<-data.frame()
                  v$S2<-data.frame()
                  v$S3<-data.frame()
                  v$I<-0
                  updateSelectInput(session,'typ', selected=ST[1])
                  updateSelectInput(session,'nm', selected=character(0))
                 }
              else {}
                    )

output$vesselName<- renderUI({
     selectInput('nm', 'Vessel Name', choice=v$N, selected='')
                             })
                             
output$table<-renderTable(
                      PosTable(v$S3)
                          )
                          
output$text<-renderPrint(
        {
         if (length(v$S3)!=0)
            {paste0('Distance between positions: ',v$DST,' m')}
          else {'Find your ship, matey!'}
          }
                        )

output$map <- renderLeaflet({
    if(length(v$S3)!=0){
      m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=c(v$S3[1,2],v$S3[2,2]), lat=c(v$S3[1,1],v$S3[2,1]), popup=c('Position 1', 'Position 2')) %>%
      addPolylines(lng=c(v$S3[1,2],v$S3[2,2]), lat=c(v$S3[1,1],v$S3[2,1])) %>%
      addCircleMarkers(lng=(v$S3[1,2]+v$S3[2,2])/2, lat=(v$S3[1,1]+v$S3[2,1])/2,
      label = paste0(v$DST,' m'), radius=2, col='red',
      labelOptions = labelOptions(noHide = T, textsize = "18px"))
                         }
    else{
      m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(-.001545, 51.477928, label=c('Find your ship, matey!'),
      labelOptions = labelOptions(noHide = T, textsize = "18px")
      )
        }
                            })
  
  }
                                
shinyApp(ui=ui, server=server)



