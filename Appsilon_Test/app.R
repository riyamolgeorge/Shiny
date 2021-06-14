### App Name            : ShipVoyage
### Description         : Top Distance travelled by the selected Vessel in a Stretch
###                         Created in accordance with Application Development Best Practices
### Author              : Riyamol George(riyaannageorge@gmail.com)
### Last Modified On    : 13-Jun-2021


##Packages in Use
#require(bit64)
library(readr)
library(shiny)
library(shiny.semantic)
library(dplyr)
library(leaflet)
library(magrittr)
library(geosphere)
library(data.table)
system.time(ships <<- ships <- read_csv("ships.csv", col_types = cols(date = col_skip(),
                                                                      week_nb = col_skip(), SHIPTYPE = col_skip(),
                                                                      port = col_skip())))


## File read for better performance
# system.time(ships <<- fread("ships.csv",select=c(1:9,11:15,18,20),skip=1,
#            col.names=c("LAT","LON","SPEED","COURSE","HEADING","DESTINATION","FLAG","LENGTH","SHIPNAME","SHIP_ID","WIDTH","DWT","DATETIME","PORT","ship_type","is_parked")))

## Filter only non parked observations
ships<- ships %>% filter(is_parked == 0) 

## sorting  ship type
SHIP_TYPE<<- sort(unique(ships$ship_type))

##only run in interactive sessions


## UI part
ui <- semanticPage(
  theme = "spacelab",
  header( 
    title = "Map View of Ship Voyages", 
    description = "Top Distance travelled by the selected Vessel in a Stretch",
    icon="compass"),
  div(style="display: inline-block;vertical-align:top;  width: 300px;",
      selectInput('Vessel',label="Vessel Type",
                  choices = SHIP_TYPE,
                  #default_text = "",
                  value = NULL,
                  multiple=FALSE
      )),
  div(style="display: inline-block;vertical-align:top;  width: 300px;",
      selectInput("vessel_name",
                  label = "Select a Vessel",
                  choices = "",
                  value = NULL,
                  multiple = FALSE)),
  div(style="display: inline-block;vertical-align:bottom;  width: 300px;",
      actionButton("action_button", "Take to Map!")),br(),br(),
  tags$style(type = "text/css", "#map {height: 75%}"),
  leafletOutput("map", width = "100%", height = "75%")
  
)

server <- shinyServer(function(input, output,session) {
  
  ## Reactive values store
  storage <- reactiveValues()
  ## Reactive chnage for the vessel name
  observeEvent(input$Vessel, {
    x<-sort(unique(ships[which(ships$ship_type==input$Vessel), ]$SHIPNAME))
    updateSelectInput(session,'vessel_name',choices = x ,
                      selected=NULL)
    
  })
  ##Click control on action button
  ntext <- eventReactive(input$action_button, {
    
  })
  ## reactive calculations on action button
  observeEvent(input$action_button,{
    #data filtered for the inputs
    data<- ships[which(ships$ship_type==input$Vessel & ships$SHIPNAME == input$vessel_name), ] 
    #data arranged on datetime
    data<- data %>% select(SHIPNAME,LAT,LON,DATETIME,DESTINATION,SPEED,FLAG) %>% arrange(SHIPNAME,desc(DATETIME))  
    #  calculating distance and duration of the stretch
    DISTDAT<- mutate(data,DURATION=round(difftime(DATETIME,lead(DATETIME))/60,digit=1),Distance = distHaversine(cbind(LON, LAT), cbind(lead(LON), lead(LAT))))
    DAT2<- mutate(DISTDAT,LEADLON=lead(LON),LEADLAT=lead(LAT))
    #finding the top record on distance most covered 
    DISTDAT<- DAT2 %>% arrange(desc(Distance,DATETIME)) %>% head(n=1)
    #reactive data for long,lat values to plot
    storage$df2 <- data.frame(group = c("A"),
                              lat = c(DISTDAT$LEADLAT, DISTDAT$LAT), long = c(DISTDAT$LEADLON, DISTDAT$LON))
    #labels created here
    start= paste('Start Position: (',storage$df2$long[1],",",storage$df2$lat[1],")",sep="" )
    end=  paste('End Position: (',storage$df2$long[2],",",storage$df2$lat[2],")",sep="" )
    #additional fields for Legend
    DISTDAT2<- DISTDAT%>% rename(`SPEED(KNOTS)`=SPEED,`DURATION(Mins)`=DURATION) %>% mutate(DATETIME=as.character(DISTDAT$DATETIME),
                                                                                            `DISTANCE(M)`=round(Distance,digits=2)) %>%
      select(SHIPNAME,`DISTANCE(M)`,`DURATION(Mins)`,`SPEED(KNOTS)`, DATETIME,DESTINATION,FLAG)
    #legend used as label
    label1= paste(colnames(DISTDAT2),"&#9; :<b>",DISTDAT2,"</b>",sep="")
    #color value created for Legend
    color=as.list(rep("#2A5BF7",7))
    #leaflet map
    storage$map= leaflet(data = storage$df2) %>%
      addTiles() %>% flyTo(DISTDAT$LON,DISTDAT$LAT, zoom= 14) %>%
      addPolylines( lng = ~long, lat = ~lat, group = ~group, popup= label1[2]) %>%
      addCircleMarkers(~long[1], ~lat[1], label = start,radius = 3,labelOptions=list(permanent=T),opacity=0) %>%
      addCircleMarkers(~long[2], ~lat[2], label = end,radius = 3,labelOptions=list(permanent=T),opacity=0) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization = "en" )   %>%
      addLegend(position="topright", colors=color, labels=label1)
    
    # })
  })
  
  output$map <- renderLeaflet({
    ntext()
    storage$map})
  
  
})

shinyApp(ui, server)
