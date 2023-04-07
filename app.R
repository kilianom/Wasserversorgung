library(shiny)
library(mapedit)
library(leaflet)
library(sf)
library(shinydashboard)
library(bslib)
library(shinyStorePlus)
#install chromium
#install webshot2
# install.packages("devtools")
# install_github("trestletech/shinyStore")
# devtools::install_github("trestletech/shinyStore")
theme_a<-bs_theme(
   version = 3,
  bootswatch = "readable")

ui <- fluidPage(theme = theme_a,tags$head(tags$style('
   body {
      font-family: Arial}')),
  shinyjs::useShinyjs(),
  initStore(),
  titlePanel(
    fluidRow(
      column(width = 2,"Wasserversorgung Weide"),
     
      column(width= 3,actionButton("help1","Information", onclick ="window.open(' helper/Manual_Wasserversorgung.html','_blank')",icon = icon("question"))),
      # column(width=12,offset = 11,
      #        tags$a(href="https://www.mud-tierschutz.de/mud-tierschutz/wissen-dialog-praxis/milchkuehe/weidehaltung-von-milchkuehen",target="blank",
      #        tags$style(".topimg {
      #                         margin-left:-100px;
      #                         margin-right:0px;
      #                         margin-top:-50px;
      #                       }"),
      #        div(class="topimg",img(src='BLElogo.png', align = "top",width="10%")),
      #        ))
    ),
    windowTitle = "Wasserversorgung" ),
 

  mainPanel(
      tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  
    column(width = 9,
       wellPanel(   id="box1", 
              shinyhelper::helper(h2("Eingabe Parzellen und Tränken"),type="markdown",content="help_trough",icon = "circle-question"),
              
    editModUI("map_a",height = "70vh"),
    
  ),


wellPanel(id="box2",
  h2("Bereiche Wasserversorgung"),
   leafletOutput("map",width = "100%",height = "70vh"),
 

  )),
column(width = 3,
actionButton(label = "Wasserversorgung erfassen",inputId="buffer",
             style="background-color: #69b62d;border-color: #69b62d;font-weight: bold"),
downloadLink("downloadData", "Karte als Bild speichern"),
actionButton("sPNG","Karte speichern"),
sliderInput("slider","slider",1,100,1)
)
)
)

server <- function(input, output,session) {
  observe({
    shinyjs::hide("box2")
    shinyjs::hide("sPNG")
  })
  shinyhelper::observe_helpers(help_dir = "helper")
  

   map<-leaflet()%>%
    addTiles(group="Karte")%>%
    addProviderTiles("Esri.WorldImagery",group = "Satellit")%>%
    setView(lng = 9.0589, lat = 51.3601, zoom = 5)%>%
    addLayersControl(baseGroups = c("Karte","Satellit"))%>%
    leaflet.extras::addSearchOSM(options = leaflet.extras::searchOptions(collapsed = TRUE, autoCollapse = TRUE))%>%
    addMeasure(primaryLengthUnit = "meters",
               primaryAreaUnit = "hectares",
               position = "topleft",
               activeColor = "red",
               completedColor = "red",
               localization = "de")

output$map<-renderLeaflet({
  leaflet() %>%
    setView(lng = 9.0589, lat = 51.3601, zoom = 5)%>%
    addTiles(group="Karte")%>%
    addProviderTiles("Esri.WorldImagery",group = "Satellit")%>%
    addLayersControl(baseGroups = c("Karte","Satellit"))
})
    
edits<-callModule(editMod,leafmap=map,id="map_a",
                  editorOptions = list(polylineOptions=F,circleMarkerOptions=F,circleOptions=F,rectangleOptions=F),
                  editor = "leaflet.extras")
                 
observeEvent(input$buffer,{
  if(input$buffer[1]%%2==1){
    shinyjs::hide("box1")
    shinyjs::show("box2")
    updateActionButton(session,"buffer",label = "Bearbeiten")
    shinyjs::show("sPNG")
    # output$sPNG<-renderUI({
    #   actionButton("savePNG","Karte speichern")

    #})
  } else {
    shinyjs::show("box1")
    shinyjs::hide("box2")
    updateActionButton(session,"buffer",label = "Wasserabdeckung zeigen")
    shinyjs::hide("sPNG")
  }
  geom_r<-edits()$finished
  
  req(geom_r)
  geom<-geom_r[(st_is_valid(geom_r))==T,]
  geom_c<-geom_r[(st_is_valid(geom_r))==F,]
 
  p<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POLYGON"),])
  t<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POINT"),])

    if(nrow(geom_c)!=0){
  showNotification(paste0(nrow(geom_c),"  Element/Elemente konnte nicht verwendet werden. \n Kreuzen sich die Grenzen?"))}
  req(t)
  b<-st_as_sf(st_union(do.call(rbind,lapply(1:nrow(t),function(x){
    st_intersection(st_buffer(t[x,],150),p[unlist(st_intersects(t[x,],p)),])
  }))))
 
 


  
  req(b)
  if(st_is_empty(b)==F){
    bb<-c(st_bbox(p))
    names(bb)<-NULL
  leafletProxy("map")%>%
            clearShapes()%>%
            clearMarkers()%>%
            clearControls()%>%
            fitBounds(lng1=bb[3],lat1=bb[4],lng2=bb[1],lat2=bb[2])%>%
            addPolygons(data=p,color = "#c68c53")%>%
            addPolygons(data=b,color = "#0000ff",opacity = 1,fill="#0000ff",fillOpacity = 1)%>%
            addMarkers(data=t)%>%
      addLegend(position = "bottomright",labels=c("Weidefläche","Wasserversorgung"),colors = c("#c68c53","#0000ff"),opacity = 1)
  
      
  }

})
observeEvent(input$sPNG,{
  
  geom_r<-edits()$finished
  
  req(geom_r)
  geom<-geom_r[(st_is_valid(geom_r))==T,]
  geom_c<-geom_r[(st_is_valid(geom_r))==F,]
  
  p<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POLYGON"),])
  t<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POINT"),])
  
  
  req(t)
  b<-st_as_sf(st_union(do.call(rbind,lapply(1:nrow(t),function(x){
    st_intersection(st_buffer(t[x,],150),p[unlist(st_intersects(t[x,],p)),])
  }))))
  
  
  
  
  
  req(b)
  if(st_is_empty(b)==F){
    bb<-c(st_bbox(p))
    names(bb)<-NULL
  
  
  map_save<-leaflet() %>%
    addTiles(group="Karte")%>%
    addProviderTiles("Esri.WorldImagery",group = "Satellit")%>%
    addLayersControl(baseGroups = c("Karte","Satellit"))%>%
    fitBounds(lng1=bb[3],lat1=bb[4],lng2=bb[1],lat2=bb[2])%>%
    addPolygons(data=p,color = "#c68c53")%>%
    addPolygons(data=b,color = "#0000ff",opacity = 1,fill="#0000ff",fillOpacity = 1)%>%
    addMarkers(data=t)%>%
    addLegend(position = "bottomright",labels=c("Weidefläche","Wasserversorgung"),colors = c("#c68c53","#0000ff"),opacity = 1)
  }
  
  
  htmlwidgets::saveWidget(map_save, "temp.html", selfcontained=TRUE)
  webshot2::webshot("temp.html", file="Rplot.png", cliprect="viewport")
  
})

}

# Run the application 
shinyApp(ui = ui, server = server)
