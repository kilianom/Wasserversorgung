library(shiny)
library(mapedit)
library(leaflet)
library(sf)
library(shinydashboard)
library(bslib)
library(shinyStore)


#####timeout#######
timeoutSeconds <- 60*10

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)


#install chromium
#install webshot2
# install.packages("devtools")
# install_github("trestletech/shinyStore")
# devtools::install_github("trestletech/shinyStore")
theme_a<-bs_theme(
   version = 3,
  bootswatch = "readable")

sf_use_s2(T)

ui <- fluidPage(theme = theme_a,tags$head(tags$style('
   body {
      font-family: Arial}')),
                
  tags$script(inactivity),    #timeout
  shinyjs::useShinyjs(),
  initStore("store","store1"),
    titlePanel(
    fluidRow(
      column(width = 5,"Wasserversorgung Weide"),
     
      column(width= 1,offset = 6,actionButton("help1","Information", onclick ="window.open(' helper/Manual_Wasserversorgung.html','_blank')",icon = icon("question"))),
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
 

  mainPanel(width = 12,
      tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
  
    column(width = 9,
       wellPanel(   id="box1", 
            fluidRow(
              column(width=11,
              h4("Eingabe Tränken und Parzellen")),
              column(width=1,
             div(     style="float:right",   
             actionButton("help","",icon = icon("circle-question","fa-1x"))
             ),
            )),
           
    editModUI("map_a",height = "78vh"),
    
  ),


wellPanel(id="box2",
  h4("Bereiche Wasserversorgung"),
   leafletOutput("map",width = "100%",height = "78vh"),
 

  )),
column(width = 3,
div(img(src="Tbutton40.png",width="60%" ),style="float: center"),
br(),
br(),
actionButton(label = "Wasserversorgung zeigen",inputId="buffer",
             style="background-color: #69b62d;border-color: #69b62d;font-weight: bold"),
br(),
br(),
actionButton("save", "Im Browser speichern", icon("save")),
actionButton("clear", "Browserspeicher entfernen", icon("trash")),
br(),
downloadButton("downloadData", "Karte als Bild herunterladen"),
br()

)
),
fluidRow(column(width = 12,offset = 11 ,tags$a("Datenschutzerklärung", href="datenschutz.html",target="_blank",style = "font-size: 80%;color: #000000 ;margin-top=0px;"))),        

)
###########################################server#################################################
server <- function(input, output,session) {
 observe({
   showModal(modalDialog(size = 'l',
                         title = "Anleitung",
                         fluidRow(
                           includeHTML("www/help_wasserversorgung.html")),
                         footer = tagList(modalButton("Verstanden"))
   ))
 })

  
observeEvent(input$timeOut, { 
    showModal(modalDialog(
      title = "Timeout",
      paste("Die Anwendung wurde durch längere Inaktivität beendet."),
      footer = NULL
    ))
    session$close()
  })
  
  observeEvent(input$help,{
  showModal(modalDialog(size = 'l',
                        title = "Anleitung",
                        fluidRow(
                          includeHTML("www/help_wasserversorgung.html")),
                          footer = tagList(modalButton("Verstanden"))
                        ))
})
  
  rv<-reactiveValues(p=NULL,t=NULL)
  observe({
    shinyjs::hide("box2")
    shinyjs::hide("downloadData")
    shinyjs::hide("save")
    shinyjs::hide("clear")
  })
  shinyhelper::observe_helpers(help_dir = "helper")
observeEvent(input$save,{
  if(!is.null(map_p())){
  updateStore(session,name = "map_p",map_p())
  } else {
    updateStore(session, name = "map_p", NULL)
  }
  if(!is.null(map_t())){
    updateStore(session,name = "map_t",map_t())
  } else {
    updateStore(session, name = "map_t", NULL)
        }

})

observeEvent(input$clear, {
  updateStore(session, name = "map_p", NULL)
  updateStore(session, name = "map_t", NULL)
  
})



map_p<-reactive({
  if(!is.null(rv$p)){
  st_as_text(st_as_sfc(rv$p))
  }else{
    NULL
    }
  })

map_t<-reactive({
  if(!is.null(rv$t)){
  st_as_text(st_as_sfc(rv$t))
  }else{
    NULL
  }
})


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
   
map<-if(!is.character(isolate(input$store$map_p))&!is.character(isolate(input$store$map_t))){#any(isolate(is.null(input$store$map)))
       map
    }else if(is.character(isolate(input$store$map_p))&!is.character(isolate(input$store$map_t))) {
       p<-st_set_crs(st_as_sfc(isolate(input$store$map_p)),4326)
       map%>%
         addPolygons(data=p,group = "editable")
    }else if(!is.character(isolate(input$store$map_p))&is.character(isolate(input$store$map_t))) {
      t<-st_set_crs(st_as_sfc(isolate(input$store$map_t)),4326)
      map%>%
        addMarkers(data=t,group = "editable")
    
    }else{
      print(isolate(input$store$map_t))
      t<-st_set_crs(st_as_sfc(isolate(input$store$map_t)),4326)
      p<-st_set_crs(st_as_sfc(isolate(input$store$map_p)),4326)
      map%>%
        addPolygons(data=p,group = "editable")%>%
        addMarkers(data=t,group = "editable")

    }
       
   

edits<-callModule(editMod,leafmap=map,id="map_a",
                  editorOptions = list(polylineOptions=F,circleMarkerOptions=F,circleOptions=F,rectangleOptions=F),
                  editor = "leaflet.extras",
                  targetLayerId = "editable")

observeEvent(input$buffer,{
  
  if(any(is.null(edits()$all))){
    shinyalert::shinyalert("Fehlende Eingabe","Bitte existierende Eingabe verändern oder neue Eingabe hinzufügen")
  }  else if(input$buffer[1]%%2==1){
    shinyjs::hide("box1")
    shinyjs::show("box2")
    shinyjs::show("save")
    shinyjs::show("clear")

    updateActionButton(session,"buffer",label = "Bearbeiten")
    shinyjs::show("downloadData")

  } else {
    shinyjs::show("box1")
    shinyjs::hide("box2")
    updateActionButton(session,"buffer",label = "Wasserabdeckung zeigen")
    shinyjs::hide("downloadData")
    shinyjs::hide("save")
    shinyjs::hide("clear")
  }
  
  geom_r<-edits()$all
  req(geom_r)
  geom<-geom_r[(st_is_valid(geom_r))==T,]
  geom_c<-geom_r[(st_is_valid(geom_r))==F,]
  p<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POLYGON"),])
  t<-st_as_sf(geom[which(as.character(st_geometry_type(geom))=="POINT"),])
  
  rv$p<-p
  rv$t<-t

  if(nrow(geom_c)!=0){
    showNotification(paste0(nrow(geom_c),"  Element/Elemente konnte nicht verwendet werden. \n Kreuzen sich die Grenzen?"))}
    req(t)
    b<-st_as_sf((do.call(rbind,lapply(1:nrow(t),function(x){
      st_intersection(st_buffer(t[x,],150),p[unlist(st_intersects(t[x,],p)),])
    }))))

#sf_use_s2(F)
b<-st_transform(b,3857)
p<-st_transform(p,3857)
t<-st_transform(t,3857)

  
  if(input$buffer[1]%%2==1){
    w_df<-reactive({data.frame(
        w=st_area(st_union(b))/st_area(st_union(p))*100,
        w_pn=sum(unlist((lapply(st_contains(p,t),sum,na.rm=T)))>0),
        pn=nrow(p),
        t=nrow(t)
        )
      })


    shinyalert::shinyalert(
      title = "Wasserversorgung",
      text =   if(w_df()$w_pn==w_df()$pn){
      paste0(round(w_df()$w,0),"% der Gesamtweidefläche sind mit Wasser versorgt.")}
 else{
        paste0("Achtung! ",sum(w_df()$pn,na.rm = T)-sum(w_df()$w_pn,na.rm = T)," Parzelle(n) ohne Tränke. ",round(w_df()$w,0),"% der Gesamtweidefläche sind mit Wasser versorgt.")
    },
    type = "info"
    )
  }

    
    
  req(b)
  b<-st_transform(b,4326)
  p<-st_transform(p,4326)
  t<-st_transform(t,4326)
  
  if(any(st_is_empty(b)==F)){
    
    bb<-c(st_bbox(p))
    names(bb)<-NULL
      map<-reactive({
        leaflet()%>%
        addTiles(group="Karte")%>%
        addProviderTiles("Esri.WorldImagery",group = "Satellit")%>%
        addLayersControl(baseGroups = c("Karte","Satellit"))%>%
                  clearShapes()%>%
                  clearMarkers()%>%
                  clearControls()%>%
                  fitBounds(lng1=bb[3],lat1=bb[4],lng2=bb[1],lat2=bb[2])%>%
                  addPolygons(data=p,color = "#c68c53")%>%
                  addPolygons(data=b,color = "#0000ff",opacity = 1,fill="#0000ff",fillOpacity = 1)%>%
                  addMarkers(data=t)%>%
                  addLegend(position = "bottomright",labels=c("Weidefläche","Wasserversorgung"),colors = c("#c68c53","#0000ff"),opacity = 1)
      })
      output$map<-renderLeaflet(map() )
  
      
  } else if(any(nrow(p)>=1 | nrow(t)>=1 )){
    
   
    
    map<-reactive({
      leaflet()%>%
        addTiles(group="Karte")%>%
        addProviderTiles("Esri.WorldImagery",group = "Satellit")%>%
        addLayersControl(baseGroups = c("Karte","Satellit"))%>%
        clearShapes()%>%
        clearMarkers()%>%
        clearControls()%>%
        addPolygons(data=p,color = "#c68c53")%>%
        addMarkers(data=t)%>%
        addLegend(position = "bottomright",labels=c("Weidefläche","Wasserversorgung"),colors = c("#c68c53","#0000ff"),opacity = 1)
    })
    output$map<-renderLeaflet(map() )
    
  } 
  
  output$downloadData<-downloadHandler(
    
    
    filename=function(){sub(" ","",paste0("Wasserversorgung",Sys.time(),".png"))},
    content = function(file)  {
      htmlwidgets::saveWidget(map(), "temp.html", selfcontained=TRUE)
      webshot2::webshot("temp.html", file=file, cliprect="viewport")
      
      }

    
  )
  
})


}

# Run the application 
shinyApp(ui = ui, server = server)
