library(leaflet)
library(shiny)
library(shinythemes)
library(DT)
library(plyr)
library(RColorBrewer)
library(Cairo)
library(dplyr)
library(shinyBS)
library(KernSmooth)
library(MASS)
library(raster)
library(htmltools)
library(rgbif)

## GOALS 3 panel page that uses side bar format to visualize data in a variety of ways
# Phenology
## denisty plots of nesting occurrences by month
# Summary plots
## box plots/violin plots and bar graphs showing counts
# Map
## Points subset by various terms 

# Import data
tbn34 <- read.csv(file = "./BeesNeedsdata.csv", strip.white = T)
str(tbn34)

#convert to time format
tbn34$sys.time <- as.POSIXct(tbn34$sys.time, tz = "MST")
tbn34$month<- as.numeric(substring(tbn34$sys.time,first = 6,7))

taxa.names<- list('Anthidium', 
                  'Ashmeadiella',
                  'Auplopus', 
                  "Dianthidium",
                  'Dipogon',
                  'Eumenid', 
                  'Formicids', 
                  'Heriades', 
                  'Hoplitis', 
                  'Hylaeus', 
                  "Isodontia", 
                  'Megachile: chewed leaves', 
                  'Megachile: whole leaves', 
                  'Megachile: petal pieces', 
                  'Megachile: resin', 
                  'Megachilidae', 
                  'Osmia: mud', 
                  'Osmia: chewed leaves', 
                  'Pemphredine', 
                  'Pompilid', 
                  'Solierella', 
                  'Trypoxylon') 

taxa.choices <- levels(tbn34$Va.General)
taxa.choices <- taxa.choices[-1] # drop blanks
names(taxa.choices)  <- taxa.names # created a named list
all <- c("All" = '1') # add an all option
taxa.choices <- c(taxa.choices, all) # named list for selectize input

city.choices <- levels(tbn34$City)
city.choices <- city.choices[-1] # drop blanks
trim.trailing <- function (x) sub("\\s+$", "", x) # function to trim trailing spaces
city.choices <- trim.trailing(x = city.choices) # trim spaces
city.choices <- paste(unique(city.choices)," Colorado",sep = ",")
names(city.choices)  <- city.choices # created a named list
all <- c("All" = '1')
city.choices <- c(city.choices, all) # named list for selectize input

match("Block.number",names(tbn34))
block.numbers <- (unique(as.character(tbn34[,23])))
names(block.numbers) <- (block.numbers)

block.numbers <- sort(block.numbers)


#User Interface
ui3 <- fluidPage( theme = shinytheme('spacelab'),
                  fluidRow( 
                    h1("Bees Needs Interactive Data", align = "center"),
                    h5(a(href = "http://collinschwantes.github.io/", "By Collin Schwantes"), align = "center"),
                    #use www folder to make files accessible ~ for images, css, etc 
                    p("Use this interactive data page to explore trends in cavity nesting bee data.
                      Have fun playing around with the data and let me know if there is anything
                      else you would like to be able to do!", style = "margin-left:15px; margin-right:15px; text-align:center;")
                    ),
                  fluidRow(style = "margin-left:15px;",
                           column(12,
                                  #tabs for differnt plots 
                                  tabsetPanel(
                                  tabPanel("Map",
                                           column(3,
                                                  #City
                                                  selectizeInput(inputId = 'cityM',
                                                                 label = 'City',
                                                                 select = city.choices[sample(1:length(city.choices),1)],
                                                                 multiple = T,
                                                                 choices = city.choices,
                                                                 options = list(maxItems = 1, create = F)),
                                                    #set maptype
                                                   radioButtons(inputId = "mtype", label = h5("Map Type"),
                                                                choices = list("Satellite" = "Esri.WorldImagery", "Toner" = "Stamen.Toner",
                                                                 "HikeBike" = "HikeBike.HikeBike"), selected = "Esri.WorldImagery"),
                                                  #contour colors
                                                  uiOutput("ConCol"),
                                                  #taxa for the map
                                                  selectizeInput(inputId = "taxaM",
                                                                 label = "Insect type",
                                                                 choices = taxa.choices,
                                                                 select = 1,
                                                                 multiple = T,
                                                                 options = list(maxItems = 4)),
                                                  #year
                                                  selectizeInput(inputId = "yearM",
                                                                 label = "Year",
                                                                 select = 1,
                                                                 choices = c(all,list( "2013" = 2013, "2014" = 2014))),
                                                  
                                                  #Date 
                                                  sliderInput(inputId = "monthsM",
                                                              label = "Month Range",
                                                              min = 1,
                                                              max = 12,
                                                              value = c(1,12))
                                           
                                                  )
                                                  ,
                                          column(9, 
                                                leafletOutput(outputId = "Map", height = "500px")
                                                 )
                                            )
                                        )
                                    )
                                  )
                  )
                       


server3 <- function(input, output){
  
  ###### MAP  
  #location for map center   
  
   map.center <- reactive({
    as.matrix(geocode(input$cityM, output = "latlon"))
    })
   
   print("MAP CENTER")
  
   #mtype <- reactive({as.character(input$mytpe)})

   
   #add geojson
   
   dataM <- reactive({ 
     data <- tbn34[!is.na(tbn34$Trusted.Latitude),c(6,10:11,15,21,23,30,33,51,53,56)]
     data <- data[data$month >= input$monthsM[[1]] & data$month <= input$monthsM[[2]],]
     if(input$yearM != 1) {data <- data[data$Year == input$yearM,]} else {data}
     if(input$taxaM != 1) {data <- data[data$Va.General %in% input$taxaM,]} else {data}
     #View(data)
     # print((data))
     # message("****Data str****")
     # str(data)
     # print(5400)
   })
     
  
   
   
  Points <- reactive({ 
    
    str(dataM())
    
    ForGeoJsonSP <- na.omit(dataM())
    
    #message("strForgeoJsonSP")
    #str(ForGeoJsonSP)
    
    #give it coords
    coordinates(ForGeoJsonSP) <- ~Trusted.Longitude+Trusted.Latitude
    #give it projection
    proj4string(ForGeoJsonSP) <- "+init=epsg:4326 +proj=longlat +ellps=WGS84"
    
    Content <- paste(sep = "<br/>",
                     paste("<b>","Block Number ", ForGeoJsonSP@data$Block.number,"</b>",sep = ""),
                     paste("Plug Type:", ForGeoJsonSP@data$Va.Plug,sep = " "),
                     paste("Genus:",ForGeoJsonSP@data$Va.General,sep = " "),
                     paste("Date: ",ForGeoJsonSP@data$month,"/",ForGeoJsonSP@data$Year,sep = "")
    )
    print(head(Content))
    
    ForGeoJsonSP@data$Content <- Content 
    message("strForgeoJsonSP post processing")
    str(ForGeoJsonSP)
    ForGeoJsonSP
   })
   
   
  # contour lines
  Polys <- reactive({
    
   ForGeoJson  <- na.omit(dataM())
   #get kernel density bandwidth
   bwy <- bandwidth.nrd(ForGeoJson[,2])
   bwx <- bandwidth.nrd(ForGeoJson[,3])

    #ForGeoJson <-ForGeoJson[ForGeoJson$Va.General == "OSMIM",]
    #table(ForGeoJson$Va.General)

    DF_contour <- ForGeoJson[ForGeoJson$Trusted.Longitude > -105.4 & ForGeoJson$Trusted.Longitude < -105 & ForGeoJson$Trusted.Latitude > 39.9 & ForGeoJson$Trusted.Longitude < 40.1, 3:2]
    #make contours
    kde <- bkde2D(x = DF_contour,
                 bandwidth=c(bwy/10, bwx/10), gridsize = c(1000,1000))


    CL <- contourLines(kde$x1, kde$x2, kde$fhat)


    ## EXTRACT CONTOUR LINE LEVELS
    LEVS <- as.factor(sapply(CL, `[[`, "level"))
    NLEV <- length(levels(LEVS))

    ## CONVERT CONTOUR LINES TO POLYGONS
    pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i)
     )
    spgons <- SpatialPolygons(pgons, proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") )

    spgonsdf <- SpatialPolygonsDataFrame(Sr = spgons,data = data.frame("Density" = vapply(1:length(CL), function(x) CL[[x]]$level, FUN.VALUE = c(1))))

   })

   output$ConCol <- renderUI({
      PALS <- rev(rownames(brewer.pal.info))[1:18]
      PALS <- PALS[c(7,5,14,18,13)]
      PALSel <- setNames(as.list(PALS),PALS)
      radioButtons(inputId = "ConCols",label = "Contour Color",choices = PALSel, selected = "Blues")
      })


    ConCol <- reactive({
     # message("SpPolygon Structure")
     # str(Polys())
      polys <- Polys()
      NLEV <- length(levels(as.factor(polys$density)))
      print(c("NUMBER OF LEVLES",NLEV))
      pal <- colorNumeric(palette = input$ConCols ,domain =  0:NLEV)
      ConCol <- pal(NLEV)
      })

     
  
     
    # ,
    # clusterOptions = markerClusterOptions( showCoverageOnHover = TRUE, spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = F)
     
  output$Map <- renderLeaflet({
    
     Points <- Points()
    
    mc <- map.center()
    leaflet() %>% 
      setView(lng = mc[1], lat = mc[2], zoom = 10) %>% 
      addProviderTiles(input$mtype) %>%
      addPolygons(data = Polys(), color = ConCol(), stroke = 0,group = "Contours") %>%
      #addLegend(position = "bottomright", pal = ConCol(), values = 0:100) %>%
      addCircleMarkers(data = Points, stroke = F, group = "Blocks",
                       popup =Points@data$Content, 
                       clusterOptions = markerClusterOptions( showCoverageOnHover = TRUE, spiderfyOnMaxZoom = TRUE, removeOutsideVisibleBounds = F))  %>%
      addLayersControl(overlayGroups = c("Contours","Blocks"))
      
  })
  
  
}


shinyApp(ui = ui3,server = server3)



