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
                  'Osmia chewed leaves', 
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
city.choices<- trim.trailing(x = city.choices) # trim spaces
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
                                                                 "HikeBike" = "HikeBike.HikeBike"), selected = "Esri.WorldImagery"))
                                                  ,
                                          column(9,
                                                leafletOutput(outputId = "Map", width = "1000", height = "750")
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
  #mtype <- reactive({as.character(input$mytpe)})

   
   #add geojson
   ForGeoJson <- tbn34[!is.na(tbn34$Trusted.Latitude),]
   
   names(ForGeoJson)
   
   ForGeoJson <- ForGeoJson[,c(6,10:11,15,21,23,30,33,51,53,56)]
   
   str(ForGeoJson)
   
   ForGeoJsonSP <- ForGeoJson
   
   #give it coords
   coordinates(ForGeoJsonSP) <- ~Trusted.Longitude+Trusted.Latitude
   class(ForGeoJsonSP)
   #give it projection
   proj4string(ForGeoJsonSP) <- "+proj=longlat +ellps=WGS84"
   proj4string(ForGeoJsonSP)
   
   #make it geojson
   test_geojson <- geojson_json(ForGeoJsonSP, lat = Trusted.Latitude, lon = Trusted.Longitude, pretty = T)
   
   
   #get kernel density bandwidth 
   bwy <- bandwidth.nrd(ForGeoJson[,2])
   bwx <- bandwidth.nrd(ForGeoJson[,3])
   
  #ForGeoJson <-ForGeoJson[ForGeoJson$Va.General == "OSMIM",] 
    table(ForGeoJson$Va.General)
  
  DF_contour  <- ForGeoJson[ForGeoJson$Trusted.Longitude > -105.4 & ForGeoJson$Trusted.Longitude < -105 & ForGeoJson$Trusted.Latitude > 39.9 & ForGeoJson$Trusted.Longitude < 40.1 ,2:3]
   #make contours
   kde <- bkde2D(x = DF_contour, 
                 bandwidth=c(bwy/10, bwx/10), gridsize = c(1000,1000))
   
   
   test <- raster(list(x= kde$x1,y = kde$x2,z = kde$fhat))
   
   plot(test)
   (CL)
    contour(kde$x1,kde$x2,kde$fhat)
   
   CL <- contourLines(kde$x2 , kde$x1 , kde$fhat)
   
  
   ## EXTRACT CONTOUR LINE LEVELS
   LEVS <- as.factor(sapply(CL, `[[`, "level"))
   NLEV <- length(levels(LEVS))
   
   ## CONVERT CONTOUR LINES TO POLYGONS
   pgons <- lapply(1:length(CL), function(i)
     Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
   spgons <- SpatialPolygons(pgons,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") )
   #spgons_df <- SpatialPolygonsDataFrame(Sr = spgons, data = data.frame(color = c(heat.colors(NLEV,NULL)[LEVS])))
   #poly_geojson <- geojson_json(input = spgons_df, geometry = "FeatureCollection",pretty = T)
   
   rev(rownames(brewer.pal.info))[1:18]
   PAL <- brewer.pal(9,name = input$Color)
   colfunc <- colorRampPalette(c(PAL[1],PAL[length(PAL)]))
   colfunc(NLEV)[LEVS]
   
  output$Map <- renderLeaflet({
    mc <- map.center()
    leaflet() %>% 
      addPolygons(data = spgons, color = colfunc(NLEV)[LEVS],stroke = 0) %>%
      setView(lng = mc[1], lat = mc[2], zoom = 10) %>% 
      addProviderTiles(input$mtype)# %>%
     # addGeoJSON(geojson = test_geojson) 
      
  })
  
  
}


shinyApp(ui = ui3,server = server3)


