library(ggmap) # used for geocode
library(ggplot2)
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
#library(rgbif)

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
ui2 <- fluidPage( theme = shinytheme('spacelab'),
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
                            tabPanel(title = "Block",
                                     fluidRow(
                                        h5("Enter your block number to see how your block compares to 
                                       the whole bees needs data set. On the pie chart below, the inner graph summarizes the whole dataset, the outer summarizes a single block.", style = "margin-top:15px;"),
                                        selectizeInput(inputId = "block",
                                                       label = "Bee Block Number",
                                                       choices = block.numbers,
                                                       multiple = F,
                                                       options = list(create = T))
                                        ),
                                     fluidRow(
                                       column(4,  
                                              h3("Number of Nests"),
                                              h1(textOutput("plug_tot")),
                                              h4("Percentile: ", 
                                                 tipify(textOutput("tot_per"),"50th percentile is average")
                                                 )
                                       ),      
                                       column(4, 
                                              h3("Types of Nests"),
                                              h1(textOutput("plug_div")),
                                              h4("Percentile: ", 
                                                 tipify(textOutput("plug_per"),"Higher percentile is better"))
                                              
                                       ),
                                       column(4, 
                                              h3("Block Summary Statistics"),
                                              h5("Average number of nests: 12"),
                                              h5("Average number of types: 3"),
                                              h5("Total records used: 5597")
                                       )
                                     ),
                                       
                                     plotOutput(outputId = "piechart",width = "100%")
          
                                     ),
                            tabPanel("Phenology", 
                                     checkboxInput(inputId = "subset", 
                                                   label =  p(icon("eye"), "Data Panel"), value = TRUE), 
                                     conditionalPanel( 
                                       condition = "input.subset == true", 
                                       column(width = 4,
                                              
                                              #Taxa
                                              selectizeInput(inputId = "taxa",
                                                             label = "Insect type",
                                                             choices = taxa.choices,
                                                             select = 1,
                                                             multiple = T,
                                                             options = list(maxItems = 4)),
                                              #year
                                              selectizeInput(inputId = "year",
                                                             label = "Year",
                                                             select = 1,
                                                             choices = c(all,list( "2013" = 2013, "2014" = 2014))),
                                              #City
                                              selectizeInput(inputId = 'city',
                                                             label = 'City',
                                                             select = 1,
                                                             multiple = T,
                                                             choices = city.choices,
                                                             options = list(maxItems = 4, create = TRUE)),
                                              #Date 
                                              sliderInput(inputId = "months",
                                                          label = "Month Range",
                                                          min = 5,
                                                          max = 11,
                                                          value = c(5,11)),
                                              #Facet Wrap 
                                              checkboxGroupInput(inputId = "wrap", 
                                                                 label = "Split Plots by:", 
                                                                 #select = NULL,
                                                                 choices = list("City" =  "City",
                                                                                "Insect" = "Va.General",
                                                                                "Project" = "Project"), #add facet wrap
                                                                 inline = T)
                                              
                                             
                                       )
                                       
                                     ),
                                     column(8,plotOutput(outputId = "phenology", width = "100%", click = "plot_click")
                                            )
                                     ),
                            tabPanel("Summary",
                                     checkboxInput(inputId = "subsets", 
                                                             label =  p(icon("eye"), "Data Panel"), value = TRUE), 
                                     conditionalPanel( 
                                       condition = "input.subsets == true", 
                                       column(width = 4,
                                              #Taxa
                                              selectizeInput(inputId = "taxas",
                                                             label = "Insect type",
                                                             choices = taxa.choices,
                                                             select = 1,
                                                             multiple = T,
                                                             options = list(maxItems = 4)),
                                              #year
                                              selectizeInput(inputId = "years",
                                                             label = "Year",
                                                             select = 1,
                                                             choices = c(all,list( "2013" = 2013, "2014" = 2014))),
                                              #City
                                              selectizeInput(inputId = 'citys',
                                                             label = 'City',
                                                             select = 1,
                                                             multiple = T,
                                                             choices = city.choices,
                                                             options = list(maxItems = 4, create = TRUE)),
                                              #Date 
                                              sliderInput(inputId = "monthss",
                                                          label = "Month Range",
                                                          min = 5,
                                                          max = 11,
                                                          value = c(5,11)),
                                              #Facet Wrap 
                                              checkboxGroupInput(inputId = "wraps", 
                                                                 label = "Sort Plots by:", 
                                                                 #select = NULL,
                                                                 choices = list("City" =  "City",
                                                                                "Insect" = "Va.General",
                                                                                "Project" = "Project"), #add facet wrap
                                                                 inline = T)
                                              
                                       )
                                       ),
                                     column(8,
                                     plotOutput(outputId = "summary", width = "100%", click = "")
                                     )
                                     ),
                          tabPanel("Map",
                                    column(3,
                                           #City
                                           selectizeInput(inputId = 'cityM',
                                                          label = 'City',
                                                          select = "Boulder",
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
                                   
                                    column( 9, 
                                           tags$style(type = "text/css", "#Map {height: calc(100vh - 80px) !important;}"),
                                           leafletOutput(outputId = "Map")
                                    )
                          )

                            #tabPanel("Table",dataTableOutput(outputId = "table", width = "100%"))
                     )
                   )
                 )
               )


server2 <- function(input, output){
  options(shiny.usecairo=TRUE)
  # subset data for phenology
  #year
  #month
  #taxa 
  #city
  
  f <- function(y) c(label=round(mean(y), digits = 1), y=median(y))
  
  tbn34$Date_md <- format(tbn34$sys.time,format = "%m-%d")
  
  tbn34$Date_md <- as.Date(tbn34$Date_md,"%m-%d")
  
  tbn34$month <- as.numeric(format(tbn34$sys.time,format = "%m"))
    
  pheno.plot <- reactive({ 
    tbn.ph <- tbn34
    taxa.labels <- c("Unidentified", taxa.names)
    tbn.ph <- tbn.ph[tbn.ph$month >= input$months[[1]] & tbn.ph$mont <= input$months[[2]],]
    if(input$year != 1){tbn.ph <- tbn.ph[tbn.ph$Year == input$year,]}
    if(input$taxa != 1) {tbn.ph <- tbn.ph[tbn.ph$Va.General %in% input$taxa,]
    taxa.selected <- paste(input$taxa,collapse = "|")
    taxa.labels <- names(taxa.choices[grepl(pattern = taxa.selected,x =  taxa.choices)])
    print(taxa.selected)
    }
    if(input$city != 1) {tbn.ph <- tbn.ph[tbn.ph$City %in% input$city,]}
    
    pheno  <- ggplot(tbn.ph,aes(x = Date_md, y = ..count.., fill = Va.General)) +
      geom_density(alpha = 0.25) + 
      labs( x = "Date", y= "Count") +
      theme_bw() +
      theme( axis.text.x  = element_text(size=16), 
             axis.text.y = element_text(size=16),
             axis.title.x  = element_text(size=16), 
             axis.title.y = element_text(size=16)) +
      scale_fill_discrete(drop = T,
                          name = "Scientific Name",
                          labels = taxa.labels)
    
    
    if(is.null(input$wrap)){
    
      pheno
      
    } else {
      if(input$wrap == "City") {
        
        pheno +  facet_wrap( ~City, drop = T,nrow = if(length(input$city) == 4){2} else{NULL}) } else {
            if(input$wrap == "Va.General") { #TAXA
              pheno +
                facet_wrap( ~Va.General,drop = T,
                            nrow = if(length(input$taxa) == 4){2}else{NULL}) 
            }
          }
    }
  })
  
  output$phenology <- renderPlot({pheno.plot()}, height = 500)
  
  #### summary plots
  
  summary.plot <- reactive({
    
    sum.tbn34 <- ddply(tbn34, .variables = c("Block.number","Projects","City","Va.General","month","Year"),.fun = summarize,
                       Abundance = length(Va.Plug)
    ) 
    
    
    sum.tbn34 <- sum.tbn34[sum.tbn34$month == input$monthss[[1]]:input$monthss[[2]],]
    if(input$years != 1){sum.tbn34 <- sum.tbn34[sum.tbn34$Year == input$years,]}
    if(input$taxas != 1){sum.tbn34 <- sum.tbn34[sum.tbn34$Va.General %in% input$taxas,]}
    if(input$citys != 1){sum.tbn34 <- sum.tbn34[sum.tbn34$City %in% input$citys,]}
    
    
    
    if(is.null(input$wraps)){  
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = City))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "City", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$citys != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black")
      
    } else { if(input$wraps == 'City'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = City))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "City", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$citys != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black") 
      
    } else { if(input$wraps == 'Va.General'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = Va.General))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "Insect Type", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$taxas != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black")
    } else {if(input$wraps == 'Project'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = Projects))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "Project", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle= 0, size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black")
    }
    }
    } 
    }
  })
  
  output$summary <- renderPlot({summary.plot()}, height = 500)
  
  
  ###### MAP  
  #location for map center   
  
  map.center <- reactive({
    
    mc <- paste(input$cityM," Colorado")
    
    as.matrix(geocode(mc, output = "latlon"))
    
  })
  
  
  output$ConCol <- renderUI({
    PALS <- rev(rownames(brewer.pal.info))[1:18]
    PALS <- PALS[c(7,5,14,18,13)]
    PALSel <- setNames(as.list(PALS),PALS)
    radioButtons(inputId = "ConCols",label = "Contour Color",choices = PALSel, selected = "Blues")
  })
  
  print("MAP CENTER")
  
  #mtype <- reactive({as.character(input$mytpe)})
  
  
  #add geojson
  
  dataM <- reactive({ 
    #tbn1314 <- read.csv(file = "./BeesNeedsdata.csv", strip.white = T)
    message("Structure for Map data")
    str(tbn34)
    names(tbn34)
    col_names <-c("Year", "Trusted.Latitude","Trusted.Longitude","Substrate.CATEGORY", "Block.type",
       "Block.number", "Va.Plug", "Va.General", "sys.time","UTM.x","month")
    #convert to time format
    #tbn1314$sys.time <- as.POSIXct(tbn1314$sys.time, tz = "MST")
    #tbn1314$month<- as.numeric(substring(tbn1314$sys.time,first = 6,7))
    
    data <- tbn34[!is.na(tbn34$Trusted.Latitude),col_names]
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
    
    mc <- map.center() #keep map center from updating each time the a subsetting parameter changes
    
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
  
  
####### Pie Chart  
    
    tbnPIE <- tbn34
    
    #if(input$year != 1) {tbnPIE <- tbnPIE[tbnPIE$Year == input$year,]}
    
    tbnPIE$Block.number <-as.factor(as.character(tbnPIE$Block.number))
    str(tbnPIE$Block.number)
    
    tbn_pie <- ddply(tbnPIE,.variables = .(Block.number,Year, Va.Plug), summarize,
                     plug_tot = length(Va.Plug)
    )
    
    pie_sums <- tapply(tbn_pie$plug_tot,tbn_pie$Block.number,sum)
    
    pie_sums <- as.data.frame((pie_sums))
    
    str(pie_sums)
    
    Bl_pi <- row.names(pie_sums)
    pie_sums$Block.number <- Bl_pi
    
    pie_data<- merge(tbn_pie,pie_sums, by = "Block.number")
    
    pie_data$pie_sums <- unname(pie_data$`(pie_sums)`)
    
    pie_data$per <- pie_data$plug_tot/pie_data$pie_sums
    
    
    pie_data <- pie_data[,-5]
  
    
    pie_cumsum <- tapply(pie_data$per,pie_data$Block.number, cumsum)
    
    ### get into format with block number and cumsum of length(pie_data)
    
    list2df <- function (x) {
      df <- do.call(rbind, lapply(x, data.frame))
      #  names(df)[1] <- col_name
    }
    
    pie_cumsum_df <- list2df(x = pie_cumsum)
    
    pie_data$ymax_l <-  pie_cumsum_df[[1]]
    pie_data$ymin_l <- c(0,pie_data$ymax_l[1:(length(pie_data$ymax_l)-1)])
    
    
    plug_color <- brewer.pal(12,"Paired")
    
    plug_color <- c(plug_color, "#000000", "#969696")
    
    pie_data[pie_data$ymin_l == 1,8] <- 0
  
    #re think how to get total block attributes 
 
    grand_plugs <- sum(pie_data$plug_tot)
    
    pie_data$grand_plugs <- rep(grand_plugs,length(pie_data$Block.number))
    
    pie_data$grand_per <- pie_data$plug_tot/pie_data$grand_plugs
    
    pie_data <- arrange(pie_data,Va.Plug, grand_per)
    
    pie_data$ymax_T <- cumsum(pie_data$grand_per)
    
    pie_data$ymin_T <- c(0,pie_data$ymax_T[1:(length(pie_data$ymax_T)-1)])
    
    grand_pie_data <- summarize(group_by(pie_data,Va.Plug), grand_per = sum(grand_per))
    
    grand_pie_data$ymax <- cumsum(grand_pie_data$grand_per)
    
    grand_pie_data$ymin <- c(0,grand_pie_data$ymax[1:(length(grand_pie_data$ymax)-1)])
    
    piechart <- reactive({  
    
    Pie_Chart <- ggplot(pie_data[pie_data$Block.number == input$block,]) + 
      geom_rect(aes(fill=Va.Plug, ymax=ymax_l, ymin=ymin_l, xmax=12, xmin=7)) +
      geom_rect(data = grand_pie_data, aes(fill=Va.Plug, ymax=ymax, ymin=ymin, xmax=6, xmin = 0)) +
      xlim(c(0, 13)) + 
      scale_fill_manual(values = plug_color, name = "Plug Type") +
      geom_text(aes(label = Va.Plug, y = (ymax_l+ymin_l)/2 ,x = 13)) + 
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()
      ) +
      coord_polar(theta="y") 
    
    Pie_Chart
    
  })
  
  output$piechart <- renderPlot({piechart()},height = 750)
  #Number of plugs per block
  Plug_total <- reactive({ 
    
    Num_plugs <- pie_data[pie_data$Block.number == input$block,5][1]
  })
  
  output$plug_tot <- renderText({ Plug_total() })
  
  #Plug total Percentile
  Plug_tot <- reactive({
    pie_stats <- summarize(group_by(pie_data, Block.number), 
                           div = length(Va.Plug), 
                           tots = mean(pie_sums))
    
    pie_stats$percentile_tots <- as.character(round(percent_rank(pie_stats$tots)*100,0)) 
    
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    
    if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "1" ) 
    {paste(pie_stats[pie_stats$Block.number == input$block, 4],"st", sep= "")} else {
      if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "2" ) 
      { paste(pie_stats[pie_stats$Block.number == input$block, 4],"nd", sep= "")} else {
        if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "3" ) 
        { paste(pie_stats[pie_stats$Block.number == input$block, 4],"rd", sep= "")} else {
          { paste(pie_stats[pie_stats$Block.number == input$block, 4],"th", sep= "")}
          
        }
        
      }
      
    }
    
  })
  
  output$tot_per <- renderText({ Plug_tot() })
  
  # Plug richness
  Plug_div <- reactive({
    plug_div <- length(pie_data[pie_data$Block.number == input$block,3])
  })  
  
  output$plug_div <- renderText({ Plug_div() })
  
  #Plug richness Percentile
  Plug_per <- reactive({
    pie_stats <- summarize(group_by(pie_data, Block.number), 
                           div = length(Va.Plug), 
                           tots = mean(pie_sums))
    
    pie_stats$percentile <- as.character(round(percent_rank(pie_stats$div)*100,0)) 
    
    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }
    grep
  if(pie_stats[input$block == pie_stats$Block.number, 4] == "11|12|13") {
    paste(pie_stats[pie_stats$Block.number == input$block, 4],"th", sep= "")
  } else {
    if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "1" ) 
    {paste(pie_stats[pie_stats$Block.number == input$block, 4],"st", sep= "")} else {
      if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "2" ) 
      { paste(pie_stats[pie_stats$Block.number == input$block, 4],"nd", sep= "")} else {
        if(substrRight(x = pie_stats[pie_stats$Block.number == input$block, 4], n = 1) == "3" ) 
        { paste(pie_stats[pie_stats$Block.number == input$block, 4],"rd", sep= "")} else {
           { paste(pie_stats[pie_stats$Block.number == input$block, 4],"th", sep= "")}
          
          }
        
        }
      
      }
  }
  })
  
output$plug_per <- renderText({ Plug_per() })
  
output$Rep_freq <- reactive({
  
  report_dt <- tbn34[tbn34$Block.number == 131002,]
  
  ggplot(report_dt,aes(x = sys.time)) +
  geom_dotplot( method = "histodot") +
    labs( x = "Date", y= "Count") +
    theme( axis.text.x  = element_text(size=16), 
           axis.text.y = element_text(size=16),
           axis.title.x  = element_text(size=16), 
           axis.title.y = element_text(size=16)) +
    theme_bw() 
  
})  
  
  output$table <- renderDataTable({ tbnattcomp} , options = list(scrollX = T)) 
}


shinyApp(ui = ui2,server = server2)



