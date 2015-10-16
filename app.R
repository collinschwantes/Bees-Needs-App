library(ggmap)
library(shiny)
library(shinythemes)
library(DT)
library(plyr)

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

levels(tbn34$Va.General)

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
block.numbers <- as.numeric(as.character(tbn34[!is.na(tbn34$lon) ,23]))
names(block.numbers) <- as.character(block.numbers)

#User Interface
ui2 <- fluidPage( theme = shinytheme('spacelab'),
                 fluidRow( 
                   h1("Bees Needs Interactive Data", align = "center"),
                   h5(a(href = "www.collinschwantes.com", "By Collin Schwantes"), align = "center"),
                   #use www folder to make files accessible ~ for images, css, etc 
                   p("Use this interactive data page to explore trends in cavity nesting bee data.
                     Have fun playing around with the data and let me know if there is anything
                     else you would like to be able to do!", style = "margin-left:15px;")
                   ),
                 fluidRow(
                   column(4,wellPanel(
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
                                        inline = T),
                     
                     #Map
                     tags$h3("Map Customization"),
                     tags$p("- Select a city or add your address"),
                     tags$p("- Click Update"),
                     #center map
                     #selectizeInput(inputId = "myloc",
                     #              label = "Address or Bee Block Number",
                     #           choices = block.numbers,
                     #            options = list(create = TRUE)),
                     #set zoom
                     sliderInput(inputId = "zoom",
                                 label = "Set your Map Scale",
                                 value = 13,
                                 min = 1, max = 18),
                     #set map type
                     radioButtons(inputId = "mtype", label = h5("Map Type"),
                                  choices = list("Watercolor" = "watercolor", "Toner" = "toner",
                                                 "Terrain" = "terrain"), selected = "toner"),
                     #points or contourscheckbox
                     checkboxGroupInput(inputId = "dtrep", 
                                        label = "Data representation", 
                                        select = "Contours",
                                        choices = list("Nest Blocks" =  "Points",
                                                       "Contours" = "Contours"),
                                        inline = T),
                     actionButton(inputId = "update", label = "Update")
                   )
                   
                   ),
                   column(8,
                          #tabs for differnt plots 
                          tabsetPanel(
                            tabPanel("Phenology", plotOutput(outputId = "phenology", width = "100%", click = "plot_click")),
                            tabPanel("Summary", plotOutput(outputId = "summary", width = "100%", click = "")),
                            tabPanel("Map", plotOutput(outputId = "map", width = "100%"))
                            #tabPanel("Table",dataTableOutput(outputId = "table", width = "100%"))
                          )
                   )
                 )
                   )

server2 <- function(input, output){
  # subset data for phenology
  #year
  #month
  #taxa 
  #city
  
  f <- function(y) c(label=round(mean(y), digits = 1), y=median(y))
  
  pheno.plot <- reactive({ 
    tbn.ph <- tbn34
    tbn.ph <- tbn.ph[tbn.ph$month == input$months[[1]]:input$months[[2]],]
    if(input$year != 1){tbn.ph <- tbn.ph[tbn.ph$Year == input$year,]}
    if(input$taxa != 1) {tbn.ph <- tbn.ph[tbn.ph$Va.General %in% input$taxa,]}
    if(input$city != 1) {tbn.ph <- tbn.ph[tbn.ph$City %in% input$city,]}
    if(is.null(input$wrap)){
      ggplot(tbn.ph,aes(sys.time, fill = Va.General)) +
        geom_density(alpha = 0.25, aes(y = ..density..)) + 
        labs( x = "Date", y= "Reported Nesting Events (density)") +
        theme_bw() +
        theme( axis.text.x  = element_text(size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        scale_fill_discrete(drop = F,
                            name = "Scientific Name",
                            labels = c("Unidentified",taxa.names)) 
      
    } else {
      if(input$wrap == "City") {
        ggplot(tbn.ph,aes(sys.time, fill = Va.General)) +
          geom_density(alpha = 0.25, aes(y = ..density..)) + #get the units of density from minutes to months
          labs( x = "Date", y= "Reported Nesting Events (density)") +
          theme( axis.text.x  = element_text(size=16), 
                 axis.text.y = element_text(size=16),
                 axis.title.x  = element_text(size=16), 
                 axis.title.y = element_text(size=16)) +
          theme_bw() +
          scale_fill_discrete( drop = F,
                               name = "Scientific Name",
                               labels = c("Unidentified",taxa.names)) +
          facet_wrap( ~City, drop = T,nrow = if(length(input$city) == 4){2} else{NULL}) } else {
            if(input$wrap == "Va.General") { #TAXA
              ggplot(tbn.ph,aes(sys.time, fill = Va.General)) +
                geom_density(alpha = 0.25, aes(y =..density..)) + #get the units of density from minutes to months
                labs( x = "Date", y= "Reported Nesting Events (density)") +
                theme( axis.text.x  = element_text(size=16), 
                       axis.text.y = element_text(size=16),
                       axis.title.x  = element_text(size=16), 
                       axis.title.y = element_text(size=16)) +
                theme_bw()+
                guides(fill=FALSE) +
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
    
    
    sum.tbn34 <- sum.tbn34[sum.tbn34$month == input$months[[1]]:input$months[[2]],]
    if(input$year != 1){sum.tbn34 <- sum.tbn34[sum.tbn34$Year == input$year,]}
    if(input$taxa != 1) {sum.tbn34 <- sum.tbn34[sum.tbn34$Va.General %in% input$taxa,]}
    if(input$city != 1) {sum.tbn34 <- sum.tbn34[sum.tbn34$City %in% input$city,]}
    
    
    
    if(is.null(input$wrap)){  
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = City))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "City", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$city != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black")
      
    } else { if(input$wrap == 'City'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = City))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "City", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$city != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black") 
      
    } else { if(input$wrap == 'Va.General'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = Va.General))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = .1, shape = 20) +
        labs( x = "Insect Type", y= "Reported Nests per Block") +
        theme_bw() +
        theme( axis.text.x  = element_text(angle=if(input$taxa != '1'){0}else{90},size=16), 
               axis.text.y = element_text(size=16),
               axis.title.x  = element_text(size=16), 
               axis.title.y = element_text(size=16)) +
        stat_summary(fun.data=f, geom="text", vjust=-0.7, col="black")
    } else {if(input$wrap == 'Project'){
      
      bxplot <- ggplot(sum.tbn34, aes(y = Abundance, x = Projects))
      bxplot + geom_boxplot() + 
        geom_jitter(position = position_jitter(height = 0.1,width = .05), alpha = , shape = 20) +
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
  # subest data Map
  Map.plot <- eventReactive(input$update,{ 
    data <- tbn34
    data <- data[data$month == input$months[[1]]:input$months[[2]],]
    if(input$year != 1) {data <- data[data$Year == input$year,]}
    if(input$taxa != 1) {data <- data[data$Va.General %in% input$taxa,]}
    
    #location for map center   
    map.center <- as.matrix(geocode(input$city[[1]], output = "latlon"))
    
    myMap <- get_map(location = map.center, 
                     zoom = input$zoom,
                     source = "stamen",
                     maptype = input$mtype)
    tbn.map <- ggmap(myMap, extent = "device")
    
    #adding contours 
    if(input$dtrep == "Contours"){
      map1 <- tbn.map +
        stat_density2d(
          aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
          bins = 10, data = data,
          geom = "polygon"
        ) +
        scale_fill_continuous(name = "Abundance", low = "#deebf7", high = "#08589e",na.value = "black") +
        guides(alpha=FALSE)
    }  
    #adding point
    if(input$dtrep == "Points"){
      map1 <- tbn.map +
        geom_point(data = data, aes(x = lon, y = lat, color = Va.General),
                   position = position_jitter(width = .0005, height = .0005), 
                   size = 5, 
                   alpha = .5
        ) +
        scale_color_discrete( drop = F,
                              name = "Scientific Name",
                              labels = c("Unidentified",taxa.names))
    }
    #adding point and contour
    if(length(input$dtrep) > 1) {
      map1 <-  tbn.map +
        stat_density2d(
          aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
          bins = 10, data = data,
          geom = "polygon"
        ) +
        scale_fill_continuous(name = "Abundance", low = "#deebf7", high = "#08589e",na.value = "black") +
        guides(alpha=FALSE) +
        geom_point(data = data, position = position_jitter(width = .0005, height = .0005), size = 3, alpha = .5, aes(x = lon, y = lat))
    }
    
    
    map1 
    
    
    
    
    #    if(tbn34$Block.number %in% input$myloc) { 
    #     
    #    block_ll <- tbn34[tbn34$Block.number == input$myloc ,c(4,3)]
    #   
    #  block_text <- paste(block_ll[1,1],block_ll[1,2], sep = ",")
    # 
    #myMap <- get_map(location = block_text,
    #                       zoom = input$zoom,
    #                      source = "stamen",
    #                       maptype = input$mtype 
    #   )
    #  
    # data <- tbn34
    #data <- data[data$month == input$months[[1]]:input$months[[2]],]
    #      if(input$year != 1) {data <- data[data$Year == input$year,]}
    #     if(input$taxa != 1) {data <- data[data$Va.General %in% input$taxa,]}
    #    if(input$city != 1) {data <- data[data$City %in% input$city,]}
    #   
    #  
    #str(tbn34)
    
    #      ggmap(myMap) +
    #       stat_density2d(
    #        aes(x = lon, y = lat, fill = Va.General, alpha = ..level..),
    #        bins = 6, data = data,
    #      geom = "polygon"
    #  scale_fill_continuous(low = "#fff7ec", high = "#7f0000",na.value = "black")
    
    # geom_point(data = data, 
    #           aes_string( x = "long", y = "lat",
    #                      size = input$size, 
    #                     colour = input$color, 
    #                    alpha = input$alpha)
    #) +
    #guides( alpha = F) +
    #scale_color_gradient(low = "yellow", high = "red") +
    #scale_size_continuous(range = c(5,25), guide = F) +
    #ggtitle(label = paste("Cavity Nesting Insects \n",
    #                     "Size:",input$size, "Color:",
    #                    input$color,"Transparecny:",
    #                   input$alpha, sep = " ")) +
    #theme(legend.position = "bottom") +
    #labs(x = "Longitude", y = "Latitude")  +
    #geom_point(data = block, aes_string(aes_string(x = "long", y = "lat")))
    #} else {
    #      myMap <- get_map(location = as.matrix(geocode(input$myloc, output = "latlon")),
    #                      zoom = input$zoom,
    #                     source = "stamen",
    #                    maptype = input$mtype
    #  )
    
    # data <- tbn34
    #data <- data[data$month == input$months[[1]]:input$months[[2]],]
    #      if(input$year != 1) {data <- data[data$Year == input$year,]}
    #     if(input$taxa != 1) {data <- data[data$Va.General %in% input$taxa,]}
    #    if(input$city != 1) {data <- data[data$City %in% input$city,]}
    #   
    
    #      ggmap(myMap) +
    #       stat_density2d(
    #        aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    #       bins = 6, data = data,
    #      geom = "polygon"
    #   ) +
    #  scale_fill_continuous(low = "#fff7ec", high = "#7f0000",na.value = "black")
    # geom_point(data = data, 
    #           aes_string( x = "long", y = "lat",
    #                      size = input$size, 
    #                     colour = input$color, 
    #                    alpha = input$alpha)
    #) +
    #        guides( alpha = F) +
    #       scale_color_gradient(low = "yellow", high = "red") +
    #      scale_size_continuous(range = c(5,25), guide = F) +
    #     ggtitle(label = paste("Cavity Nesting Insects \n",
    #                          "Size:",input$size, "Color:",
    #                         input$color,"Transparecny:",
    #                        input$alpha, sep = " ")) +
    # theme(legend.position = "bottom") +
    #labs(x = "Longitude", y = "Latitude")
    #}
  })
  
  
  output$map <- renderPlot(height = 750,{ Map.plot()
  })
  
  # output$mapbrushed <- renderPrint({
  #  brushedPoints(tbn34, input$map_brush)
  # })
  
  output$table <- renderDataTable({ tbnattcomp} , options = list(scrollX = T)) 
}


shinyApp(ui = ui2,server = server2)




