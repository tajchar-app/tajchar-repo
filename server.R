library(shiny) 
# graphic
library(ggplot2)
library(RColorBrewer)
# maps
library(ggmap)
library(leaflet)
library(mapview)
library(rgdal)
library(maptools)
# shiny and leaflet add-ons
library(shinydashboard)
library(shinyjs)
library(shiny.semantic)
library(shinyalert)
library(leaflet.extras)
# data reading and manipulation
library(DT)
library(plyr)
library(dplyr)
library(readxl)
library(data.table)
# 
library(varhandle)
library(data.table)
library(crosstalk)
library(gpclib)
library(hashids)
# library(mailR)
library(rdrop2)
library(googlesheets)



shinyServer(function(input, output, session) {
  
  
  IP <- reactive({ input$getIP })
  # # Questionnaire ----------------------------------------------------------------
  # 
  # # Create an empty vector to hold survey results
  results <<- as.character(rep(" ", nrow(Qlist)))
  # # Name each element of the vector based on the
  # # second column of the Qlist
  
  # names(results)  <- Qlist[,2]
  names(results)  <<- c( "Mojim najobľúbenejším tajchom je :",
    "Na oddych pri tajchu a kúpanie je najlepšie vybavené okolie tajchu:",
    "Najlepší bufet má jednoznačne:",
    "Najteplejšia voda je podľa mňa v tajchu:",
    "Najviac znečistené je podľa mňa okolie tajchu:")
  
  # # Hit counter
  output$counter <-
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")
      
      paste0("Počet zapojených respondentov: ", nrow(df.presults))
      # paste0("Počet zapojených respondentov: ", counter)
    })
  # 
  # # This renderUI function holds the primary actions of the
  # # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  # 
  # # Dynamic UI is the interface which changes as the survey
  # # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message.
    if (input$Click.Counter==0)
      return(
        list(
          h5("Klikni na tlačidlo nižšie a odpovedaj na 5 nasledujúcich otázok!")
          # h6("by Tajchár")
        )
      )
    #   
    #   # Once the next button has been clicked once we see each question
    #   # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))
      return(
        list(
          h2(textOutput("question")),
          radioButtons("survey", "",
            c(option.list(),"Neviem"))
        )
      )
    
    #   # Finally we see results of the survey as well as a
    #   # download button.
    if (input$Click.Counter>nrow(Qlist)) {
      
      return(
        list(
          h4(" "),
          # tableOutput("surveyresults"),
          h3("Gratulujeme k úspešnému zodpovedaniu otázok."),
          # downloadButton('downloadData', 'Download Individual Results'),
          # br(),
          h4("Odpovede budú spracované a zahrnuté do interaktívnej mapy.")
        )
        
      )
    }
  })
  
  
  
  
  # This reactive function is concerned primarily with
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist))){
      
      try(results[input$Click.Counter] <<- input$survey, silent = TRUE)
    }
    
    
    
    if (input$Click.Counter==nrow(Qlist)+1) {
      
      suppressWarnings(mat.presults <- rbind(mat.presults, results))
      ulozDotaznik <- as.data.frame(mat.presults)
      write.csv(ulozDotaznik, "ulozDotaznik.csv", row.names = FALSE, quote = TRUE)
      # write.csv(ipAdresy, "ipAdresy.premail.csv")
      gs_upload("ulozDotaznik.csv", overwrite = TRUE)
      
      disable("nastred2")
      disable("Click.Counter")
      
    }
    # Because there has to be a UI object to call this
    # function I set up render text that distplays the content
    # of this funciton.
    ""
    
  })
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix.
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Otázka", input$Click.Counter,":  ",
      Qlist[input$Click.Counter,2]
    )
  })
  
  
  
  
  
  output$ps <- renderUI({
    # gallery DIV
    photoswipe_gallery(images[sample(1:nrow(images), 9, replace = FALSE),], 'ps-gallery',display = TRUE)
    # lightbox_gallery(images[sample(1:nrow(images), 12, replace = FALSE),], 'gallery', display = TRUE)
  })
  
  
  output$isItMobile <- renderText({
    ifelse(input$isMobile, "Používaš mobilné zariadenie. Po zapnutí GPS a povolení prístupu k tvojej polohe bude možné ťa lokalizovať.", "Nepoužívaš mobilné zariadenie s GPS. Nie je možné ťa správne lokalizovať")
  })
  
  # GEOLOC
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  output$geolocation <- renderPrint({
    input$geolocation
  })
  
  # output$locButton <-
  #   # req(input$geolocation)
  #   renderUI(expr = if (!is.null(input$lat)) {
  #     actionButton("button", "Lokalizuj ma!")
  #   } else {
  #     NULL
  #   })
  
  
  output$locButton <-
    renderUI(expr = if (!is.null(input$lat)) {
      actionButton(
        title = ifelse(
          input$isMobile,
          "Používaš mobilné zariadenie. So zapnutým GPS a povoleným prístupom k tvojej polohe je možné ťa správne lokalizovať.",
          "Nepoužívaš mobilné zariadenie s GPS. Nie je možné ťa správne lokalizovať!"
        ),
        "button",
        "Geolokácia",
          if(input$isMobile){
          icon = icon("street-view")
          } else {
          icon = icon("times")
          }
        )
    } else
    {
      NULL
    })
  # observe({
  #   if (is.null("geolocation")) {
  #     shinyjs::disable("button")
  #   } else {
  #     shinyjs::enable("button")
  #   }
  # })

  
  # output$button <-
  #   renderUI(expr = if ("input.geolocation" == TRUE) {
  #     submitButton()
  #   } else {
  #     NULL
  #   })
  
  observeEvent(input$preview, {
    
    randInt <- as.numeric(sample(1:NROW(pikosky), 1))
    # Show a modal when the button is pressed
    shinyalert(paste("Fakt ", randInt), paste(pikosky[randInt,1]), type = "", imageUrl = "tajch-01.png", imageWidth = 150,
  imageHeight = 150)
    
    
  })
  
  
  # Popisky Zap/Vyp ---------------------------------------------------------
  counter <- reactiveValues(countervalue = 2)
  # po kliknuti narastie hodnota o +1 a vypne/zapne popisky
  observeEvent(input$doPopis, {
    counter$countervalue <- counter$countervalue + 1 
  })
  # volam ako argument labelu - noHide
  vypinacPopis <- reactive({
    if(counter$countervalue%%2==TRUE){
      FALSE
    } else {
      TRUE
    }
  })
  
  
  # Legenda Zap/Vyp ---------------------------------------------------------
  counterLeg <- reactiveValues(countervalueLeg = 2)
  # po kliknuti narastie hodnota o +1 a vypne/zapne popisky
  observeEvent(input$doLegenda, {
    counterLeg$countervalueLeg <- counterLeg$countervalueLeg + 1 
  })
  # volam ako argument labelu - noHide
  vypinacLegenda <- reactive({
    if(counterLeg$countervalueLeg%%2==TRUE){
      FALSE
    } else {
      TRUE
    }
  })
  
  
  # 1 INTERAKTIVNA MAPA TAJCHOV -----------------------------------------------
  
  # VYBER SKUPINY -----------------------------------------
  shp_selected <- reactive({
    req(input$skupina)
    if (input$skupina == "celySystem"){
      shpTajchy
    }
    else {
      shpTajchy[shpTajchy$skupinaKod %in% input$skupina, ]
    }
  })
  
  # INFOPANEL ---------------------------------------------------------------
  output$obsahInfoPanela <- renderUI({
    if (is.null(input$vybranaInfo))
      return()
    
    switch(input$vybranaInfo,
           "infoHist" =  selectInput("typInfo", "História:", choices = infoList$infoHist),
           "infoTech" = selectInput("typInfo", "Technické parametre:", choices = infoList$infoTech),
           "infoDnes" =   selectInput("typInfo", "Súčasné informácie:", choices = infoList$infoDnes),
      "xxx" =   selectInput("typInfo", "Názorové mapy:", choices = infoList$xxx))
      
  })
  
  polyOptions <- options(
    stroke = TRUE,
    color = "grey",
    weight = 2,
    smoothFactor = 1,
    fillOpacity = 0.8,
    highlightOptions = highlightOptions(color = "lightgreen", weight = 2,
                                        bringToFront = TRUE),
    style = list(
      "box-shadow" = "10px 10px rgba(0,0,0,0.45)",
      "border-color" = "rgba(0,0,0,0.5)"
    )
  )
  
  
  # FAREBNE PALETY ------------------------------------------------------------------
  # HISTORIA
  
  pal.name <-
    colorFactor(rainbow(n = length(unique(dtTajchy$name))), dtTajchy$name)
  pal.skupina <-
    colorFactor(rainbow(n = length(unique(dtTajchy$skupina))), dtTajchy$skupina)
  pal.vznik <-
    colorNumeric(palette = "Purples", domain = dtTajchy$vznikNum)
  pal.stavitel <-
    colorFactor(topo.colors(n = length(unique(dtTajchy$stavitel))), dtTajchy$stavitel, reverse = TRUE)
  pal.pamiatka <-
    colorFactor(c("darkturquoise", "gray77", "gray11"), dtTajchy$pamiatka)
  pal.rekonstr <-
    colorFactor(topo.colors(n = length(unique(dtTajchy$rekonstr))), dtTajchy$rekonstr, reverse = TRUE)
  pal.ineMeno <-
    colorFactor(topo.colors(n = length(unique(dtTajchy$ineMeno))),
                dtTajchy$ineMeno, reverse = TRUE)
  
  pal.hlavnyUcel <-
    colorFactor(topo.colors(n = length(unique(dtTajchy$hlavnyUcel))), dtTajchy$hlavnyUcel, reverse = TRUE)
  pal.vedlUcel <-
    colorFactor(rainbow(n = length(unique(dtTajchy$vedlUcel))), dtTajchy$vedlUcel, reverse = TRUE)
  
  pal.plocha <-
    colorNumeric(palette = "GnBu",
                 domain = dtTajchy$plocha,
                 reverse = TRUE)
  # TECHNICKE
  pal.nvMNM <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$nvMNM)
  pal.dlzkaKorun <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$dlzkaKorun)
  pal.sirkaKor <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$sirkaKor)
  pal.vyskaHradz <-
    colorNumeric(palette = "PuRd", domain = dtTajchy$vyskaHradz)
  pal.povodie <-
    colorFactor(topo.colors(n = length(unique(dtTajchy$povodie))), dtTajchy$povodie, reverse = TRUE)
  pal.objemM3 <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$objemM3)
  pal.dlzkaZbJrk <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$dlzkaZbJrk)
  pal.dlzkaStoln <-
    colorNumeric(palette = "Oranges", domain = dtTajchy$Stoln)
  pal.maxHlbk <-
    colorNumeric(palette = "Blues", domain = dtTajchy$maxHlbk)
  pal.matHradze <-
    colorFactor(topo.colors(n = length(unique(
      dtTajchy$matHradze
    ))), dtTajchy$matHradze, reverse = TRUE)
  pal.xy <- colorNumeric(palette = "Blues", domain = dtTajchy$clickLon)
  
  # SUCASNOST
  pal.druhZstRyb <-
    colorFactor(topo.colors(n = length(unique(
      dtTajchy$druhZstRyb
    ))), dtTajchy$druhZstRyb, reverse = TRUE)
  pal.prTvpdy <-
    colorNumeric(palette = "Reds", domain = dtTajchy$prTvpdy)
  pal.revir <-
    colorFactor(cm.colors(n = length(unique(dtTajchy$revir))), dtTajchy$revir, reverse = FALSE)
  pal.kupanie <-
    colorFactor(c("royalblue3", "red3"), dtTajchy$kupanie)
  pal.bufet <-
    colorFactor(c("darkturquoise", "gray77"), dtTajchy$bufet)
  pal.icold <- colorFactor(c("gold", "gray77"), dtTajchy$icold)
  
  
  
  # XXX
  pal.oblubenost <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$oblubenost.num)
  pal.vybava <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$vybava.num)
  pal.topbufet <-
    colorNumeric(palette = "YlGnBu", domain = dtTajchy$bufet.num)
  pal.teplota <-
    colorNumeric(palette = "PuRd", domain = dtTajchy$teplota.num)
  pal.smeti <-
    colorNumeric(palette = "PuRd", domain = dtTajchy$smeti.num)
  
  
  # LEAFLET -----------------------------------------------------------------
  output$map <- renderLeaflet({
    

    
    # input$bmap  # zachyti kliknutie tlacidla a ak treba, zmeni podklad 
    isolate(leaflet() %>%
              addProviderTiles(input$bmap, 
                               options = providerTileOptions(minZoom = 0, maxZoom = 17))) %>%
      addPolygons(
        data = shp_selected(),
        options = polyOptions
      ) %>%
      mapOptions(zoomToLimits = "always") 
  })
  
  observe({
    
    input$bmap 
    
    leafletProxy("map") %>%
      addProviderTiles(input$bmap, 
        options = providerTileOptions(minZoom = 0, maxZoom = 17))
  })


  

# search box - hladaj tajch -----------------------------------------------
  observe({
    tajch <- input$searchTajch

        ds <- subset(dtTajchy, dtTajchy$name %in% tajch, select = c("name","clickLon", "clickLat")) 
    
    isolate(leafletProxy("map") %>%
              setView(ds$clickLon, ds$clickLat, 16))
    # setView(48.4, 18.4, 21),
  })
  
  observeEvent(input$button, {
    isolate(
      leafletProxy("map") %>%
        removeMarker(layerId = "hereIm") %>%
        setView(as.numeric(input$long), as.numeric(input$lat), zoom = 17) %>%
        addPulseMarkers(layerId = "hereIm",
          lng = as.numeric(input$long),
          lat = as.numeric(input$lat),
          label = "Tvoja poloha!",
          icon = makePulseIcon(heartbeat = 1.5)
        )
    )
    
    
    
    # addCircleMarkers(as.numeric(input$long), as.numeric(input$lat), label = "Tvoja poloha!"))
  })
  
  # observe -----------------------------------------------------------------
    
  observe({
    
    map <- leafletProxy("map")
    # map %>% clearShapes()
    
    map %>%
      addPolygons(
        data = shp_selected(),
        label = shp_selected()$name,
        
        fillColor = ~pal.name(name)
      )
    

    # nastavenia Labelov a Legendy --------------------------------------------
    
    labOptHist <-labelOptions(noHide = vypinacPopis(), direction = "left", offset=c(1,-17),
                              style = list(
                                "background-color"= "rgba(0,0,0,0.55)",
                                "color" = "#fff",
                                "font-family" = "Exo 2",
                                "font-style" = "bold",
                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                "font-size" = "1.1em",
                                "border-color" = "rgba(0,0,0,0.5)",
                                "border-radius" = "35px",
                                "padding" = "2px 5px 2px 5px"
                              ))
    
    labOptTech <-labelOptions(noHide = vypinacPopis(), direction = "left", offset=c(1,-17),
                              style = list(
                                "background-color"= "rgba(63, 191, 191, 0.65)",
                                "color" = "#fff",
                                "font-family" = "Exo 2",
                                "font-style" = "bold",
                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                "font-size" = "1.1em",
                                "border-color" = "rgba(0,0,0,0.5)",
                                "border-radius" = "35px",
                                "padding" = "2px 5px 2px 5px"
                              ))
    
    labOptDnes <-labelOptions(noHide = vypinacPopis(), direction = "left", offset=c(1,-17),
                              style = list(
                                "background-color"= "rgba(0, 97, 255, 0.55)",
                                "color" = "#fff",
                                "font-family" = "Exo 2",
                                "font-style" = "bold",
                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                "font-size" = "1.1em",
                                "border-color" = "rgba(0,0,0,0.5)",
                                "border-radius" = "35px",
                                "padding" = "2px 5px 2px 5px"
                              ))
    
    labOptHistTajch <-labelOptions(noHide = vypinacPopis(), direction = "left", offset=c(1,-37),
                              style = list(
                                "background-color"= "rgba(206, 0, 0, 0.55)",
                                "color" = "#fff",
                                "font-family" = "Exo 2",
                                "font-style" = "bold",
                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                "font-size" = "1.1em",
                                "border-color" = "rgba(0,0,0,0.5)",
                                "border-radius" = "35px",
                                "padding" = "2px 5px 2px 5px"
                              ))
    
    labOptIdeaTajch <-labelOptions(noHide = vypinacPopis(), direction = "left", offset=c(1,-37),
                                   style = list(
                                     "background-color"= "rgba(0, 199, 181, 0.55)",
                                     "color" = "#fff",
                                     "font-family" = "Exo 2",
                                     "font-style" = "bold",
                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                     "font-size" = "1.1em",
                                     "border-color" = "rgba(0,0,0,0.5)",
                                     "border-radius" = "35px",
                                     "padding" = "2px 5px 2px 5px"
                                   ))
    
    # TYPINFO -----------------------------------------------------------------
    typInfo = input$typInfo
    # -----------------------
    
    input$typInfo
      
jednotka <- "m"      

if("maxHlbk" %in% typInfo | 'dlzkaZbJrk' %in% typInfo | 'dlzkaKorun' %in% typInfo | 'vyskaHradz' %in% typInfo | 'dlzkaKorun' %in% typInfo | 'sirkaKor' %in% typInfo ) {
  jednotka <- "m"
}
    if('nvMNM' %in% typInfo ) {
      jednotka <- "m n.m."
    }
    if('plocha' %in% typInfo) {
      jednotka <- "m2"
    }
    if('objemM3' %in% typInfo ) {
      jednotka <- "m3"
    }
if('vznik' %in% typInfo ) {
  jednotka <- " "
}


if('oblubenost.num' %in% typInfo |'vybava.num' %in% typInfo |'bufet.num' %in% typInfo |'teplota.num' %in% typInfo |'smeti.num' %in% typInfo){
  jednotka <- "x"
}
# bar plot ----------------------------------------------------------------
    
if ('maxHlbk' %in% typInfo | 'dlzkaZbJrk' %in% typInfo | 'dlzkaKorun' %in% typInfo | 'vznik' %in% typInfo | 'nvMNM' %in% typInfo | 'objemM3' %in% typInfo | 'vyskaHradz' %in% typInfo | 'dlzkaKorun' %in% typInfo | 'sirkaKor' %in% typInfo | 'plocha' %in% typInfo | 'oblubenost.num' %in% typInfo |'vybava.num' %in% typInfo |'bufet.num' %in% typInfo |'teplota.num' %in% typInfo |'smeti.num' %in% typInfo) {

      
      dtf <- data.table::setDT(as.data.frame(shp_selected()))
      
      dataset <- subset(dtf, select = c("name", typInfo, "skupina")) 
      dataset <- dataset[order(dataset[,2])]
      names(dataset) <- c("name", "typInfo", "skupina")
      
      prvy <- dataset[NROW(dataset)]
      druhy <- dataset[NROW(dataset)-1]
      treti <- dataset[NROW(dataset)-2]
      
      output$plot_nice <- renderPlot({
        
        ggplot(dataset, aes_string(
          x=paste0("reorder(",colnames(dataset)[1],", -typInfo)"),
          y="typInfo",
          fill = "skupina"))+
          geom_bar(stat = "identity",  colour="white")  +
          
          theme(legend.position="none") +
          
          theme(
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            # axis.text.x=element_blank(),
            # axis.text.y=element_blank(),
            axis.ticks.x=element_blank()
            # axis.ticks.y=element_blank()
          ) +
          
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      }, height = 200)
      
      output$umiestnenie <- renderText({
        HTML(paste(tags$span(style="color:#2dc4bf","REKORDÉRI:"), sep = ""))
      })
      output$zlato <- renderText({
        HTML(paste(tags$span(style="color:#F3CA15","1.", prvy$name, " -", prvy$typInfo, jednotka), sep = ""))
        # HTML(paste("This text is ", tags$span(style="color:blue", "red"), sep = ""))
      })
      output$striebro <- renderText({
        HTML(paste(tags$span(style="color:#D7D8D7","2.", druhy$name, " -", druhy$typInfo, jednotka), sep = ""))
      })
      output$bronz <- renderText({
        HTML(paste(tags$span(style="color:#EA8A24","3.", treti$name, " -", treti$typInfo, jednotka), sep = ""))
      })
      
      show("zlato", time = 0.8, anim = FALSE)
      show("striebro", time = 0.8, anim = FALSE)
      show("bronz", time = 0.8, anim = FALSE)
      show("umiestnenie", time = 0.8, anim = FALSE)
      
      show("plot_nice")
    }
    


    # FACTOR CATEGORY ---------------------------------------------------------
    
if ('skupina' %in% typInfo |'matHradze' %in% typInfo |'stavitel' %in% typInfo |'povodie' %in% typInfo |'pamiatka' %in% typInfo |'icold' %in% typInfo |'kupanie' %in% typInfo |'revir' %in% typInfo |'bufet' %in% typInfo) {
      
      dataset <- subset(shp_selected(), select = typInfo) 
      names(dataset) <- "typInfo"
      
      
      output$plot_nice <- renderPlot({
        
        ggplot(data.frame(dataset), aes(x=typInfo,  fill = typInfo)) +
          geom_bar(colour="white")  +
          # 
          theme(legend.position="none") +
          
          theme(
            axis.title.x=element_blank(),
            # axis.title.y=element_blank(),
            # axis.text.x=element_blank(),
            # axis.text.y=element_blank(),
            axis.ticks.x=element_blank()
            # axis.ticks.y=element_blank()
          ) +
          
          labs(y = "Počet tajchov") +
          
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      }, height = 200)
      
      hide("zlato", time = 0.8, anim = FALSE)
      hide("striebro", time = 0.8, anim = FALSE)
      hide("bronz", time = 0.8, anim = FALSE)
      hide("umiestnenie", time = 0.8, anim = FALSE)
      # hide("summary", time = 0.8, anim = FALSE)
      show("plot_nice")
      
    }
    
    if ('hlavnyUcel' %in% typInfo |'vedlUcel' %in% typInfo |'bufet' %in% typInfo |'druhZstRyb' %in% typInfo | 'ineMeno' %in% typInfo | 'empty' %in% typInfo  | 'xy' %in% typInfo | 'foto' %in% typInfo) {
      hide("plot_nice", time = 0.8, anim =FALSE)
      
      
      hide("zlato", time = 0.8, anim = FALSE)
      hide("striebro", time = 0.8, anim = FALSE)
      hide("bronz", time = 0.8, anim =  FALSE)
      hide("umiestnenie", time = 0.8, anim = FALSE)
    }


    # TYPINFO -----------------------------------------------------------------
    
    if (length(typInfo) > 0) {
      
      # EMPTY ---------------------------------------------------------
      if ('empty' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%
          addPolygons(
            data = shp_selected(),
            label = shp_selected()$name,
            fillColor = "#002FB1",
            
            options = polyOptions,
            labelOptions = labOptHist
          ) %>% 
          addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))  
        
        if (vypinacLegenda() == TRUE) {
          map %>%  addLegend(
            position = 'bottomright',
            colors = "#002FB1",
            labels = "Tajch", opacity = 1,
            title = 'Legenda:'
          )
        }
      }
      
      
      # 0_Skupina ---------------------------------------------------------------
      if ('skupina' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%   addPolygons(
          data = shp_selected(),
          label = shp_selected()$skupina,
          fillColor = ~pal.skupina(skupina),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.skupina, values = shp_selected()$skupina,
                             title = "Skupina:",
                             opacity = 1)
        }
      }
      
      # H_Vznik -----------------------------------------------------------------
      if ('vznik' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          clearPopups() #    ...............................?
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(dtTajchy$vznik),
          fillColor = ~pal.vznik(vznikNum),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.vznik, values = shp_selected()$vznikNum,
                             title = "Rok vzniku:",
                             opacity = 1)
        }}
      
      
      # H_Zastarany nazov -------------------------------------------------------
      if ('ineMeno' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$ineMeno,
          color = ~pal.ineMeno(ineMeno),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.ineMeno, values = shp_selected()$ineMeno,
                             title = "Historický názov:",
                             opacity = 1)
        }}
      
      
      # H_Stavitel -------------------------------------------------------
      if ('stavitel' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$stavitel,
          color = ~pal.stavitel(stavitel),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.stavitel, values = shp_selected()$stavitel,
                             title = "Staviteľ:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      
      # H_Pamiatka -------------------------------------------------------
      if ('pamiatka' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$name,
          color = ~pal.pamiatka(pamiatka),
          # label = shp_selected()$pamiatka,
          labelOptions = labOptHist,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.pamiatka, values = shp_selected()$pamiatka,
                             title = "Kultúrna pamiatka:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      
      
      # H_Zaujimavost -------------------------------------------------------
      if ('info' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$info,
          
          
          fillColor = "lightgreen",
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
      }
      
      
      # H_rekonstr ---------------------------------------------------------
      if ('rekonstr' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$rekonstr,
          color = ~pal.rekonstr(rekonstr),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.rekonstr, values = shp_selected()$rekonstr,
                             title = "Roky rekonštrukcie:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      
      # H_Hlavny ucel -------------------------------------------------------
      if ('hlavnyUcel' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$hlavnyUcel,
          color = ~pal.hlavnyUcel(hlavnyUcel),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.hlavnyUcel, values = shp_selected()$hlavnyUcel,
                             title = "Hlavný účel tajchu:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      
      # H_Vedlajsi ucel -------------------------------------------------------
      if ('vedlUcel' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$vedlUcel,
          color = ~pal.vedlUcel(vedlUcel),
          
          options = polyOptions,
          labelOptions = labOptHist
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.vedlUcel, values = shp_selected()$vedlUcel,
                             title = "Vedľajší účel tajchu:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      # T_Max hlbka ---------------------------------------------------------
      if ('maxHlbk' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$maxHlbk),
          color = ~pal.maxHlbk(maxHlbk),
          
          labelOptions = labOptTech,
          options = polyOptions
        )  
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.maxHlbk, values = shp_selected()$maxHlbk,
                             title = "Maximálna hĺbka v m:",
                             opacity = 1)
        }
      }
      
      # T_Plocha ---------------------------------------------------------
      if ('plocha' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$plocha),
          color = ~pal.plocha(plocha),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.plocha, values = shp_selected()$plocha,
                             title = HTML(paste("Plocha vodnej hladiny:", "v m2:", sep="<br/>")),
                             opacity = 1)
        }
      }
      
      # T_Dlzka stolni ---------------------------------------------------------
      if ('dlzkaStoln' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$dlzkaStoln),
          color = ~pal.dlzkaStoln(dlzkaStoln),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.dlzkaStoln, values = shp_selected()$dlzkaStoln,
                             title =  HTML(paste("Dĺžka štôlní", "v metroch:", sep="<br/>")), 
                             opacity = 1)
        }
      }
      
      # T_Dlzka zbernych jarkov ---------------------------------------------------------
      if ('dlzkaZbJrk' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$dlzkaZbJrk),
          color = ~pal.dlzkaZbJrk(dlzkaZbJrk),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.dlzkaZbJrk, values = shp_selected()$dlzkaZbJrk,
                             title = HTML(paste("Dĺžka zberných jarkov", "v metroch:", sep="<br/>")), 
                             opacity = 1)
        }
      }
      
      
      # T_Sirka koruny hradze ---------------------------------------------------------
      if ('sirkaKor' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$sirkaKor),
          color = ~pal.sirkaKor(sirkaKor),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.sirkaKor, values = shp_selected()$sirkaKor,
                             title = HTML(paste("Šírka koruny hrádze", "v metroch:", sep="<br/>")),
                             opacity = 1)
        }
      }
      
      
      # T_Dlzka koruny hradze ---------------------------------------------------------
      if ('dlzkaKorun' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$dlzkaKorun),
          color = ~pal.dlzkaKorun(dlzkaKorun),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.dlzkaKorun, values = shp_selected()$dlzkaKorun,
                             title = HTML(paste("Dĺžka koruny hrádze", "v metroch:", sep="<br/>")),
                             opacity = 1)
        }
      }
      
      
      # T_Vyska Hradze ---------------------------------------------------------
      if ('vyskaHradz' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$vyskaHradz),
          color = ~pal.vyskaHradz(vyskaHradz),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.vyskaHradz, values = shp_selected()$vyskaHradz,
                             title = HTML(paste("Výška hrádze", "v metroch:", sep="<br/>")),
                             opacity = 1)
        }
      }
      
      
      # T_Povodie ---------------------------------------------------------
      if ('povodie' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$povodie,
          color = ~pal.povodie(povodie),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.povodie, values = shp_selected()$povodie,
                             title = "Príslušnosť k povodiu:",
                             # labFormat = labelFormat(prefix = "$"),
                             opacity = 1)
        }}
      
      # T_Objem ---------------------------------------------------------
      if ('objemM3' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$objemM3),
          color = ~pal.objemM3(objemM3),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.objemM3, values = shp_selected()$objemM3,
                             title = "Objem v m3:",
                             opacity = 1)
        }
      }
      
      # T_Nadmorska vyska ---------------------------------------------------------
      if ('nvMNM' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$nvMNM),
          color = ~pal.nvMNM(nvMNM),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.nvMNM, values = shp_selected()$nvMNM,
                             title = HTML(paste("Nadmorská výška", "v m n.m.:", sep="<br/>")),
                             opacity = 1)
        }}
      
      # T_Material hradze ---------------------------------------------------------
      if ('matHradze' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$matHradze),
          color = ~pal.matHradze(matHradze),
          
          labelOptions = labOptTech,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.matHradze, values = shp_selected()$matHradze,
                             title = "Materiál hrádze:",
                             opacity = 1)
        }}
      
      # T_Zem suradnice ---------------------------------------------------------
      if ('xy' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        x.lab <- as.character(round(shp_selected()$clickLon,3))
        y.lab <- as.character(round(shp_selected()$clickLat, 3))
        xy.lab <- paste(sep = ",", x.lab, y.lab)
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = xy.lab,
          # label = as.character(shp_selected()$clickLon),
          color = ~pal.xy(clickLon),
          
          labelOptions = labOptTech,
          options = polyOptions
        )  
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.xy, values = shp_selected()$clickLon,
                             title = "Zemepisné súradnice:",
                             opacity = 1)
        }}
      
      
      
      # S_Revír ---------------------------------------------------------
      if ('revir' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          color = ~pal.revir(revir),
          label = shp_selected()$revir,
          
          labelOptions = labOptDnes,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.revir, values = shp_selected()$revir,
                             title = "Lovný revír:",
                             opacity = 1)
        }
      }
      
      # S_Druhove zastupenie ryb ---------------------------------------------------------
      if ('druhZstRyb' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          color = ~pal.druhZstRyb(druhZstRyb), #♥
          label = shp_selected()$druhZstRyb,
          
          labelOptions = labOptDnes,
          options = polyOptions
        ) 
        
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.druhZstRyb, values = shp_selected()$druhZstRyb,
                             title = "Druhové  zastúpenie rýb:",
                             opacity = 1)
        }
      }
      
      
      # S_Bufet ---------------------------------------------------------
      if ('bufet' %in% typInfo) {
        
        dt = data.table::setDT(as.data.frame(shp_selected())) %>%
          select(clickLon, clickLat, bufetImg) %>%
          filter(!is.na(bufetImg))
  
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$name,
          color = ~pal.bufet(bufet),
          labelOptions = labOptDnes,
          options = polyOptions
          

        )  %>%             addMarkers(icon = i.food, data = dt, lng =~clickLon, lat = ~clickLat, #~quakeIcons[group]
          popup = paste0("<img src = ", dt$bufetImg, ".jpg>"),
          popupOptions = popupOptions( minWidth = 300, minHeight = 150))
        
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.bufet, values = shp_selected()$bufet,
                             title = "Bufet:",
                             opacity = 1)
        }
      }
      
      # S_Kupanie ---------------------------------------------------------
      if ('kupanie' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$name,
          color = ~pal.kupanie(kupanie),
          
          labelOptions = labOptDnes,
          options = polyOptions
          
        )  %>%     addMarkers(data = data.table::setDT(as.data.frame(shp_selected())) %>%
                                select(kupanie, clickLon, clickLat, link)%>%
                                filter(!is.na(link)), lng =~clickLon, lat = ~clickLat,
                              icon = ~i.swim,  popup=~link)
        
        
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.kupanie, values = shp_selected()$kupanie,
                             title = "Voda vhodná na kúpanie:",
                             opacity = 0.8)
        }
      }
      
      # S_ICOLD ---------------------------------------------------------
      if ('icold' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$name,
          color = ~pal.icold(icold),
          labelOptions = labOptDnes,
          options = polyOptions
        ) 
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.icold, values = shp_selected()$icold,
                             title = HTML(paste("Zápis v ICOLD", "(Svetový zoznam veľkých priehrad)", sep="<br/>")),
                             opacity = 1)
        }
      }
      
      
      # S_Fotografie ---------------------------------------------------------
      if ('foto' %in% typInfo) {
        popup <- popupImage(paste0(shp_selected()$imgSource, ".jpg"))
        
        leafletProxy("map") %>% 
          addMarkers( lng=-81, lat=37,popup=popup)
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = shp_selected()$name,
          labelOptions = labOptDnes,
          options = polyOptions
          
        ) %>%
          addMarkers(icon = i.photo, data = shp_selected(), lng =~clickLon, lat = ~clickLat, #~quakeIcons[group]
                     popup = popup, 
                     popupOptions = popupOptions( minWidth = 300, minHeight = 150))
      }
      
      
      
      # X_Oblubenost ----------------------------------------------------------------
      if ('oblubenostNum' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$oblubenost.num),
          color = ~pal.oblubenost(oblubenost.num),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.oblubenost, values = shp_selected()$oblubenost.num,
            title = "Najobľubenejší tajch:",
            opacity = 1)
        }}
      
      
      # X_Vybava ------------------------------------------------------------------
      
      
      if ('vybava.num' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$vybava.num),
          color = ~pal.vybava(vybava.num),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.vybava, values = shp_selected()$vybava.num,
            title = "Najlepšie vybavenie:",
            opacity = 1)
        }}
      
      
      # X_najlepsi bufet ----------------------------------------------------------
      
      
      if ('bufet.num' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$bufet.num),
          color = ~pal.topbufet(bufet.num),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.topbufet, values = shp_selected()$bufet.num,
            title = "Najlepší bufet:",
            opacity = 1)
        }}
      
      
      # X_najteplejsia voda -------------------------------------------------------
      
      
      if ('teplota.num' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$teplota.num),
          color = ~pal.teplota(teplota.num),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.teplota, values = shp_selected()$teplota.num,
            title = "Najteplejšia voda:",
            opacity = 1)
        }}
      
      
      # X_znecistene okolie -------------------------------------------------------
      
      if ('smeti.num' %in% typInfo) {
        
        map %>% clearShapes() %>%
          clearControls() %>%
          clearMarkers()
        
        leafletProxy("map")  %>%  addPolygons(
          data = shp_selected(),
          label = as.character(shp_selected()$smeti.num),
          color = ~pal.smeti(smeti.num),
          
          labelOptions = labOptTech,
          options = polyOptions
        )
        
        if (vypinacLegenda() == TRUE) {
          
          map %>%  addLegend("bottomright", pal = pal.smeti, values = shp_selected()$smeti.num,
            title = "Znečistené okolie:",
            opacity = 1)
        }}
      
      
      
      
    } # end: typinfo
    
# popCup= ~paste("<b>", latitude, longitude, "</b></br>", actionButton("showmodal", "Show modal", onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())')))
    

# ShpSelect ---------------------------------------------------------------
    
    shpSelect <- input$shpSelect 

    
    if (length(shpSelect) > 0) {
      # JARKY
      if ('shpJarky' %in% shpSelect) {
        leafletProxy("map")  %>%
          clearControls() %>%
          clearMarkers() %>% 
        addPolylines(data = shpJarky,
                                              weight = 1,
                                              col = 'blue')}
      # VODNE STOLNE
      if ('shpStolneVodne' %in% shpSelect) {
        leafletProxy("map")  %>% addPolylines(data = shpStolneVodne,
                                              weight = 2,
                                              col = 'green')}
      # STOLNE
      if ('shpStolne' %in% shpSelect) {
        leafletProxy("map")  %>%   addCircles(lng = shpStolne@coords[,1], lat = shpStolne@coords[,2],
                                              weight = 1, radius=8,
                                              color="red", stroke = TRUE, fillOpacity = 0.8)}
      # PINGY
      if ('shpPingy' %in% shpSelect) {
        leafletProxy("map")  %>% addCircles(lng = shpPingy@coords[,1], lat = shpPingy@coords[,2],
                                            weight = 1, radius=5,
                                            color="#ffa500", stroke = TRUE, fillOpacity = 0.8)}
      
      if ('shpTajchyHist' %in% shpSelect) {
        leafletProxy("map")  %>%
          clearControls() %>%
          clearMarkers() %>% 
          clearPopups() %>% 
          
          addPolygons(
          data = shpTajchyHist,
          label = shpTajchyHist$meno,
          labelOptions = labOptHistTajch,
          fillColor= "red",
          fillOpacity = 0.4,
          weight = 0.2,
          smoothFactor = 0.2,
          highlightOptions = highlightOptions(color = "white", weight = 2,
                                              bringToFront = TRUE))
        
        if (vypinacLegenda() == TRUE) {
          map %>%  addLegend(
            position = 'bottomright',
            colors = "red",
            labels = "Historické tajchy (Zdroj: Prehľadná povrchová mapa banskoštiavnického banského obvodu, rok:1884)", opacity = 1,
            title = 'Legenda:'
          )
        }
      }
      
      
      # if ('shpTajchyIdea' %in% shpSelect) {
      #   leafletProxy("map")  %>% addPolygons(
      #     data = shpTajchyIdea,
      #     fillColor= "green",
      #     fillOpacity = 0.4,
      #     weight = 0.2,
      #     smoothFactor = 0.2,
      #     highlightOptions = highlightOptions(color = "orange", weight = 2,
      #                                         bringToFront = TRUE))
      # }
      if ('shpTajchyIdea' %in% shpSelect) {
        leafletProxy("map")  %>%
          clearControls() %>%
          clearMarkers() %>% 
          clearPopups() %>% 
          
          addPolygons(
            data = shpTajchyIdea,
            label = shpTajchyIdea$meno,
            labelOptions = labOptIdeaTajch,
            fillColor= "rgba(0, 199, 181, 0.90)",
            fillOpacity = 0.4,
            weight = 0.2,
            smoothFactor = 0.2,
            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                bringToFront = TRUE))
        
        if (vypinacLegenda() == TRUE) {
          map %>%  addLegend(
            position = 'bottomright',
            colors = "red",
            labels = "Plánované priargské tajchy (Zdroj: doplnit)", opacity = 1,
            title = 'Legenda:'
          )
        }
      }
      
      
    }
  })
})
