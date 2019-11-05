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




# 1_DashboardHeader -------------------------------------------------------
header <- dashboardHeader(
  # Main panel
  tags$li(
    class = "dropdown",
    tags$style(
      ".wrapper {overflow-y: hidden;}",
      ".main-header {max-height: 100px !important;
      font-size:10px; 
      font-weight:bold; 
      line-height:10px;}"),
    # LOGO
    tags$style(
      ".main-header .logo {height: 45px;
      padding: -10px 0px;
      font-size:14px; 
      font-weight:bold; 
      line-height: 45px !important;}"),
    tags$style(
      ".sidebar-toggle {
      display:none;
      }")
  ),
  
  
  title = HTML(
    "<div style = 'background-color:#FFFFFF; vertical-align:middle'>
    <img src = 'logo.test.jpg' align = 'centre' height = '50px'>
    <pisane>Guide to Historical Water Reservoirs in the Vicinity of Banská Štiavnica<pisane/><nadpis><nadpis/>
    </div>"),
  titleWidth = "97%"
  
)


# 2_Sidebar ---------------------------------------------------------------
sidebar <- dashboardSidebar(
  # GEOLOCATION
  tags$script('
              $(document).ready(function () {
              
              function getLocation(callback){
              var options = {
              enableHighAccuracy: true,
              timeout: 5000,
              maximumAge: 0
              };
              
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              var timestamp = new Date();
              
              console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              Shiny.onInputChange("accuracy", coords.accuracy);
              Shiny.onInputChange("time", timestamp)
              
              console.log(timestamp);
              
              if (callback) {
              callback();
              }
              }, 1100)
              }
              }
              
              var TIMEOUT = 1000; //SPECIFY
              var started = false;
              function getLocationRepeat(){
              //first time only - no delay needed
              if (!started) {
              started = true;
              getLocation(getLocationRepeat);
              return;
              }
              
              setTimeout(function () {
              getLocation(getLocationRepeat);
              }, TIMEOUT);
              
              };
              
              getLocationRepeat();
              
              });
              '),
  
  
  
  useShinyalert(),
  HTML('<meta name="viewport" content="width=1024">'),
  
  # CSS StyleSheet
  titlePanel("", 
    tags$head(
      # tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "icon", type = "image/png", href = "logo-01.png"),
      windowTitle = "title",
      tags$head(includeHTML(("google-analytics.html")))
    )),
  
  width = "15%",
  
  
  sidebarMenu(
    
    id="tabs",
    # SIDEBAR MENU ITEMS ------------------------------------------------------------
    menuItem(span("Interactive map", style="color: #0D87FF ;font-weight:500"), tabName = "mapa", icon = icon("map"), selected=TRUE),
    menuItem(span(" About Tajchár",style="color:#2c94f9 ; font-weight:500"),tabName = "autori", icon = icon("book")),
    menuItem(span("Questionnaire",style="color:#2c94f9 ; font-weight:500"),tabName = "zapojsa", icon = icon("hands-helping")),
    menuItem(span("Source code",style="color:#2c94f9; font-weight:500"), icon = icon("file-code-o"),
      href = "https://github.com/tajchar-app/tajchar-repo"),
    menuItem(span("Photo gallery",style="color:#2c94f9 ; font-weight:500"),tabName = "fotogaleria", icon = icon("book")),
    
    # INTERACTIVE MAP - CONTENT -------------------------------------------
    conditionalPanel(
      "input.tabs=='mapa'",
      
      mobileDetect('isMobile'),
      uiOutput('locButton'),
      
      tags$br(),
      
      # # GROUP SELECT
      selectInput(
        inputId = "skupina",
        label = span("Group of Tajchs:", style =
            "color:#2dc4bf;
                                              font-size: 12px"),
        c(
          "The complete system" = "celySystem",
          "Piarg" = 5,
          "Štiavnica" = 7,
          "Hodruša" = 2,
          "Vyhne" = 8,
          "Belá" = 1,
          "Kolpachy" = 3,
          "Moderštôlňa" = 4,
          "Pukanec" = 6
        ),
        selectize = TRUE,
        selected = "The complete system",
        multiple = FALSE
      ),
      
      
      
      # ANOTHER SHAPEFILES TO DISPLAY
      checkboxGroupInput(
        "shpSelect",
        label = span("Show on the map:", style =
            "color:#2dc4bf;
                                                      font-size: 12px"),
        c(
          "Water canals and galleries" = "shpJarky",
          # "Štôlne" = "shpStolne",
          # "Vodné štôlne" = "shpStolneVodne",
          # "Pingy" = "shpPingy",
          "Historical tajchs" = "shpTajchyHist"
        ),
        # "Plánované piargske tajchy"="shpTajchyIdea"),
        selected = ""
      ),
      
      
      radioButtons(
        "vybranaInfo",
        label = span("Select the type of information:", style = "color:#2dc4bf;
                                                               font-size: 12px"),
        c(
          "History" = "infoHist",
          "Technical parameters" = "infoTech",
          "Present" ="infoDnes",
          "Opinion Maps" = "xxx"
        )
      ),
      
      # INFO - Abs.panel ---------------------------------------------------
      wellPanel(uiOutput("obsahInfoPanela")),
      
      # textOutput("lat"),
      # textOutput("long"),
      # textOutput("geolocation"),
      
      # text info - record holders
      htmlOutput('umiestnenie'),
      htmlOutput("zlato"),
      htmlOutput('striebro'),
      htmlOutput('bronz'),
      tags$br(),
      plotOutput("plot_nice",  width = "250%")
    )
  )
)


# 3_ DashboardBody --------------------------------------------------------
body <- dashboardBody(
  useShinyjs(),
  includeCSS(file.path('www', 'styles.css')),
  
  
  tabItems(
    # 3.1_TAB - MAP -----------------------------------------------------
    tabItem(tabName = "mapa", 
      tags$style(type = "text/css", "#map {height: calc(100vh - 50px) !important;}"),
      
      
      # LEAFLET -----------------------------------------------------------------
      leafletOutput("map"),
      
      
      # BASE MAP - Abs.panel ----------------------------------------------
      absolutePanel(id = "control", top = 80, left = "auto", right = 28, bottom = "auto",
        width="11%", height = "auto",
        fixed = TRUE, draggable = FALSE,
        
        selectInput("bmap", span("BaseMap:", style="color: white ;font-weight:600; text-shadow: rgba(0, 97, 255, 1) 0.1em 0.1em 0.2em"),
          choices = c("OpenStreetMap.Mapnik",
            "Esri.WorldStreetMap",
            "Esri.WorldImagery",
            "Esri.WorldTopoMap",
            "Stamen.Watercolor",
            "Stamen.Toner"), 
          selected = "Esri.WorldImagery"),
        
        tags$head(tags$script(src = "message-handler.js"),
          tags$script(src="getIP.js")),
        
        actionButton("doLegenda", "Legend On/Off", icon("info-circle")),
        actionButton("doPopis", "Labels On/Off", icon("tags")),
        tags$br(),
        tags$br(),
        selectInput("searchTajch", span("Search Tajch:", style="color: white ;font-weight:600; text-shadow: rgba(0, 97, 255, 1) 0.1em 0.1em 0.2em"),
          # choices =order(dtTajchy$name),
          choices = c("-", as.character(dtTajchy$name[order(dtTajchy$name)])) ,
          selected = "-")
        ,
        actionButton("preview", "  Interesting Facts")
      )
      # actionButton("CR1_S1", "Button"),
      # bsModal("modalExample", "Your Table", "CR1_S1", size = "large", uiOutput("mytable")),
      # slickROutput("slickr", width="500px")
    ),
    
    tabItem(tabName = "autori",
      
      box(width=12,
        title = "About TAJCHÁR",
        
        
        # About Tajchar ------------------------------------------------------------
        tags$div(class = "text1",
          "The idea to create", tags$b("map application Tajchár"),  "originated in May of 2018. 
          The main idea was to combine wide- range possibilities of the", tags$b(" programming language R with fondness of Tajchs in Banská Štiavnica. 
          ")),
        
        tags$img(class="znamka", src="znamka.png"),
        tags$div(class = "text2",
          "R"),
        
        tags$div(class = "text1", tags$a(href="https://www.r-project.org/about.html", 
          "Programming language R"), "is open source and its main difference of other languages is that",tags$b(" it´s primarily focused an data analytics, statistics, data mining, data science, and data visualization."), "There is a huge amount of", tags$b("packages,"), "which are immediately accessible to solve many different problems."),
        tags$div(class = "text1",
          "One of the used packages is", tags$a(href="https://shiny.rstudio.com", "Shiny,"), "which provides", tags$b("easy creation of interactive web applications."), "It also allows the usage of marking language CSS, HTML widgets or JavaScript actions. R package", tags$a(href = "https://rstudio.github.io/leaflet/", "Leaflet"), "is, for a change, one of the most popular open source libraries for", tags$b("the creation of interactive maps.")),
        
        
        
        # About Author ---------------------------------------------------------------
        tags$img(class="znamka", src="loga.png"),
        tags$br(),
        tags$div(class = "text2",
          "ABOUT AUTHOR"),
        
        tags$img(class="foto", src="profilovka.jpg"),
        tags$br(),
        
        tags$div(class = "text2",
          "Ing. Veronika Soldánová"),
        
        tags$div(class = "text1",
          "Currently a student of doctoral studies at", tags$a(href="http://www.kvhk.sk", "Department of Land and Water Resources Management,"), " of Faculty of Civil Engineering of the Slovak University of Technology in Bratislava."),
        
        tags$div(class = "text1",
          "Within the range of her dissertation thesis, which is focused on", tags$b("water management assessment of irrigation reservoir,"), "she became acquainted with the programming language R, which has become her hobby over time."),
        tags$div(class = "text1",
          "During the Landscape Studies at", tags$a(href="http://www.fzki.uniag.sk/sk/", "Faculty of Horticulture and Landscape Engineering"), "at SUoA in Nitra, the author collected many materials about", tags$b("Tajchs and aquaculture in Banská Štiavnica,"), "which, she decided to open up to the public by the form of", tags$b("the map application.")),
        
        
        
        #Sources ------------------------------------------------------------
        
        tags$div(class = "text2",
          "SOURCES AND HISTORICAL SOURCES"),
        
        tags$div(class = "text1",
          "Main source of data were materials collected or processed within the range of", tags$b("bachelor and diploma thesis.")),
        
        tags$div(class = "text1",
          "To the used historical sources belong", tags$b("maps from State Central Mining Archive in Banská Štiavnica,"), "which were used as a base of vectorization of historical state of Tajchs."),
        
        tags$div(class = "text1",
          "Big part of historical information, also as interesting information in section 'Interesting Facts' is from the book ", tags$b("Banskoštiavnická vodohospodárska sústava - Tajomstvá štiavnických hôr,"), "which author, Ing. Michal Červeň, were during creation of map application, always willing and helpful consultant, for that, he deserves a great thanks."),
        
        tags$img(class="znamka", src="kniha.jpg"),
        
        tags$div(class = "text1",
          "From ",  tags$b("Facebook page"), tags$a(href="https://www.facebook.com/banskostiavnicketajchy/", "BanskoŠtiavnické tajchy"), "are also photographs of Tajchs,  in the section 'Present information'."),
        
        
        tags$div(class = "text2",
          "THE LAST WORD"),
        
        tags$div(class = "text1",
          "It is possible, that some of the provided information is slightly different as information from other authors. It´s mostly about the historical guesswork or parameters, which, in the present, cannot be correctly specified. Every notes or idea, which will help to improve this application and create a place, which captures exceptionality of historical water reservoirs in the vicinity of Banská Štiavnica, are welcome."),
        
        
        # Contact  -----------------------------------------------------------------
        
        a(actionButton(class="nastred", inputId = "email1", label = "Contact author", 
          icon = icon("envelope", lib = "font-awesome")),
          href="mailto:veronika.soldanova@stuba.sk")
      )),
    
    
    tabItem(tabName = "fotogaleria",
      
      box(
        width = 12,
        title = "FOTOGALLERY",
        
        tabsetPanel(
          tabPanel('Bakomi',
            fluidRow(column(12,
              uiOutput('ps')))),
          tabPanel('Bančiansky',
            fluidRow(column(12,
              uiOutput('ps1')))),
          
          tabPanel('Beliansky',
            fluidRow(column(12,
              uiOutput('ps2')))),
          tabPanel('Brennerský',
            fluidRow(column(12,
              uiOutput('ps3')))),
          tabPanel('Červená Studňa',
            fluidRow(column(12,
              uiOutput('ps4')))),
          tabPanel('Dolný Hodrušský',
            fluidRow(column(12,
              uiOutput('ps5')))),
          tabPanel('Dolný Komorovský',
            fluidRow(column(12,
              uiOutput('ps6')))),
          tabPanel('Dolný Michalštôlniansky',
            fluidRow(column(12,
              uiOutput('ps7')))),
          tabPanel('Evička',
            fluidRow(column(12,
              uiOutput('ps8')))),
          
          tabPanel('Halčiansky',
            fluidRow(column(12,
              uiOutput('ps9')))),
          tabPanel('Horný Hodrušský',
            fluidRow(column(12,
              uiOutput('ps10')))),
          tabPanel('Horný Komorovský',
            fluidRow(column(12,
              uiOutput('ps11')))),
          tabPanel('Klinger',
            fluidRow(column(12,
              uiOutput('ps12')))),
          
          tabPanel('Krechsengrund',
            fluidRow(column(12,
              uiOutput('ps13')))),
          tabPanel('Malá Richňava',
            fluidRow(column(12,
              uiOutput('ps14')))),
          tabPanel('Malá Vodárenská',
            fluidRow(column(12,
              uiOutput('ps15')))),
          tabPanel('Malý Kolpašský',
            fluidRow(column(12,
              uiOutput('ps16')))),
          tabPanel('Moderštôlniansky',
            fluidRow(column(12,
              uiOutput('ps17')))),
          tabPanel('Ottergrund',
            fluidRow(column(12,
              uiOutput('ps18')))),
          tabPanel('Počúvadliansky',
            fluidRow(column(12,
              uiOutput('ps19')))),
          
          tabPanel('Rozgrund',
            fluidRow(column(12,
              uiOutput('ps20')))),
          tabPanel('Štampoch',
            fluidRow(column(12,
              uiOutput('ps21')))),
          tabPanel('Veľká Richňava',
            fluidRow(column(12,
              uiOutput('ps22')))),
          tabPanel('Veľká Vindšachta',
            fluidRow(column(12,
              uiOutput('ps23')))),
          
          tabPanel('Velká Vodárenská',
            fluidRow(column(12,
              uiOutput('ps24')))),
          tabPanel('Velký Kolpašský',
            fluidRow(column(12,
              uiOutput('ps25'))))
          
          
        ))
      
      # actionButton("CR1_S1", "Button"),
      # bsModal("modalExample", "Your Table", "CR1_S1", size = "large", uiOutput("mytable")),
      # slickROutput("slickr", width="500px")
    ),
    
    tabItem(tabName = "zapojsa",
      
      box(width=12,
        title = "ZAPOJ SA!",
        
        
        # O Tajcharovi ------------------------------------------------------------
        tags$div(class = "text1",
          tags$b("DO YOU KNOW TAJCHS OF BANSKÁ ŠTIAVNICA?")),
        
        tags$div(class = "text1",
          "Help improve this app. Share your", tags$b("FAVORITE RESERVOIR, the BEST BEACH or BUFFET"), "with others", " Fill out the questionnaire and become a co-author of the thematic maps in the ", tags$b("Opinion Maps category.")),
        
        tags$img(class="znamka", src="dotaznik.png"),
        tags$div(class = "text3",
          "DOTAZNÍK"),
        
        
        h6(textOutput("save.results")),
        # h5("Created by:"),
        # tags$a("Econometrics by Simulation", 
        # href="http://www.econometricsbysimulation.com"),
        # h5("For details on how data is generated:"),
        # tags$a("Blog Post", 
        #        href=paste0("http://www.econometricsbysimulation.com/",
        #                    "2013/19/Shiny-Survey-Tool.html")),
        # h5("Github Repository:"),
        # tags$a("Survey-Tool", 
        #        href=paste0("https://github.com/EconometricsBySimulation/",
        #                    "Shiny-Demos/tree/master/Survey")),
        # Display the page counter text.
        tags$div(class="stred",textOutput("counter")),
        # tags$br(),
        
        tags$div(class="stred",uiOutput("MainAction")),
        # This displays the action putton Next.
        # tags$br(),
        actionButton(class="nastred2","Click.Counter", "NEXT"),
        tags$br()
        
      ))
    
    
  ) # end:tabItem(tabName = "grafy"
)  # end:dashboardBody



# DASHBOARD UI ------------------------------------------------------------
ui <- dashboardPage(title="Tajchár | Your guide to the Tajchs of Banská Štiavnica",
  
  
  skin = "black",
  header = header,
  sidebar = sidebar,
  body = body)
