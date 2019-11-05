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

source('src/lightbox.R')
source('src/photoswipe.R')


# GOOGLE SHEETS
# Google authenticate via Token
gs_auth(token = "gdrive_token.rds")
# register a sheet in your own drive via its title
gsheet <- gs_title("ulozDotaznik")
# read data
load_gsheet <- gs_read(gsheet, ws="ulozDotaznik") # as tibble




images <<- data.frame(src = list.files('www/img')) %>%
  tidyr::separate(col = 'src', c('txt', 'date', 'time', 'msec'), sep = '_|\\.', remove = FALSE) %>%
  rowwise() %>%
  mutate(date = lubridate::ymd(date),
         key = hashids::encode(1e3 + as.integer(msec), hashid_settings(salt = 'this is my salt')))

tajch="Bakomi"




# ipAdress for.. geoloc or unique user identification             ...? 
ipAdresy <- "empty"
# ipAdresy <- append(ipAdresy, "127.0.0.1:5031")

# csv file with questions for questionnaire
Qlist <- read_excel("Qlist.xlsx")
Qlist <- as.data.frame(Qlist)
Qlist[is.na(Qlist)] <- ""


df.presults <- load_gsheet
# df.presults <- read.csv("ulozDotaznik.csv", header = F, stringsAsFactors = FALSE)[-1, ]

mat.presults <- as.matrix(df.presults)
colnames(mat.presults) <- c( "Mojim najobľúbenejším tajchom je :",
  "Na oddych pri tajchu a kúpanie je najlepšie vybavené okolie tajchu:",
  "Najlepší bufet má jednoznačne:",
  "Najteplejšia voda je podľa mňa v tajchu:",
  "Najviac znečistené je podľa mňa okolie tajchu:")
# df.presults <- read.csv("ulozDotaznik.csv", row.names=1, stringsAsFactors = FALSE)
#
# load("survey.results.Rdata")
# df.presults <- as.data.frame(presults)
# df.presults[6] <- NULL
names(df.presults) <- c("oblubenost", "vybava", "bufet", "teplota", "smeti")
# INPUT DATA -----------------------------------------------------------
# lsenv <- ls(pattern= "shp", all.names = TRUE)
# if (!length(lsenv)) {
#   
#   # load shp
shps <- dir(getwd(), "*.shp")
shps <- substr(shps, 1, nchar(shps)-4)
for (shp in shps) assign(shp, readOGR(".", shp, dropNULLGeometries=TRUE, encoding = "UTF-8", use_iconv = TRUE))

# zjednodusenie bohuzial zmaze polozky s diakritikou
# object.size(shpTajchyFS)
# shpTajchy <- rmapshaper::ms_simplify(shpTajchyFS, weighting=0.5, keep=0.4)
# object.size(shpTajchy)
# docasne -----------------------------------------------------------------
# shpTajchy <- readOGR(".",'tajchy', dropNULLGeometries=TRUE, encoding = "UTF-8", use_iconv = TRUE)

# set CRS
shpTajchy <- spTransform(shpTajchy, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shpTajchyHist <- spTransform(shpTajchyHist, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# shpTajchyIdea <- spTransform(shpTajchyIdea, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shpJarky <- spTransform(shpJarky, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# shpStolneVodne <- spTransform(shpStolneVodne, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# shpStolne <- spTransform(shpStolne, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# shpPingy <- spTransform(shpPingy, CRS("+proj=longlat +datum=WGS84 +no_defs"))



shpTajchy$vznikNum <- as.numeric(as.character(shpTajchy$vznik))
# shpTajchy$vznik <- zoo::as.Date(zoo::as.yearmon(shpTajchy$vznik, "%Y"), origin = "1960-01-01")

shpTajchy$objemM3 <-  varhandle::unfactor(shpTajchy$objemM3)
shpTajchy$dlzkaZbJrk <-  varhandle::unfactor(shpTajchy$dlzkaZbJrk)
shpTajchy$dlzkaStoln <-  varhandle::unfactor(shpTajchy$dlzkaStoln)
shpTajchy$vznik <-  varhandle::unfactor(shpTajchy$vznik)

dtTajchy <- data.table::setDT(as.data.frame(shpTajchy))
nameTajchy <- names(dtTajchy)

dtTajchy$x <- round(dtTajchy$x, 3)
dtTajchy$y <- round(dtTajchy$y, 3)

pikosky <- read_excel("pikosky.xlsx", col_names = FALSE)

# dtTajchy$objemM3 <-  varhandle::unfactor(dtTajchy$objemM3)
# dtTajchy$dlzkaZbJrk <-  varhandle::unfactor(dtTajchy$dlzkaZbJrk)
# dtTajchy$dlzkaStoln <-  varhandle::unfactor(dtTajchy$dlzkaStoln)

# 
infoObsah <- c("Názov tajchu" = "name", # 1
  "Skupina" = "skupina", # 2
  "Rok vzniku" = "vznik", # 3
  "Plocha vodnej hladiny" = "plocha", # 4
  "Zemepisné súradnice" = "xy", # 5
  "Historický názov" = "ineMeno", # 6
  "Staviteľ" = "stavitel", # 7
  "Materiál hrádze" = "matHradze", # 8
  "Bufet" = "bufet", # 9
  "Dĺžka koruny hrádze" = "dlzkaKorun", # 10
  "Výška hrádze" = "vyskaHradz", # 11
  "Povodie" = "povodie", # 12
  "Objem v m3" = "objemM3", # 13
  "Nadmorská výška" = "nvMNM", # 14
  "Šírka koruny hrádze" = "sirkaKor", # 15
  "Kultúrna pamiatka" = "pamiatka", # 16
  "Dĺžka zberných jarkov"= "dlzkaZbJrk", # 17
  "Dĺžka vodných štôlní" = "dlzkaStoln", # 18
  "Rekonštrukcie" = "rekonstr", # 19
  "Maximálna hĺbka" = "maxHlbk", # 20
  "Zaujímavosť" = "info", # 21
  "Druhové zastúpenie rýb" = "druhZstRyb", # 22
  "Revír" = "revir", # 23
  "Priemerná teplota vody" = "prTvpdy", # 24
  "Možnosť kúpania" = "kupanie",# 25
  "Fotografie" = "foto",# 26
  "Zápis v ICOLD" = "icold", # 27
  "Názvy tajchov" = "empty", #28
  "Hlavný účel" = "hlavnyUcel", #29
  "Vedľajší účel" = "vedlUcel", #30
  "Najobľúbenejší tajch" = "oblubenostNum", #31
  "Najlepšie vybavenie" = "vybavaNum", #32
  "Najlepší bufet"="bufetNum",#33
  "Najteplejšia voda"="teplotaNum",#34
  "Znečistené okolie"="smetiNum" #35
) 


infoList <- list("infoHist" = infoObsah[c(28,2,3,6,7, 29, 30)],
  "infoTech" = infoObsah[c(12,14,13,8,11,17,20,10,15,4,5)],
  "infoDnes" = infoObsah[c(26,16,27,25,23,22,9)],
  "nazory" = infoObsah[c(31:35)])
# } 


ciselne <- c('dlzkaKorun','vznik','nvMNM','objemM3','vyskaHradz','dlzkaKorun','sirkaKor','plocha')
  
# Vlastne IKONY ------------------------------------------------------------
i.food = makeIcon("food.svg", iconWidth = 32, iconHeight = 32)
i.photo = makeIcon("photo.svg", iconWidth = 30, iconHeight = 30)
i.swim = makeIcon("swim.svg", iconWidth = 35, iconHeight = 35)
# i.swim.link = makeIcon("swim.link.svg", iconWidth = 35, iconHeight = 35)
i.antique = makeIcon("antique.svg", iconWidth = 25, iconHeight = 25)
i.ok = makeIcon("ok.svg", iconWidth = 30, iconHeight = 30)
i.temp = makeIcon("temp.png", iconWidth = 30, iconHeight = 30)
i.fish = makeIcon("fish.svg", iconWidth = 30, iconHeight = 30)
i.info = makeIcon("info.svg", iconWidth = 15, iconHeight = 15)
i.bar = makeIcon("bar-chart.svg", iconWidth = 15, iconHeight = 15)
# i.revir = makeIcon("revir.svg", iconWidth = 30, iconHeight = 30)


revirIcons <- iconList(
  revir = makeIcon("revir.svg", 24, 24),
  revir0 = makeIcon("revir0.svg", 24, 24)
)


iconSet = pulseIconList(
  red = makePulseIcon(color = "#ff0000"),
  blue = makePulseIcon(color = "#0000ff")
)

iconSet[c("red", "blue")]
# options(warn = 1) 

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}
