#-----------------#
# 0. Settings ####
#-----------------#

# R Version 4.3.3
R.Version()

# clean environment
rm(list = ls())

# adjust local settings to support german umlauts
Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "de_DE.UTF-8") 


# load packages
packages <- c("tidyverse", "tidyr", "dplyr", "stringr", "shiny", "shinydashboard", "readxl", "sf", "leaflet")

lapply(packages, function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
  library(x, character.only = TRUE)
})

rm(packages)


#----------------------------------#
# 1. load and prepare datasets ####
#----------------------------------#

#---------------------------#
#* 1.1 Datensätze laden ####
#---------------------------#


path.data <- "C:/Users/jonas/sciebo2/Persönlich/Bewerbung/Krebsregister/"

# Datensatz bösartige Neubildungen pro Bundesland
bn <- read.csv(paste0(path.data,"BN.csv"), skip = 5, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")

# Datensatz Einwohner pro Bundesland 
einwohner <- read.csv(paste0(path.data,"Einwohner.csv"), skip = 5, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")

# Datensatz shapefiles Bundesländer
sf <- st_read(paste0(path.data,"Shapefiles/vg2500_bld.shp"), options = "ENCODING=ISO-8859-1")

#sf_land$GEN <- iconv(sf_land$GEN, "ISO-8859-1", "UTF-8")


#--------------------------------#
#* 1.2 Datensätze bearbeiten ####
#--------------------------------#

# BN bearbeiten
bn <- bn %>%
  select(!X.1) %>% 
  rename(Jahr = X,
         BN = X.2) %>%
  slice(1:(n() - 3)) %>% # letzten 3 Zeilen löschen
  pivot_longer(cols = -c(Jahr, BN), names_to = "Bundesland", values_to = "Fälle") %>% # Datensatz in long format umwandeln 
  filter(BN != "Insgesamt") %>% # Zeilen mit Gesamtsummen löschen
  mutate(Fälle = na_if(Fälle, "-")) %>% # NAs löschen
  drop_na(Fälle) %>%
  mutate(Fälle = as.numeric(Fälle),
         Bundesland = str_replace(Bundesland, "\\.", "-"))
  

# sf bearbeiten
sf <- sf %>%
  select(GEN, geometry) %>%
  rename(Bundesland = GEN)

ggplot(sf) + geom_sf() # mit geom_sf können shape-files mit ggplot herausgegeben werden


# einwohner bearbeiten
einwohner <- einwohner %>%
  rename(Jahr = X) %>%
  slice(1:(n() - 3)) %>%
  pivot_longer(cols = -Jahr, names_to = "Bundesland", values_to = "Einwohner") %>%
  mutate(Jahr = substr(Jahr, 7, 10),
         Bundesland = str_replace(Bundesland, "\\.", "-"))


#----------------------------#
#* 1.3 Datensätze mergen ####
#----------------------------#

data <- bn %>%
  left_join(einwohner, by = c("Jahr", "Bundesland")) %>%
  mutate(bn_per_capita = Fälle/Einwohner) %>%
  left_join(sf) %>%
  mutate(Jahr = as.numeric(Jahr))

data <- st_as_sf(data)

saveRDS(data, paste0(path.data,"data.rds"))

rm(bn)


#--------------#
# 2. Shiny ####
#--------------#

ui <- dashboardPage(
  
  dashboardHeader(title = "Krankenhausstatistik: Entlassene Patienten - Bösartige Neubildungen"),
  
  dashboardSidebar(
    selectInput("bn", "Art der bösartigen Neubildung", choices = unique(data$BN)),
    sliderInput("jahr", "Jahr", min = min(data$Jahr), max = max(data$Jahr), value = min(data$Jahr))
  ), 
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #bn_map { height: calc(100vh - 80px); }
        .leaflet-control-attribution, .leaflet-control-layers {
          display: none !important;
        }
      "))
    ),
    leafletOutput("bn_map", height = "95vh")
  )
)





server <- function(input, output, session) {
  
  data1 <- reactive({
    data %>%
      filter(BN == input$bn, Jahr == input$jahr)
  })
  
  output$bn_map <- renderLeaflet({
    
    # Zugriff auf die reaktiven Daten
    leaflet(data1()) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", bn_per_capita)(bn_per_capita),
        fillOpacity = 0.7,
        color = "#444444",
        weight = 0.5,
        smoothFactor = 0.5,
        label = ~paste(Bundesland, format(bn_per_capita, scientific = FALSE)),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      ) %>%
      addControl("Deutsche Bundesländer", position = "topright")
  })
  
  observe({
    leafletProxy("bn_map", data = data1()) %>%
      setView(lng = 10.5, lat = 51.2, zoom = 6) %>%
      setMaxBounds(lng1 = 5.5, lat1 = 47, lng2 = 15, lat2 = 55) # Begrenzen Sie die Ansicht auf Deutschland
  })
}

# ...

# ...



shinyApp(ui, server)

rsconnect::deployApp('C:/Users/jonas/Desktop/app')