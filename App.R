#install.packages('shinydashboard')
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(DT)
library(dplyr)
library(leaflet)
library(rgdal)
library(geojsonio)
library(sp)
library(RColorBrewer)
library(shinyWidgets)
library(dashboardthemes)


#ui
ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = tags$a(href='www',
                   tags$img(src="logo1.png", height='30', width='150'),
                   '.'),
  titleWidth = 200,
  tags$li(class="dropdown",tags$a(href="https://api.whatsapp.com/send?phone=6282259709316&text=Halo!%0ASaya%20tertarik%20dengan%20jasa%20anda:D",
                                  icon("whatsapp"), "Hubungi Saya", target="_blank")),
  enable_rightsidebar = F,
  rightSidebarIcon = "gears"
),

dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem('Dashboard', tabName = 'menu1', icon = icon("area-chart")),
    menuItem('Lihat Data', tabName = 'menu2', icon = icon("download")),
    menuItem('About', tabName = 'menu3', icon = icon("users"))
  )
),

#body
dashboardBody(
  shinyDashboardThemes(
    theme = "poor_mans_flatly"),
  setBackgroundColor(
    color = "white",
    shinydashboard = T
  ),
  fluidRow(
    # A static infoBox
    infoBox("Sumber Data :","Sensus Pertanian 2013", icon = icon("credit-card"), color = "olive", width = 6),
    infoBox("Progres","35%", icon = icon("thumbs-up", lib = "glyphicon"), color = "olive", width = 6),
    
    ),
  tabItems(
    #tab_pertama
    tabItem(
      tabName = "menu1",
      fluidRow(
        box(width = 4,
            selectInput('tingkat_wil', label = 'Tingkat Wilayah',
                        choices = list('Provinsi' = 1, 'Kabupaten' = 2), selected = 2)
        ),
        box(width = 12,
            leafletOutput('pt')
      ),
      
      fluidRow(
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Aceh",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig1')),
        
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Sumatera Utara",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig2')),
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Sumatera Barat",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig3'))
        
      ),
      fluidRow(
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Riau",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig4')),
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Jambi",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig5')),
        box(width = 4,
            title = "Perbandingan Satwa yang Telah diburu Secara Liar dan Satwa yang Masuk Penangkaran di Sumatera Selatan",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "primary",
            plotlyOutput('fig6'))
      ),
      
      fluidRow(
        box( title = "ket : semakin besar lingkaran, maka populasi satwa semakin menurun.")
        
      ),
      
      )
    ),
    
    #tab_kedua
    tabItem(
      tabName = "menu2",
        selectInput('tingkat_wil_tabel', label = 'Tingkat Wilayah',
                    choices = list('Provinsi' = 1, 'Kabupaten' = 2), selected = 2,
        ),
        dataTableOutput('tabel_data')
    ),
    
    #tab_ketiga
    tabItem(
      tabName = "menu3",
      tabPanel(id="tentang_saya","Tentang Saya",
               includeHTML("www/about.html"))
    )
  )
),
rightsidebar = NULL,
footer = dashboardFooter(
  left_text = "Made with ð by rohisrachman | Support KERABAT DATA MINICLASS ð§¡",
  right_text = "Palu, 2020"
),
title = "leaflife"

)

server <- function(input, output, session) { 
  
    
  #data peta
  
    #provinsi
  ur1 <- readOGR('data/indonesia_provinces_ClipToShore_simplified.geojson')
  ur <- read.csv('data/prov_kht.csv',sep = ';', stringsAsFactors = T)
  
  ur1@data<-inner_join(ur1@data,ur, by='provinsi')
  ur1@data$provinsi <- factor(ur1@data$provinsi)
  
  #function fig
  fig <- function(data) {
    plot_ly(data, x = ~satwa, y = ~tangkaran, 
            text = ~kota, type = 'scatter', mode = 'markers',
            marker = list(size = ~fig, opacity = 0.5))
  }

    #kabupaten
  kabsatwa <- read.csv('data/kab_satwa.csv', sep = ";", stringsAsFactors = T)
  output$pt <- renderLeaflet({
    
    if(input$tingkat_wil == 1){
    #peta_provinsi
      labels <- sprintf("Provinsi: %s <br/> Satwa yang di tangkap secara ilegal: %s",
                        ur1@data$provinsi,
                        ur1@data$Penangkapan.Satwa.Liar
      ) %>% 
        lapply(htmltools::HTML)
      
      
      labels
      bins <- c(0, 100, 500, 1000, 5000, 10000, 20000, Inf)
      pal <- colorBin("YlOrRd", domain = ur1@data$Penangkapan.Satwa.Liar, bins = bins)
      pal
      
      b <- leaflet(ur1) %>%
        addTiles() %>%
        addPolygons()
      
      b <- b %>% addPolygons(
        fillColor = ~pal(ur1@data$Penangkapan.Satwa.Liar),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))
      b <- b %>%
        addLegend("bottomright", pal = pal, values = ~ur1@data$Penangkapan.Satwa.Liar,
                  title = "Penangkapan Satwa Liar",
                  opacity = 1)
      b
    
    } else {
      labels_kabsatwa <- sprintf("Kabupaten : %s <br/> satwa yang diburu secara liar: %s",
                                 kabsatwa$Nama.Kabupaten.Kota, kabsatwa$Penangkapan.Satwa.Liar
                                 
      ) %>% 
        lapply(htmltools::HTML)
      
      leaflet(kabsatwa) %>%
        addTiles() %>%
        addAwesomeMarkers (lng = ~longitude,
                           lat = ~latitude,
                           label = labels_kabsatwa)
    }
  })
  output$fig1 <- renderPlotly({
    Aceh <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    aceh <- Aceh %>%
      filter(provinsi == 'Aceh')
    
    fig(aceh)
  })
  
  output$fig2 <- renderPlotly({
    Sumut <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    sumut <- Sumut %>%
      filter(provinsi == 'Sumatera Utara')
    
    fig(sumut)
  })

  output$fig3 <- renderPlotly({
    Sumbar <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    sumbar <- Sumbar %>%
      filter(provinsi == 'Sumatera Barat')
    
    fig(sumbar)
  })
  output$fig4<- renderPlotly({
    Riau <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    riau <- Riau %>%
      filter(provinsi == 'Riau')
    
    fig(riau)
  })
  
  output$fig5 <- renderPlotly({
    Jambi <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    jambi <- Jambi %>%
      filter(provinsi == 'Jambi')
    
    fig(jambi)
  })
  
  output$fig6 <- renderPlotly({
    Sumsel <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    sumsel <- Sumsel %>%
      filter(provinsi == 'Sumatera Selatan')
    
    fig(sumsel)
  })
  
  output$tabel_data <- renderDataTable({
    
    if(input$tingkat_wil_tabel == 1){
      data_tabel <- read.csv('data/prov_kht.csv',sep = ';', stringsAsFactors = T)
    } else {
      data_tabel <- read.csv('data/Full.csv',sep = ';', stringsAsFactors = T)
    }
    
    datatable(data_tabel, filter = 'top',
              options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)

