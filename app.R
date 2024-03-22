library(shiny)
library(ggplot2)
library(sf)
library(dplyr)

# Örnek olarak, 'veriniz' yerine gerçek sf veri setinizi yükleyin
veriniz <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/json/gadm41_TUR_1.json")


ui <- fluidPage(
  titlePanel("2024 Yerel Seçimler Tahmin Haritası"),
  sidebarLayout(
    sidebarPanel(
      selectInput("il", "İl Seçiniz", choices = veriniz$NAME_1),
      selectInput("parti", "Parti Seçiniz", choices = c("CHP" = "darkred", 
                                                        "AKP" = "darkorange", 
                                                        "MHP" = "blue",
                                                        "İYİP" = "steelblue",
                                                        "YRP" = "pink",
                                                        "DEM" = "purple",
                                                        "TİP" = "brown",
                                                        "Diğer"="grey")),
      textInput("isimsoyisim","Haritayı Hazırlayan"),
      actionButton("btnEkle", "İl Ekle"),
      hr(),
      uiOutput("partiRenkleri"),
      downloadButton("indirButon", "Haritayı PNG olarak indir")
    ),
    mainPanel(
      plotOutput("harita")
    )
  ),
  tags$footer(
    style = "margin-top: 20px; text-align: center; width: 100%;",
    tags$hr(),
    "Ozancan Özdemir (c) Powered by R Shiny"
  )
)

server <- function(input, output) {
  secilenler <- reactiveVal(data.frame(il = character(), color = character()))
  
  observeEvent(input$btnEkle, {
    yeni_secim <- data.frame(il = input$il, color = input$parti)
    mevcut_secimler <- secilenler()
    secilenler(rbind(mevcut_secimler, yeni_secim))
  })
  
  output$harita <- renderPlot({
    mevcut_secimler <- secilenler()
    
    harita_data <- veriniz %>%
      mutate(renk = ifelse(NAME_1 %in% mevcut_secimler$il, mevcut_secimler$color[match(NAME_1, mevcut_secimler$il)], "grey")) 
    
    ggplot(data = harita_data) +
      geom_sf(aes(fill = renk)) +
      theme_void() +
      labs(title = "2024 Yerel Seçim Tahmin Haritası", fill = "İl",caption = "@OzancanOzdemir",
           subtitle = paste("Haritayı Hazırlayan:",input$isimsoyisim)) + theme(plot.title = element_text(size = 18,face="bold"),
                                                                               plot.subtitle = element_text(size = 15,face="bold"),
                                                                               plot.caption = element_text(size = 15,face="bold"))+
      scale_fill_identity()
  })
  
  
  output$partiRenkleri <- renderUI({
    partiAdlari <- c("CHP", "AKP", "MHP", "İYİP", "YRP", "DEM", "TİP", "Diğer")
    renkler <- c("darkred", "darkorange", "blue", "steelblue", "pink", "purple", "brown", "grey")
    tags$div(style = "display: flex; flex-wrap: wrap; align-items: center;",
             lapply(1:length(partiAdlari), function(i) {
               tags$div(style = "margin: 5px; display: flex; align-items: center;",
                        tags$div(style = sprintf("margin-right: 5px; background-color: %s; border-radius: 50%%; width: 20px; height: 20px;", renkler[i])),
                        tags$span(partiAdlari[i])
               )
             })
    )
  })
  
  output$indirButon <- downloadHandler(
    filename = function() {
      paste("harita-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, plot = grafikNesnesi(), device = "png", width = 10, height = 8)
    }
  )
}
  
  

shinyApp(ui = ui, server = server)
