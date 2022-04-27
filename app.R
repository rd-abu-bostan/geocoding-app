library(shiny)
library(shinyjs)
library(shinycssloaders)
library(tidygeocoder)
library(tidyverse)
library(readxl)
library(writexl)




# Define UI for data upload app ----
ui <- fluidPage(
  
  useShinyjs(),  # Include shinyjs
  
  
  
  
  
  # App title ----
  titlePanel(h1("Geocoder", align="center"), windowTitle="Geocoder"),
  
  
  
  tags$b(
    class = "glyphicon glyphicon-info-sign", 
    style = "color:#0072B2;font-size:25px;",
    
  ),
  
  (h6("Achtung: Die Excel Datei muss folgende Felder beinhalten: LIEF_STR, LIEF_PLZ, LIEF_ORT ", align="left", style="color:red")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Lade eine Excel Datei hoch: ",
                multiple = FALSE,
                accept = c(".xlsx")),
      
      # Horizontal line ----
      tags$hr(),
      
      downloadButton("downloadButton", "Download",class = "btn-success"),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      withSpinner(tableOutput("contents"))
      
    )
    
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  
  #Einlesen der Excel Datei
  dataLoad<-reactive({ 
    
    
    req(input$file1)
    dat<-read_excel(input$file1$datapath, sheet =  1)
    
    
    return(dat)
  })
  
  
  #Die benötigten Spalten werden an den Server gesendet, Daten empfangen 
  #und ein neues Df geschrieben
  dataSet<-reactive({ 
    
    df <- dataLoad()
    
    
    if("LIEF_STR" %in% colnames(df) && "LIEF_PLZ" %in% colnames(df) && "LIEF_ORT" %in% colnames(df)){
      
      #Concatenate Strassenname + PLZ + Ort in 1 String
      concat_adress <- unite(df,new_adress, LIEF_STR, LIEF_PLZ,LIEF_ORT,sep=",")
      
      #Google Maps Api Requests
      adress_with_coordinates <- concat_adress %>% geocode(address=new_adress, method = "google", verbose = FALSE)
      
      #Formattieren der Nachkommstellen bei  Longitude, Latitude
      adress_with_coordinates$LATITUDE <- format(adress_with_coordinates$lat, digits = 9)
      adress_with_coordinates$LONGITUDE <- format(adress_with_coordinates$long, digits = 9)
      
      #Originaldatei + die neuen Spalten mit den Koordinaten in einen neuen Data Frame mergen
      sep_df1 <- df
      sep_df2 <- adress_with_coordinates %>% select(tail(names(.), 2))
      sep_df <- data.frame(sep_df1,sep_df2)
      
      return(sep_df)
      
    }
    
    else{
      return ("DIE ERFORDERLICHEN FELDER WURDEN NICHT GEFUNDEN! ")
    }
    
  })
  
  #Hauptanzeige 
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. 
    req(input$file1)
    
    return(dataSet())
    
  })
  
  
  # DownloadHandler
  # Der neue DataFrame inkl. Koordinaten wird in eine xlsx Datei geschrieben
  # und ein Fenster popt auf
  output$downloadButton <- downloadHandler(
    
    filename = function() {
      #paste("data.xlsx")
      paste(input$file1$name)
    },
    content = function(file) {
      write_xlsx(dataSet(), file)
    }
  )
  
  
  #Observer - Anzeigen des Downloadbutton
  #Button wird aktiv falls eine Datei mit der Endung xlsx hochgeladen wird
  observe({
    
    if (is.null(input$file1)) {
      disable("downloadButton")
    }
    else if( tools::file_ext(input$file1$datapath) != "xlsx"){
      disable("downloadButton")
    }
    else {
      enable("downloadButton")
    }
    
  })

}

# Create Shiny app ----
shinyApp(ui, server)
