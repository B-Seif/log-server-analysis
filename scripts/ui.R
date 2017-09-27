
library(shiny)



fluidPage(
  

  
  titlePanel("Log Analysis"),
  
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      radioButtons(inputId = "protocole",label = "Protocol",choices = list("TCP"="TCP","UDP"="UDP","TCP & UDP"="ALL")) , 
      
      radioButtons(inputId = "ports",label = "Ports",choices = list("Inf 1024"="Inf1024","Sup 1024"="Sup1024","All"="Tous")) , 
      
      selectInput(inputId =   "nbr_port", label="Nombre de ports a considerer dans l'histogramme",selected= "10",choices = as.character(seq(1, length(unique(src_fw$portdst))   ))),
      
      checkboxInput(inputId = "ordre", label="Most used Ports",value=TRUE),
      
      sliderInput(inputId  = "parcours",label="Browse",value=52,min=1,max=dim(traitement)[1]),
      
      selectInput(inputId =   "choix_ip", label=" Select IP", choices =  as.character(unique(src_fw$ipdst)),selected="156.84.20.252"  ),
      
      selectInput(inputId =   "method", label="CAH link method", choices = list("complete","ward.D2","ward.D","single","average")  ),
      
      selectInput(inputId =   "class", label="Number of class",choices = as.character(seq(2,10)), selected="4"),
      
      checkboxInput(inputId = "encadrer", label="Frame ?",value=TRUE),
      
      checkboxInput(inputId = "inertie", label="Inertia",value=TRUE)
      
    ),
    
    
    
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Synthese des flux",plotOutput("pie"),plotOutput("barplotPort"),plotOutput("permitVSdeny")),
        
        tabPanel("Visualition donnees brutes",dataTableOutput("donnees")),
        
        tabPanel("Parcourir",plotOutput("seBalader")),
        
        tabPanel("CAH sur serveur principal",plotOutput("cah"),tableOutput("tablebrute"))
        
      )
      
      
    )
    
  )
  
  
  
  
  
)