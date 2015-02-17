library(shiny)
library(forecast)



source("auction.R")

shinyUI(pageWithSidebar(
  headerPanel(span("NetBooster Enchere  estimation",style="color:pink")),
  
  sidebarPanel(
    
    selectInput("regle", label = strong("Choisissez le regle:"),choices=list("max. revenu","min. cost","max. clicks","max. impressions","max. conversions"),selected = "max. revenu"),
    br(),
    numericInput("niveau",label=strong("Niveau de group:"),
                 value=0.25,min=0,max=1),
    br(),
    selectInput("months", "Choissez le mois", 
                choices=list("Janvier","Fevrier","Mars","Avril","Mai","Juin"),selected = "Janvier"),
    
    selectInput("type", "Choissez le type de correspondance", 
                choices=list("Exact","Expression","Large"),selected = "Exact"),
    selectInput("devise", "Choissez le devise", 
                choices=list("EUR","GBP"),selected = "EUR"),
    selectInput("appar", "Choissez le appareil", 
                choices=list("Mobiles dotÃ©s d'un navigateur Internet complet","Ordinateurs","Tablettes dotÃ©es d'un navigateur Internet complet"),selected = "Ordinateurs"),
    checkboxInput(inputId = "position",
                  label = strong("Changer le position"),
                  value = FALSE),
    uiOutput("pos")
    
    
  ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Group",dataTableOutput("look")),
      tabPanel("Est",dataTableOutput("lk"))
      
    )
    
    
    
    
    
    
  )
))
