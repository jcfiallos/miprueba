
library(shiny)
library(dygraphs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage(
    theme = "cerulean",  # <--- To use a theme, uncomment this
    "Aplicacion para Series de Tiempo",
    tabPanel("Cargar Serie",
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Elija Archivo',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 checkboxInput('header', 'Cabecera', FALSE),
                 selectInput('sep', 'Separador', 
                             c('Coma'=',',
                               'Punto y coma' =';',
                               'Tabulador' ='\t'),
                             '\t'),
                 selectInput('deci', 'Separador decimal',
                             c('Coma'=",",
                               'Punto'="."),
                             ","),
                 hr(),
                 
                 selectInput('frec', 'Periodicidad:',
                             c('Diaria'="days",
                               'Mensual'="month",
                               'Trimestral'="quarter",
                               'Anual'="years"),
                             "Diario"),
                 dateInput("fecha", "Fecha Inicio:", value = "2017-09-29"),
                 div(strong("Desde: "), textOutput("from", inline = TRUE)),
                 div(strong("Hasta: "), textOutput("to", inline = TRUE)),
                 hr(),
                 selectInput('xcol', 'X Variable', "")
                 
               ),
               mainPanel(
                 dygraphOutput("dygraph")
               )
             )
             
    ),
    tabPanel("Analisis Inicial", 
             tabsetPanel(type = "tabs",
                         tabPanel("LM", sidebarPanel(
                           radioButtons("tipo", "Tipo:",
                                        c("Aditivo" = "Aditivos",
                                          "Multiplicativo" = "Multiplicativos"), "Aditivos"),
                           radioButtons("metodo", "Grado:",
                                        c("Lineal" = "Lineal",
                                          "Cuadratico" = "Cuadratico",
                                          "Cubico" = "Cubico",
                                          "Orden 4" = "Orden 4"), "Lineal"),
                           checkboxInput("est", "Estacionaria", FALSE),
                           
                           br(),
                           
                           checkboxInput("show.resid", "Show residuals", FALSE),
                           
                           br(),
                           
                           helpText("Texto de ayuda."),
                           br()),
                           mainPanel(
                             plotOutput("scatter"),
                             br(),
                             br(),
                             plotOutput("residuals")
                             
                           )),
                         tabPanel("Holt-Winter", sidebarPanel(
                           radioButtons("tipohw", "Tipo:",
                                        c("Aditivo" = "Aditivos",
                                          "Multiplicativo" = "Multiplicativos"), "Aditivos"),
                           
                           br(),
                           
                           checkboxInput("show.resid", "Show residuals", FALSE),
                           
                           br(),
                           
                           helpText("Texto de ayuda."),
                           br()),
                           mainPanel(
                           plotOutput("scatter2"),
                           br(),
                           br(),
                           plotOutput("residuals2")
                           
                            )),
                         tabPanel("Descomposicion y LOESS", sidebarPanel(
                           radioButtons("tipolo", "LOESS:",
                                        c("Lineal" = "Lineal",
                                          "Cuadratico" = "Cuadratico"), "Lineal"),
                           radioButtons("tipob", "Descomposicion:",
                                        c("Aditivo" = "Aditivos",
                                          "Multiplicativo" = "Multiplicativos"), "Aditivos"),
                           
                           br(),
                           
                           checkboxInput("show.resid", "Show residuals", FALSE),
                           
                           br(),
                           
                           helpText("Texto de ayuda."),
                           br()),
                           mainPanel(
                             plotOutput("scatter3"),
                             br(),
                             br(),
                             plotOutput("residuals3")
                                     
                                     
                           ))
             )
    ),
    tabPanel("Resultados", "Este panel muestra los resultados")
  )
))
