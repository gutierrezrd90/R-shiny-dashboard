library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)

ui <- 
  
  fluidPage(
    
    dashboardPage(skin = "green",
                  
                  dashboardHeader(title = "Shiny App team 16", titleWidth = 300),
                  
                  dashboardSidebar( 
                    
                    sidebarMenu(
                      menuItem("Primera", tabName = "Primera", icon = icon("r")),
                      menuItem("Desglose de goles por equipo", tabName = "Dashboard", icon = icon("futbol")),
                      menuItem("Probabilidad de goles casa - visitante", tabName = "goles", icon = icon("chart-column")),
                      menuItem("Tabla de datos", tabName = "data_table", icon = icon("table")),
                      menuItem("Factores de ganancia", tabName = "momios", icon = icon("chart-line")),
                      menuItem("Integrantes del equipo", tabName = "info", icon = icon("list"))
                    ),
                    width = 300
                    
                  ),
                  
                  dashboardBody(
                    
                    tabItems(
                      
                      # Introducción
                      tabItem(tabName = "Primera",
                              fluidRow(
                                
                                titlePanel("Introducción"),
                                mainPanel(
                                  h3("Dashboard del proyecto final"),
                                  p("¡Bienvenid@! Este dashboard es el proyecto final del equipo 16 del módulo de Estadística y Programación con R."),
                                  p("En éste se presenta un trabajo basado en un conjunto de datos recopilados de los resultados de los partidos de la Liga Española de Fútbol, desde el año 2010 al 2020."),
                                  img(src="r.png", height = "280", width = "420", align = "left"),
                                  img(src="bedu.png", height = "280", width = "420", align = "right")
                                )
                              )
                      ),
                      
                      # Histograma
                      tabItem(tabName = "Dashboard",
                              fluidRow(
                                
                                titlePanel("Goles a favor y en contra por equipo"),
                                mainPanel(p("En esta pestaña puedes encontrar los resultados obtenidos por los equipos de 
                                            la Liga Española durante las temporadas del año 2010 al año 2020."),
                                          p("Las barras de color azul indican los goles como equipo local, las barras verdes indican empates y las barras rojas indican los goles como equipo visitante."),
                                          p("Utiliza el botón desplegable para cambiar entre los resultados como local y como visitante.")),
                                selectInput("x", "Seleccione el valor de X",
                                            choices = c("home.score", "away.score")),
                                
                                
                                plotOutput("plot1", height = 550, width = 1200),
                                
                                p("FTR = Resultado final (H = Victoria de local, D = Empate, A = Victoria
                                  de visitante)"),
                              )
                      ),
                      
                      # imágenes
                      tabItem(tabName = "goles",
                              fluidRow(
                                titlePanel(h3("Probabilidad de goles de local y visitante")),
                                p("Se muestran las probabilidades de anotar determinada cantidad de goles
                                  en condición de local o visitante."),
                                h4("Probabilidad de que el equipo que juega de local anote
                                  'x' goles (x = 0, 1, 2, ...)."),
                                img(src = "casa.png",
                                    height = 550, width = 750),
                                h4("Probabilidad de que el equipo que juega de visitante anote 'y'
                                  goles (y = 0, 1, 2, ...)."),
                                img(src = "visitante.png",
                                    height = 550, width = 750),
                                h4("Probabilidad de que el equipo que juega en casa anote 'x'
                                  goles y el equipo que juega como visitante anote
                                  'y' goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)."),
                                img(src = "calor.png",
                                    height = 550, width = 750)
                              )
                      ),
                      
                      
                      
                      tabItem(tabName = "data_table",
                              fluidRow(        
                                titlePanel(h3("Tabla con todos los datos")),
                                dataTableOutput ("data_table")
                              )
                      ), 
                      
                      tabItem(tabName = "momios",
                              fluidRow(
                                titlePanel(h3("Factores de ganancia")),
                                p("Factores de ganancia máximo y promedio después de una secuencia de juegos."),
                                h4("Factor de ganancia máximo"),
                                img( src = "max.png",
                                     height = 550, width = 750),
                                h4("Factor de ganancia promedio"),
                                img( src = "prom.png",
                                     height = 550, width = 750)
                              )
                      ),
                      
                      tabItem(tabName = "info",
                              fluidRow(
                                titlePanel(h3("Integrantes del equipo")),
                                tags$a(href="https://github.com/sebastianzacarias", "Diego Zacarías"), br(),
                                tags$a(href="https://github.com/BaltaV", "Baltasar Vez"), br(),
                                tags$a(href="https://github.com/f-val15", "Francisco Valerio"), br(),
                                tags$a(href="https://github.com/axiom-of-choice", "Isaac Hernández"), br(),
                                tags$a(href="https://github.com/gutierrezrd90", "Daniel Gutiérrez")
                              )
                      )
                      
                    )
                  )
    )
  )

server <- function(input, output) {
  library(ggplot2)
  
  #Gráfico de barras
  output$plot1 <- renderPlot({
    
    data <-  read.csv("match.data.csv", header = T)
    
    data <- mutate(data, FTR = ifelse(home.score > away.score, "H", ifelse(home.score < away.score, "A", "D")))
    
    x <- data[,input$x]
    
    #summary(data)
    data %>% ggplot(aes(x, fill = FTR)) + 
      geom_bar() + 
      facet_wrap("away.team") +
      labs(x =input$x, y = "Goles") + 
      ylim(0,50)
    
    
  })
  
  # Gráficas de dispersión
  output$output_momios <- renderPlot({ 
    
    ggplot(mtcars, aes(x =  mtcars[,input$a] , y = mtcars[,input$y], 
                       colour = mtcars[,input$z] )) + 
      geom_point() +
      ylab(input$y) +
      xlab(input$x) + 
      theme_linedraw() + 
      facet_grid(input$z)  #selección del grid
    
  })   
  
  #Data Table
  output$data_table <- renderDataTable( {data}, 
                                        options = list(aLengthMenu = c(10,25,50),
                                                       iDisplayLength = 10)
  )
  
}


shinyApp(ui, server)