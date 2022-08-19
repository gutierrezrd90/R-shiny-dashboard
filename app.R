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
          menuItem("Gráficos de barras", tabName = "Dashboard", icon = icon("futbol")),
          menuItem("Goles casa - visitante", tabName = "goles", icon = icon("chart-column")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Factores de ganancia", tabName = "momios", icon = icon("chart-line"))
        ),
        width = 300
        
      ),
      
      dashboardBody(
        
        tabItems(
          
          # Histograma
          tabItem(tabName = "Dashboard",
                  fluidRow(
                    
                    titlePanel("Goles a favor y en contra por equipo"), 
                    selectInput("x", "Seleccione el valor de X",
                                choices = c("home.score", "away.score")),
                    
                    
                    plotOutput("plot1", height = 550, width = 1200)
                    
                    
                  )
          ),
          
          # imágenes
          tabItem(tabName = "goles", 
                  fluidRow(
                    titlePanel(h3("Probabilidad de goles en casa y visitante")),
                    
                    img(src = "casa.png", 
                        height = 550, width = 750) ,
                    img(src = "visitante.png",
                        height = 550, width = 750),
                    img(src = "calor.png",
                        height = 550, width = 750)
                    
                  )
          ),
          
          
          
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ), 
          
          tabItem(tabName = "momios",
                  fluidRow(
                    titlePanel(h3("Imágen de calor para la correlación de las variables")),
                    h3("Factor de ganancia Máximo"),
                    img( src = "max.png", 
                         height = 550, width = 750),
                    h3("Factor de ganancia Promedio"),
                    img( src = "prom.png", 
                         height = 550, width = 750)
                    
                    
                    
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