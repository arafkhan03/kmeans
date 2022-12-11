library(shiny)
library(tidyverse)
library(ggplot2)
library(ggforce)
vars <- setdiff(names(iris), "Species")

ui <- pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)


server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    # palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
    #           "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    # 
    # par(mar = c(5.1, 4.1, 0, 1))
    # plot(selectedData(),
    #      col = clusters()$cluster,
    #      pch = 20, cex = 3)
    # points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    df = selectedData()
    x<-paste0("`",input$xcol,"`")
    y<-paste0("`",input$ycol,"`")
    df <- df %>%
      mutate(cluster = clusters()$cluster)
    ggplot(df,aes_string(x,y))+
      geom_mark_hull(aes(fill = factor(cluster),colour = factor(cluster)), concavity=10) +
      geom_point(aes(colour = factor(cluster)),size=5)+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()
      )
    
  })
}
shinyApp(ui,server)