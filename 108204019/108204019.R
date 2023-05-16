library(shiny)
library(data.table)
library(ggplot2)
library(ggbiplot)
library(ca)
library(MASS)

plotPCA <- function(ir.pca, ir.species, val1, val2){
  
  g <- ggbiplot(ir.pca, choices = c(val1, val2), obs.scale = 1, var.scale = 1, groups = ir.species)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
  print(g)
}

ui <- navbarPage("尤敏米茲夠的hw4",
  tabPanel("PCA",
    fluidPage(
      titlePanel("PCA"),
      tabsetPanel(
       tabPanel("pca plot",
         fluidPage(
           titlePanel("PCA"),
           sidebarLayout(
             sidebarPanel("x axis",
               selectInput("pca1", label = NULL,
                 c("PCA1" = 1, "PCA2" = 2, "PCA3" = 3, "PCA4" = 4)),
               "y axis",
               selectInput("pca2", label = NULL,
                 c("PCA1" = 1, "PCA2" = 2, "PCA3" = 3, "PCA4" = 4)
               )),
             mainPanel(plotOutput("pcaPlot"))
           )
         )),
       tabPanel("result data",
         fluidPage(
           titlePanel("result data"),
           sidebarLayout(
             sidebarPanel("shows the value of each row after PCA"),
             mainPanel(dataTableOutput("pcaResult"))
           )
         )),
       tabPanel("input data(log)",
         fluidPage(
           titlePanel("input data(log)"),
           sidebarLayout(
             sidebarPanel("log iris data"),
             mainPanel(dataTableOutput("pcaLog"))
           )
         )
       ),
       tabPanel("extended results",
         fluidPage(
           titlePanel("extended results"),
           column(4,
             h3("sdev"),
             tableOutput("sdev")
           ),
           column(4,
             h3("center"),
             tableOutput("center")
           ),
           column(4,
             h3("scale"),
             tableOutput("scale")
           )
         )
       )
      )
    )
  ),
  tabPanel("CA",
    fluidPage(
      titlePanel("CA"),
      tabsetPanel(
        tabPanel("CA plot",
          fluidPage(
            titlePanel("CA plot"),
            sidebarLayout(
              sidebarPanel("kmean clusters",
                sliderInput("kmean", label = NULL, min = 3, max = 11, value = 5, animate = TRUE)
              ),
              mainPanel(plotOutput("caPlot"))
            )
          )
        ),
        tabPanel("extended results")
      )
    )
  ),
  tabPanel("iris data",
    fluidPage(
      sidebarLayout(
        sidebarPanel(),
        mainPanel()
      )
    )
  )
)

# ui <- navbarPage(
#   "尤敏米茲夠的hw4",
#   tabPanel("PCA",
#            fluidPage(
#              titlePanel("PCA"),
#              tabsetPanel(
#                tabPanelBody("pca plot",
#                         sidebarLayout(
#                           sidebarPanel(
#                             # inputs
#                             "x axis",
#                             selectInput("pca1", label = NULL, 
#                                          c("PCA1" = 1,
#                                            "PCA2" = 2,
#                                            "PCA3" = 3,
#                                            "PCA4" = 4)),
#                             "y axis",
#                             selectInput("pca2", label = NULL,
#                                          c("PCA1" = 1,
#                                            "PCA2" = 2,
#                                            "PCA3" = 3,
#                                            "PCA4" = 4))
#                             ),
#                           mainPanel(
#                             # outputs
#                             plotOutput("pcaPlot")
#                             )
#                           )
#                         )
#                )
#            )
#            ),
#   tabPanel("CA", "ca"),
#   tabPanel("iris Data", "data"))

# ir.scale <- scale(iris[, 1:4])
# ir.kmean <- kmeans(iris[, 1:4], centers = 5)
# ir.kmean.table <- table(iris$Species, ir.kmean$cluster)
# ir.ca <- ca(ir.kmean.table, nd = 2)
# plot(ir.ca, arrows = c(FALSE, TRUE))

server <- function(input, output, session) {
  data <- reactive(get("iris", "package:datasets"))
  ir.species <- reactive(iris[, 5])
  ir.log <- reactive(log(iris[, 1:4]))
  ir.pca <- reactive(prcomp(ir.log(),center = TRUE, scale. = TRUE))
  
  output$pcaPlot <- renderPlot({
    val1 <- as.numeric(input$pca1)
    val2 <- as.numeric(input$pca2)
    plotPCA(ir.pca(), ir.species(), val1, val2)
  }, res = 96)
  
  output$pcaResult <- renderDataTable({
    ir.pca()[["x"]]
  })
  
  output$pcaLog <- renderDataTable({
    ir.log()
  })
  
  output$sdev <- renderTable({
    ir.pca()[["sdev"]]
  })
  
  output$center <- renderTable({
    ir.pca()[["center"]]
  })
  
  output$scale <- renderTable({
    ir.pca()[["scale"]]
  })
  
  output$caPlot <- renderPlot({
    ir.scale <- scale(iris[, 1:4])
    ir.kmean <- kmeans(iris[, 1:4], centers = input$kmean)
    ir.kmean.table <- table(iris$Species, ir.kmean$cluster)
    ir.ca <- ca(ir.kmean.table, nd = 2)
    plot(ir.ca, arrows = c(FALSE, TRUE))
  })
}

shinyApp(ui, server)
