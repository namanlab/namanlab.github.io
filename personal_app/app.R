# Install and load necessary packages
# install.packages(c("shiny", "shinydashboard", "plotly", "cluster"))
library(shiny)
library(shinydashboard)
library(plotly)
library(cluster)
library(tidyverse)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "K-Means Clustering App"),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Scatterplot", tabName = "scatter"),
      menuItem("K-Means", tabName = "kmeans")
    ),
    selectInput("dfgen", label = "Choose Data: ", choices = c("Non-spherical", "Uneven-size"))
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "scatter",
        fluidRow(
          plotlyOutput("scatterplot")
        )
      ),
      tabItem(
        tabName = "kmeans",
        fluidRow(
          box(
            title = "K-Means Clustering",
            status = "primary",
            solidHeader = TRUE,
            width = 10,
            sliderInput("k", "Number of clusters:", min = 1, max = 10, value = 3),
            actionButton("run_kmeans", "Run K-Means"),
            plotlyOutput("cluster_plot"),
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Generate random data for scatterplot
  set.seed(123)
  data <- reactive({
    if(input$dfgen == "Non-spherical"){
      num_points <- 100
      circle_radii_1 <- runif(10, 5, 7)
      circle_radii_2 <- runif(10, 10, 12)
      theta <- seq(0, 2*pi, length.out = num_points)
      d1 <- data.frame(x = circle_radii_1 * cos(theta), y = circle_radii_1 * sin(theta))
      d2 <- data.frame(x = circle_radii_2 * cos(theta), y = circle_radii_2 * sin(theta))
      rbind(d1, d2)
    } else {
      sizes <- c(20, 100, 500)
      centers <- data.frame(x = c(1, 4, 6), y = c(5, 0, 6), n = sizes,
                            cluster = factor(1:3))
      points <- centers %>% group_by(cluster) %>%
        do(data.frame(x = rnorm(.$n, .$x), y = rnorm(.$n, .$y)))
      data.frame(x = points$x, y = points$y)
    }
  })
  
  
  output$scatterplot <- renderPlotly({
    plot_ly(data(), x = ~x, y = ~y, type = 'scatter', mode = 'markers')
  })
  
  # Perform k-means clustering
  kmeans_result <- reactiveVal(NULL)
  observeEvent(input$run_kmeans, {
    k <- input$k
    kmeans_result(kmeans(data(), centers = k))
  })
  
  # Visualize clustering result
  output$cluster_plot <- renderPlotly({
    if (!is.null(kmeans_result())) {
      df <- data()
      df$cluster <- as.factor(kmeans_result()$cluster)
      plot_ly(df, x = ~x, y = ~y, color = ~cluster, type = 'scatter', mode = 'markers')
    }
  })
}


# Run the Shiny app
shinyApp(ui, server)
