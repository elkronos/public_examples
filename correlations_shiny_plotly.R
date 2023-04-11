# Hosted: https://jchase.shinyapps.io/correlations/
# Git: https://github.com/elkronos/public_examples

# Load packages
library(shiny)
library(ggplot2)
library(plotly)
library(easystats)
library(BayesFactor)
library(see)
library(dplyr)
library(lme4)

# Define user input
ui <- fluidPage(
  titlePanel("Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("conditional_controls")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Scatter Plot", plotlyOutput("scatter_plot")),
        tabPanel("Correlation Plot", plotOutput("correlation_plot"))
      )
    )
  )
)

# Specify functionality
server <- function(input, output) {
  
  data <- reactive({
    iris
  })
  
  output$conditional_controls <- renderUI({
    if (req(input$tabs) == "Scatter Plot") {
      tagList(
        selectInput("x_var", "X Variable:", choices = intersect(names(data())[1], names(data()))),
        selectInput("y_var", "Y Variable:", choices = intersect(names(data())[2], names(data()))),
        selectInput("x_var", "X Variable:", choices = names(data()), selected = names(data())[1]),
        selectInput("y_var", "Y Variable:", choices = names(data()), selected = names(data())[2]),
        selectInput("facet_var", "Facet Variable:", c("", names(data())), selected = ""),
        checkboxInput("add_loess", "Add Loess Line", value = FALSE)
      )
    } else if (req(input$tabs) == "Correlation Plot") {
      tagList(
        selectInput("correlation_type", "Correlation Type:",
                    c("Pearson", "Spearman", "Kendall", "Bayesian"),
                    selected = "Pearson"),
        checkboxInput("include_factors", "Include Factors", value = FALSE),
        checkboxInput("multilevel", "Multi-level Correlation", value = FALSE),
        checkboxInput("partial", "Partial Correlation", value = FALSE)
      )
    }
  })
  
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var)
    req(data())
    
    plot_data <- data()[, c(input$x_var, input$y_var)]
    
    if (!is.null(input$facet_var) && input$facet_var != "") {
      plot_data <- data()[, c(input$x_var, input$y_var, input$facet_var)]
    }
    
    p <- ggplot(plot_data, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point()
    
    if (!is.null(input$facet_var) && input$facet_var != "") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet_var)))
    }
    
    if (input$add_loess) {
      p <- p + geom_smooth(method = "loess", aes(group = if (is.null(input$facet_var) || input$facet_var == "") 1 else input$facet_var))
    }
    
    ggplotly(p)
  })
  
  output$correlation_plot <- renderPlot({
    req(input$correlation_type)
    req(data())
    
    data() %>%
      correlation(method = input$correlation_type,
                  bayesian = input$correlation_type == "Bayesian",
                  include_factors = input$include_factors,
                  multilevel = input$multilevel,
                  partial = input$partial) %>%
      summary(redundant = TRUE) %>%
      plot()
  })
}

# Run app
shinyApp(ui, server)