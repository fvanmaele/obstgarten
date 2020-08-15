library(shiny)
library(tidyverse)
library(plot3D)
library(rayshader)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(bbplot)

#' Method for generating plots in the shiny app
#' @param datatype choose either gaussian, sine, sine2D, sineclass, or iris
#' for different datasets
#' @param n integer number of samples in the dataset
#' @param d integer number of dimension in the generated dataset
#' @param m RF parameter
#' @param B integer number of independent bootstrap samples
#' @param depth integer depth of the CART generated tree
#' @param display_d integer dimension displayed in multivariate case gaussian
#' @param sd double > 0 inherent noise in the generated data
#' @param k integer specifying field of view in sine2D case
#' @param grid
#'
#' @return print(plot)
rf_plot <- function(datatype, n, d, m, B, depth, display_d=1L, sd=0.1, k=10, grid=NULL) {
  if (datatype == "gaussian") {
    pred_plot_rf(n=n, d=d, m=m, B=B, depth=depth, display_d=display_d, sd=sd)
  }
  else if (datatype == "sine") {
    pred_plot_bagging(depth=depth, B=B, sigma=sd, n=n, random_forest = FALSE, grid=grid)
  }
  else if (datatype == "sine2D") {
    pred_plot_sine2D(n=n, B=B, depth=depth, sd=sd, k=k)
  }
  else if (datatype == "sineclass") {
    pred_plot_bagging_class(B=B, depth=depth, sigma=sd, n=n)
  }
  else if (datatype == "iris") {
    pred_plot_iris_class(depth=depth, B=B)
  }

}

rf_plot(datatype="iris", n=100, d=2, m=1, B=10L, depth=3, display_d = 1, sd=0.25)



#' Starts the Shiny App
#'
#' @description All arguments are selectable in the App
#'
#' @param Number_of_obs number of generated data rows (numeric)
#' @param Sigma standard deviation of irreducible error in y (numeric)
#' @param Random_forest TRUE: random forest, FALSE: bagging (logical)
#' @param Depth depths of the CART generated regression tree (numeric)
#' @param Bootstrap_samples number of bootstrap samples (numeric)
#' @param Simulate starts the calculation with the given parameters (button)
#'
#' @return
#' @export
start_shiny_app <- function(){
  ui <- fluidPage(
    titlePanel("Greedy"),
    sidebarLayout(
      sidebarPanel(
        numericInput('n_greedy', 'Number of obs', 200),
        sliderInput('depth_greedy', 'Depth', 1, 10, 3, 1),
        sliderInput('sigma_greedy', 'Sigma', 0, 0.5, 0.2, 0.05),
        checkboxInput("classification_greedy", "Classification", value=FALSE),
        checkboxInput("random_greedy", "Random forest", value=FALSE),
        actionButton("simulate_greedy", "Simulate CART!")
      ),
      mainPanel(
        plotOutput('plot_greedy'),
      )
    ),

    titlePanel("Bagging Regression"),
    sidebarLayout(
      sidebarPanel(
        numericInput('n_bagging', 'Number of obs', 200),
        sliderInput('B', 'Bootstrap samples', 100, 1000, 100, 100),
        sliderInput('depth_bagging', 'Depth', 1, 10, 3, 1),
        sliderInput('sigma_bagging', 'Sigma', 0, 0.5, 0.2, 0.05),
        checkboxInput("random_bagging", "Random forest", value=FALSE),
        actionButton("simulate_bagging", "Simulate Bagging!")
      ),
      mainPanel(
        plotOutput('plot_bagging')
      )
    ),
    checkboxInput("m_NULL_bagging", "Heuristically motivated values", value=TRUE),
    sliderInput('m_bagging', 'Count of random dimensions (m)', 1, 10, 1, 1)
  )

  server <- function(input, output) {
    # Greedy
    depth_greedy <- eventReactive(input$simulate_greedy, {
      input$depth_greedy
    })
    sigma_greedy <- eventReactive(input$simulate_greedy, {
      input$sigma_greedy
    })
    n_greedy <- eventReactive(input$simulate_greedy, {
      input$n_greedy
    })
    classification_greedy <- eventReactive(input$simulate_greedy, {
      input$classification_greedy
    })
    random_greedy <- eventReactive(input$simulate_greedy, {
      input$random_greedy
    })
    output$plot_greedy <- renderPlot({
      if(classification_greedy()){
        pred_plot_greedy_class(depth=depth_greedy(), n = n_greedy(), sigma = sigma_greedy(), random = random_greedy())
      }
      else{
        pred_plot_greedy(depth=depth_greedy(), n = n_greedy(), sigma = sigma_greedy(), random = random_greedy())
      }
    })

    # Bagging Regression
    B <- eventReactive(input$simulate_bagging, {
      input$B
    })
    depth_bagging <- eventReactive(input$simulate_bagging, {
      input$depth_bagging
    })
    sigma_bagging <- eventReactive(input$simulate_bagging, {
      input$sigma_bagging
    })
    n_bagging <- eventReactive(input$simulate_bagging, {
      input$n_bagging
    })
    random_bagging <- eventReactive(input$simulate_bagging, {
      input$random_bagging
    })
    output$plot_bagging <- renderPlot({
      pred_plot_bagging(depth=depth_bagging(), B=B(), sigma=sigma_bagging(), n=n_bagging(), random_forest=random_bagging())
    })



    m_NULL_bagging <- eventReactive(input$simulate_bagging, {
      input$m_NULL_bagging
    })
    m_bagging <- eventReactive(input$simulate_bagging, {
      input$m_bagging
    })
  }

  return(shinyApp(ui, server))
}

