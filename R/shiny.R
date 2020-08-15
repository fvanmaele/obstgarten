library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(plot3D)
library(rayshader)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(bbplot)

#rf_plot <- function(datatype, n=500, d=2, m=1, B=10L, depth=3, display_d=1L, sd=0.1, k=10, grid=NULL, random_forest=FALSE) {

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
rf_plot <- function(datatype, n, d, m, B, depth, display_d=1L, sd=0.1, k=10, grid=NULL, random_forest) {
  if (datatype == "gaussian") {
    pred_plot_rf(n=n, d=d, m=m, B=B, depth=depth, display_d=display_d, sd=sd)
  }
  else if (datatype == "sine_bagging") {
    pred_plot_bagging(depth=depth, B=B, sigma=sd, n=n, grid=grid)
  }
  else if (datatype == "sine2D") {
    pred_plot_sine2D(n=n, B=B, depth=depth, sd=sd, k=k)
  }
  else if (datatype == "sineclass_bagging") {
    pred_plot_bagging_class(B=B, depth=depth, sigma=sd, n=n, random_forest=random_forest)
  }
  else if (datatype == "sine_CART") {
    pred_plot_greedy(depth=depth, n=n, sigma=sd,random=random_forest)
  }
  else if (datatype == "sineclass_CART") {
    pred_plot_greedy_class(depth=depth, n=n, sigma=sd,random=random_forest)
  }
  else if (datatype == "iris") {
    pred_plot_iris_class(depth=depth, B=B)
  }
}

# rf_plot(datatype="sine", n=1000, d=5, m=3, B=10L, depth=3, display_d = 1, sd=0.25)



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
    titlePanel("Greedy CART"),
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        selectInput("datatype", "Datatype", choices=c("sine_CART", "sineclass_CART", "sine_bagging", "sineclass_bagging", "sine2D", "gaussian")),

        box(id = "sine_CART", width = '800px',
            numericInput('n_sine_CART', 'Number of obs', 200),
            sliderInput('depth_sine_CART', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sine_CART', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_sine_CART", "Random forest", value=FALSE),
        ),
        box(id = "sineclass_CART", width = '800px',
            numericInput('n', 'Number of obs', 200),
            sliderInput('depth', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random", "Random forest", value=FALSE),
        ),


        #numericInput('n', 'Number of obs', 200),
        #sliderInput('d', 'Dimension', 1, 10, 3, 1),
        #checkboxInput("m_NULL", "Heuristically motivated values", value=TRUE),
        #sliderInput('m', 'Count of random dimensions (m)', 1, 10, 1, 1),
        #sliderInput('B', 'Number of bootstrap samples', 10, 100, 10, 10),
        #sliderInput('depth', 'Tree depth', 1, 10, 3, 1),
        #sliderInput('display_d', 'Display dimension', 1, 10, 3, 1),
        #sliderInput('sigma', 'Sigma', 0, 0.5, 0.2, 0.05),
        #sliderInput('k', 'Borders of the sine2D plot', 1, 10, 3, 1),
        #checkboxInput("classification", "Classification", value=FALSE),
        #checkboxInput("random", "Random forest", value=FALSE),
        actionButton("simulate", "Simulate CART!")
      ),
      mainPanel(
        plotOutput('plot')
      )
    )
  )

  server <- function(input, output) {

    datatype <- eventReactive(input$simulate, {input$datatype})

    observeEvent(input$datatype, {
      if(input$datatype != "sine_CART"){
        shinyjs::hide(id = "sine_CART")
      }else{
        shinyjs::show(id = "sine_CART")
      }
      if(input$datatype != "sineclass_CART"){
        shinyjs::hide(id = "sineclass_CART")
      }else{
        shinyjs::show(id = "sineclass_CART")
      }
    })

    #sine_CART
    n_sine_CART <- eventReactive(input$simulate, {input$n_sine_CART})
    depth_sine_CART <- eventReactive(input$simulate, {input$depth_sine_CART})
    sigma_sine_CART <- eventReactive(input$simulate, {input$sigma_sine_CART})
    random_sine_CART <- eventReactive(input$simulate, {input$random_sine_CART})


    n <- eventReactive(input$simulate, {input$n})
    d <- eventReactive(input$simulate, {input$d})
    m_NULL <- eventReactive(input$simulate, {input$m_NULL})
    m <- eventReactive(input$simulate, {input$m})
    B <- eventReactive(input$simulate, {input$B})
    depth <- eventReactive(input$simulate, {input$depth})
    display_d <- eventReactive(input$simulate, {input$display_d})
    sigma <- eventReactive(input$simulate, {input$sigma})
    k <- eventReactive(input$simulate, {input$k})
    classification <- eventReactive(input$simulate, {input$classification})
    random <- eventReactive(input$simulate, {input$random})

    output$plot <- renderPlot({
      if(datatype() = "sine_CART"){
        rf_plot(datatype=datatype(), n=n(), d=d(), m=m(), B=B(), depth=depth(), display_d=display_d(), sd=sigma(), k=k(), random_forest=random())
      }
      rf_plot(datatype=datatype(), n=n(), d=d(), m=m(), B=B(), depth=depth(), display_d=display_d(), sd=sigma(), k=k(), random_forest=random())
    })

  }

  return(shinyApp(ui, server))
}

