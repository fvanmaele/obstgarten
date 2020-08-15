library(shiny)
library(tidyverse)
library(plot3D)
library(rayshader)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(bbplot)

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

}

# rf_plot(datatype="sine2D", n=1000, d=2, m=1, B=10L, depth=3, display_d = 1, sd=0.05)


# plots prediction of CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
pred_plot_greedy2 <- function(depth, x = NULL, random_forest=FALSE) {
  if (random_forest == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  if(is.null(x)){
    n <- 150
    grid <- seq(0, 1, len=n)
    x <- generate_sin_data(n, grid=grid, sigma=0.25)
  }
  else{
    n <- nrow(x)
    grid <- seq(0, 1, len=n)
  }

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  tree <- cart_greedy(x, depth=depth, random=random_forest)
  pred <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2])) +
    ggtitle(str_c("Prediction of CART Regression Tree with Depth ", depth)) +
    xlab("") +
    ylab("")

  print(gg)

}


# plots prediction of CART generated regression tree
#'
#' @param depth Integer depths of the CART generated regression tree
#' @example pred_plot_greedy(5, sigma=0.25, n=150)
pred_plot_greedy3 <- function(depth, sigma=0.25, n=150, random_forest=FALSE) {
  if (random_forest == TRUE & depth <= 2) {
    stop("Random Forest require depth > 1!")
  }

  grid <- seq(0, 1, len=n)

  predict <- function(x) {
    return(cart_predict(x, node=tree$root))
  }

  if(random_forest){
    stop("not implemented")
  }
  else{
    x <- generate_sin_data(n, grid=grid, sigma=sigma)
  }

  tree <- cart_greedy(x, depth=depth, random=random_forest, m = 1)
  pred <- apply(x[, -ncol(x), drop=FALSE], MARGIN=1, predict) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2])) +
    ggtitle(str_c("Prediction of CART Regression Tree with Depth ", depth)) +
    xlab("") +
    ylab("")

  print(gg)

}


# pred_plot_greedy(depth=3, random=TRUE)


#' plots prediction of Bagging generated regression tree with depth 5
#' and specified number of bootstrap samples B
#'
#' @param B integer number of bootstrap samples
#' @param random_forest logical: TRUE: random forest, FALSE: bagging
#'
#' @example pred_plot_bagging(100, sigma=0.25, n=150)
pred_plot_bagging <- function(B, sigma=0.25, n=150, random_forest = FALSE) {

  grid <- seq(0, 1, len=n)

  x <- generate_sin_data(n, grid=grid, sigma=sigma)
  #TODO data cannot be used for random_forest

  pred <- bagging(B=B, x_train=x, x_test=x, random_forest = random_forest) # predicting with current tree

  df_plot <- data.frame(grid=grid, y=pred)

  gg <- ggplot(data=df_plot, mapping=aes(x=grid, y=pred)) +
    scale_colour_manual("",
                        breaks = c("Prediction", "True"),
                        values = c("Prediction"="Blue", "True"="red")) +
    geom_line(aes(colour="Prediction")) +
    geom_line(aes(x=grid, y=sin(2*pi*grid), colour="True")) +
    geom_point(aes(x=grid, y=x[, 2])) +
    ggtitle(str_c("Prediction of CART Regression Tree with ", B, " Bootstrap Samples")) +
    xlab("") +
    ylab("")

  print(gg)

}


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
    titlePanel("CART Regression"),
    sidebarLayout(
      sidebarPanel(
        numericInput('n_greedy', 'Number of obs', 200),
        sliderInput('depth', 'Depth', 1, 10, 3, 1),
        sliderInput('sigma_greedy', 'Sigma', 0, 0.5, 0.2, 0.05),
        checkboxInput("random_greedy", "Random forest", value=FALSE),
        actionButton("simulate_greedy", "Simulate CART!")
      ),
      mainPanel(
        plotOutput('plot_greedy'),
        verbatimTextOutput("static_table_greedy")
      )
    ),

    titlePanel("Bagging Regression"),
    sidebarLayout(
      sidebarPanel(
        numericInput('n_bagging', 'Number of obs', 200),
        sliderInput('B', 'Bootstrap samples', 100, 1000, 100, 100),
        sliderInput('sigma_bagging', 'Sigma', 0, 0.5, 0.2, 0.05),
        checkboxInput("random_bagging", "Random forest", value=FALSE),
        actionButton("simulate_bagging", "Simulate Bagging!")
      ),
      mainPanel(
        plotOutput('plot_bagging')
      )
    )
  )

  server <- function(input, output) {
    data_greedy <- reactive({
      generate_sin_data(n_greedy(), sigma = sigma_greedy())
    })

    # CART Regression
    depth <- eventReactive(input$simulate_greedy, {
      input$depth
    })
    sigma_greedy <- eventReactive(input$simulate_greedy, {
      input$sigma_greedy
    })
    n_greedy <- eventReactive(input$simulate_greedy, {
      input$n_greedy
    })
    random_greedy <- eventReactive(input$simulate_greedy, {
      input$random_greedy
    })
    output$plot_greedy <- renderPlot({
      pred_plot_greedy2(depth(), x = data_greedy(), random = random_greedy())
    })
    output$static_table_greedy <- renderPrint({
      summary(data_greedy())
    })

    # Bagging Regression
    B <- eventReactive(input$simulate_bagging, {
      input$B
    })
    sigma_bagging <- eventReactive(input$simulate_bagging, {
      input$sigma_bagging
    })
    n_bagging <- eventReactive(input$simulate_bagging, {
      input$n_bagging
    })
    random_bagging <- eventReactive(input$simulate_greedy, {
      input$random_bagging
    })
    output$plot_bagging <- renderPlot({
      pred_plot_bagging(B(), sigma = sigma_bagging(), n = n_bagging(), random_forest = random_bagging())
    })
  }

  return(shinyApp(ui, server))
}

