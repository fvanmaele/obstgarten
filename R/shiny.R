library(shiny)

#' Starts the Shiny App
#'
#' @description All arguments are selectable in the App
#'
#' @param Number_of_obs number of generated data rows (numeric)
#' @param Sigma standard deviation of irreducible error in y (numeric)
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
        actionButton("simulate_greedy", "Simulate CART!")
      ),
      mainPanel(
        plotOutput('plot_greedy')
      )
    ),

    titlePanel("Bagging Regression"),
    sidebarLayout(
      sidebarPanel(
        numericInput('n_bagging', 'Number of obs', 200),
        sliderInput('B', 'Bootstrap samples', 100, 1000, 100, 100),
        sliderInput('sigma_bagging', 'Sigma', 0, 0.5, 0.2, 0.05),
        actionButton("simulate_bagging", "Simulate Bagging!")
      ),
      mainPanel(
        plotOutput('plot_bagging')
      )
    )
  )

  server <- function(input, output) {
    #data <- reactive({
    #  cart_greedy(generate_sin_data(input$n))
    #})

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
    output$plot_greedy <- renderPlot({
      pred_plot_greedy(depth(), sigma = sigma_greedy(), n = n_greedy())
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
    output$plot_bagging <- renderPlot({
      pred_plot_bagging(B(), sigma = sigma_bagging(), n = n_bagging())
    })
  }

  return(shinyApp(ui, server))
}

