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
#' @param lambda Cost weight
#' @param grid specify data points on x axis
#' @param random_forest TRUE: random forest, FALSE: bagging
#'
#' @return print(plot)
rf_plot <- function(datatype, n, d=5, m=1, B=10L, depth=3, display_d=1L, sd=0.1, k=10, lambda=0.001, grid=NULL, random_forest=FALSE) {
  if (datatype == "gaussian") {
    pred_plot_rf(n=n, d=d, m=m, B=B, depth=depth, display_d=display_d, sd=sd)
  }
  else if (datatype == "sine_bagging") {
    pred_plot_bagging(depth=depth, B=B, sigma=sd, n=n, grid=grid, random_forest=random_forest)
  }
  else if (datatype == "sine2D") {
    pred_plot_sine2D(n=n, B=B, depth=depth, sd=sd, k=k, random_forest=random_forest)
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
  else if (datatype == "pruning") {
    pred_plot_pruning(lambda = lambda, depth=depth, sigma=sd, n=n, random_forest=random_forest, simul=FALSE)
  }
}

# rf_plot(datatype="sine", n=1000, d=5, m=3, B=10L, depth=3, display_d = 1, sd=0.25)



#' Starts the Shiny App
#'
#' Datatype choose a datatype (Dropdown menu)
#' Number_of_obs number of generated data rows (numeric)
#' Dimension dimension of the record (numeric)
#' Display_dimension number of shown dimensions (numeric)
#' m count of random dimensions (numeric)
#' Sigma standard deviation of irreducible error in y (numeric)
#' Random_forest TRUE: random forest, FALSE: bagging (logical)
#' Depth depths of the CART generated regression tree (numeric)
#' Bootstrap_samples number of bootstrap samples (numeric)
#' k borders of the sine2D plot (numeric)
#' lambda Cost weight
#' Simulate starts the calculation with the given parameters (button)
#'
#' @return
#' @rawNamespace import(shiny, except = runExample)
#' @rawNamespace import(shinyjs, except = runExample)
#' @import shinydashboard
#' @export
start_shiny_app <- function(){
  ui <- fluidPage(
    titlePanel(""),
    sidebarLayout(
      sidebarPanel(
        shinyjs::useShinyjs(),
        selectInput("datatype", "Datatype", choices=c("sine_CART", "sineclass_CART", "sine_bagging", "sineclass_bagging", "pruning", "sine2D", "gaussian", "iris")),

        box(id = "sine_CART", width = '800px',
            numericInput('n_sine_CART', 'Number of obs', 200),
            sliderInput('depth_sine_CART', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sine_CART', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_sine_CART", "Random forest", value=FALSE),
        ),
        box(id = "sineclass_CART", width = '800px',
            numericInput('n_sineclass_CART', 'Number of obs', 200),
            sliderInput('depth_sineclass_CART', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sineclass_CART', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_sineclass_CART", "Random forest", value=FALSE),
        ),
        box(id = "sine_bagging", width = '800px',
            numericInput('n_sine_bagging', 'Number of obs', 200),
            sliderInput('B_sine_bagging', 'Number of bootstrap samples', 10, 100, 10, 10),
            sliderInput('depth_sine_bagging', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sine_bagging', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_sine_bagging", "Random forest", value=FALSE),
        ),
        box(id = "sineclass_bagging", width = '800px',
            numericInput('n_sineclass_bagging', 'Number of obs', 200),
            sliderInput('B_sineclass_bagging', 'Number of bootstrap samples', 10, 100, 10, 10),
            sliderInput('depth_sineclass_bagging', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sineclass_bagging', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_sineclass_bagging", "Random forest", value=FALSE),
        ),
        box(id = "sine2D", width = '800px',
            numericInput('n_sine2D', 'Number of obs', 200),
            sliderInput('B_sine2D', 'Number of bootstrap samples', 10, 100, 10, 10),
            sliderInput('depth_sine2D', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_sine2D', 'Sigma', 0, 0.5, 0.2, 0.05),
            sliderInput('k_sine2D', 'Borders of the sine2D plot (k)', 5, 15, 10, 1),
            checkboxInput("random_sine2D", "Random forest", value=FALSE),
        ),
        box(id = "gaussian", width = '800px',
            numericInput('n_gaussian', 'Number of obs', 200),
            sliderInput('d_gaussian', 'Dimension', 1, 10, 3, 1),
            sliderInput('m_gaussian', 'Count of random dimensions (m)', 1, 10, 1, 1),
            sliderInput('B_gaussian', 'Number of bootstrap samples', 10, 100, 10, 10),
            sliderInput('depth_gaussian', 'Tree depth', 1, 10, 3, 1),
            sliderInput('display_d_gaussian', 'Display dimension', 1, 10, 3, 1),
            sliderInput('sigma_gaussian', 'Sigma', 0, 0.5, 0.2, 0.05),
        ),
        box(id = "iris", width = '800px',
            sliderInput('B_iris', 'Number of bootstrap samples', 10, 100, 10, 10),
            sliderInput('depth_iris', 'Tree depth', 1, 10, 3, 1),
        ),
        box(id = "pruning", width = '800px',
            numericInput('n_pruning', 'Number of obs', 200),
            sliderInput('lambda_pruning', 'Lambda', 0, 0.1, 0.002, 0.001),
            sliderInput('depth_pruning', 'Tree depth', 1, 10, 3, 1),
            sliderInput('sigma_pruning', 'Sigma', 0, 0.5, 0.2, 0.05),
            checkboxInput("random_pruning", "Random forest", value=FALSE),
        ),

        actionButton("simulate", "Simulate CART!")
      ),
      mainPanel(
        plotOutput('plot', width = "100%", height = "850px")
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
      if(input$datatype != "sine_bagging"){
        shinyjs::hide(id = "sine_bagging")
      }else{
        shinyjs::show(id = "sine_bagging")
      }
      if(input$datatype != "sineclass_bagging"){
        shinyjs::hide(id = "sineclass_bagging")
      }else{
        shinyjs::show(id = "sineclass_bagging")
      }
      if(input$datatype != "sine2D"){
        shinyjs::hide(id = "sine2D")
      }else{
        shinyjs::show(id = "sine2D")
      }
      if(input$datatype != "gaussian"){
        shinyjs::hide(id = "gaussian")
      }else{
        shinyjs::show(id = "gaussian")
      }
      if(input$datatype != "iris"){
        shinyjs::hide(id = "iris")
      }else{
        shinyjs::show(id = "iris")
      }
      if(input$datatype != "pruning"){
        shinyjs::hide(id = "pruning")
      }else{
        shinyjs::show(id = "pruning")
      }
    })

    #sine_CART
    n_sine_CART <- eventReactive(input$simulate, {input$n_sine_CART})
    depth_sine_CART <- eventReactive(input$simulate, {input$depth_sine_CART})
    sigma_sine_CART <- eventReactive(input$simulate, {input$sigma_sine_CART})
    random_sine_CART <- eventReactive(input$simulate, {input$random_sine_CART})

    #sineclass_CART
    n_sineclass_CART <- eventReactive(input$simulate, {input$n_sineclass_CART})
    depth_sineclass_CART <- eventReactive(input$simulate, {input$depth_sineclass_CART})
    sigma_sineclass_CART <- eventReactive(input$simulate, {input$sigma_sineclass_CART})
    random_sineclass_CART <- eventReactive(input$simulate, {input$random_sineclass_CART})

    #sine_bagging
    n_sine_bagging <- eventReactive(input$simulate, {input$n_sine_bagging})
    B_sine_bagging <- eventReactive(input$simulate, {input$B_sine_bagging})
    depth_sine_bagging <- eventReactive(input$simulate, {input$depth_sine_bagging})
    sigma_sine_bagging <- eventReactive(input$simulate, {input$sigma_sine_bagging})
    random_sine_bagging <- eventReactive(input$simulate, {input$random_sine_bagging})

    #sineclass_bagging
    n_sineclass_bagging <- eventReactive(input$simulate, {input$n_sineclass_bagging})
    B_sineclass_bagging <- eventReactive(input$simulate, {input$B_sineclass_bagging})
    depth_sineclass_bagging <- eventReactive(input$simulate, {input$depth_sineclass_bagging})
    sigma_sineclass_bagging <- eventReactive(input$simulate, {input$sigma_sineclass_bagging})
    random_sineclass_bagging <- eventReactive(input$simulate, {input$random_sineclass_bagging})

    #sine2D
    n_sine2D <- eventReactive(input$simulate, {input$n_sine2D})
    B_sine2D <- eventReactive(input$simulate, {input$B_sine2D})
    depth_sine2D <- eventReactive(input$simulate, {input$depth_sine2D})
    sigma_sine2D <- eventReactive(input$simulate, {input$sigma_sine2D})
    k_sine2D <- eventReactive(input$simulate, {input$k_sine2D})
    random_sine2D <- eventReactive(input$simulate, {input$random_sine2D})

    #gaussian
    n_gaussian <- eventReactive(input$simulate, {input$n_gaussian})
    d_gaussian <- eventReactive(input$simulate, {input$d_gaussian})
    m_gaussian <- eventReactive(input$simulate, {input$m_gaussian})
    B_gaussian <- eventReactive(input$simulate, {input$B_gaussian})
    depth_gaussian <- eventReactive(input$simulate, {input$depth_gaussian})
    display_d_gaussian <- eventReactive(input$simulate, {input$display_d_gaussian})
    sigma_gaussian <- eventReactive(input$simulate, {input$sigma_gaussian})

    #iris
    B_iris <- eventReactive(input$simulate, {input$B_iris})
    depth_iris <- eventReactive(input$simulate, {input$depth_iris})

    #pruning
    n_pruning <- eventReactive(input$simulate, {input$n_pruning})
    lambda_pruning <- eventReactive(input$simulate, {input$lambda_pruning})
    depth_pruning <- eventReactive(input$simulate, {input$depth_pruning})
    sigma_pruning <- eventReactive(input$simulate, {input$sigma_pruning})
    random_pruning <- eventReactive(input$simulate, {input$random_pruning})

    output$plot <- renderPlot({
      if(datatype() == "sine_CART"){
        rf_plot(datatype=datatype(), n=n_sine_CART(), depth=depth_sine_CART(), sd=sigma_sine_CART(), random_forest=random_sine_CART())
      }
      else if(datatype() == "sineclass_CART"){
        rf_plot(datatype=datatype(), n=n_sineclass_CART(), depth=depth_sineclass_CART(), sd=sigma_sineclass_CART(), random_forest=random_sineclass_CART())
      }
      else if(datatype() == "sine_bagging"){
        rf_plot(datatype=datatype(), n=n_sine_bagging(), B=B_sine_bagging(), depth=depth_sine_bagging(), sd=sigma_sine_bagging(), random_forest=random_sine_bagging())
      }
      else if(datatype() == "sineclass_bagging"){
        rf_plot(datatype=datatype(), n=n_sineclass_bagging(), B=B_sineclass_bagging(), depth=depth_sineclass_bagging(), sd=sigma_sineclass_bagging(), random_forest=random_sineclass_bagging())
      }
      else if(datatype() == "sine2D"){
        rf_plot(datatype=datatype(), n=n_sine2D(), B=B_sine2D(), depth=depth_sine2D(), sd=sigma_sine2D(), k=k_sine2D(), random_forest=random_sine2D())
      }
      else if(datatype() == "gaussian"){
        rf_plot(datatype=datatype(), n=n_gaussian(), d=d_gaussian(), m=m_gaussian(), B=B_gaussian(), depth=depth_gaussian(), display_d=display_d_gaussian(), sd=sigma_gaussian())
      }
      else if(datatype() == "iris"){
        rf_plot(datatype=datatype(), B=B_iris(), depth=depth_iris())
      }
      else if(datatype() == "pruning"){
        rf_plot(datatype=datatype(), n=n_pruning(), lambda=lambda_pruning(), depth=depth_pruning(), sd=sigma_pruning(), random_forest=random_pruning())
      }
      else{
        stop("invalid datatype")
      }
    })

  }

  return(shinyApp(ui, server))
}

