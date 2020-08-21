#' plot_3D
#'
#' @description
#' CPU Heavy!!!
#' Method to 3D render Gaussian Multivariates estimates coming from
#' generated samples or predicted values
#' @param df data.frame with columns x, y and z. samples are in the rows
#' @import ggplot2
#' @import rayshader
#' @importFrom grDevices colorRampPalette
plot_3D <- function(df) {
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 1))
  gg <- ggplot(df, aes(x=x, y=y, z=z)) +
    geom_point(aes(x=x, y=y, color=z),size=1) +
    sc

  plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
          zoom = 0.55, phi = 30)
  render_snapshot()
}


#' plot_3D_compare
#'
#' @description
#' Method to plot 3D comparisons of predictions
#' for different methods
#' @param path character specifying path to compare_plot_data file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#' if FALSE only 2D plots are returned
#' @param margin margin of the scale
#'
#' @import dplyr
#' @import ggplot2
#' @import rayshader
#' @import RColorBrewer
#' @import tidyr
#' @importFrom grDevices colorRampPalette
plot_3D_compare <- function(path, render=FALSE, margin=1.0) {
  stopifnot("Path should be a character specifying path to compare_plot_data file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "CART"=CART, "Pruning"=pruning, "Bagging"=bagging, "Random Forest" = rf) %>%
    tidyr::pivot_longer(cols = c(True, CART, Pruning, Bagging, "Random Forest"), names_to = c("pred")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, margin))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~pred)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}


#' plot_3D_compare_DIFF
#'
#' @description
#' Method to plot 3D comparisons of prediction DIFFERENCES
#' between ground truth and model for different methods
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#'  if FALSE only 2D plots are returned
#' @param margin margin of the scale
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @import rayshader
#' @importFrom grDevices colorRampPalette
plot_3D_compare_DIFF <- function(path, margin=1, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  ret %>%
    rename("True"=y, "CART"=CART, "Pruning"=pruning, "Bagging"=bagging, "RandomForest" = rf) %>%
    mutate("Squared Diff. CART"=(True - CART)**2, "Squared Diff. Pruning"=(True - Pruning)**2,
           "Squared Diff. Bagging"=(True - Bagging)**2, "Squared Diff. Random Forest"=(True - RandomForest)**2) %>%
    select(-True, -CART, -Pruning, -Bagging, -RandomForest) %>%
    pivot_longer(cols = c("Squared Diff. CART", "Squared Diff. Pruning", "Squared Diff. Bagging"
                          , "Squared Diff. Random Forest"), names_to = c("diff")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, margin))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~diff)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}


#' plot_3D_compare_m
#'
#' @description
#' Method to plot 3D comparisons of prediction SQUARED DIFFERENCES
#' between ground truth and Random Forest Predictions for
#' different m
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#' if FALSE only 2D plots are returned
#' @param margin margin of the scale
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @import rayshader
#' @import tidyr
#' @importFrom grDevices colorRampPalette
plot_3D_compare_m <- function(path, render=FALSE, margin=1) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  # ret %>%
  #   rename("True"=y, "m=1"=m1, "m=3"=m2, "m=5"=m3, "m=10" = m4) %>%
  #   pivot_longer(cols = c("True", "m=1", "m=3", "m=5", "m=10"), names_to = c("pred")) -> df

  ret %>%
    rename("True"=m4, "m=1"=m1, "m=2"=m2, "m=3"=m3) %>%
    tidyr::pivot_longer(cols = c("True", "m=1", "m=2", "m=3"), names_to = c("pred")) -> df


  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, margin))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~pred)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}


#' plot_3D_compare_m_DIFF
#'
#' @description
#' Method to plot 3D comparisons of prediction DIFFERENCES
#' between ground truth and Random Forest for different m
#' @param path character specifying path to compare_RF_m file
#' @param render logical should 3D plots be rendered? if TRUE HIGH CPU LOAD!!
#'  if FALSE only 2D plots are returned
#' @param margin margin of the scale
#'
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @import rayshader
#' @importFrom grDevices colorRampPalette
plot_3D_compare_m_DIFF <- function(path, margin=0.01, render=FALSE) {
  stopifnot("Path should be a character specifying path to compare_RF_m file!" =
              is.character(path))
  load(path)

  # ret %>%
  #   rename("True"=y, "m1"=m1, "m3"=m2, "m5"=m3, "m10" = m4) %>%
  #   mutate("Squared Diff. m=1"=(True - m1)**2, "Squared Diff. m=3"=(True - m3)**2,
  #          "Squared Diff. m=5"=(True - m5)**2, "Squared Diff. m=10"=(True - m10)**2) %>%
  #   select(-True, -m1, -m3, -m5, -m10) %>%
  #   pivot_longer(cols = c("Squared Diff. m=1", "Squared Diff. m=3", "Squared Diff. m=5"
  #                         , "Squared Diff. m=10"), names_to = c("diff")) -> df

  ret %>%
    rename("True"=m4, "m1"=m1, "m3"=m2, "m5"=m3) %>%
    mutate("Squared Diff. m=1"=(True - m1)**2, "Squared Diff. m=2"=(True - m3)**2,
           "Squared Diff. m=3"=(True - m5)**2) %>%
    select(-True, -m1, -m3, -m5) %>%
    pivot_longer(cols = c("Squared Diff. m=1", "Squared Diff. m=2", "Squared Diff. m=3"), names_to = c("diff")) -> df

  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, margin))

  endp <- ncol(df) - ncol(df) %% 2 - 2

  for (i in seq(1, endp, by=2)) {

    gg <- ggplot(df, aes(x=df[[i]], y=df[[i+1]])) +
      geom_point(aes(x=df[[i]], y=df[[i+1]], color=value),size=1) +
      xlab(str_c("x", i)) +
      ylab(str_c("x", i+1)) +
      sc +
      facet_wrap(~diff)

    print(gg)

    if (render) {
      plot_gg(gg,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
              zoom = 0.55, phi = 30)
      render_snapshot()
    }
  }
}

