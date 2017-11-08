library(tidyr)
library(ggplot2)
# ------------------- function definitions ------------------------ #

# target function
f <- function(x) {
  out <- sin(x + x**2)
  return(out)
}

# calculate mean squared error
get_mse <- function(x,y) {
  out <- mean((x - y)^2)
  return(out)
}

# Prepares the training and test outputs
# @param x a numeric vector containing the input grid
# @param N_TRAIN numeric, it incidicates the number of training examples from the input grid
# @param STD_NOISE numer, a non-negative real number that determines the standard deviation of normal random noise
# @return a list of a train and a test element which contains the training and testing outputs, respectively
output_preparer <- function(x, N_TRAIN, STD_NOISE) {
  # add normally distributed noise
  y = f(x) + rnorm(length(x), 0, STD_NOISE)
  y_train = y[1:N_TRAIN]
  y_test = y[(N_TRAIN + 1) : length(x)]
  return(list(train =  y_train, test = y_test))
}

# Fits a polynomial via ordinary least squares
# @param input a list of train and test elements each containing the training and tesing inputs
# @param output a list of train and test elements each containg the training and testing outputs
# @degree numeric, a positive integer that determines the degree of the polynomial being fit
# @return a list of four elements: fit: a list of fitted training and testing set, mse: a data frame of 
#        mean squared error of the training fit and testing fit and degree, the r-squared statistics and 
#        the degree of the polynomial that was fit
polynomial_fitter <- function(input, output, degree) {
  # prepare inputs and outputs for training
  x <- input$train
  y <- output$train
  x_test <- input$test
  y_test <- output$test
  # fit a polynomial function of the inputs to the outputs
  model = lm( y ~ poly(x, degree))
  model_summary <- summary(model)
  fit_train <- predict(model)
  fit_test <- predict(model, newdata = data.frame(x = x_test))
  # calculate mean squared error
  mse_train = get_mse(fit_train, y)
  mse_test = get_mse(fit_test, y_test)
  return(list(
    fit = list(train = fit_train, test = fit_test),
    mse = data.frame(train = mse_train, test = mse_test),
    rsquared = data.frame(r_squared = model_summary$r.squared, adj_r_squared = model_summary$adj.r.squared),
    degree = degree))
}
# Prepares the data and fits the model
# @param input a list of two elements that contain the training and testing set
# @param degree numeric, the degree of the polynomial being fit
# @param N_TRAIN numeric, the number of training examples out of the full set
# @param STD_NOISE a positive real number that determines the standard deviation of normal random noise
# @return a list of three elements: fit: a list of fitted training and testing set, mse: a data frame of 
#        mean squared error of the training fit and testing fit and degree, the degree of the polynomial that was fit
get_fit <- function(input, degree, N_TRAIN, STD_NOISE) {
  output <- output_preparer(input$full, N_TRAIN, STD_NOISE)
  fit <- polynomial_fitter(input, output, degree)
  return(fit)
  
}

get_errors <- function(fit) {
  tmp <- lapply(FUN = function(x) x$mse, X =  fit)
  tmp_dat <- do.call('rbind', tmp)
  error <- colMeans(tmp_dat)
  return(error)
}

get_bias_squared <-function(fit, x_train) {
  tmp <- lapply(FUN = function(x) x$fit$train, X =  fit)
  tmp_dat <- do.call('rbind', tmp)
  bias_squared <- mean((colMeans(tmp_dat) - f(x_train))**2)
  return(bias_squared)
}

get_variance <- function(fit) {
  tmp <- lapply(FUN = function(x) x$fit$test, X =  fit)
  tmp_dat <- do.call('rbind', tmp)
  variance <- mean(apply(X = tmp_dat, FUN = var, MARGIN = 2))
  return(variance)
}

get_rsquared <- function(fit) {
  tmp <- lapply(FUN = function(x) x$rsquared, X =  fit)
  tmp_dat <- do.call('rbind', tmp)
  rsq <- colMeans(tmp_dat)
  return(rsq)
}


get_results <- function(input, degree, N_REP, N_TRAIN, STD_NOISE) {
  fits <- replicate(N_REP, get_fit(input, degree, N_TRAIN, STD_NOISE), simplify = FALSE)
  error <- get_errors(fits)
  bias_squared <- get_bias_squared(fits, input$train)
  variance <- get_variance(fits)
  rsqs <- get_rsquared(fits)
  results <- data.frame(train_error = error[1], test_error = error[2], bias_squared, variance, r_squared = rsqs[1], adj_r_squared = rsqs[2], degree = degree)
  return(results)
}
show_results <- function(N, N_REP, STD_NOISE, N_TRAIN, MAX_POLY) {
  
  # ------------- prepare container for results ------------------ #
  tmp_results <- data.frame(train_error = NULL, test_error = NULL, bias_squared = NULL, variance = NULL,  r_squared = NULL,
                            adj_r_squared = NULL, degree = NULL)
  # -------------------- fit ---------------------- #
  x_grid = seq(-1  ,1, length = N)
  # permute input to assign randomly to train and test sets 
  x <- sample(x_grid) 
  x_train = x[1:N_TRAIN]
  x_test = x[(N_TRAIN + 1) : length(x)]
  input <- list(train = x_train, test = x_test, full = x)
  
  
  for(degree in 1:MAX_POLY) {
    tmp_results <- rbind(tmp_results, get_results(input, degree, N_REP, N_TRAIN, STD_NOISE))
  }
  
  tmp_results$reducible_error <- tmp_results$bias_squared + tmp_results$variance
  tmp_results$irreducible_error <- STD_NOISE**2
  tmp_results$theoretical_test_error <- tmp_results$reducible_error + tmp_results$irreducible_error
  results <- gather(tmp_results, statistic, value, c('train_error', 'test_error', 'bias_squared', 'variance', 'r_squared', 
                                                     'adj_r_squared','reducible_error','irreducible_error','theoretical_test_error'))
  return(results) 
}

show_example <- function(N, STD_NOISE, N_TRAIN, MAX_POLY) {
  x_grid = seq(-1  ,1, length = N)
  # permute input to assign randomly to train and test sets 
  x <- sample(x_grid) 
  x_train = x[1:N_TRAIN]
  x_test = x[(N_TRAIN + 1) : length(x)]
  input <- list(train = x_train, test = x_test, full = x)
  output <- output_preparer(input$full, N_TRAIN, STD_NOISE)
  res <- data.frame(x = input$train, y = output$train, degree = 'data')
  for(degree in 1:MAX_POLY) {
    fit <- polynomial_fitter(input, output, degree)
    res <- rbind(res, data.frame(x = input$train, y = fit$fit$train, degree = as.character(degree))) 
  }
  return(res)
}

shinyServer(function(input, output) {
  # ---------------- set constants ------------------------------ #
  #N =  input$N # OF OBSERVATIONS PER DATASET
  #N_REP = input$N_REP # OF DATASETS
  #STD_NOISE = input$STD_NOISE # NOISE STANDARDE DEV.
  #RATIO_TRAIN = input$RATIO_TRAIN # RATIO OF TRAINING TO FULL DATA
  #N_TRAIN = ceiling(N*RATIO_TRAIN) # OF TRAINING POINTS
  #MAX_POLY = input$MAX_POLY # MAXIMUM MODEL COMPLEXITY
  plot_results <- reactive({
    show_results(input$N, input$N_REP, input$STD_NOISE, ceiling(input$N*input$RATIO_TRAIN), input$MAX_POLY)
  })
  
  plot_example <- reactive({
    show_example(input$N, input$STD_NOISE, ceiling(input$N*input$RATIO_TRAIN), input$MAX_POLY) 
  })
  output$plot1 <- renderPlotly({
    gp  <- ggplot(data = subset(plot_example(), degree != 'data'), aes(x = x, y = y, color = degree))
    gp + geom_line() + theme_bw() + geom_point(data = subset(plot_example(), degree == 'data'), col = 'darkgrey') + 
      theme(legend.position = 'none')
  })
  output$plot2 <- renderPlotly({
  gp <- ggplot(data = plot_results(), aes(x = degree, y = value, color = statistic))
  gp + geom_line() + theme_bw() + scale_x_continuous(breaks = 1:12) 
  })
})

