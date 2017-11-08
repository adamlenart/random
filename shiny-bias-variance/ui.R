library(plotly)

shinyUI(
  fluidPage(
   titlePanel('Bias-variance trade-off'),
   sidebarLayout(
     sidebarPanel(
       fluidRow(column(8, numericInput('N', label = 'Sample size', value = 25))),
       fluidRow(column(8, numericInput('N_REP', label = 'Number of samples', value = 1000))),
       fluidRow(column(8, sliderInput('RATIO_TRAIN', label = 'Training set ratio', value = 0.9, min = 0, max = 1, step = 0.01))),
       fluidRow(column(8, numericInput('STD_NOISE', label = 'Standard deviation of random noise', value = 0.5))),
       fluidRow(column(8, sliderInput('MAX_POLY', label = 'Maximum model complexity (polynomial degree)', 
                                      value = 12, min = 1, max = 30, step = 1))),
       submitButton('Calculate')
       ),
     mainPanel(
       plotlyOutput('plot1'),
       plotlyOutput('plot2')
     )
   )
  )
)