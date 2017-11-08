# Visualizing the bias-variance tradeoff

Yet another clone to use the idea of <https://theclevermachine.wordpress.com/tag/bias-variance-tradeoff/> to visualize the bias-variance trade-off by fitting polynomials to _sin(x + x^2)_.

If you start the app by

```
library(shiny)
runGitHub("random","adamlenart",subdir="shiny-bias-variance")
``` 

you may see that there two panels. The upper panel shows an example fit for one sample, while the lower panel shows the training and test errors with all the components. R-squared statistics are added as an extra.
