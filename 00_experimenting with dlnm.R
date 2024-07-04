


library(dlnm)

# we're using daily mortality data along with weather and pollution data for Chicago 1987-2000
data(chicagoNMMAPS)

# crossbasis(x, lag, argvar, arglag)
## builds a cross-basis matrix of the dimensions of the predictor and lags
#    - x: complete set of exposure time series data
#    - lag: defines the maximum lag (or lag range, when using a vector l=2)
#    - argvar: list of arguments to be passed to onebasis for generating the basis matrix for predictor
#    - arglag: list of arguments to be passed to onebasis for generating the basis matrix for lag


cb1.pm <- crossbasis(chicagoNMMAPS$pm10, 
                     lag = 15, 
                     argvar = list(fun = "lin"), 
                     arglag = list(fun = "poly", degree = 4))

cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag = 3, argvar = list(df = 5), #
                       arglag = list(fun = "strata", breaks = 1))
