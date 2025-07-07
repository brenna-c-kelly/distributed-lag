
# script purpose: getting acquainted with dlnm functions

library(dlnm)

# we're using daily mortality data along with weather and pollution data for Chicago 1987-2000
data(chicagoNMMAPS)

# crossbasis(x, lag, argvar, arglag)
## builds a cross-basis matrix of the dimensions of the predictor and lags
#    - first arg: complete set of exposure time series data
#    - lag: defines the maximum lag (or lag range, when using a vector l=2)
#    - argvar: list of arguments to be passed to onebasis for generating the basis matrix for predictor
#    - arglag: list of arguments to be passed to onebasis for generating the basis matrix for lag

head(chicagoNMMAPS)
cb1.pm <- crossbasis(chicagoNMMAPS$pm10, 
                     lag = 15, 
                     argvar = list(fun = "lin"), 
                     arglag = list(fun = "poly", degree = 4))

cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag = 3, argvar = list(df = 5), #
                       arglag = list(fun = "strata", breaks = 1))

summary(cb1.pm)
summary(cb1.temp)
# this describes the crossbasis functions and basis for the predictor and lag

library(splines)
model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
                family=quasipoisson(), chicagoNMMAPS)

pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=0.2, cumul=TRUE)


