
library(dlnm)
library(tidyr)
library(dplyr)
library(stringr)
library(splines)
library(lubridate)
library(data.table)

# read in data
birth_wf <- read.csv("data/birth_wf mock data.csv")

# exposure-lag-responses
# - trying to manually code an interaction between smoke and PM2.5 by including
#   - a cross-basis for any PM2.5
#   - a cross-basis for wildfire smoke (0 / 1)
#   - a cross-basis for any PM2.5

# wf-specific pm2.5
cb1.wf_pm25 <- crossbasis(birth_wf$max_wf_pm, 
                          lag = 37, 
                          argvar = list(fun = "ns", knots = 4), 
                          arglag = list(fun = "ns", knots = 4))
summary(cb1.wf_pm25)

# smoke
cb1.wf <- crossbasis(birth_wf$smoke,
                     lag = 37,
                     argvar = list(fun = "lin"),
                     arglag = list(fun = "ns", knots = 4))
summary(cb1.wf)

# any PM2.5
cb1.pm25 <- crossbasis(birth_wf$max_pm, 
                       lag = 37, 
                       argvar = list(fun = "ns", knots = 4), 
                       arglag = list(fun = "ns", knots = 4))
summary(cb1.pm25)


# model
model1 <- glm(early_preterm ~ cb1.wf_pm25 + 
                cb1.pm25 + cb1.wf,
              family = "binomial", birth_wf)
summary(model1)
# this runs, and I can see coefficients for each of the crossbases

# I can plot the surfaces for pm2.5 and wildfire-specific pm2.5:
pred1.wf_pm25 <- crosspred(cb1.wf_pm25, model1, at = 0:10*12,#0:120, 
                           bylag = 1, cumul = TRUE, cen = 0)
plot(pred1.wf_pm25, col = "red",
     zlab = "Risk of PTB", 
     ylab = "Gestational week", xlab = "Smoke-specific PM2.5")

pred1.pm25 <- crosspred(cb1.pm25, model1, at = 0:10*20, 
                        bylag = 1, cumul = TRUE, cen = 0)
plot(pred1.pm25, col = "darkorange",
     zlab = "Risk of PTB", 
     ylab = "Gestational week", xlab = "PM2.5")

### but I can't plot the results for wildfire smoke
crosspred(cb1.wf, model1)

# when I remove the wildfire-specific pm2.5 crossbasis, I can get the crosspred for wildfire smoke
model2 <- glm(early_preterm ~ #cb1.wf_pm25 + 
                cb1.pm25 + cb1.wf,
              family = "binomial", birth_wf)

# this runs just fine now:
pred1.wf <- crosspred(cb1.wf, model2)

plot(pred1.wf, col = "darkorange",
     zlab = "Risk of PTB", 
     ylab = "Gestational week", xlab = "PM2.5")

# does the model think, because there's so much correlation between
# variables, that the model is overfit?



# disregarding time-varying exposure effects,
# it seems like this approach to manually coding 
# the interaction should work:

model_intx <- glm(early_preterm ~ max_pm*smoke,
                  data = birth_wf, family = "binomial")
coef(model_intx)
model_manual_intx <- glm(early_preterm ~ max_pm + smoke + max_wf_pm,
                         data = birth_wf, family = "binomial")
coef(model_manual_intx)
# we get the same coefficients

# if I were to use a normal interaction between the crossbases:
#     cb1.pm25 * cb1.wf
# each exposure-lag interacts with each exposure-lag,
# whereas I'm only concerned with the week-wise interaction



