

library(gnm)
library(lme4)
library(dlnm)
library(tidyr)
library(dplyr)
library(stringr)
library(splines)
library(lubridate)
library(data.table)

## set up exposure
aq <- fread("/Users/brenna/Downloads/clean_dataset.csv")


case <- read.csv("/Users/brenna/Downloads/clean_case_data.csv")
case$birth_date <- as_date(paste(case$birthccyy,
                                 case$birthmm,
                                 case$birthdd, sep = "-"))

case$conception_date <- case$birth_date - (case$gestation)*7
case$conception_week <- case$week - case$gestation
case <- rename(case, birth_week = week)

head(case)

case <- case |>
  filter(conception_date > "2013-01-01")

# constructing the composite outcome > outcomes related to placental insufficiency
case$pl_insuf <- case_when(case$sga10_by_sex > case$birthweightgrams ~ 1, # sga
                           case$hypertension %in% c("H", "E", "P") ~ 1)
                           # case$placenta_abr == 1 ~ 1)
case$pl_insuf <- ifelse(is.na(case$pl_insuf), 0, 1)

case <- case[, c("grid_id",  "momid", "birth_week",
                 "gestation", "pl_insuf",
                 "conception_week")]

head(case)

grid_weeks <- case[, c("grid_id", "momid",
                       "conception_week",
                       "birth_week", "pl_insuf")]

expand_preg_fx <- function(x) {
  
  seq.int(x["conception_week"], x["birth_week"], by = 1)
  
}

exp_preg <- apply(grid_weeks, 1, expand_preg_fx)

grid_weeks$exp_preg <- exp_preg

grid_weeks <- grid_weeks |>
  separate(exp_preg, paste0("week_", 0:47)) |>
  select(-week_0)

grid_weeks <- grid_weeks |>
  pivot_longer(c("week_1":"week_47"), 
               names_to = "date_type", values_to = "exposure_week")

## getting exposures for each week of each pregnancy

preg_exp <- merge(grid_weeks, aq, 
                  by.x = c("grid_id", "exposure_week"), 
                  by.y = c("grid_id", "week")) |>
  select(-c("V1"))


## plots
preg_exp$pregnancy_week <- gsub("week_", "", preg_exp$date_type)
# library(ggplot2)
# ggplot(preg_exp, aes(x = exposure_week, y = mean_no2)) +
#   geom_point(alpha = 0.01)

early_preg_exp <- preg_exp |>
  mutate(pregnancy_week = as.numeric(pregnancy_week)) |>
  filter(pregnancy_week <= 25)


## build the cross-basis of pm
cb1.pm <- crossbasis(early_preg_exp$max_pm, 
                     lag = 25, 
                     argvar = list(fun="ns", knots = 3), 
                     arglag = list(fun="lin"))
summary(cb1.pm)

## pm
m <- glm(pl_insuf ~ 1 + cb1.pm, # (1 | week)
         data = early_preg_exp, family = "binomial")

pred1.pm <- crosspred(cb1.pm, m, at = 0:200,#0.27:199, 
                      bylag = 1, cumul = TRUE, cen = mean(early_preg_exp$mean_pm))

plot(pred1.pm, col = "blue")

pred1.pm <- crosspred(cb1.pm, m, at = 25, bylag = 1, cumul = TRUE, 
                      cen = mean(early_preg_exp$mean_pm))

plot(pred1.pm, var = 25, xlim = c(0, 25), xlab = "Lag from 25 (weeks)", ylab = "OR for maxpm = 50",
     ylim = c(0.9,1.1), lab = c(8,5,5), col = 1, lwd = 1.5, ci = "area")


## no2
cb1.no2 <- crossbasis(early_preg_exp$max_no2, 
                      lag = 25, 
                      argvar = list(fun = "ns", knots = 4), 
                      arglag = list(fun = "ns", knots = 4))
summary(cb1.no2)

m <- glm(pl_insuf ~ 1 + cb1.no2, # (1 | week)
         data = early_preg_exp, family = "binomial")

pred1.no2 <- crosspred(cb1.no2, m, at=0.1:192, bylag=1, cumul=TRUE)

plot(pred1.no2, col = "orange")

pred1.no2 <- crosspred(cb1.no2, m, at = 50, bylag = 1, cumul = TRUE)

plot(pred1.no2, var = 50, xlim = c(0, 25), xlab = "Lag (weeks)", ylab = "OR for max_no2 = 70",
     ylim = c(0.95, 1.05), lab = c(8,5,5), col = 1, lwd = 1.5, ci = "area")

## temp
cb1.temp <- crossbasis(early_preg_exp$max_temp, 
                      lag = 25, 
                      argvar = list(fun = "lin"), 
                      arglag = list(fun="ns", knots = 3))
summary(cb1.temp)

m <- glm(pl_insuf ~ 1 + cb1.temp, # (1 | week)
         data = early_preg_exp, family = "binomial")
exp(m$coefficients[[1]])
pred1.temp <- crosspred(cb1.temp, m, at=251:301, bylag=1, cumul=TRUE)

plot(pred1.temp, col = "red")

pred1.temp <- crosspred(pred1.temp, m, at = 300, bylag = 1, cumul = TRUE)

plot(pred1.temp, var = 300, xlim = c(0, 20), xlab = "Lag (weeks)", ylab = "HR for 100 WLM/year",
     ylim = c(0.9,1.1), lab = c(8,5,5), col = 1, lwd = 1.5, ci = "area")


## o3
cb1.o3 <- crossbasis(early_preg_exp$mean_o3, 
                       lag = 25, 
                       argvar = list(fun="ns", knots = 4),
                       arglag = list(fun="ns", knots = 3))
summary(cb1.o3)
early_preg_exp$momid <- as.factor(early_preg_exp$momid)
early_preg_exp$exposure_week <- as.factor(early_preg_exp$exposure_week)

# m <- glmer(pl_insuf ~ 1 + (1 | momid) + 1(),# + cb1.o3, 
#          data = early_preg_exp, family = "binomial", link = "log")

m_simple <- glm(pl_insuf ~ 1 + cb1.o3, 
         data = early_preg_exp, family = "binomial")

pred1.o3 <- crosspred(cb1.o3, m, at = 7.5:91, bylag = 1, cumul= TRUE)

plot(pred1.o3, col = "darkolivegreen1")

pred1.o3 <- crosspred(cb1.o3, m, at = 60, bylag = 1, cumul = TRUE)

plot(pred1.o3, var = 60, xlim = c(0, 20), xlab = "Lag (weeks)", ylab = "HR for 100 WLM/year",
     ylim = c(0.9,1.1), lab = c(8,5,5), col = 1, lwd = 1.5, ci = "area")




library(INLA)
cb1.temp <- crossbasis(early_preg_exp$mean_temp, 
                     lag = 25, 
                     argvar = list(fun="lin"),
                     arglag = list(fun="lin"))
pred1.temp <- crosspred(cb1.temp, m_inla, at=247:298, bylag=1, cumul=TRUE)
summary(cb1.temp)

str(cb1.temp)


system.time(
  m_inla <- inla(pl_insuf ~ 1 + f(momid, model = "iid") + 
                   f(exposure_week, model = "iid"),
                 data = early_preg_exp,
                 verbose = FALSE,
                 family = "zeroinflatedbinomial1") # runs in 23m
)

summary(m_inla)

plot(m_inla, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, cex = 1.25)

1 - exp(m_inla$summary.fixed$mean) # predicted: 84%, slight improvement with ZIB_1
prop.table(table(early_preg_exp$pl_insuf)) # observed: 89%
# underfitting zeros


# cbpoll <- crossbasis(data$pollen, lag=3, argvar=list(knots=c(40,100)),
#                      arglag=list(knots=1), group=data$id)
# cbpm <- crossbasis(data$pm, lag=3, arglag=list("integer"), group=data$id)
# cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
#                       arglag=list(knots=1), group=data$id)
# 
# 
# 
# 
# 
