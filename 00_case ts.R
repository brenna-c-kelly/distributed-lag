

library(gnm)
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

case <- case |>
  filter(conception_date > "2013-01-01")

# constructing the composite outcome > outcomes related to placental insufficiency
case$pl_insuf <- case_when(case$sga10_by_sex > case$birthweightgrams ~ 1, # sga
                           case$placenta_abr == 1 ~ 1)
case$pl_insuf <- ifelse(is.na(case$pl_insuf), 0, 1)

case <- case[, c("grid_id",  "momid", "birth_week",
                 "gestation", "placenta_abr", "sga",
                 "conception_week")]

head(case)

grid_weeks <- case[, c("grid_id", "momid",
                       "conception_week",
                       "birth_week")]

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

head(preg_exp)

early_preg_exp <- preg_exp |>
  mutate(pregnancy_week = as.numeric(pregnancy_week)) |>
  filter(pregnancy_week <= 20)


## build the cross-basis of predictors and lags
cb1.pm <- crossbasis(early_preg_exp$max_pm, 
                     lag = 20, 
                     argvar = list(fun="ns", knots = 4), 
                     arglag = list(fun="ns", knots = 4))
summary(cb1.pm)

# test <- merge(early_preg_exp, case, by = "momid", all.x = TRUE)

m <- glm(case ~ 1 + cb1.pm, # (1 | week)
         data = early_preg_exp, family = "binomial")

pred1.pm <- crosspred(cb1.pm, m, at=2.5:141, bylag=1, cumul=TRUE)

plot(pred1.pm, col = "blue")



cbpoll <- crossbasis(data$pollen, lag=3, argvar=list(knots=c(40,100)),
                     arglag=list(knots=1), group=data$id)
cbpm <- crossbasis(data$pm, lag=3, arglag=list("integer"), group=data$id)
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
                      arglag=list(knots=1), group=data$id)





