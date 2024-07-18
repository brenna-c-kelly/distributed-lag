
library(MASS)
library(dplyr)
library(moments)
library(performance)

case <- read.csv("/Users/brenna/Downloads/clean_case_data.csv")

# constructing the composite outcome > outcomes related to placental insufficiency
case$pl_insuf <- case_when(case$sga10_by_sex > case$birthweightgrams ~ 1, # sga
                           case$placenta_abr == 1 ~ 1)
case$pl_insuf <- ifelse(is.na(case$pl_insuf), 0, 1)

table(case$pl_insuf)


nu <- aggregate(case$pl_insuf, by = list(case$grid_id, case$week), FUN = sum)
denom <- case |>
  group_by(grid_id, week) |>
  count() |>
  rename(denom = n)
head(denom)

# all_st <- expand.grid(a$Group.1,
#                       a$Group.2)

grid_dat <- data.frame(grid_id = nu$Group.1,
                       week = nu$Group.2,
                       case = nu$x)
grid_dat <- merge(grid_dat, denom, by = c("grid_id", "week"), all.x = TRUE)
head(grid_dat)

m <- glm(case ~ 1 + offset(denom),
         family = "poisson",
         data = grid_dat)
check_zeroinflation(m) # no
check_overdispersion(m) # yes

skewness(grid_dat$denom)
# log transformation, for skewness
grid_dat$denom_log <- log(grid_dat$denom)
skewness(grid_dat$denom_log)
# mm that didn't help much

m <- glm.nb(case ~ 1 + offset(denom),
         data = grid_dat)
check_overdispersion(m) # it's close, but no

summary(m)
exp(m$coefficients)




model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
              family = quasipoisson(), cases_grid)


